// Logic for compiling SSS blocks
#include <assert.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <gc/cord.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>

#include "../ast.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../typecheck.h"
#include "../types.h"
#include "../parse.h"
#include "../util.h"

void compile_statement(env_t *env, gcc_block_t **block, ast_t *ast)
{
    if (is_discardable(env, ast)) {
        gcc_rvalue_t *val = compile_expr(env, block, ast);
        if (val && *block)
            gcc_eval(*block, ast_loc(env, ast), val);
    } else {
        sss_type_t *t = get_type(env, ast);
        bool was_generator = (t->tag == GeneratorType);
        while (t->tag == GeneratorType) t = Match(t, GeneratorType)->generated;
        if (!(t->tag == VoidType || t->tag == AbortType)) {
            if (was_generator)
                compiler_err(env, ast, "This expression can produce a value of type %s but the value is being ignored. If you want to intentionally ignore the value, assign the body of the block to a variable called \"_\".",
                            type_to_string(t));
            else
                compiler_err(env, ast, "This expression has a type of %s but the value is being ignored. If you want to intentionally ignore it, assign the value to a variable called \"_\".",
                            type_to_string(t));
        }
        gcc_rvalue_t *val = compile_expr(env, block, ast);
        if (val && *block)
            gcc_eval(*block, ast_loc(env, ast), val);
    }
}

gcc_func_t *prepare_use(env_t *env, ast_t *ast)
{
    auto use = Match(ast, Use);
    sss_type_t *t = Type(ModuleType, .path=use->path);
    sss_hashmap_t *namespace = hget(env->type_namespaces, type_to_string(t), sss_hashmap_t*);
    binding_t *b = NULL;
    if (namespace) {
        b = hget(namespace, "#load", binding_t*);
    } else {
        namespace = new(sss_hashmap_t, .fallback=env->global_bindings);
        hset(env->type_namespaces, type_to_string(t), namespace);
    }
    // Look up old value to avoid recompiling the same module if it's reimported:
    if (!b) {
        env_t module_env = *env;
        module_env.bindings = namespace;
        gcc_func_t *load_func = gcc_new_func(
            env->ctx, NULL, GCC_FUNCTION_EXPORTED, sss_type_to_gcc(env, t), fresh("load_module"), 0, NULL, 0);
        b = new(binding_t, .type=Type(FunctionType, .arg_types=LIST(sss_type_t*), .ret=t), .func=load_func);
        hset(namespace, "#load", b);

        gcc_block_t *enter_load = gcc_new_block(load_func, fresh("enter_load")),
                    *do_loading = gcc_new_block(load_func, fresh("do_loading")),
                    *finished_loading = gcc_new_block(load_func, fresh("finished_loading"));

        // static is_loaded = false; if (is_loaded) return; is_loaded = true; load...
        gcc_lvalue_t *is_loaded = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, gcc_type(env->ctx, BOOL), fresh("module_was_loaded"));
        gcc_jit_global_set_initializer_rvalue(is_loaded, gcc_rvalue_bool(env->ctx, false));
        gcc_jump_condition(enter_load, NULL, gcc_rval(is_loaded), finished_loading, do_loading);
        gcc_lvalue_t *module_val = gcc_local(load_func, NULL, sss_type_to_gcc(env, t), "_module");
        gcc_return(finished_loading, NULL, gcc_rval(module_val));

        gcc_assign(do_loading, NULL, is_loaded, gcc_rvalue_bool(env->ctx, true));
        sss_file_t *file = use->file ? use->file : sss_load_file(use->path);
        module_env.file = file;
        if (!file) compiler_err(env, ast, "The file %s doesn't exist", use->path);
        ast_t *module_ast = parse_file(file, env->on_err);
        gcc_rvalue_t *exported = compile_block_expr(&module_env, &do_loading, module_ast);
        if (do_loading) {
            if (exported) gcc_eval(do_loading, NULL, exported);
            for (uint32_t i = 1; i <= namespace->count; i++) {
                auto entry = hnth(namespace, i, const char*, binding_t*);
                if (entry->value->func) continue;
                gcc_lvalue_t *global = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, sss_type_to_gcc(env, entry->value->type), fresh(entry->key));
                gcc_assign(do_loading, NULL, global, entry->value->rval);
                hset(namespace, entry->key, new(binding_t, .type=entry->value->type, .rval=gcc_rval(global), .lval=global,
                                                .visible_in_closures=entry->value->visible_in_closures));
            }
            gcc_return(do_loading, NULL, gcc_rval(module_val));
        }
    }
    return b->func;
}

void populate_uses(env_t *env, ast_t *ast)
{
    if (ast->tag == Declare && Match(ast, Declare)->value->tag == Use) {
        auto use = Match(Match(ast, Declare)->value, Use);
        (void)prepare_use(env, Match(ast, Declare)->value);
        hset(env->bindings, Match(Match(ast, Declare)->var, Var)->name,
             new(binding_t, .type=Type(TypeType, .type=Type(ModuleType, .path=use->path))));
    } else if (ast->tag == DocTest) {
        return populate_uses(env, Match(ast, DocTest)->expr);
    }
}

void predeclare_def_types(env_t *env, ast_t *def)
{
    if (def->tag == StructDef) {
        auto struct_def = Match(def, StructDef);
        const char* name = struct_def->name;
        // This is a placeholder type, whose fields will be populated later.
        // This is necessary because of recursive/corecursive structs.
        sss_type_t *t = Type(StructType, .name=name, .field_names=LIST(const char*),
                            .field_types=LIST(sss_type_t*), .field_defaults=LIST(ast_t*));
        binding_t *b = new(binding_t, .type=Type(TypeType, .type=t), .visible_in_closures=true);
        if (hget(env->bindings, name, binding_t*))
            compiler_err(env, def, "The name '%s' is already being used by something else", name);
        hset(env->bindings, name, b);
        env_t *struct_env = fresh_scope(env);
        hset(env->type_namespaces, name, struct_env->bindings);

        foreach (struct_def->definitions, def, _)
            predeclare_def_types(struct_env, *def);
    } else if (def->tag == TaggedUnionDef) {
        auto tu_def = Match(def, TaggedUnionDef);
        const char* name = tu_def->name;
        sss_type_t *t = Type(TaggedUnionType, .name=name, .members=LIST(sss_tagged_union_member_t));
        binding_t *b = new(binding_t, .type=Type(TypeType, .type=t), .visible_in_closures=true);
        if (hget(env->bindings, name, binding_t*))
            compiler_err(env, def, "The name '%s' is already being used by something else", name);
        hset(env->bindings, name, b);
    } else if (def->tag == UnitDef) {
        auto unit_def = Match(def, UnitDef);
        env->derived_units = new(derived_units_t, 
                                 .derived=Match(unit_def->derived, Num)->units,
                                 .base=Match(unit_def->base, Num)->units,
                                 .ratio=Match(unit_def->base, Num)->n / Match(unit_def->derived, Num)->n,
                                 .next=env->derived_units);
    } else if (def->tag == DocTest) {
        return predeclare_def_types(env, Match(def, DocTest)->expr);
    }
}

void populate_def_members(env_t *env, ast_t *def)
{
    if (def->tag == StructDef) {
        auto struct_def = Match(def, StructDef);
        const char* name = struct_def->name;
        binding_t *binding = get_binding(env, name);
        assert(binding && binding->type->tag == TypeType);
        sss_type_t *t = Match(binding->type, TypeType)->type;

        env_t inner_env = *env;
        inner_env.bindings = get_namespace(env, t);

        auto struct_type = Match(t, StructType);
        sss_hashmap_t used_names = {0};
        for (int64_t i = 0, len = LIST_LEN(struct_def->field_names); i < len; i++) {
            const char *name = ith(struct_def->field_names, i);
            if (hget(&used_names, name, bool))
                compiler_err(env, def, "This struct has a duplicated field name: '%s'", name);
            hset(&used_names, name, true);
            APPEND(struct_type->field_names, name);
            ast_t *type = ith(struct_def->field_types, i);
            ast_t *default_val = ith(struct_def->field_defaults, i);
            sss_type_t *ft = type ? parse_type_ast(env, type) : get_type(env, default_val);
            if (ft->tag == VoidType)
                compiler_err(env, type ? type : default_val, "This field is a Void type, but that isn't supported for struct members.");
            if (ft->tag == PointerType && Match(ft, PointerType)->is_stack)
                compiler_err(env, type ? type : default_val, "Structs are not allowed to hold stack references, because the struct might outlive the stack frame.");
            APPEND(struct_type->field_types, ft);
            APPEND(struct_type->field_defaults, default_val);
        }
        binding->rval = gcc_str(env->ctx, type_to_string(t));
        foreach (struct_def->definitions, def, _) {
            populate_def_members(&inner_env, *def);
        }
    } else if (def->tag == TaggedUnionDef) {
        auto tu_def = Match(def, TaggedUnionDef);
        const char* tu_name = tu_def->name;
        binding_t *binding = get_binding(env, tu_name);
        assert(binding && binding->type->tag == TypeType);
        sss_type_t *t = Match(binding->type, TypeType)->type;
        auto members = Match(t, TaggedUnionType)->members;
        sss_hashmap_t used_names = {0};
        for (int64_t i = 0; i < length(tu_def->tag_names); i++) {
            ast_t *member_type_ast = ith(tu_def->tag_types, i);
            sss_type_t *member_t = member_type_ast ? parse_type_ast(env, member_type_ast) : NULL;
            if (member_t && member_t->tag == PointerType && Match(member_t, PointerType)->is_stack)
                compiler_err(env, member_type_ast, "Tagged unions are not allowed to hold stack references, because the tagged union might outlive the stack frame.");
            const char *name = ith(tu_def->tag_names, i);
            if (hget(&used_names, name, bool))
                compiler_err(env, def, "This definition has a duplicated field name: '%s'", name);
            hset(&used_names, name, true);
            sss_tagged_union_member_t member = {
                .name=ith(tu_def->tag_names, i),
                .tag_value=ith(tu_def->tag_values, i),
                .type=member_t,
            };
            APPEND_STRUCT(members, member);
        }

        sss_hashmap_t *type_ns = get_namespace(env, t);
        type_ns->fallback = env->bindings;

        gcc_type_t *gcc_tagged_t = sss_type_to_gcc(env, t);
        gcc_struct_t *gcc_tagged_s = gcc_type_if_struct(gcc_tagged_t);
        gcc_field_t *tag_field = gcc_get_field(gcc_tagged_s, 0);
        gcc_type_t *tag_gcc_t = get_tag_type(env, t);
        gcc_field_t *union_field = gcc_get_field(gcc_tagged_s, 1);
        gcc_type_t *union_gcc_t = get_union_type(env, t);
        for (int64_t i = 0; i < length(members); i++) {
            auto member = ith(members, i);
            // Constructor:
            if (member.type) {
                gcc_param_t *params[] = {
                    gcc_new_param(env->ctx, NULL, sss_type_to_gcc(env, member.type), fresh(member.name)),
                };
                gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, sss_type_to_gcc(env, t),
                                                fresh(member.name), 1, params, 0);
                gcc_block_t *func_body = gcc_new_block(func, fresh("constructor"));
                assert(gcc_get_union_field(union_gcc_t, i));
                gcc_return(func_body, NULL, gcc_struct_constructor(
                        env->ctx, NULL, gcc_tagged_t, 2, (gcc_field_t*[]){
                            tag_field,
                            union_field,
                        }, (gcc_rvalue_t*[]){
                            gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value),
                            gcc_union_constructor(env->ctx, NULL, union_gcc_t, gcc_get_union_field(union_gcc_t, i), gcc_param_as_rvalue(params[0])),
                        }));
                hset(type_ns, member.name, new(binding_t, .type=Type(FunctionType, .arg_types=LIST(sss_type_t*, member.type),
                                                                     .arg_names=LIST(const char*, member.name),
                                                                     .ret=t),
                                               .visible_in_closures=true,
                                               .func=func, .rval=gcc_get_func_address(func, NULL)));
            } else {
                gcc_rvalue_t *val = gcc_struct_constructor(
                    env->ctx, NULL, gcc_tagged_t, 1, &tag_field, (gcc_rvalue_t*[]){
                        gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value),
                    });
                hset(type_ns, member.name, new(binding_t, .type=t, .rval=val, .visible_in_closures=true));
            }
        }
        binding->rval = gcc_str(env->ctx, tu_name);

        // Populate union fields
        hset(env->type_namespaces, tu_name, type_ns);
    } else if (def->tag == DocTest) {
        return populate_def_members(env, Match(def, DocTest)->expr);
    }
}

void predeclare_def_funcs(env_t *env, ast_t *def)
{
    if (def->tag == FunctionDef) {
        auto fndef = Match(def, FunctionDef);
        gcc_func_t *func = get_function_def(env, def, fresh(fndef->name));
        binding_t *b =  new(binding_t, .type=get_type(env, def),
                            .func=func, .rval=gcc_get_func_address(func, NULL),
                            .visible_in_closures=true);
        hset(env->bindings, fndef->name, b);
    } else if (def->tag == StructDef || def->tag == Extend) {
        List(ast_t*) members;
        if (def->tag == StructDef) {
            auto struct_def = Match(def, StructDef);
            binding_t *b = get_binding(env, struct_def->name);
            env = get_type_env(env, Match(b->type, TypeType)->type);
            members = struct_def->definitions;
        } else {
            auto extend = Match(def, Extend);
            members = Match(extend->body, Block)->statements;
            env = get_type_env(env, parse_type_ast(env, extend->type));
        }
        foreach (members, member, _) {
            if ((*member)->tag == FunctionDef) {
                auto fndef = Match(*member, FunctionDef);
                gcc_func_t *func = get_function_def(env, *member, fresh(fndef->name));
                binding_t *b =  new(binding_t, .type=get_type(env, (*member)),
                                    .func=func, .rval=gcc_get_func_address(func, NULL),
                                    .visible_in_closures=true);
                hset(env->bindings, fndef->name, b);
            } else {
                predeclare_def_funcs(env, *member);
            }
        }
    } else if (def->tag == DocTest) {
        return predeclare_def_funcs(env, Match(def, DocTest)->expr);
    }
    // TODO: tagged union methods
}

gcc_rvalue_t *_compile_block(env_t *env, gcc_block_t **block, ast_t *ast, bool give_expression)
{
    auto statements = ast->tag == Block ? Match(ast, Block)->statements : LIST(ast_t*, ast);

    // Design constraints:
    // - Struct/tagged union members can be struct/tagged union values defined earlier in the file
    // - Struct/tagged union members can be pointers to structs/tagged union defined *later* in the file
    // - Structs can define inner classes, which can be referenced by other classes
    // - Structs can define inner methods, which can be referenced inside the bodies of functions
    // - Function arguments can be struct/tagged union values/pointers defined anywhere in the file
    // - Function bodies can have references to functions declared anywhere in the file (corecursion)
    // Therefore the order of operations is:
    // 1) Predeclare all structs/tagged union with placeholder opaque structs/tagged union
    //    1B) Also predeclare all struct/tagged union 
    // 2) Populate all struct/tagged union members
    // 3) Predeclare all functions
    //    3B) Also predeclare all inner methods
    // 4) Populate all function bodies
    //    4B) Also populate all inner method bodies
    // 5) Compile each statement

    // Handle 'use' imports
    foreach (statements, stmt, _) {
        populate_uses(env, *stmt);
    }
    // Struct and tagged union defs are visible in the entire block (allowing corecursive structs)
    foreach (statements, stmt, _) {
        predeclare_def_types(env, *stmt);
    }
    // Populate struct fields:
    foreach (statements, stmt, _) {
        populate_def_members(env, *stmt);
    }
    // Function defs are visible in the entire block (allowing corecursive funcs)
    foreach (statements, stmt, _) {
        predeclare_def_funcs(env, *stmt);
    }

    defer_t *prev_deferred = env->deferred;
    gcc_rvalue_t *ret = NULL;
    foreach (statements, stmt, last_stmt) {
        if (!*block)
            compiler_err(env, *stmt, "This code can never be reached because there is an unconditional control flow statement before it.");
        if (stmt == last_stmt && give_expression) {
            sss_type_t *last_t = get_type(env, *stmt);
            if (last_t->tag == VoidType || last_t->tag == AbortType)
                give_expression = false;
        }
        if (stmt == last_stmt && give_expression) {
            ret = compile_expr(env, block, *stmt);
        } else {
            env_t tmp = *env;
            tmp.comprehension_callback = NULL;
            compile_statement(&tmp, block, *stmt);
            env->derived_units = tmp.derived_units;
            env->deferred = tmp.deferred;
        }
    }

    insert_defers(env, block, prev_deferred);

    return ret;
}

gcc_rvalue_t *compile_block_expr(env_t *env, gcc_block_t **block, ast_t *ast)
{
    return _compile_block(env, block, ast, true);
}

void compile_block_statement(env_t *env, gcc_block_t **block, ast_t *ast)
{
    if (!ast) return;
    gcc_rvalue_t *rval = _compile_block(env, block, ast, env->comprehension_callback != NULL);
    if (rval && *block) gcc_eval(*block, NULL, rval);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
