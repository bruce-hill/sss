// Logic for compiling Blang blocks
#include <assert.h>
#include <bhash.h>
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
#include "../util.h"

void compile_statement(env_t *env, gcc_block_t **block, ast_t *ast)
{
    check_discardable(env, ast);
    gcc_rvalue_t *val = compile_expr(env, block, ast);
    if (val && *block)
        gcc_eval(*block, ast_loc(env, ast), val);
}

static bl_type_t *predeclare_def_types(env_t *env, ast_t *def)
{
    if (def->tag == StructDef) {
        auto struct_def = Match(def, StructDef);
        istr_t name = struct_def->name;
        NEW_LIST(istr_t, field_names);
        NEW_LIST(bl_type_t*, field_types);
        NEW_LIST(ast_t*, field_defaults);
        if (hashmap_get(env->bindings, name))
            compile_err(env, def, "Something called %s is already defined.", name);
        // This is a placeholder type, whose fields will be populated later.
        // This is necessary because of recursive/corecursive structs.
        bl_type_t *t = Type(StructType, .name=name, .field_names=field_names, .field_types=field_types, .field_defaults=field_defaults);
        gcc_lvalue_t *lval = gcc_global(env->ctx, ast_loc(env, def), GCC_GLOBAL_EXPORTED, gcc_get_ptr_type(gcc_type(env->ctx, CHAR)), name);
        lval = gcc_global_set_initializer_rvalue(lval, gcc_str(env->ctx, name));

        binding_t *b = new(binding_t, .type=Type(TypeType, .type=t), .rval=gcc_rval(lval));
        hashmap_set(env->global_bindings, name, b);
        env_t *struct_env = fresh_scope(env);
        hashmap_set(env->type_namespaces, t, struct_env->bindings);

        foreach (struct_def->definitions, def, _)
            predeclare_def_types(struct_env, *def);
        return t;
    } else if (def->tag == TaggedUnionDef) {
        auto tu_def = Match(def, TaggedUnionDef);
        istr_t tu_name = tu_def->name;
        if (hashmap_get(env->bindings, tu_name))
            compile_err(env, def, "Something called %s is already defined.", tu_name);
        NEW_LIST(bl_type_t*, union_field_types);
        NEW_LIST(istr_t, union_field_names);
        bl_type_t *tag_t = Type(TagType, .name=tu_name, .names=tu_def->tag_names, .values=tu_def->tag_values);
        bl_type_t *union_t = Type(UnionType, .field_names=union_field_names, .field_types=union_field_types, .fields=LIST(gcc_field_t*));
        bl_type_t *t = Type(TaggedUnionType, .name=tu_name, .tag_type=tag_t, .data=union_t);
        gcc_rvalue_t *rval = gcc_str(env->ctx, tu_name);

        hashmap_t *type_ns = get_namespace(env, t);
        type_ns->fallback = env->bindings;

        // Populate union fields
        binding_t *binding = new(binding_t, .type=Type(TypeType, .type=t), .rval=rval);
        hashmap_set(env->global_bindings, tu_name, binding);
        hashmap_set(env->type_namespaces, t, type_ns);

        hashmap_t *tag_namespace = get_namespace(env, tag_t);
        tag_namespace->fallback = type_ns;
        binding_t *tag_binding = new(binding_t, .type=Type(TypeType, .type=tag_t),
                                     .rval=gcc_str(env->ctx, intern_strf("%s.__Tag", tu_name)));
        hashmap_set(type_ns, intern_str("__Tag"), tag_binding);
        hashmap_set(env->type_namespaces, tag_t, tag_namespace);

        gcc_type_t *tag_gcc_t = bl_type_to_gcc(env, tag_t);

        // Bind tag values:
        env_t type_env = *env;
        type_env.bindings = type_ns;
        for (int64_t i = 0, len = length(tu_def->tag_names); i < len; i++) {
            istr_t tag_name = ith(tu_def->tag_names, i);
            gcc_rvalue_t *tag_val = gcc_rvalue_from_long(env->ctx, tag_gcc_t, ith(tu_def->tag_values, i));
            // Bind the tag:
            hashmap_set(tag_namespace, tag_name, new(binding_t, .type=tag_t, .is_constant=true, .rval=tag_val));

            ast_t *field_type_ast = ith(tu_def->tag_types, i);
            if (field_type_ast) {
                bl_type_t *field_t = predeclare_def_types(&type_env, field_type_ast);
                assert(field_t || field_type_ast->tag != StructDef);
                APPEND(union_field_names, tag_name);
                if (!field_t)
                    field_t = parse_type_ast(env, field_type_ast);
                APPEND(union_field_types, field_t);

                // Bind the struct type:
                binding_t *b = new(binding_t, .type=Type(TypeType, .type=field_t), .is_constant=true, .tag_rval=tag_val,
                                   .rval=gcc_str(env->ctx, intern_strf("%s.%s", tu_name, tag_name)));
                hashmap_set(type_ns, tag_name, b);
                // Also visible in the enclosing namespace:
                // hashmap_set(env->bindings, tag_name, b);
            }
        }
        return t;
    } else if (def->tag == UnitDef) {
        auto unit_def = Match(def, UnitDef);
        env->derived_units = new(derived_units_t, 
                                 .derived=Match(unit_def->derived, Num)->units,
                                 .base=Match(unit_def->base, Num)->units,
                                 .ratio=Match(unit_def->base, Num)->n / Match(unit_def->derived, Num)->n,
                                 .next=env->derived_units);
        return NULL;
    } else {
        return NULL;
    }
}

static void populate_def_members(env_t *env, ast_t *def)
{
    if (def->tag == StructDef) {
        auto struct_def = Match(def, StructDef);
        istr_t name = struct_def->name;
        binding_t *binding = hashmap_get(env->bindings, name);
        assert(binding && binding->type->tag == TypeType);
        bl_type_t *t = Match(binding->type, TypeType)->type;
        if (length(struct_def->field_names) == 0)
            compile_err(env, def, "This struct has no fields, which is currently not supported");

        env_t inner_env = *env;
        inner_env.bindings = get_namespace(env, t);

        auto struct_type = Match(t, StructType);
        for (int64_t i = 0, len = LIST_LEN(struct_def->field_names); i < len; i++) {
            APPEND(struct_type->field_names, ith(struct_def->field_names, i));
            ast_t *type = ith(struct_def->field_types, i);
            ast_t *default_val = ith(struct_def->field_defaults, i);
            bl_type_t *ft = type ? parse_type_ast(env, type) : get_type(env, default_val);
            if (ft->tag == VoidType)
                compile_err(env, type ? type : default_val, "This field is a Void type, but that isn't supported for struct members.");
            APPEND(struct_type->field_types, ft);
            APPEND(struct_type->field_defaults, default_val);
        }
        foreach (struct_def->definitions, def, _) {
            populate_def_members(&inner_env, *def);
        }
    } else if (def->tag == TaggedUnionDef) {
        auto tu_def = Match(def, TaggedUnionDef);
        istr_t name = tu_def->name;
        binding_t *binding = hashmap_get(env->bindings, name);
        assert(binding && binding->type->tag == TypeType);
        bl_type_t *t = Match(binding->type, TypeType)->type;
        if (length(tu_def->tag_names) == 0)
            compile_err(env, def, "This tagged union no fields, which is currently not supported");

        env_t inner_env = *env;
        inner_env.bindings = get_namespace(env, t);

        // populate the unioned structs for the tagged data:
        auto union_t = Match(Match(t, TaggedUnionType)->data, UnionType);
        int64_t field_index = 0;
        for (int64_t i = 0, len = length(tu_def->tag_names); i < len; i++) {
            ast_t *field_type_ast = ith(tu_def->tag_types, i);
            if (!field_type_ast)
                continue;

            populate_def_members(&inner_env, field_type_ast);
            bl_type_t *bl_field_type = ith(union_t->field_types, field_index);
            gcc_type_t *field_type = bl_type_to_gcc(env, bl_field_type);
            gcc_field_t *field = gcc_new_field(env->ctx, NULL, field_type, ith(tu_def->tag_names, i));
            APPEND(union_t->fields, field);
            ++field_index;
        }

        bl_type_t *tag_t = Match(t, TaggedUnionType)->tag_type;
        gcc_type_t *tag_gcc_t = bl_type_to_gcc(env, tag_t);

        // Bind any tagged union fields without data (X is Foo.Tag.X)
        for (int64_t i = 0, len = length(tu_def->tag_names); i < len; i++) {
            ast_t *field_type_ast = ith(tu_def->tag_types, i);
            if (field_type_ast) continue;
            gcc_type_t *gcc_tagged_t = bl_type_to_gcc(env, t);
            gcc_struct_t *gcc_tagged_s = gcc_type_if_struct(gcc_tagged_t);
            gcc_field_t *tag_field = gcc_get_field(gcc_tagged_s, 0);
            gcc_rvalue_t *tag_val = gcc_rvalue_from_long(env->ctx, tag_gcc_t, ith(tu_def->tag_values, i));
            gcc_rvalue_t *singleton = gcc_struct_constructor(env->ctx, NULL, gcc_tagged_t, 1, &tag_field, &tag_val);
            binding_t *singleton_binding = new(binding_t, .type=t, .rval=singleton);
            istr_t tag_name = ith(tu_def->tag_names, i);
            hashmap_set(inner_env.bindings, tag_name, singleton_binding);
            // hashmap_set(env->bindings, tag_name, singleton_binding);
        }
    }
}

static void predeclare_def_funcs(env_t *env, ast_t *def)
{
    if (def->tag == FunctionDef) {
        auto fndef = Match(def, FunctionDef);
        bl_type_t *t = get_type(env, def);
        gcc_func_t *func = get_function_def(env, def, fndef->is_exported ? fndef->name : fresh(fndef->name), fndef->is_exported);
        gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
        hashmap_set(env->global_bindings, fndef->name, new(binding_t, .type=t, .func=func, .rval=fn_ptr));
    } else if (def->tag == StructDef || def->tag == Extend) {
        List(ast_t*) members;
        if (def->tag == StructDef) {
            auto struct_def = Match(def, StructDef);
            binding_t *b = hashmap_get(env->bindings, struct_def->name);
            env = get_type_env(env, Match(b->type, TypeType)->type);
            members = struct_def->definitions;
        } else {
            auto extend = Match(def, Extend);
            members = Match(extend->body, Block)->statements;
            env = get_type_env(env, parse_type_ast(env, extend->type));
        }
        foreach (members, member, _) {
            if ((*member)->tag == FunctionDef) {
                auto fndef = Match((*member), FunctionDef);
                bl_type_t *t = get_type(env, *member);
                gcc_func_t *func = get_function_def(env, *member, fresh(fndef->name), true);
                gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
                hashmap_set(env->bindings, fndef->name, new(binding_t, .type=t, .func=func, .rval=fn_ptr));
            } else {
                predeclare_def_funcs(env, *member);
            }
        }
    }
    // TODO: tagged union methods
}

gcc_rvalue_t *_compile_block(env_t *env, gcc_block_t **block, ast_t *ast, bool give_expression)
{
    auto statements = ast->tag == Block ? Match(ast, Block)->statements : LIST(ast_t*, ast);

    // Imports first
    foreach (statements, stmt, _) {
        if ((*stmt)->tag == Use) {
            load_module(env, block, *stmt);
        }
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
            compile_err(env, *stmt, "This code can never be reached because there is an unconditional control flow statement before it.");
        if (stmt == last_stmt && give_expression) {
            bl_type_t *last_t = get_type(env, *stmt);
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
            env->conversions = tmp.conversions;
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

ast_t *globalize_decls(ast_t *block_ast)
{
    auto block = Match(block_ast, Block);
    NEW_LIST(ast_t*, stmts);
    for (int64_t i = 0; i < LIST_LEN(block->statements); i++) {
        ast_t *stmt = LIST_ITEM(block->statements, i);
        if (stmt->tag == Declare)
            stmt = WrapAST(stmt, Declare, .var=Match(stmt, Declare)->var, .value=Match(stmt, Declare)->value, .is_global=true);
        APPEND(stmts, stmt);
    }
    return WrapAST(block_ast, Block, .statements=stmts);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
