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
                compiler_err(env, ast, "This expression can produce a value of type %T but the value is being ignored. If you want to intentionally ignore the value, assign the body of the block to a variable called \"_\".", t);
            else
                compiler_err(env, ast, "This expression has a type of %T but the value is being ignored. If you want to intentionally ignore it, assign the value to a variable called \"_\".", t);
        }
        gcc_rvalue_t *val = compile_expr(env, block, ast);
        if (val && *block)
            gcc_eval(*block, ast_loc(env, ast), val);
    }
}

void predeclare_def_types(env_t *env, ast_t *def, bool lazy)
{
    if (def->tag == TypeDef) {
        const char *name = Match(def, TypeDef)->name;
        if (Table_gets(env->bindings, name))
            compiler_err(env, def, "The name '%s' is already being used by something else", name);

        ast_t *type_ast = Match(def, TypeDef)->type;
        auto definitions = Match(def, TypeDef)->definitions;

        sss_type_t *t;
        if (type_ast->tag == TypeStruct) {
            // This is a placeholder type, whose fields will be populated later.
            // This is necessary because of recursive/corecursive structs.
            t = Type(StructType, .field_names=EMPTY_ARRAY(const char*),
                     .field_types=EMPTY_ARRAY(sss_type_t*), .field_defaults=EMPTY_ARRAY(ast_t*));
        } else if (type_ast->tag == TypeTaggedUnion) {
            auto tu = Match(type_ast, TypeTaggedUnion);
            t = Type(TaggedUnionType, .members=EMPTY_ARRAY(sss_tagged_union_member_t), .tag_bits=tu->tag_bits);
        } else {
            t = parse_type_ast(env, type_ast);
        }
        t = Type(VariantType, .name=name, .filename=sss_get_file_pos(def->file, def->start), .variant_of=t);
        binding_t *b = new(binding_t, .type=Type(TypeType, .type=t), .visible_in_closures=true);
        Table_sets(env->bindings, name, b);
        env_t *type_env = get_type_env(env, t);
        type_env->bindings->fallback = env->bindings;
        foreach (definitions, def, _)
            predeclare_def_types(type_env, *def, lazy);
    } else if (def->tag == UnitDef) {
        auto unit_def = Match(def, UnitDef);
        env->derived_units = new(derived_units_t, 
                                 .derived=Match(unit_def->derived, Num)->units,
                                 .base=Match(unit_def->base, Num)->units,
                                 .ratio=Match(unit_def->base, Num)->n / Match(unit_def->derived, Num)->n,
                                 .next=env->derived_units);
    } else if (def->tag == DocTest) {
        predeclare_def_types(env, Match(def, DocTest)->expr, lazy);
    }
}

void populate_tagged_union_constructors(env_t *env, sss_type_t *t)
{
    env = get_type_env(env, t);
    auto members = Match(base_variant(t), TaggedUnionType)->members;
    gcc_type_t *gcc_tagged_t = sss_type_to_gcc(env, t);
    gcc_struct_t *gcc_tagged_s = gcc_type_if_struct(gcc_tagged_t);
    gcc_field_t *tag_field = gcc_get_field(gcc_tagged_s, 0);
    gcc_type_t *tag_gcc_t = get_tag_type(env, t);
    gcc_field_t *union_field = gcc_get_field(gcc_tagged_s, 1);
    gcc_type_t *union_gcc_t = get_union_type(env, t);
    for (int64_t i = 0; i < LENGTH(members); i++) {
        auto member = ith(members, i);
        // Constructor:
        if (member.type && member.type->tag == StructType && LENGTH(Match(member.type, StructType)->field_types) > 0) {
            gcc_type_t *member_gcc_t = sss_type_to_gcc(env, member.type);
            auto names = Match(member.type, StructType)->field_names;
            auto types = Match(member.type, StructType)->field_types;
            auto defaults = Match(member.type, StructType)->field_defaults;
            gcc_param_t *params[LENGTH(names)] = {};
            gcc_field_t *fields[LENGTH(names)] = {};
            gcc_rvalue_t *field_rvals[LENGTH(names)] = {};
            for (int64_t i = 0; i < LENGTH(names); i++) {
                params[i] = gcc_new_param(env->ctx, NULL, sss_type_to_gcc(env, ith(types, i)), ith(names, i));
                fields[i] = gcc_get_field(gcc_type_if_struct(member_gcc_t), i);
                field_rvals[i] = gcc_param_as_rvalue(params[i]);
            }
            gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_ALWAYS_INLINE, sss_type_to_gcc(env, t),
                                            fresh(member.name), LENGTH(names), params, 0);
            gcc_block_t *func_body = gcc_new_block(func, fresh("constructor"));
            assert(gcc_get_union_field(union_gcc_t, i));
            gcc_rvalue_t *struct_val = gcc_struct_constructor(env->ctx, NULL, member_gcc_t, LENGTH(names), fields, field_rvals);
            gcc_return(func_body, NULL, gcc_struct_constructor(
                    env->ctx, NULL, gcc_tagged_t, 2, (gcc_field_t*[]){
                        tag_field,
                        union_field,
                    }, (gcc_rvalue_t*[]){
                        gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value),
                        gcc_union_constructor(env->ctx, NULL, union_gcc_t, gcc_get_union_field(union_gcc_t, i), struct_val),
                    }));
            Table_sets(env->bindings, member.name,
                 new(binding_t, .type=Type(FunctionType, .arg_names=names, .arg_types=types, .arg_defaults=defaults, .ret=t),
                     .visible_in_closures=true,
                     .func=func, .rval=gcc_get_func_address(func, NULL)));
        } else {
            gcc_rvalue_t *val = gcc_struct_constructor(
                env->ctx, NULL, gcc_tagged_t, 1, &tag_field, (gcc_rvalue_t*[]){
                    gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value),
                });
            Table_sets(env->bindings, member.name, new(binding_t, .type=t, .rval=val, .visible_in_closures=true));
        }
    }
}

void populate_def_members(env_t *env, ast_t *def)
{
    if (def->tag == TypeDef) {
        const char *name = Match(def, TypeDef)->name;
        binding_t *binding = get_binding(env, name);
        assert(binding && binding->type->tag == TypeType);

        sss_type_t *t = Match(binding->type, TypeType)->type;
        env_t *inner_env = get_type_env(env, t);
        inner_env->bindings->fallback = env->bindings;

        ast_t *type_ast = Match(def, TypeDef)->type;
        if (type_ast->tag == TypeStruct) {
            auto struct_type = Match(base_variant(t), StructType);
            table_t used_names = {0};
            auto struct_def = Match(type_ast, TypeStruct);
            for (int64_t i = 0, len = LENGTH(struct_def->members.names); i < len; i++) {
                const char *name = ith(struct_def->members.names, i);
                if (Table_gets(&used_names, name))
                    compiler_err(env, def, "This struct has a duplicated field name: '%s'", name);
                Table_sets(&used_names, name, (void*)true);
                append(struct_type->field_names, name);
                ast_t *type = ith(struct_def->members.types, i);
                ast_t *default_val = struct_def->members.defaults ? ith(struct_def->members.defaults, i) : NULL;
                sss_type_t *ft = type ? parse_type_ast(env, type) : get_type(env, default_val);
                if (ft->tag == VoidType)
                    compiler_err(env, type ? type : default_val, "This field is a Void type, but that isn't supported for struct members.");
                if (ft->tag == PointerType && Match(ft, PointerType)->is_stack)
                    compiler_err(env, type ? type : default_val, "Structs are not allowed to hold stack references, because the struct might outlive the stack frame.");
                append(struct_type->field_types, ft);
                append(struct_type->field_defaults, default_val);
            }
        } else if (type_ast->tag == TypeTaggedUnion) {
            auto members = Match(base_variant(t), TaggedUnionType)->members;
            auto tu_def = Match(type_ast, TypeTaggedUnion);
            table_t used_names = {0};
            for (int64_t i = 0; i < LENGTH(tu_def->tag_names); i++) {
                args_t args = ith(tu_def->tag_args, i);
                sss_type_t *member_t = parse_type_ast(env, WrapAST(def, TypeStruct, .members=args));
                if (member_t && member_t->tag == PointerType && Match(member_t, PointerType)->is_stack)
                    compiler_err(env, def, "Tagged unions are not allowed to hold stack references, because the tagged union might outlive the stack frame.");
                const char *name = ith(tu_def->tag_names, i);
                if (Table_gets(&used_names, name))
                    compiler_err(env, def, "This definition has a duplicated field name: '%s'", name);
                Table_sets(&used_names, name, (void*)true);
                sss_tagged_union_member_t member = {
                    .name=name,
                    .tag_value=ith(tu_def->tag_values, i),
                    .type=member_t,
                };
                append(members, member);
            }
            populate_tagged_union_constructors(env, t);
        }

        binding->rval = gcc_str(env->ctx, name);
        auto definitions = Match(def, TypeDef)->definitions;
        foreach (definitions, def, _)
            populate_def_members(inner_env, *def);
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
        Table_sets(env->file_bindings, fndef->name, b);
    } else if (def->tag == TypeDef) {
        binding_t *b = get_binding(env, Match(def, TypeDef)->name);
        sss_type_t *t = Match(b->type, TypeType)->type;
        env = get_type_env(env, t);
        auto members = Match(def, TypeDef)->definitions;
        for (int64_t i = 0; members && i < LENGTH(members); i++) {
            ast_t *member = ith(members, i);
            if (member->tag == FunctionDef) {
                auto fndef = Match(member, FunctionDef);
                gcc_func_t *func = get_function_def(env, member, fresh(fndef->name));
                binding_t *b =  new(binding_t, .type=get_type(env, member),
                                    .func=func, .rval=gcc_get_func_address(func, NULL),
                                    .visible_in_closures=true);
                Table_sets(env->bindings, fndef->name, b);
            } else {
                predeclare_def_funcs(env, member);
            }
        }
    } else if (def->tag == DocTest) {
        predeclare_def_funcs(env, Match(def, DocTest)->expr);
    }
}

gcc_rvalue_t *_compile_block(env_t *env, gcc_block_t **block, ast_t *ast, bool give_expression)
{
    auto statements = ast->tag == Block ? Match(ast, Block)->statements : ARRAY(ast);

    // Design constraints:
    // - Struct/tagged union members can be struct/tagged union values defined earlier in the file
    // - Struct/tagged union members can be pointers to structs/tagged union defined *later* in the file
    // - Structs can define inner classes, which can be referenced by other classes
    // - Structs can define inner methods, which can be referenced inside the bodies of functions
    // - Function arguments can be struct/tagged union values/pointers defined anywhere in the file
    // - Function bodies can have references to functions declared anywhere in the file (corecursion)
    // Therefore the order of operations is:
    // 1) Load imports
    // 2) Predeclare all structs/tagged union with placeholder opaque structs/tagged union
    //    2B) Also predeclare all struct/tagged union 
    // 3) Populate all struct/tagged union members
    // 4) Predeclare all functions
    //    4B) Also predeclare all inner methods
    // 5) Populate all function bodies
    //    5B) Also populate all inner method bodies
    // 6) Compile each statement

    // Struct and tagged union defs are visible in the entire block (allowing corecursive structs)
    foreach (statements, stmt, _) {
        predeclare_def_types(env, *stmt, true);
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
