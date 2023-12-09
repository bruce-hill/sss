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

void populate_tagged_union_constructors(env_t *env, gcc_block_t **block, gcc_struct_t *ns_struct, gcc_lvalue_t *ns_lval, sss_type_t *t)
{
    env = get_type_env(env, t);
    auto members = Match(base_variant(t), TaggedUnionType)->members;
    gcc_type_t *gcc_tagged_t = sss_type_to_gcc(env, t);
    gcc_struct_t *gcc_tagged_s = gcc_type_as_struct(gcc_tagged_t);
    gcc_field_t *tag_field = gcc_get_field(gcc_tagged_s, 0);
    gcc_type_t *tag_gcc_t = gcc_type(env->ctx, INT32);
    gcc_field_t *union_field = gcc_get_field(gcc_tagged_s, 1);
    gcc_type_t *union_gcc_t = get_union_type(env, t);

    for (int64_t i = 0; i < LENGTH(members); i++) {
        auto member = ith(members, i);
        int64_t constructor_index = i + 1; // first index is "type" (TypeInfo) field 
        gcc_lvalue_t *field_lval = gcc_lvalue_access_field(ns_lval, NULL, gcc_get_field(ns_struct, constructor_index));
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
                fields[i] = gcc_get_field(gcc_type_as_struct(member_gcc_t), i);
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
            gcc_assign(*block, NULL, field_lval, gcc_get_func_address(func, NULL));
            Table_str_set(env->bindings, member.name,
                 new(binding_t, .type=Type(FunctionType, .arg_names=names, .arg_types=types, .arg_defaults=defaults, .ret=t),
                     .visible_in_closures=true,
                     .func=func, .rval=gcc_get_func_address(func, NULL)));
        } else {
            gcc_rvalue_t *val = gcc_struct_constructor(
                env->ctx, NULL, gcc_tagged_t, 1, &tag_field, (gcc_rvalue_t*[]){
                    gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value),
                });
            gcc_assign(*block, NULL, field_lval, val);
            Table_str_set(env->bindings, member.name, new(binding_t, .type=t, .rval=gcc_rval(field_lval), .visible_in_closures=true));
        }
    }
}

void predeclare_def_funcs(env_t *env, ast_t *def)
{
    if (def->tag == FunctionDef) {
        auto fndef = Match(def, FunctionDef);
        gcc_func_t *func = get_function_def(env, def, fresh(fndef->name));
        binding_t *b = get_binding(env, fndef->name);
        if (b) {
            b->func = func;
        } else {
            binding_t *b = new(binding_t, .type=get_type(env, def),
                               .func=func, .rval=gcc_get_func_address(func, NULL),
                               .visible_in_closures=true);
            Table_str_set(env->bindings, fndef->name, b);
        }
    } else if (def->tag == TypeDef) {
        sss_type_t *t = get_type_by_name(env, Match(def, TypeDef)->name);
        env = get_type_env(env, t);
        auto members = Match(Match(def, TypeDef)->namespace, Namespace)->statements;
        for (int64_t i = 0; members && i < LENGTH(members); i++) {
            ast_t *member = ith(members, i);
            if (member->tag == FunctionDef) {
                auto fndef = Match(member, FunctionDef);
                gcc_func_t *func = get_function_def(env, member, fresh(fndef->name));
                binding_t *b =  new(binding_t, .type=get_type(env, member),
                                    .func=func, .rval=gcc_get_func_address(func, NULL),
                                    .visible_in_closures=true);
                Table_str_set(env->bindings, fndef->name, b);
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
