// Logic for compiling Blang blocks
#include <assert.h>
#include <bhash.h>
#include <libgccjit.h>
#include <bp/files.h>
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
    check_discardable(env->file, env->bindings, ast);
    gcc_rvalue_t *val = compile_expr(env, block, ast);
    if (val)
        gcc_eval(*block, ast_loc(env, ast), val);
}

static void predeclare_def_types(env_t *env, ast_t *def)
{
    if (def->kind == StructDef) {
        istr_t name = def->struct_.name;
        NEW_LIST(istr_t, field_names);
        NEW_LIST(bl_type_t*, field_types);
        // Placeholder type, will be populated later:
        if (hashmap_get(env->bindings, name))
            ERROR(env, def, "Something called %s is already defined.", name);
        bl_type_t *t = Type(StructType, .struct_.name=name, .struct_.field_names=field_names, .struct_.field_types=field_types);
        gcc_lvalue_t *lval = gcc_global(env->ctx, ast_loc(env, def), GCC_GLOBAL_EXPORTED, gcc_type(env->ctx, STRING), name);
        lval = gcc_global_set_initializer_rvalue(lval, gcc_new_string(env->ctx, name));

        hashmap_t *namespace = hashmap_new();
        namespace->fallback = env->bindings;
        hashmap_set(env->bindings, name, new(binding_t, .type=Type(TypeType), .type_value=t, .is_global=true, .rval=gcc_lvalue_as_rvalue(lval), .namespace=namespace));

        env_t struct_env = *env;
        struct_env.bindings = namespace;
        foreach (def->struct_.members, member, _) {
            predeclare_def_types(&struct_env, *member);
        }
    } else if (def->kind == EnumDef) {
        istr_t enum_name = def->enum_.name;
        if (hashmap_get(env->bindings, enum_name))
            ERROR(env, def, "Something called %s is already defined.", enum_name);
        bl_type_t *t = Type(EnumType, .enum_.name=enum_name,
                            .enum_.field_names=def->enum_.field_names, .enum_.field_values=def->enum_.field_values);
        gcc_rvalue_t *rval = gcc_new_string(env->ctx, enum_name);

        hashmap_t *namespace = hashmap_new();
        namespace->fallback = env->bindings;

        binding_t *binding = new(binding_t, .type=Type(TypeType), .type_value=t, .is_global=true, .rval=rval, .namespace=namespace);
        hashmap_set(env->bindings, enum_name, binding);
        for (int64_t i = 0, len = length(t->enum_.field_names); i < len; i++) {
            gcc_rvalue_t *rval = gcc_int64(env->ctx, ith(t->enum_.field_values, i));
            istr_t field_name = ith(t->enum_.field_names, i);
            hashmap_set(namespace, field_name, new(binding_t, .type=t, .is_constant=true, .is_global=true, .rval=rval));
            // Default to also visible outside the enum's namespace:
            hashmap_set(env->bindings, field_name, new(binding_t, .type=t, .is_constant=true, .is_global=true, .rval=rval));
        }
    }
}

static void populate_def_members(env_t *env, ast_t *def)
{
    if (def->kind != StructDef)
        return;

    istr_t name = def->struct_.name;
    binding_t *binding = hashmap_get(env->bindings, name);
    assert(binding && binding->type->kind == TypeType && binding->type_value);
    bl_type_t *t = binding->type_value;
    if (length(def->struct_.members) == 0)
        ERROR(env, def, "This struct has no fields, which is currently not supported");

    env_t inner_env = *env;
    inner_env.bindings = binding->namespace;

    foreach (def->struct_.members, member, _) {
        if ((*member)->kind == StructFieldDef) {
            bl_type_t *ft = parse_type(env->file, inner_env.bindings, (*member)->fields.type);
            if (ft->kind == VoidType)
                ERROR(env, (*member)->fields.type, "This field is a Void type, but that isn't supported for struct members.");
            foreach((*member)->fields.names, fname, __) {
                APPEND(t->struct_.field_names, *fname);
                APPEND(t->struct_.field_types, ft);
            }
        } else {
            populate_def_members(&inner_env, *member);
        }
    }
}

static void predeclare_def_funcs(env_t *env, ast_t *def)
{
    if (def->kind == FunctionDef) {
        bl_type_t *t = get_type(env->file, env->bindings, def);
        gcc_func_t *func = get_function_def(env, def, fresh(def->fn.name), false);
        gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
        hashmap_set(env->bindings, def->fn.name,
                    new(binding_t, .type=t, .is_global=true, .func=func, .rval=fn_ptr));
    } else if (def->kind == StructDef) {
        binding_t *b = hashmap_get(env->bindings, def->struct_.name);
        assert(b && b->namespace);
        env_t inner_env = *env;
        inner_env.bindings = b->namespace;
        env = &inner_env;
        // Struct methods:
        foreach (def->struct_.members, member, _) {
            if ((*member)->kind == FunctionDef) {
                bl_type_t *t = get_type(env->file, env->bindings, *member);
                gcc_func_t *func = get_function_def(env, *member, intern_strf("%s__%s", def->struct_.name, (*member)->fn.name), true);
                gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
                hashmap_set(env->bindings, (*member)->fn.name,
                            new(binding_t, .type=t, .is_global=true, .func=func, .rval=fn_ptr));
            } else {
                predeclare_def_funcs(env, *member);
            }
        }
    }
}

gcc_rvalue_t *_compile_block(env_t *env, gcc_block_t **block, ast_t *ast, bool give_expression)
{
    assert(ast->kind == Block);

    // Struct and enum defs are visible in the entire block (allowing corecursive structs)
    foreach (ast->children, stmt, _) {
        predeclare_def_types(env, *stmt);
    }
    // Populate struct fields:
    foreach (ast->children, stmt, _) {
        populate_def_members(env, *stmt);
    }
    // Function defs are visible in the entire block (allowing corecursive funcs)
    foreach (ast->children, stmt, _) {
        predeclare_def_funcs(env, *stmt);
    }

    foreach (ast->children, stmt, last_stmt) {
        if (!*block)
            ERROR(env, *stmt, "This code can never be reached because there is an unconditional control flow statement before it.");
        if (stmt == last_stmt && give_expression) {
            return compile_expr(env, block, *stmt);
        } else {
            compile_statement(env, block, *stmt);
        }
    }
    return NULL;
}

gcc_rvalue_t *compile_block_expr(env_t *env, gcc_block_t **block, ast_t *ast)
{
    return _compile_block(env, block, ast, true);
}

void compile_block_statement(env_t *env, gcc_block_t **block, ast_t *ast)
{
    if (ast)
        (void)_compile_block(env, block, ast, false);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
