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

gcc_rvalue_t *_compile_block(env_t *env, gcc_block_t **block, ast_t *ast, bool give_expression)
{
    assert(ast->kind == Block);
    // Struct and enum defs are visible in the entire block (allowing corecursive structs)
    foreach (ast->children, stmt, last_stmt) {
        if ((*stmt)->kind == StructDef) {
            istr_t name = (*stmt)->struct_.name;
            NEW_LIST(istr_t, field_names);
            NEW_LIST(bl_type_t*, field_types);
            // Placeholder type, will be populated later:
            if (hashmap_get(env->bindings, name))
                ERROR(env, *stmt, "Something called %s is already defined.", name);
            bl_type_t *t = Type(StructType, .struct_.name=name, .struct_.field_names=field_names, .struct_.field_types=field_types);
            gcc_rvalue_t *rval = gcc_new_string(env->ctx, name);
            hashmap_set(env->bindings, name, new(binding_t, .type=Type(TypeType), .type_value=t, .is_global=true, .rval=rval));
        } else if ((*stmt)->kind == EnumDef) {
            istr_t enum_name = (*stmt)->enum_.name;
            if (hashmap_get(env->bindings, enum_name))
                ERROR(env, *stmt, "Something called %s is already defined.", enum_name);
            bl_type_t *t = Type(EnumType, .enum_.name=enum_name,
                                .enum_.field_names=(*stmt)->enum_.field_names, .enum_.field_values=(*stmt)->enum_.field_values);
            gcc_rvalue_t *rval = gcc_new_string(env->ctx, enum_name);
            hashmap_set(env->bindings, enum_name, new(binding_t, .type=Type(TypeType), .type_value=t, .is_global=true, .rval=rval));
            for (int64_t i = 0, len = length(t->enum_.field_names); i < len; i++) {
                gcc_rvalue_t *rval = gcc_int64(env->ctx, ith(t->enum_.field_values, i));
                istr_t field_name = ith(t->enum_.field_names, i);
                hashmap_set(env->bindings, field_name, new(binding_t, .type=t, .is_global=true, .rval=rval));
                istr_t qualified_name = intern_strf("%s.%s", enum_name, field_name);
                hashmap_set(env->bindings, qualified_name, new(binding_t, .type=t, .is_global=true, .rval=rval));
            }
        }
    }
    // Populate struct fields:
    foreach (ast->children, stmt, last_stmt) {
        if ((*stmt)->kind != StructDef)
            continue;

        ast_t *def = *stmt;
        istr_t name = def->struct_.name;
        binding_t *binding = hashmap_get(env->bindings, name);
        assert(binding && binding->type->kind == TypeType && binding->type_value);
        bl_type_t *t = binding->type_value;
        if (length(def->struct_.members) == 0)
            ERROR(env, def, "This struct has no fields, which is currently not supported");
        LIST_FOR (def->struct_.members, member, _) {
            if ((*member)->kind == StructFieldDef) {
                bl_type_t *ft = parse_type(env->file, env->bindings, (*member)->fields.type);
                if (ft->kind == VoidType)
                    ERROR(env, (*member)->fields.type, "This field is a Void type, but that isn't supported for struct members.");
                LIST_FOR((*member)->fields.names, fname, __) {
                    APPEND(t->struct_.field_names, *fname);
                    APPEND(t->struct_.field_types, ft);
                }
            }
        }
    }
    
    // Function defs are visible in the entire block (allowing corecursive funcs)
    foreach (ast->children, stmt, last_stmt) {
        if ((*stmt)->kind == FunctionDef) {
            bl_type_t *t = get_type(env->file, env->bindings, *stmt);
            gcc_func_t *func = get_function_def(env, *stmt, false);
            gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
            hashmap_set(env->bindings, (*stmt)->fn.name,
                        new(binding_t, .type=t, .is_global=true, .func=func, .rval=fn_ptr));
        } else if ((*stmt)->kind == StructDef) {
            // Struct methods:
            foreach ((*stmt)->struct_.members, member, _) {
                if ((*member)->kind == FunctionDef) {
                    bl_type_t *t = get_type(env->file, env->bindings, *member);
                    gcc_func_t *func = get_function_def(env, *member, false);
                    gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
                    istr_t name = intern_strf("%s.%s", (*stmt)->struct_.name, (*member)->fn.name);
                    hashmap_set(env->bindings, name,
                                new(binding_t, .type=t, .is_global=true, .func=func, .rval=fn_ptr));
                }
            }
        }
    }

    foreach (ast->children, stmt, last_stmt) {
        // Declarations are visible from here onwards:
        if ((*stmt)->kind == FunctionDef) {
            binding_t *binding = hashmap_get(env->bindings, (*stmt)->fn.name);
            assert(binding);
            // Compile the function here instead of above because we need the type information
            // from the other functions in the block.
            compile_function(env, binding->func, *stmt);
        } else if ((*stmt)->kind == StructDef) {
            // Struct methods:
            foreach ((*stmt)->struct_.members, member, _) {
                if ((*member)->kind == FunctionDef) {
                    istr_t name = intern_strf("%s.%s", (*stmt)->struct_.name, (*member)->fn.name);
                    binding_t *binding = hashmap_get(env->bindings, name);
                    assert(binding);
                    compile_function(env, binding->func, *member);
                }
            }
        }
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
