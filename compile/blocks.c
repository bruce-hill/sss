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
    // Struct defs are visible in the entire block (allowing corecursive structs)
    foreach (ast->children, stmt, last_stmt) {
        if ((*stmt)->kind == StructDef) {
            bl_type_t *t = get_type(env->file, env->bindings, *stmt);
            hashmap_set(env->bindings, (*stmt)->fn.name, new(binding_t, .type=t, .is_global=true));
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
                    CORD name;
                    CORD_sprintf(&name, "%s.%s", (*stmt)->struct_.name, (*member)->fn.name);
                    hashmap_set(env->bindings, intern_str(CORD_to_char_star(name)),
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
                    CORD name;
                    CORD_sprintf(&name, "%s.%s", (*stmt)->struct_.name, (*member)->fn.name);
                    binding_t *binding = hashmap_get(env->bindings, intern_str(CORD_to_char_star(name)));
                    assert(binding);
                    compile_function(env, binding->func, *member);
                }
            }
        }
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
