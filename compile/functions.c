// Logic for compiling Blang functions
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
#include "../util.h"

void compile_function(env_t *env, gcc_func_t *func, ast_t *def)
{
    auto t = Match(get_type(env, def), FunctionType);

    // Use a set of bindings that don't include any closures
    env = global_scope(env);
    env->return_type = t->ret;

    auto arg_names = def->tag == FunctionDef ? Match(def, FunctionDef)->arg_names : Match(def, Lambda)->arg_names;
    auto arg_types = def->tag == FunctionDef ? Match(def, FunctionDef)->arg_types : Match(def, Lambda)->arg_types;
    auto body = def->tag == FunctionDef ? Match(def, FunctionDef)->body : FakeAST(Return, .value=Match(def, Lambda)->body);
    for (int64_t i = 0; i < length(arg_names); i++) {
        const char* argname = ith(arg_names, i);
        bl_type_t *argtype = ith(t->arg_types, i);
        if (argtype->tag == VoidType)
            compiler_err(env, ith(arg_types, i), "'Void' can't be used as the type of an argument because there is no value that could be passed as a Void argument.");
        gcc_param_t *param = gcc_func_get_param(func, i);
        gcc_lvalue_t *lv = gcc_param_as_lvalue(param);
        gcc_rvalue_t *rv = gcc_param_as_rvalue(param);
        hset(env->bindings, argname, new(binding_t, .type=argtype, .lval=lv, .rval=rv));
    }

    gcc_block_t *block = gcc_new_block(func, fresh("func"));
    compile_statement(env, &block, body);
    if (block) {
        if (t->ret->tag != VoidType)
            compiler_err(env, def, "You declared that this function returns a value of type %s, but the end of the function can be reached without returning a value",
                  type_to_string(t->ret));
        gcc_return_void(block, NULL);
    }
}

gcc_func_t *get_function_def(env_t *env, ast_t *def, const char* name)
{
    gcc_func_t *cached = hget(env->ast_functions, def, gcc_func_t*);
    if (cached) return cached;

    auto t = Match(get_type(env, def), FunctionType);
    NEW_LIST(gcc_param_t*, params);
    auto arg_names = def->tag == FunctionDef ? Match(def, FunctionDef)->arg_names : Match(def, Lambda)->arg_names;
    for (int64_t i = 0; i < length(arg_names); i++) {
        const char* argname = ith(arg_names, i);
        bl_type_t *argtype = ith(t->arg_types, i);
        gcc_param_t *param = gcc_new_param(env->ctx, NULL, bl_type_to_gcc(env, argtype), fresh(argname));
        append(params, param);
    }

    bool is_inline = def->tag == FunctionDef && Match(def, FunctionDef)->is_inline;
    cached = gcc_new_func(
        env->ctx, ast_loc(env, def), is_inline ? GCC_FUNCTION_ALWAYS_INLINE : GCC_FUNCTION_EXPORTED,
        bl_type_to_gcc(env, t->ret), name, length(params), params[0], 0);
    hset(env->ast_functions, def, cached);
    return cached;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
