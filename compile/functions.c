// Logic for compiling Blang functions
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

void compile_function(env_t *env, gcc_func_t *func, ast_t *def)
{
    bl_type_t *t = get_type(env->file, env->bindings, def);

    env_t body_env = *env;
    body_env.return_type = t->ret;
    // Use a set of bindings that don't include any closures
    body_env.bindings = global_bindings(env->bindings);

    for (int64_t i = 0; i < length(def->fn.arg_names); i++) {
        istr_t argname = ith(def->fn.arg_names, i);
        bl_type_t *argtype = ith(t->args, i);
        if (argtype->kind == VoidType)
            ERROR(env, ith(def->fn.arg_types, i), "'Void' can't be used as the type of an argument because there is no value that could be passed as a Void argument.");
        gcc_param_t *param = gcc_func_get_param(func, i);
        gcc_lvalue_t *lv = gcc_param_as_lvalue(param);
        gcc_rvalue_t *rv = gcc_param_as_rvalue(param);
        hashmap_set(body_env.bindings, argname, new(binding_t, .type=argtype, .lval=lv, .rval=rv));
    }

    gcc_block_t *block = gcc_new_block(func, fresh("func"));
    compile_statement(&body_env, &block, def->fn.body);
    if (block) {
        if (t->ret->kind != VoidType)
            ERROR(env, def, "You declared that this function returns a value of type %s, but the end of the function can be reached without returning a value",
                  type_to_string(t->ret));
        gcc_return_void(block, NULL);
    }
}

gcc_func_t *get_function_def(env_t *env, ast_t *def, istr_t name, bool is_global)
{
    bl_type_t *t = get_type(env->file, env->bindings, def);
    NEW_LIST(gcc_param_t*, params);
    for (int64_t i = 0; i < length(def->fn.arg_names); i++) {
        istr_t argname = ith(def->fn.arg_names, i);
        bl_type_t *argtype = ith(t->args, i);
        gcc_param_t *param = gcc_new_param(env->ctx, NULL, bl_type_to_gcc(env, argtype), fresh(argname));
        append(params, param);
    }

    return gcc_new_func(
        env->ctx, ast_loc(env, def), is_global ? GCC_FUNCTION_EXPORTED : GCC_FUNCTION_INTERNAL,
        bl_type_to_gcc(env, t->ret), name, length(params), params[0], 0);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
