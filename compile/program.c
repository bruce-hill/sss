// Logic for compile a file containing a Blang program
#include <assert.h>
#include <bhash.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <limits.h>
#include <stdint.h>

#include "../ast.h"
#include "../environment.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"
#include "../files.h"
#include "compile.h"
#include "libgccjit_abbrev.h"

main_func_t compile_file(gcc_ctx_t *ctx, jmp_buf *on_err, bl_file_t *f, ast_t *ast, bool debug, gcc_jit_result **result)
{
    env_t *env = new_environment(ctx, on_err, f, debug);

    bl_type_t *str_type = Type(ArrayType, .item_type=Type(CharType));
    gcc_type_t *gcc_string_t = bl_type_to_gcc(env, str_type);
    gcc_param_t* main_params[] = {
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, INT), "argc"),
        gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_string_t), "argv"),
    };
    const char *main_name = fresh("main");
    gcc_func_t *main_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_EXPORTED, gcc_type(ctx, INT),
        main_name, 2, main_params, 0);
    gcc_block_t *block = gcc_new_block(main_func, fresh("main"));

    // Set up `PROGRAM_NAME`
    gcc_lvalue_t *program_name = gcc_local(main_func, NULL, gcc_string_t, "PROGRAM_NAME");
    gcc_lvalue_t *name_lval = gcc_rvalue_dereference(gcc_param_as_rvalue(main_params[1]), NULL);
    gcc_assign(block, NULL, program_name, gcc_rval(name_lval));
    hashmap_set(env->bindings, intern_str("PROGRAM_NAME"),
                new(binding_t, .rval=gcc_rval(program_name), .type=str_type));

    // Set up `args`
    bl_type_t *args_t = Type(ArrayType, .item_type=str_type);
    gcc_type_t *args_gcc_t = bl_type_to_gcc(env, args_t);
    gcc_func_t *arg_func = gcc_new_func(
        env->ctx, NULL, GCC_FUNCTION_IMPORTED, args_gcc_t, "arg_list", 2, (gcc_param_t*[]){
            gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, INT), "argc"),
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_string_t), "argv"),
        }, 0);

    gcc_lvalue_t *args = gcc_local(main_func, NULL, args_gcc_t, "args");
    hashmap_set(env->bindings, intern_str("args"), new(binding_t, .rval=gcc_rval(args), .type=args_t));
    gcc_rvalue_t *arg_list = gcc_callx(env->ctx, NULL, arg_func, 
        gcc_param_as_rvalue(main_params[0]),
        gcc_param_as_rvalue(main_params[1]));
    gcc_assign(block, NULL, args, arg_list);

    compile_statement(env, &block, ast);
    if (block)
        gcc_return(block, NULL, gcc_zero(ctx, gcc_type(ctx, INT)));

    *result = gcc_compile(ctx);
    if (*result == NULL)
        compile_err(env, NULL, "Compilation failed");

    // Extract the generated code from "result".   
    main_func_t main_fn = (main_func_t)gcc_jit_result_get_code(*result, main_name);
    return main_fn;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
