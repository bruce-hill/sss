// Logic for compile a file containing a SSS program
#include <assert.h>
#include <ctype.h>
#include <err.h>
#include <libgccjit.h>
#include <limits.h>
#include <stdint.h>

#include "../ast.h"
#include "../environment.h"
#include "../typecheck.h"
#include "../parse.h"
#include "../types.h"
#include "../util.h"
#include "../files.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../SipHash/halfsiphash.h"

main_func_t compile_file(gcc_ctx_t *ctx, jmp_buf *on_err, sss_file_t *f, ast_t *ast, bool tail_calls, bool debug, gcc_jit_result **result)
{
    env_t *env = new_environment(ctx, on_err, f, tail_calls, debug);

    sss_type_t *str_t = Type(ArrayType, .item_type=Type(CharType));
    sss_type_t *str_array_t = Type(ArrayType, .item_type=str_t);
    gcc_type_t *gcc_string_t = sss_type_to_gcc(env, str_t);

    // Set up `PROGRAM_NAME`
    gcc_lvalue_t *program_name = gcc_global(ctx, NULL, GCC_GLOBAL_EXPORTED, gcc_string_t, "PROGRAM_NAME");
    hset(env->global_bindings, "PROGRAM_NAME",
         new(binding_t, .rval=gcc_rval(program_name), .type=str_t, .visible_in_closures=true));

    // Set up `ARGS`
    gcc_type_t *args_gcc_t = sss_type_to_gcc(env, str_array_t);
    gcc_lvalue_t *args = gcc_global(ctx, NULL, GCC_GLOBAL_EXPORTED, args_gcc_t, "ARGS");
    hset(env->global_bindings, "ARGS", new(binding_t, .rval=gcc_rval(args), .type=str_array_t, .visible_in_closures=true));

    // Set up `USE_COLOR`
    gcc_lvalue_t *use_color = gcc_global(ctx, NULL, GCC_GLOBAL_EXPORTED, gcc_type(ctx, BOOL), "USE_COLOR");
    hset(env->global_bindings, "USE_COLOR",
         new(binding_t, .rval=gcc_rval(use_color), .type=Type(BoolType), .visible_in_closures=true));

    // Compile main(int argc, char *argv[]) function
    gcc_param_t* main_params[] = {
        gcc_new_param(ctx, NULL, gcc_type(ctx, INT), "argc"),
        gcc_new_param(ctx, NULL, gcc_get_ptr_type(gcc_string_t), "argv"),
    };
    gcc_func_t *main_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_EXPORTED, gcc_type(ctx, INT),
        "main", 2, main_params, 0);
    gcc_block_t *main_block = gcc_new_block(main_func, fresh("main"));

    // Initialize `PROGRAM_NAME`
    gcc_func_t *prog_name_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_string_t, "first_arg", 1, (gcc_param_t*[]){
        gcc_new_param(ctx, NULL, gcc_get_ptr_type(gcc_string_t), "argv"),
        }, 0);
    gcc_assign(main_block, NULL, program_name, gcc_callx(ctx, NULL, prog_name_func, gcc_param_as_rvalue(main_params[1])));

    gcc_func_t *getenv_fn = hget(env->global_funcs, "getenv", gcc_func_t*);
    gcc_rvalue_t *use_color_env_flag = gcc_comparison(
        ctx, NULL, GCC_COMPARISON_EQ, gcc_callx(ctx, NULL, getenv_fn, gcc_str(ctx, "NO_COLOR")),
        gcc_null(ctx, gcc_type(ctx, STRING)));
    gcc_func_t *isatty_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, BOOL), "isatty", 1, (gcc_param_t*[]){
            gcc_new_param(ctx, NULL, gcc_type(ctx, INT), "fd"),
        }, 0);
    gcc_assign(main_block, NULL, use_color,
        gcc_binary_op(ctx, NULL, GCC_BINOP_LOGICAL_AND, gcc_type(ctx, BOOL), use_color_env_flag,
                      gcc_callx(ctx, NULL, isatty_func, gcc_rvalue_int(ctx, STDOUT_FILENO))));

    // Initialize `ARGS`
    gcc_func_t *arg_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_IMPORTED, args_gcc_t, "arg_list", 2, (gcc_param_t*[]){
        gcc_new_param(ctx, NULL, gcc_type(ctx, INT), "argc"),
        gcc_new_param(ctx, NULL, gcc_get_ptr_type(gcc_string_t), "argv"),
        }, 0);
    gcc_rvalue_t *arg_list = gcc_callx(ctx, NULL, arg_func, 
                                       gcc_param_as_rvalue(main_params[0]),
                                       gcc_param_as_rvalue(main_params[1]));
    gcc_assign(main_block, NULL, args, arg_list);

    // Seed the RNG used for random floats
    gcc_func_t *arc4_rng = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, UINT32), "arc4random", 0, NULL, false);
    gcc_func_t *srand48_func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, VOID), "srand48", 1, (gcc_param_t*[]){
        gcc_new_param(ctx, NULL, gcc_type(ctx, UINT32), "seed")}, false);
    gcc_eval(main_block, NULL, gcc_callx(ctx, NULL, srand48_func, gcc_callx(ctx, NULL, arc4_rng)));

    // Run the program:
    gcc_rvalue_t *val = compile_expr(env, &main_block, WrapAST(ast, Use, .path=f->filename, .file=f, .main_program=true));
    gcc_eval(main_block, NULL, val);
    gcc_return(main_block, NULL, gcc_zero(ctx, gcc_type(ctx, INT)));

    // Actually compile the functions:
    for (uint32_t i = 1; i <= env->ast_functions->count; i++) {
        auto entry = hnth(env->ast_functions, i, ast_t*, func_context_t*);
        compile_function(&entry->value->env, entry->value->func, entry->key);
    }

    *result = gcc_compile(ctx);
    if (*result == NULL)
        compiler_err(env, ast, "Compilation failed");

    // Extract the generated code from "result".   
    main_func_t main_fn = (main_func_t)gcc_jit_result_get_code(*result, "main");
    return main_fn;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
