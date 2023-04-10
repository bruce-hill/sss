// Logic for compile a file containing a Blang program
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

main_func_t compile_file(gcc_ctx_t *ctx, jmp_buf *on_err, bl_file_t *f, ast_t *ast, bool debug, gcc_output_kind_e output_kind, gcc_jit_result **result)
{
    env_t *env = new_environment(ctx, on_err, f, debug);

    bl_type_t *str_t = Type(ArrayType, .item_type=Type(CharType));
    bl_type_t *str_array_t = Type(ArrayType, .item_type=str_t);
    gcc_type_t *gcc_string_t = bl_type_to_gcc(env, str_t);

    // TODO: define a SOURCE_CODE global when compiling a shared lib

    char hash_random_vector[] = "Blang modulehash--------------------------------------";
    uint64_t hash;
    halfsiphash(ast->span.file->text, strlen(ast->span.file->text), hash_random_vector, (uint8_t*)&hash, sizeof(hash));
    const char *load_fn_name = heap_strf("load_%lx", hash);

    // Set up `PROGRAM_NAME`
    gcc_lvalue_t *program_name = gcc_global(env->ctx, NULL, GCC_GLOBAL_EXPORTED, gcc_string_t, "PROGRAM_NAME");
    hset(env->global_bindings, "PROGRAM_NAME",
         new(binding_t, .rval=gcc_rval(program_name), .type=str_t));

    // Set up `ARGS`
    gcc_type_t *args_gcc_t = bl_type_to_gcc(env, str_array_t);
    gcc_lvalue_t *args = gcc_global(ctx, NULL, GCC_GLOBAL_EXPORTED, args_gcc_t, "ARGS");
    hset(env->global_bindings, "ARGS", new(binding_t, .rval=gcc_rval(args), .type=str_array_t));

    // Define load_1badd00d() function:
    bl_type_t *exported_t = get_type(env, ast);
    gcc_func_t *load_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_EXPORTED, bl_type_to_gcc(env, exported_t), load_fn_name, 0, NULL, 0);
    gcc_block_t *load_block = gcc_new_block(load_func, fresh("load"));
    gcc_rvalue_t *exported = compile_block_expr(env, &load_block, ast);
    if (load_block) {
        if (exported)
            gcc_return(load_block, NULL, exported);
        else
            gcc_return_void(load_block, NULL);
    }

    if (output_kind == GCC_OUTPUT_KIND_EXECUTABLE) {
        // Compile main(int argc, char *argv[]) function
        gcc_param_t* main_params[] = {
            gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, INT), "argc"),
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_string_t), "argv"),
        };
        gcc_func_t *main_func = gcc_new_func(
            ctx, NULL, GCC_FUNCTION_EXPORTED, gcc_type(ctx, INT),
            "main", 2, main_params, 0);
        gcc_block_t *main_block = gcc_new_block(main_func, fresh("main"));

        // Initialize `PROGRAM_NAME`
        gcc_func_t *prog_name_func = gcc_new_func(
            env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_string_t, "first_arg", 1, (gcc_param_t*[]){
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_string_t), "argv"),
            }, 0);
        gcc_assign(main_block, NULL, program_name, gcc_callx(env->ctx, NULL, prog_name_func, gcc_param_as_rvalue(main_params[1])));

        // Initialize `ARGS`
        gcc_func_t *arg_func = gcc_new_func(
            env->ctx, NULL, GCC_FUNCTION_IMPORTED, args_gcc_t, "arg_list", 2, (gcc_param_t*[]){
            gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, INT), "argc"),
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_string_t), "argv"),
            }, 0);
        gcc_rvalue_t *arg_list = gcc_callx(ctx, NULL, arg_func, 
                                           gcc_param_as_rvalue(main_params[0]),
                                           gcc_param_as_rvalue(main_params[1]));
        gcc_assign(main_block, NULL, args, arg_list);
        gcc_eval(main_block, NULL, gcc_callx(ctx, NULL, load_func));
        gcc_return(main_block, NULL, gcc_zero(ctx, gcc_type(ctx, INT)));
    }

    *result = gcc_compile(ctx);
    if (*result == NULL)
        compiler_err(env, ast, "Compilation failed");

    // Extract the generated code from "result".   
    main_func_t main_fn = output_kind == GCC_OUTPUT_KIND_EXECUTABLE ? (main_func_t)gcc_jit_result_get_code(*result, "main") : NULL;
    return main_fn;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
