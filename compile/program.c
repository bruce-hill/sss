// Logic for compile a file containing a SSS program
#include <assert.h>
#include <ctype.h>
#include <err.h>
#include <libgccjit.h>
#include <limits.h>
#include <stdint.h>
#include <sys/stat.h>

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

main_func_t compile_file(gcc_ctx_t *ctx, jmp_buf *on_err, sss_file_t *f, ast_t *ast, bool tail_calls, gcc_jit_result **result)
{
    env_t *env = new_environment(ctx, on_err, f, tail_calls);

    sss_type_t *str_t = get_type_by_name(env, "Str");
    gcc_type_t *gcc_string_t = sss_type_to_gcc(env, str_t);

    // Set up `USE_COLOR`
    gcc_lvalue_t *use_color = gcc_global(ctx, NULL, GCC_GLOBAL_INTERNAL, gcc_type(ctx, BOOL), "USE_COLOR");
    Table_str_set(&env->global->bindings, "USE_COLOR",
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

    gcc_func_t *getenv_fn = Table_str_get(&env->global->funcs, "getenv");
    gcc_rvalue_t *use_color_env_flag = gcc_comparison(
        ctx, NULL, GCC_COMPARISON_EQ, gcc_callx(ctx, NULL, getenv_fn, gcc_str(ctx, "NO_COLOR")),
        gcc_null(ctx, gcc_type(env->ctx, STRING)));
    gcc_func_t *isatty_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, BOOL), "isatty", 1, (gcc_param_t*[]){
            gcc_new_param(ctx, NULL, gcc_type(ctx, INT), "fd"),
        }, 0);
    gcc_assign(main_block, NULL, use_color,
        gcc_binary_op(ctx, NULL, GCC_BINOP_LOGICAL_AND, gcc_type(ctx, BOOL), use_color_env_flag,
                      gcc_callx(ctx, NULL, isatty_func, gcc_rvalue_int(ctx, STDOUT_FILENO))));

    // Seed the RNG used for random floats
    gcc_func_t *arc4_rng = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, UINT32), "arc4random", 0, NULL, false);
    gcc_func_t *srand48_func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, VOID), "srand48", 1, (gcc_param_t*[]){
        gcc_new_param(ctx, NULL, gcc_type(ctx, UINT32), "seed")}, false);
    gcc_eval(main_block, NULL, gcc_callx(ctx, NULL, srand48_func, gcc_callx(ctx, NULL, arc4_rng)));

    // Run the program:
    compile_statement(env, &main_block, ast);
    gcc_return(main_block, NULL, gcc_zero(ctx, gcc_type(ctx, INT)));

    // Actually compile the functions:
    for (uint32_t i = 1; i <= Table_length(&env->global->ast_functions); i++) {
        struct {const char *key; func_context_t *value;} *entry = Table_str_entry(&env->global->ast_functions, i);
        ast_t *ast = (ast_t*)strtol(entry->key, NULL, 16);
        compile_function(&entry->value->env, entry->value->func, ast);
    }

    *result = gcc_compile(ctx);
    if (*result == NULL)
        compiler_err(env, ast, "Compilation failed");

    // Extract the generated code from "result".   
    main_func_t main_fn = (main_func_t)gcc_jit_result_get_code(*result, "main");
    return main_fn;
}

void compile_object_file(gcc_ctx_t *ctx, jmp_buf *on_err, sss_file_t *f, ast_t *ast, bool tail_calls, gcc_jit_result **result)
{
    env_t *env = new_environment(ctx, on_err, f, tail_calls);

    // Set up `USE_COLOR`
    Table_str_set(&env->global->bindings, "USE_COLOR",
         new(binding_t, .rval=gcc_rvalue_bool(ctx, true), .type=Type(BoolType), .visible_in_closures=true));

    struct stat file_stat;  
    stat(f->filename, &file_stat);  
    const char *load_name = heap_strf("load_%.*s_%ld", (int)strcspn(basename(f->filename), "."), basename(f->filename), file_stat.st_ino);
    sss_type_t *ret_type = get_file_type(env, f->filename);
    gcc_type_t *ret_gcc_type = sss_type_to_gcc(env, ret_type);
    gcc_func_t *load_func = gcc_new_func(ctx, NULL, GCC_FUNCTION_EXPORTED, ret_gcc_type, load_name, 0, NULL, 0);
    gcc_block_t *check_loaded_block = gcc_new_block(load_func, fresh("check_if_loaded")),
                *do_loading_block = gcc_new_block(load_func, fresh("do_loading")),
                *finished_loading_block = gcc_new_block(load_func, fresh("finished_loading"));

    // static is_loaded = false; if (is_loaded) return; is_loaded = true; module = load...; return module
    gcc_lvalue_t *is_loaded = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, gcc_type(env->ctx, BOOL), fresh("module_was_loaded"));
    gcc_jit_global_set_initializer_rvalue(is_loaded, gcc_rvalue_bool(env->ctx, false));
    gcc_jump_condition(check_loaded_block, NULL, gcc_rval(is_loaded), finished_loading_block, do_loading_block);
    gcc_assign(do_loading_block, NULL, is_loaded, gcc_rvalue_bool(env->ctx, true));

    // Load the module:
    sss_type_t *t = Type(VariantType, .name="Module", .filename=sss_get_file_pos(ast->file, ast->start), .variant_of=Type(VoidType));
    gcc_lvalue_t *module_var = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, ret_gcc_type, fresh("module"));
    binding_t *b = new(binding_t, .type=Type(TypeType, .type=t), .visible_in_closures=true);
    Table_str_set(env->bindings, "Module", b);
    env_t *type_env = get_type_env(env, t);
    type_env->bindings->fallback = env->bindings;

    ast_t *module_ast = WrapAST(ast, TypeDef, .name="Module", .type=WrapAST(ast, Var, "Void"), .definitions=Match(ast, Block)->statements);
    gcc_rvalue_t *val = compile_expr(env, &do_loading_block, module_ast);
    gcc_assign(do_loading_block, NULL, module_var, val);
    gcc_jump(do_loading_block, NULL, finished_loading_block);

    gcc_return(finished_loading_block, NULL, gcc_rval(module_var));

    // Actually compile the functions:
    for (uint32_t i = 1; i <= Table_length(&env->global->ast_functions); i++) {
        struct {const char*key; func_context_t *value;} *entry = Table_str_entry(&env->global->ast_functions, i);
        ast_t *ast = (ast_t*)strtol(entry->key, NULL, 16);
        compile_function(&entry->value->env, entry->value->func, ast);
    }

    *result = gcc_compile(ctx);
    if (*result == NULL)
        compiler_err(env, ast, "Compilation failed");
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
