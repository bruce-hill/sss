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

static void bind_globals(env_t *env, sss_type_t *ns_t, gcc_lvalue_t *ns_lval, ARRAY_OF(ast_t*) statements)
{
    gcc_type_t *ns_gcc_t = sss_type_to_gcc(env, ns_t);
    auto ns_struct = Match(ns_t, StructType);

    int64_t field_num = 0;
    foreach (statements, _stmt, _) {
        ast_t *stmt = *_stmt;
      doctest_inner:
        switch (stmt->tag) {
        case Declare: {
            auto decl = Match(stmt, Declare);
            decl->is_public = true;
            gcc_field_t *field = gcc_get_field(gcc_type_as_struct(ns_gcc_t), field_num);
            gcc_lvalue_t *lval = gcc_lvalue_access_field(ns_lval, ast_loc(env, stmt), field);
            sss_type_t *t = ith(ns_struct->field_types, field_num);
            assert(t);
            set_binding(env, Match(decl->var, Var)->name, new(binding_t, .type=t, .lval=lval, .rval=gcc_rval(lval), .visible_in_closures=true));
            ++field_num;
            break;
        }
        case TypeDef: {
            auto def = Match(stmt, TypeDef);
            sss_type_t *inner_ns_t = ith(ns_struct->field_types, field_num);
            gcc_field_t *field = gcc_get_field(gcc_type_as_struct(ns_gcc_t), field_num);
            gcc_lvalue_t *lval = gcc_lvalue_access_field(ns_lval, ast_loc(env, stmt), field);
            sss_type_t *def_t = get_type_by_name(env, def->name);
            bind_globals(get_type_env(env, def_t), inner_ns_t, lval, def->definitions);
            ++field_num;
            break;
        }
        case FunctionDef: {
            auto def = Match(stmt, FunctionDef);
            gcc_field_t *field = gcc_get_field(gcc_type_as_struct(ns_gcc_t), field_num);
            gcc_lvalue_t *lval = gcc_lvalue_access_field(ns_lval, ast_loc(env, stmt), field);
            sss_type_t *t = ith(ns_struct->field_types, field_num);
            assert(t);
            set_binding(env, def->name, new(binding_t, .type=t, .lval=lval, .rval=gcc_rval(lval), .visible_in_closures=true));
            ++field_num;
            break;
        }
        case DocTest: {
            stmt = Match(stmt, DocTest)->expr;
            goto doctest_inner;
        }
        default: break;
        }
    }
}

static void make_module_struct(env_t *env, ast_t *ast)
{
    sss_type_t *ns_t = get_namespace_type(env, NULL, Match(ast, Block)->statements);
    gcc_type_t *ns_gcc_t = sss_type_to_gcc(env, ns_t);
    gcc_lvalue_t *mod_lval = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, ns_gcc_t, "mod");
    bind_globals(env, ns_t, mod_lval, Match(ast, Block)->statements);
}

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

    make_module_struct(env, ast);

    // Run the program:
    compile_statement(env, &main_block, ast);
    gcc_return(main_block, NULL, gcc_zero(ctx, gcc_type(ctx, INT)));

    // Actually compile the functions:
    for (int64_t i = 1; i <= Table_length(&env->global->ast_functions); i++) {
        struct {const char *key; func_context_t *value;} *entry = Table_str_entry(&env->global->ast_functions, i);
        ast_t *ast = (ast_t*)strtol(entry->key, NULL, 16);
        compile_function(&entry->value->env, entry->value->func, ast);
    }

    // Initialize type lvalues (should be after compiling funcs)
    initialize_typeinfo_lvalues(env);

    *result = gcc_compile(ctx);
    if (*result == NULL)
        compiler_err(env, ast, "Compilation failed");

    // Extract the generated code from "result".   
    main_func_t main_fn = (main_func_t)gcc_jit_result_get_code(*result, "main");
    return main_fn;
}

void compile_object_file(gcc_ctx_t *ctx, jmp_buf *on_err, sss_file_t *f, ast_t *ast, bool tail_calls)
{
    env_t *env = new_environment(ctx, on_err, f, tail_calls);

    // Set up `USE_COLOR`
    Table_str_set(&env->global->bindings, "USE_COLOR",
         new(binding_t, .rval=gcc_rvalue_bool(ctx, true), .type=Type(BoolType), .visible_in_closures=true));

    struct stat file_stat;  
    stat(f->filename, &file_stat);  
    sss_type_t *module_type = get_file_type(env, f->filename);
    gcc_type_t *module_gcc_type = sss_type_to_gcc(env, module_type);
    const char *load_name = heap_strf("load__%s", get_module_name(f->filename));
    gcc_func_t *load_func = gcc_new_func(ctx, NULL, GCC_FUNCTION_EXPORTED, gcc_get_ptr_type(module_gcc_type), load_name, 0, NULL, 0);
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
    gcc_lvalue_t *module_var = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, module_gcc_type, fresh("module"));
    env_t *type_env = get_type_env(env, t);
    type_env->bindings->fallback = env->bindings;

    make_module_struct(env, ast);

    compile_block_statement(env, &do_loading_block, ast);
    if (do_loading_block)
        gcc_jump(do_loading_block, NULL, finished_loading_block);

    gcc_return(finished_loading_block, NULL, gcc_lvalue_address(module_var, NULL));

    // Actually compile the functions:
    for (int64_t i = 1; i <= Table_length(&env->global->ast_functions); i++) {
        struct {const char*key; func_context_t *value;} *entry = Table_str_entry(&env->global->ast_functions, i);
        ast_t *ast = (ast_t*)strtol(entry->key, NULL, 16);
        compile_function(&entry->value->env, entry->value->func, ast);
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
