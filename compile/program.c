// Logic for compile a file containing a Blang program
#include <assert.h>
#include <bhash.h>
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

main_func_t compile_file(gcc_ctx_t *ctx, jmp_buf *on_err, bl_file_t *f, ast_t *ast, bool debug, bool standalone, gcc_jit_result **result)
{
    env_t *env = new_environment(ctx, on_err, f, debug);

    bl_type_t *str_type = Type(ArrayType, .item_type=Type(CharType));
    gcc_type_t *gcc_string_t = bl_type_to_gcc(env, str_type);
    // Reserved names that shouldn't be used unadorned by anything else:
    (void)fresh("main");
    (void)fresh("get_exports");
    (void)fresh("prepare_exports");

    if (standalone) {
        // Compile main() function
        gcc_param_t* main_params[] = {
            gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, INT), "argc"),
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_string_t), "argv"),
        };
        const char *main_name = "main";
        gcc_func_t *main_func = gcc_new_func(
            ctx, NULL, GCC_FUNCTION_EXPORTED, gcc_type(ctx, INT),
            main_name, 2, main_params, 0);
        gcc_block_t *block = gcc_new_block(main_func, fresh("main"));

        // Set up `PROGRAM_NAME`
        gcc_lvalue_t *program_name = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, gcc_string_t, "PROGRAM_NAME");
        gcc_func_t *prog_name_func = gcc_new_func(
            env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_string_t, "first_arg", 1, (gcc_param_t*[]){
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_string_t), "argv"),
            }, 0);
        gcc_assign(block, NULL, program_name, gcc_callx(env->ctx, NULL, prog_name_func, gcc_param_as_rvalue(main_params[1])));
        hashmap_set(env->global_bindings, intern_str("PROGRAM_NAME"),
                    new(binding_t, .rval=gcc_rval(program_name), .type=str_type));

        // Set up `args`
        bl_type_t *args_t = Type(ArrayType, .item_type=str_type);
        gcc_type_t *args_gcc_t = bl_type_to_gcc(env, args_t);
        gcc_func_t *arg_func = gcc_new_func(
            env->ctx, NULL, GCC_FUNCTION_IMPORTED, args_gcc_t, "arg_list", 2, (gcc_param_t*[]){
            gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, INT), "argc"),
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_string_t), "argv"),
            }, 0);

        gcc_lvalue_t *args = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, args_gcc_t, "args");
        hashmap_set(env->global_bindings, intern_str("args"), new(binding_t, .rval=gcc_rval(args), .type=args_t));
        gcc_rvalue_t *arg_list = gcc_callx(env->ctx, NULL, arg_func, 
                                           gcc_param_as_rvalue(main_params[0]),
                                           gcc_param_as_rvalue(main_params[1]));
        gcc_assign(block, NULL, args, arg_list);

        compile_block_statement(env, &block, ast);
        if (block)
            gcc_return(block, NULL, gcc_zero(ctx, gcc_type(ctx, INT)));
    } else {
        // Create the module loading function
        bl_file_t *type_file = bl_spoof_file("<load signature>", "[{path:@Char,signature:@Char,symbol:@Char,deref:Bool,docs:@Char}]");
        ast_t *type_ast = parse_type(type_file, env->on_err);
        bl_type_t *exports_t = parse_type_ast(env, type_ast);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, exports_t);

        gcc_func_t *get_exports = gcc_new_func(ctx, NULL, GCC_FUNCTION_EXPORTED, gcc_t, "get_exports", 0, NULL, 0);
        gcc_block_t *block = gcc_new_block(get_exports, fresh("get_exports"));

        compile_block_statement(env, &block, ast);
        assert(block);

        gcc_lvalue_t *exports_var = gcc_local(get_exports, NULL, gcc_t, fresh("exports"));
        gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);

        int64_t num_exports = LIST_LEN(env->exports);
        bl_type_t *item_t = Match(exports_t, ArrayType)->item_type;
        gcc_func_t *alloc_func = hashmap_gets(env->global_funcs, "GC_malloc");
        gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), (long)(gcc_sizeof(env, item_t) * num_exports));
        gcc_type_t *gcc_item_ptr_t = bl_type_to_gcc(env, Type(PointerType, .pointed=item_t));
        gcc_assign(block, NULL, exports_var, gcc_struct_constructor(
                env->ctx, NULL, gcc_t, 2,
                (gcc_field_t*[]){gcc_get_field(gcc_struct, 0), gcc_get_field(gcc_struct, 2)}, // stride = 1
                (gcc_rvalue_t*[]){gcc_cast(env->ctx, NULL, gcc_callx(env->ctx, NULL, alloc_func, size), gcc_item_ptr_t),
                                  gcc_one(env->ctx, gcc_type(env->ctx, INT32))}));

        gcc_rvalue_t *items = gcc_rval(gcc_lvalue_access_field(exports_var, NULL, gcc_get_field(gcc_struct, 0)));
        gcc_lvalue_t *length_field = gcc_lvalue_access_field(exports_var, NULL, gcc_get_field(gcc_struct, 1));
        gcc_type_t *i32 = gcc_type(env->ctx, INT32);
        gcc_rvalue_t *one32 = gcc_one(env->ctx, i32);
        gcc_type_t *gcc_item_t = bl_type_to_gcc(env, item_t);
        gcc_struct_t *gcc_item_struct = gcc_type_if_struct(gcc_item_t);

        gcc_func_t *intern_str_func = hashmap_gets(env->global_funcs, "intern_str");
#define GC_STR(s) gcc_callx(env->ctx, NULL, intern_str_func, gcc_str(env->ctx, (s)))
        foreach (env->exports, exp, _) {
            // array.items[array.length] = item
            gcc_lvalue_t *item_home = gcc_array_access(env->ctx, NULL, items, gcc_rval(length_field));
            istr_t sym_name;
            binding_t *b = (*exp)->binding;
            if (b->sym_name) {
                sym_name = b->sym_name;
            } else {
                // Create an exported global to make it visible:
                sym_name = fresh((*exp)->qualified_name);
                gcc_lvalue_t *lval = gcc_global(env->ctx, NULL, GCC_GLOBAL_EXPORTED, bl_type_to_gcc(env, b->type), sym_name);
                gcc_assign(block, NULL, lval, b->rval);
            }

            gcc_rvalue_t *deref = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, BOOL), b->type->tag != FunctionType);
            gcc_rvalue_t *item_val = gcc_struct_constructor(
                env->ctx, NULL, gcc_item_t, 4,
                (gcc_field_t*[]){gcc_get_field(gcc_item_struct, 0), gcc_get_field(gcc_item_struct, 1), gcc_get_field(gcc_item_struct, 2), gcc_get_field(gcc_item_struct, 3)},
                (gcc_rvalue_t*[]){GC_STR((*exp)->qualified_name), GC_STR(type_to_string(b->type)), GC_STR(sym_name), deref});
            gcc_assign(block, NULL, item_home, item_val);

            // array.length += 1
            gcc_update(block, NULL, length_field, GCC_BINOP_PLUS, one32);
        }
#undef GC_STR

        // ast_t *debug = FakeAST(FunctionCall, .fn=FakeAST(Var, .name=intern_str("say")), .args=LIST(ast_t*, FakeAST(StringJoin, .children=LIST(ast_t*, FakeAST(StringLiteral, .str=intern_str("loaded"))))));
        // compile_statement(env, &block, debug);

        // gcc_type_t *void_star = gcc_type(env->ctx, VOID_PTR);
        // gcc_lvalue_t *stdout_var = gcc_jit_context_new_global(env->ctx, NULL, GCC_JIT_GLOBAL_IMPORTED, gcc_type(env->ctx, FILE_PTR), "stdout");
        // gcc_func_t *print_fn = get_print_func(env, exports_t);
        // gcc_type_t *hashmap_gcc_t = gcc_array_type(env->ctx, NULL, gcc_type(env->ctx, CHAR), sizeof(hashmap_t));
        // gcc_lvalue_t *cycle_checker = gcc_local(get_exports, NULL, hashmap_gcc_t, fresh("rec"));
        // gcc_assign(block, NULL, cycle_checker, gcc_array_constructor(env->ctx, NULL, hashmap_gcc_t, 0, NULL));
        // gcc_eval(block, NULL,
        //          gcc_callx(env->ctx, NULL, print_fn, 
        //                    gcc_rval(exports_var),
        //                    gcc_rval(stdout_var),
        //                    gcc_cast(env->ctx, NULL, gcc_lvalue_address(cycle_checker, NULL), void_star)));

        gcc_return(block, NULL, gcc_rval(exports_var));
    }

    *result = gcc_compile(ctx);
    if (*result == NULL)
        compiler_err(env, NULL, "Compilation failed");

    // Extract the generated code from "result".   
    main_func_t main_fn = standalone ? (main_func_t)gcc_jit_result_get_code(*result, "main") : NULL;
    return main_fn;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
