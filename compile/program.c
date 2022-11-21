// Logic for compile a file containing a Blang program
#include <assert.h>
#include <bhash.h>
#include <libgccjit.h>
#include <bp/files.h>
#include <ctype.h>
#include <err.h>
#include <gc/cord.h>
#include <limits.h>
#include <stdint.h>

#include "../ast.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

// Load a bunch of global (external) functions
static hashmap_t *get_global_functions(gcc_ctx_t *ctx)
{
    gcc_type_t *t_str = gcc_get_type(ctx, GCC_T_STRING),
               *t_int = gcc_get_type(ctx, GCC_T_INT),
               *t_void = gcc_get_type(ctx, GCC_T_VOID),
               *t_void_ptr = gcc_get_type(ctx, GCC_T_VOID_PTR),
               *t_size = gcc_get_type(ctx, GCC_T_SIZE);
    gcc_type_t *t_str_ptr = gcc_get_ptr_type(t_str);

    hashmap_t *funcs = hashmap_new();

#define PARAM(type, name) gcc_new_param(ctx, NULL, type, name)
#define MAKE_FUNC(t_ret, name, variadic, ...) hashmap_set(funcs, intern_str(name), \
    gcc_new_func(ctx, NULL, GCC_FUNCTION_IMPORTED, t_ret, name, \
                 sizeof((gcc_param_t*[]){__VA_ARGS__})/sizeof(gcc_param_t*),\
                 (gcc_param_t*[]){__VA_ARGS__}, variadic))
    MAKE_FUNC(t_void_ptr, "GC_malloc", 0, PARAM(t_size, "size"));
    MAKE_FUNC(t_void_ptr, "GC_malloc_atomic", 0, PARAM(t_size, "size"));
    MAKE_FUNC(t_str, "CORD_cat", 0, PARAM(t_str, "str"), PARAM(t_str, "str2"));
    MAKE_FUNC(t_int, "CORD_sprintf", 1, PARAM(t_str_ptr, "cord"), PARAM(t_str, "fmt"));
    MAKE_FUNC(t_str, "CORD_to_char_star", 0, PARAM(t_str, "cord"));
    MAKE_FUNC(t_str, "intern_str", 0, PARAM(t_str, "str"));
    MAKE_FUNC(t_str, "intern_strf", 1, PARAM(t_str, "fmt"));
    MAKE_FUNC(t_size, "intern_len", 0, PARAM(t_str, "str"));
    MAKE_FUNC(t_void, "fail", 1, PARAM(t_str, "message"));
#undef MAKE_FUNC
#undef PARAM

    return funcs;
}

gcc_result_t *compile_file(gcc_ctx_t *ctx, file_t *f, ast_t *ast, bool debug)
{
    env_t env = {
        .ctx = ctx,
        .file = f,
        .bindings = hashmap_new(),
        .tostring_funcs = hashmap_new(),
        .gcc_types = hashmap_new(),
        .global_funcs = get_global_functions(ctx),
        .debug = debug,
    };

    bl_type_t *string_type = Type(StringType);
    bl_type_t *say_type = Type(
        FunctionType,
        .args=LIST(bl_type_t*, string_type, Type(OptionalType, .nonnil=string_type)),
        .ret=Type(VoidType));

    gcc_param_t *gcc_str_param = gcc_new_param(ctx, NULL, gcc_type(ctx, STRING), "str");
    gcc_func_t *puts_func = gcc_new_func(ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, INT), "puts", 1, &gcc_str_param, 0);
    gcc_rvalue_t *puts_rvalue = gcc_get_func_address(puts_func, NULL);
    hashmap_set(env.bindings, intern_str("say"), new(binding_t, .rval=puts_rvalue, .type=say_type, .is_global=true));
#define DEFTYPE(t) hashmap_set(env.bindings, intern_str(#t), new(binding_t, .is_global=true, .rval=gcc_new_string(ctx, #t), .type=Type(TypeType, .type=Type(t##Type))));
    // Primitive types:
    DEFTYPE(Bool); DEFTYPE(Void); DEFTYPE(Abort);
    DEFTYPE(Int); DEFTYPE(Int32); DEFTYPE(Int16); DEFTYPE(Int8);
    DEFTYPE(Num); DEFTYPE(Num32);
    DEFTYPE(String);
#undef DEFTYPE

    gcc_func_t *range_tostring_func = gcc_new_func(
        env.ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(env.ctx, STRING),
        "range_tostring", 2, (gcc_param_t*[]){
            gcc_new_param(env.ctx, NULL, bl_type_to_gcc(&env, Type(RangeType)), "range"),
            gcc_new_param(env.ctx, NULL, gcc_type(env.ctx, VOID_PTR), "range"),
        }, 0);
    hashmap_set(env.tostring_funcs, Type(RangeType), range_tostring_func);

    gcc_func_t *main_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_EXPORTED, gcc_type(ctx, VOID),
        "main", 0, NULL, 0);
    gcc_block_t *block = gcc_new_block(main_func, NULL);
    compile_statement(&env, &block, ast);
    gcc_return_void(block, NULL);

    return gcc_compile(ctx);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
