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
static hashmap_t *load_global_functions(gcc_ctx_t *ctx)
{
    gcc_type_t *t_str = gcc_get_type(ctx, GCC_T_STRING),
               *t_int = gcc_get_type(ctx, GCC_T_INT),
               *t_double = gcc_get_type(ctx, GCC_T_DOUBLE),
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
    MAKE_FUNC(t_double, "sane_fmod", 2, PARAM(t_double, "num"), PARAM(t_double, "modulus"));
#undef MAKE_FUNC
#undef PARAM

    return funcs;
}

static void extern_method(env_t *env, const char *extern_name, bl_type_t *t, const char *method_name, bl_type_t *fn_type, int is_vararg)
{
    auto fn = Match(fn_type, FunctionType);
    gcc_param_t *params[LIST_LEN(fn->arg_types)];
    binding_t *type_binding = hashmap_get(env->bindings, type_to_string(t));
    assert(type_binding && type_binding->namespace);
    for (int64_t i = 0; i < LIST_LEN(fn->arg_types); i++) {
        istr_t arg_name = fn->arg_names ? LIST_ITEM(fn->arg_names, i) : fresh("arg");
        bl_type_t *arg_type = LIST_ITEM(fn->arg_types, i);
        params[i] = gcc_new_param(env->ctx, NULL, bl_type_to_gcc(env, arg_type), arg_name);
    }
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, bl_type_to_gcc(env, fn->ret),
                                    extern_name, LIST_LEN(fn->arg_types), params, is_vararg);
    hashmap_set(type_binding->namespace, method_name,
                new(binding_t, .is_global=true, .type=fn_type, .func=func));
}

static void load_string_methods(env_t *env)
{
    extern_method(env, "bl_string_uppercased", Type(StringType), "uppercased",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, Type(StringType)), .ret=Type(StringType)), 0);
    extern_method(env, "bl_string_lowercased", Type(StringType), "lowercased",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, Type(StringType)), .ret=Type(StringType)), 0);
    extern_method(env, "bl_string_capitalized", Type(StringType), "capitalized",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, Type(StringType)), .ret=Type(StringType)), 0);
    extern_method(env, "bl_string_titlecased", Type(StringType), "titlecased",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, Type(StringType)), .ret=Type(StringType)), 0);
    extern_method(env, "bl_string_starts_with", Type(StringType), "starts_with",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, Type(StringType), Type(StringType)), .ret=Type(BoolType)), 0);
    extern_method(env, "bl_string_ends_with", Type(StringType), "ends_with",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, Type(StringType), Type(StringType)), .ret=Type(BoolType)), 0);
    extern_method(env, "bl_string_replace", Type(StringType), "replace",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, Type(StringType), Type(StringType), Type(StringType), Type(OptionalType, .nonnil=Type(IntType))), .ret=Type(StringType)), 0);
}

gcc_result_t *compile_file(gcc_ctx_t *ctx, file_t *f, ast_t *ast, bool debug)
{
    env_t env = {
        .ctx = ctx,
        .file = f,
        .bindings = hashmap_new(),
        .tostring_funcs = hashmap_new(),
        .gcc_types = hashmap_new(),
        .global_funcs = load_global_functions(ctx),
        .debug = debug,
    };

    bl_type_t *string_type = Type(StringType);
    bl_type_t *say_type = Type(
        FunctionType,
        .arg_types=LIST(bl_type_t*, string_type, Type(OptionalType, .nonnil=Type(BoolType))),
        .ret=Type(VoidType));

    gcc_param_t *gcc_say_params[] = {
        gcc_new_param(ctx, NULL, gcc_type(ctx, STRING), "str"),
        gcc_new_param(ctx, NULL, gcc_get_ptr_type(gcc_type(ctx, BOOL)), "nl"),
    };
    gcc_func_t *say_func = gcc_new_func(ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, INT), "say", 2, gcc_say_params, 0);
    gcc_rvalue_t *say_rvalue = gcc_get_func_address(say_func, NULL);
    hashmap_set(env.bindings, intern_str("say"), new(binding_t, .rval=say_rvalue, .type=say_type, .is_global=true));
#define DEFTYPE(t) hashmap_set(env.bindings, intern_str(#t), new(binding_t, .is_global=true, .rval=gcc_new_string(ctx, #t), .type=Type(TypeType), .type_value=Type(t##Type), .namespace=hashmap_new()));
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

    load_string_methods(&env);

    gcc_param_t* main_params[] = {
        gcc_new_param(env.ctx, NULL, gcc_type(env.ctx, INT), "argc"),
        gcc_new_param(env.ctx, NULL, gcc_get_ptr_type(gcc_type(env.ctx, STRING)), "argv"),
    };
    gcc_func_t *main_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_EXPORTED, gcc_type(ctx, INT),
        "main", 2, main_params, 0);
    gcc_block_t *block = gcc_new_block(main_func, fresh("main"));

    // Set up `PROGRAM_NAME`
    gcc_lvalue_t *program_name = gcc_local(main_func, NULL, gcc_type(env.ctx, STRING), "PROGRAM_NAME");
    gcc_lvalue_t *name_lval = gcc_rvalue_dereference(gcc_param_as_rvalue(main_params[1]), NULL);
    gcc_assign(block, NULL, program_name, gcc_lvalue_as_rvalue(name_lval));
    hashmap_set(env.bindings, intern_str("PROGRAM_NAME"),
                new(binding_t, .rval=gcc_lvalue_as_rvalue(program_name), .type=Type(StringType)));

    // Set up `args`
    bl_type_t *args_t = Type(ListType, .item_type=Type(StringType));
    gcc_type_t *args_gcc_t = bl_type_to_gcc(&env, args_t);
    gcc_func_t *arg_func = gcc_new_func(
        env.ctx, NULL, GCC_FUNCTION_IMPORTED, args_gcc_t, "arg_list", 2, (gcc_param_t*[]){
            gcc_new_param(env.ctx, NULL, gcc_type(env.ctx, INT), "argc"),
            gcc_new_param(env.ctx, NULL, gcc_get_ptr_type(gcc_type(env.ctx, STRING)), "argv"),
        }, 0);

    gcc_lvalue_t *args = gcc_local(main_func, NULL, args_gcc_t, "args");
    hashmap_set(env.bindings, intern_str("args"), new(binding_t, .rval=gcc_lvalue_as_rvalue(args), .type=args_t));
    gcc_rvalue_t *arg_list_args[] = {
        gcc_param_as_rvalue(main_params[0]),
        gcc_param_as_rvalue(main_params[1]),
    };
    gcc_rvalue_t *arg_list = gcc_call(env.ctx, NULL, arg_func, 2, arg_list_args);
    gcc_assign(block, NULL, args, arg_list);

    compile_statement(&env, &block, ast);
    if (block)
        gcc_return(block, NULL, gcc_zero(ctx, gcc_type(ctx, INT)));

    return gcc_compile(ctx);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
