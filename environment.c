// Code related to the ambient state/environment used in compilation
#include <intern.h>
#include <limits.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "files.h"
#include "types.h"
#include "environment.h"
#include "compile/compile.h"
#include "compile/libgccjit_abbrev.h"

// Load a bunch of global (external) functions
static void load_global_functions(env_t *env)
{
    gcc_ctx_t *ctx = env->ctx;
    gcc_type_t *t_str = gcc_get_ptr_type(gcc_get_type(ctx, GCC_T_CHAR)),
               *t_int = gcc_get_type(ctx, GCC_T_INT),
               *t_double = gcc_get_type(ctx, GCC_T_DOUBLE),
               *t_void = gcc_get_type(ctx, GCC_T_VOID),
               *t_void_ptr = gcc_get_type(ctx, GCC_T_VOID_PTR),
               *t_size = gcc_get_type(ctx, GCC_T_SIZE),
               *t_file = gcc_get_type(ctx, GCC_T_FILE_PTR),
               *t_range = bl_type_to_gcc(env, Type(RangeType)),
               *t_bl_str = bl_type_to_gcc(env, Type(ArrayType, .item_type=Type(CharType)));
    hashmap_t *funcs = env->global_funcs;

#define PARAM(type, name) gcc_new_param(ctx, NULL, type, name)
#define LOAD_FUNC(t_ret, name, variadic, ...) hashmap_set(funcs, intern_str(name), \
    gcc_new_func(ctx, NULL, GCC_FUNCTION_IMPORTED, t_ret, name, \
                 sizeof((gcc_param_t*[]){__VA_ARGS__})/sizeof(gcc_param_t*),\
                 (gcc_param_t*[]){__VA_ARGS__}, variadic))
    LOAD_FUNC(t_void_ptr, "GC_malloc", 0, PARAM(t_size, "size"));
    LOAD_FUNC(t_void_ptr, "GC_malloc_atomic", 0, PARAM(t_size, "size"));
    LOAD_FUNC(t_void_ptr, "GC_realloc", 0, PARAM(t_void_ptr, "data"), PARAM(t_size, "size"));
    LOAD_FUNC(t_void_ptr, "memcpy", 0, PARAM(t_void_ptr, "dest"), PARAM(t_void_ptr, "src"), PARAM(t_size, "size"));
    LOAD_FUNC(t_file, "open_memstream", 0, PARAM(gcc_get_ptr_type(t_str), "buf"), PARAM(gcc_get_ptr_type(t_size), "size"));
    LOAD_FUNC(t_void, "free", 0, PARAM(t_void_ptr, "ptr"));
    LOAD_FUNC(t_int, "fwrite", 0, PARAM(t_void_ptr, "data"), PARAM(t_size, "size"), PARAM(t_size, "nmemb"), PARAM(t_file, "file"));
    LOAD_FUNC(t_int, "fputs", 0, PARAM(t_str, "str"), PARAM(t_file, "file"));
    LOAD_FUNC(t_int, "fputc", 0, PARAM(gcc_get_type(ctx, GCC_T_CHAR), "c"), PARAM(t_file, "file"));
    LOAD_FUNC(t_int, "fprintf", 1, PARAM(t_file, "file"), PARAM(t_str, "format"));
    LOAD_FUNC(t_int, "fflush", 0, PARAM(t_file, "file"));
    LOAD_FUNC(t_int, "fclose", 0, PARAM(t_file, "file"));
    LOAD_FUNC(t_str, "intern_str", 0, PARAM(t_str, "str"));
    LOAD_FUNC(t_str, "intern_strn", 0, PARAM(t_str, "str"), PARAM(t_size, "length"));
    LOAD_FUNC(t_str, "intern_strf", 1, PARAM(t_str, "fmt"));
    LOAD_FUNC(t_size, "intern_len", 0, PARAM(t_str, "str"));
    LOAD_FUNC(t_void, "fail", 1, PARAM(t_str, "message"));
    LOAD_FUNC(t_double, "sane_fmod", 1, PARAM(t_double, "num"), PARAM(t_double, "modulus"));
    LOAD_FUNC(t_int, "range_print", 1, PARAM(t_range, "range"), PARAM(t_file, "file"), PARAM(t_void_ptr, "stack"));
    hashmap_set(env->print_funcs, Type(RangeType), hashmap_get(env->global_funcs, intern_str("range_print")));
    LOAD_FUNC(t_bl_str, "range_slice", 0, PARAM(t_bl_str, "array"), PARAM(t_range, "range"), PARAM(t_size, "item_size"));
#undef LOAD_FUNC
#undef PARAM
}

static void extern_method(env_t *env, const char *extern_name, bl_type_t *t, const char *method_name, bl_type_t *fn_type, int is_vararg)
{
    auto fn = Match(fn_type, FunctionType);
    gcc_param_t *params[LIST_LEN(fn->arg_types)];
    for (int64_t i = 0; i < LIST_LEN(fn->arg_types); i++) {
        istr_t arg_name = fn->arg_names ? LIST_ITEM(fn->arg_names, i) : fresh("arg");
        bl_type_t *arg_type = LIST_ITEM(fn->arg_types, i);
        params[i] = gcc_new_param(env->ctx, NULL, bl_type_to_gcc(env, arg_type), arg_name);
    }
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, bl_type_to_gcc(env, fn->ret),
                                    extern_name, LIST_LEN(fn->arg_types), params, is_vararg);
    hashmap_t *ns = get_namespace(env, t);
    hashmap_set(ns, intern_str(method_name), new(binding_t, .is_global=true, .type=fn_type, .func=func));
}

static bl_type_t *define_string_type(env_t *env)
{
    bl_type_t *str_type = Type(ArrayType, .item_type=Type(CharType));
    gcc_rvalue_t *rval = gcc_str(env->ctx, "String");
    binding_t *binding = new(binding_t, .is_global=true, .rval=rval, .type=Type(TypeType), .type_value=str_type);
    hashmap_set(env->bindings, intern_str("String"), binding);
    hashmap_set(env->bindings, str_type, binding);

    extern_method(env, "bl_string_uppercased", str_type, "uppercased",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, str_type), .arg_names=LIST(istr_t, intern_str("str")), .ret=str_type), 0);
    extern_method(env, "bl_string_lowercased", str_type, "lowercased",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, str_type), .arg_names=LIST(istr_t, intern_str("str")), .ret=str_type), 0);
    extern_method(env, "bl_string_capitalized", str_type, "capitalized",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, str_type), .arg_names=LIST(istr_t, intern_str("str")), .ret=str_type), 0);
    extern_method(env, "bl_string_titlecased", str_type, "titlecased",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, str_type), .arg_names=LIST(istr_t, intern_str("str")), .ret=str_type), 0);
    extern_method(env, "bl_string_quoted", str_type, "quoted",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, str_type, Type(BoolType)),
                       .arg_names=LIST(istr_t, intern_str("str"), intern_str("colorize")),
                       .arg_defaults=LIST(ast_t*, NULL, FakeAST(Bool, .b=false)),
                       .ret=str_type), 0);
    extern_method(env, "bl_string_starts_with", str_type, "starts_with",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, str_type, str_type),
                       .arg_names=LIST(istr_t, intern_str("str"), intern_str("prefix")), 
                       .ret=Type(BoolType)), 0);
    extern_method(env, "bl_string_ends_with", str_type, "ends_with",
                  Type(FunctionType, .arg_types=LIST(bl_type_t*, str_type, str_type),
                       .arg_names=LIST(istr_t, intern_str("str"), intern_str("suffix")), 
                       .ret=Type(BoolType)), 0);
    extern_method(env, "bl_string_trimmed", str_type, "trimmed",
                  Type(FunctionType,
                       .arg_types=LIST(bl_type_t*, str_type, str_type, Type(BoolType), Type(BoolType)),
                       .arg_names=LIST(istr_t, intern_str("str"), intern_str("chars"), intern_str("trim_left"), intern_str("trim_right")),
                       .arg_defaults=LIST(ast_t*, NULL, FakeAST(StringJoin, .children=LIST(ast_t*,FakeAST(StringLiteral, .str=intern_str(" \t\r\n")))),
                                          FakeAST(Bool, .b=true), FakeAST(Bool, .b=true)),
                       .ret=str_type), 0);
    extern_method(env, "bl_string_replace", str_type, "replace",
                  Type(FunctionType,
                       .arg_types=LIST(bl_type_t*, str_type, str_type, str_type, Type(IntType)),
                       .arg_names=LIST(istr_t, intern_str("str"), intern_str("pattern"), intern_str("replacement"), intern_str("limit")),
                       .arg_defaults=LIST(ast_t*, NULL, NULL, NULL, FakeAST(Int, .i=-1, .precision=64)),
                       .ret=str_type), 0);

    return str_type;
}

static void define_num_types(env_t *env)
{
    bl_type_t *num_type = Type(NumType);
    {
        gcc_rvalue_t *rval = gcc_str(env->ctx, "Num");
        binding_t *binding = new(binding_t, .is_global=true, .rval=rval, .type=Type(TypeType), .type_value=num_type);
        hashmap_set(env->bindings, intern_str("Num"), binding);
        hashmap_set(env->bindings, num_type, binding);
    }

    bl_type_t *num32_type = Type(Num32Type);
    {
        gcc_rvalue_t *rval = gcc_str(env->ctx, "Num32");
        binding_t *binding = new(binding_t, .is_global=true, .rval=rval, .type=Type(TypeType), .type_value=num32_type);
        hashmap_set(env->bindings, intern_str("Num32"), binding);
        hashmap_set(env->bindings, num32_type, binding);
    }

    const char *unary_methods[][2] = {
        {"acos"},{"asin"},{"atan"},{"cos"},{"sin"},{"tan"},{"cosh"},{"sinh"},
        {"tanh"},{"acosh"},{"asinh"},{"atanh"},{"exp"},{"log"},{"log10"},
        {"exp10"},{"expm1"}, {"log1p"}, {"logb"}, {"exp2"}, {"log2"},
        {"sqrt"}, {"cbrt"}, {"ceil"}, {"fabs", "abs"}, {"floor"},
        {"significand"}, {"j0"}, {"j1"}, {"y0"}, {"y1"}, {"erf"}, {"erfc"},
        {"tgamma"}, {"rint"}, {"nextdown","next_lowest"}, {"nextup","next_highest"}, {"round"}, {"trunc","truncate"},
        {"roundeven","round_even"},
    };
    for (size_t i = 0; i < sizeof(unary_methods)/sizeof(unary_methods[0]); i++) {
        const char *c_name = unary_methods[i][0];
        const char *alias = unary_methods[i][1];
        if (!alias) alias = c_name;
        extern_method(env, c_name, num_type, alias,
                      Type(FunctionType, .arg_types=LIST(bl_type_t*, num_type), .arg_names=LIST(istr_t, intern_str("num")), .ret=num_type), 0);
        extern_method(env, intern_strf("%sf", c_name), num32_type, alias,
                      Type(FunctionType, .arg_types=LIST(bl_type_t*, num32_type), .arg_names=LIST(istr_t, intern_str("num")), .ret=num32_type), 0);
    }

    const char *binary_methods[][4] = {
        {"atan2",NULL,"y","x"}, {"pow",NULL,"base","exponent"},
        {"hypot",NULL,"x","y"}, {"fmod","modulo","num","modulus"}, {"copysign","copy_sign","num","with_sign"},
        {"nextafter","next_toward","num","toward"}, {"remainder",NULL,"num","divisor"},
        {"fmax","max","x","y"}, {"fmin", "min","x","y"},
        {"fmaxmag","biggest","x","y"}, {"fminmag","smallest","x","y"},
        {"fdim","distance","x","y"},
    };
    for (size_t i = 0; i < sizeof(binary_methods)/sizeof(binary_methods[0]); i++) {
        const char *c_name = binary_methods[i][0];
        const char *alias = binary_methods[i][1];
        if (!alias) alias = c_name;
        istr_t arg1 = intern_str(binary_methods[i][2]);
        istr_t arg2 = intern_str(binary_methods[i][3]);
        extern_method(env, c_name, num_type, alias,
                      Type(FunctionType, .arg_types=LIST(bl_type_t*, num_type, num_type), .arg_names=LIST(istr_t, arg1, arg2), .ret=num_type), 0);
        extern_method(env, intern_strf("%sf", c_name), num32_type, alias,
                      Type(FunctionType, .arg_types=LIST(bl_type_t*, num32_type, num32_type), .arg_names=LIST(istr_t, arg1, arg2), .ret=num32_type), 0);
    }

    const char *bool_methods[][2] = {
        {"isinf","is_infinite"}, {"finite","is_finite"}, {"isnan","is_nan"},
    };
    for (size_t i = 0; i < sizeof(bool_methods)/sizeof(bool_methods[0]); i++) {
        const char *c_name = bool_methods[i][0];
        const char *alias = bool_methods[i][1];
        if (!alias) alias = c_name;
        extern_method(env, c_name, num_type, alias,
                      Type(FunctionType, .arg_types=LIST(bl_type_t*, num_type), .arg_names=LIST(istr_t, intern_str("num")), .ret=Type(BoolType)), 0);
        extern_method(env, intern_strf("%sf", c_name), num32_type, alias,
                      Type(FunctionType, .arg_types=LIST(bl_type_t*, num32_type), .arg_names=LIST(istr_t, intern_str("num")), .ret=Type(BoolType)), 0);
    }

    struct {const char *name; double val;} constants[] = {
        {"E", M_E}, {"Log2_E", M_LOG2E}, {"Log10_E", M_LOG10E}, {"Ln_2", M_LN2}, {"Ln_10", M_LN10},
        {"Pi", M_PI}, {"Tau", 2*M_PI}, {"HalfPi", M_PI_2}, {"QuarterPi", M_PI_4}, {"InversePi", M_1_PI},
        {"DoubleInversePi", M_2_PI}, {"DoubleInverseSqrtPi", M_2_SQRTPI}, {"Sqrt2", M_SQRT2}, {"InverseSqrt2", M_SQRT1_2},
        {"NaN", nan("")}, {"Infinity", 1./0.},
    };

    { // Num NaN and Infinity:
        gcc_type_t *gcc_num_t = bl_type_to_gcc(env, num_type);
        hashmap_t *ns = get_namespace(env, num_type);
        for (size_t i = 0; i < sizeof(constants)/sizeof(constants[0]); i++)
            hashmap_set(ns, intern_str(constants[i].name), new(binding_t, .is_global=true, .type=num_type,
                                                               .rval=gcc_rvalue_from_double(env->ctx, gcc_num_t, constants[i].val)));
    }

    { // Num32 NaN and Infinity:
        gcc_type_t *gcc_num32_t = bl_type_to_gcc(env, num32_type);
        hashmap_t *ns = get_namespace(env, num32_type);
        for (size_t i = 0; i < sizeof(constants)/sizeof(constants[0]); i++)
            hashmap_set(ns, intern_str(constants[i].name), new(binding_t, .is_global=true, .type=num32_type,
                                                               .rval=gcc_rvalue_from_double(env->ctx, gcc_num32_t, constants[i].val)));
    }

    // oddballs: ldexp jn yn llogb lrint lround fma
}

static void define_int_methods(env_t *env)
{
    { // Int64 methods
        bl_type_t *i64 = Type(IntType);
        hashmap_t *ns = get_namespace(env, i64);
        gcc_type_t *gcc_i64 = bl_type_to_gcc(env, i64);

        gcc_func_t *abs_func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_i64,
                                            "labs", 1, (gcc_param_t*[]){gcc_new_param(env->ctx, NULL, gcc_i64, "i")}, 0);
        hashmap_set(
            ns, intern_str("abs"),
            new(binding_t, .is_global=true, .func=abs_func,
                .type=Type(FunctionType, .arg_types=LIST(bl_type_t*, i64), .arg_names=LIST(istr_t, intern_str("i")), .ret=i64)));

        gcc_func_t *rand_func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_i64, "random", 0, NULL, 0);
        hashmap_set(
            ns, intern_str("random"),
            new(binding_t, .is_global=true, .func=rand_func,
                .type=Type(FunctionType, .arg_types=LIST(bl_type_t*), .arg_names=LIST(istr_t), .ret=i64)));

        hashmap_set(ns, intern_str("Min"), new(binding_t, .is_global=true, .type=i64, .rval=gcc_rvalue_from_long(env->ctx, gcc_i64, INT64_MIN)));
        hashmap_set(ns, intern_str("Max"), new(binding_t, .is_global=true, .type=i64, .rval=gcc_rvalue_from_long(env->ctx, gcc_i64, INT64_MAX)));
    }

    { // Int32 methods
        bl_type_t *i32 = Type(Int32Type);
        hashmap_t *ns = get_namespace(env, i32);
        gcc_type_t *gcc_i32 = bl_type_to_gcc(env, i32);

        gcc_func_t *abs_func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_i32,
                                            "labs", 1, (gcc_param_t*[]){gcc_new_param(env->ctx, NULL, gcc_i32, "i")}, 0);
        hashmap_set(
            ns, intern_str("abs"),
            new(binding_t, .is_global=true, .func=abs_func,
                .type=Type(FunctionType, .arg_types=LIST(bl_type_t*, i32), .arg_names=LIST(istr_t, intern_str("i")), .ret=i32)));

        gcc_func_t *rand_func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_i32, "rand", 0, NULL, 0);
        hashmap_set(
            ns, intern_str("random"),
            new(binding_t, .is_global=true, .func=rand_func,
                .type=Type(FunctionType, .arg_types=LIST(bl_type_t*), .arg_names=LIST(istr_t), .ret=i32)));

        hashmap_set(ns, intern_str("Min"), new(binding_t, .is_global=true, .type=i32, .rval=gcc_rvalue_from_long(env->ctx, gcc_i32, INT32_MIN)));
        hashmap_set(ns, intern_str("Max"), new(binding_t, .is_global=true, .type=i32, .rval=gcc_rvalue_from_long(env->ctx, gcc_i32, INT32_MAX)));
    }

    { // Int16 methods
        bl_type_t *i16 = Type(Int16Type);
        hashmap_t *ns = get_namespace(env, i16);
        gcc_type_t *gcc_i16 = bl_type_to_gcc(env, i16);
        hashmap_set(ns, intern_str("Min"), new(binding_t, .is_global=true, .type=i16, .rval=gcc_rvalue_from_long(env->ctx, gcc_i16, INT16_MIN)));
        hashmap_set(ns, intern_str("Max"), new(binding_t, .is_global=true, .type=i16, .rval=gcc_rvalue_from_long(env->ctx, gcc_i16, INT16_MAX)));
    }

    { // Int8 methods
        bl_type_t *i8 = Type(Int8Type);
        hashmap_t *ns = get_namespace(env, i8);
        gcc_type_t *gcc_i8 = bl_type_to_gcc(env, i8);
        hashmap_set(ns, intern_str("Min"), new(binding_t, .is_global=true, .type=i8, .rval=gcc_rvalue_from_long(env->ctx, gcc_i8, INT8_MIN)));
        hashmap_set(ns, intern_str("Max"), new(binding_t, .is_global=true, .type=i8, .rval=gcc_rvalue_from_long(env->ctx, gcc_i8, INT8_MAX)));
    }
}

env_t *new_environment(gcc_ctx_t *ctx, jmp_buf *on_err, bl_file_t *f, bool debug)
{
    env_t *env = new(env_t,
        .ctx = ctx,
        .on_err = on_err,
        .file = f,
        .bindings = hashmap_new(),
        .type_namespaces = hashmap_new(),
        .tuple_types = hashmap_new(),
        .print_funcs = hashmap_new(),
        .cmp_funcs = hashmap_new(),
        .gcc_types = hashmap_new(),
        .global_funcs = hashmap_new(),
        .debug = debug,
    );

    load_global_functions(env);

    bl_type_t *string_type = define_string_type(env);
    bl_type_t *say_type = Type(
        FunctionType,
        .arg_names=LIST(istr_t, intern_str("str"), intern_str("end")),
        .arg_types=LIST(bl_type_t*, string_type, string_type),
        .arg_defaults=LIST(ast_t*, NULL, FakeAST(StringLiteral, .str=intern_str("\n"))),
        .ret=Type(VoidType));

    gcc_param_t *gcc_say_params[] = {
        gcc_new_param(ctx, NULL, bl_type_to_gcc(env, string_type), "str"),
        gcc_new_param(ctx, NULL, bl_type_to_gcc(env, string_type), "end"),
    };
    gcc_func_t *say_func = gcc_new_func(ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, VOID), "say", 2, gcc_say_params, 0);
    gcc_rvalue_t *say_rvalue = gcc_get_func_address(say_func, NULL);
    hashmap_set(env->bindings, intern_str("say"), new(binding_t, .rval=say_rvalue, .type=say_type, .is_global=true));
    define_num_types(env);
#define DEFTYPE(t) hashmap_set(env->bindings, intern_str(#t), new(binding_t, .is_global=true, .rval=gcc_str(ctx, #t), .type=Type(TypeType), .type_value=Type(t##Type)));
    // Primitive types:
    DEFTYPE(Bool); DEFTYPE(Void); DEFTYPE(Abort);
    DEFTYPE(Int); DEFTYPE(Int32); DEFTYPE(Int16); DEFTYPE(Int8); DEFTYPE(Char);
    // DEFTYPE(Num);
    // DEFTYPE(Num32);
#undef DEFTYPE
    define_int_methods(env);

    return env;
}

void compile_err(env_t *env, ast_t *ast, const char *fmt, ...)
{
    fputs("\x1b[31;7;1m", stderr);
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputs("\x1b[m\n\n", stderr);
    if (ast)
        fprint_span(stderr, ast->span, "\x1b[31;1m", 2);

    if (env->on_err)
        longjmp(*env->on_err, 1);

    raise(SIGABRT);
    exit(1);
}

binding_t *get_binding(env_t *env, const char *name)
{
    return hashmap_get(env->bindings, intern_str(name));
}

binding_t *get_ast_binding(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case Var: {
        return get_binding(env, Match(ast, Var)->name);
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        binding_t *b = get_ast_binding(env, access->fielded);
        if (!b) return NULL;
        if (b->type->tag == TypeType)
            return get_from_namespace(env, b->type_value, access->field);
        else
            return get_from_namespace(env, b->type, access->field);
    }
    default: compile_err(env, ast, "I can't figure out at compile-time what this refers to");
    }
}

hashmap_t *get_namespace(env_t *env, bl_type_t *t)
{
    hashmap_t *ns = hashmap_get(env->type_namespaces, t);
    if (!ns) {
        ns = hashmap_new();
        hashmap_set(env->type_namespaces, t, ns);
    }
    return ns;
}

binding_t *get_from_namespace(env_t *env, bl_type_t *t, const char *name)
{
    return hashmap_get(get_namespace(env, t), intern_str(name));
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
