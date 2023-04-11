// Code related to the ambient state/environment used in compilation
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

#define load_global_func(env, t_ret, name, ...) _load_global_func(env, t_ret, name, \
                 sizeof((gcc_param_t*[]){__VA_ARGS__})/sizeof(gcc_param_t*),\
                 (gcc_param_t*[]){__VA_ARGS__}, 0)
#define load_global_var_func(env, t_ret, name, ...) _load_global_func(env, t_ret, name, \
                 sizeof((gcc_param_t*[]){__VA_ARGS__})/sizeof(gcc_param_t*),\
                 (gcc_param_t*[]){__VA_ARGS__}, 1)

static inline void _load_global_func(env_t *env, gcc_type_t *t_ret, const char *name, int nargs, gcc_param_t *args[nargs], int is_vararg) {
    hset(env->global_funcs, heap_str(name),
         gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, t_ret, name, nargs, args, is_vararg));
}

typedef struct {
    const char *name;
    bl_type_t *t;
    ast_t *default_val;
} bl_arg_t;
#define ARG(...) ((bl_arg_t){__VA_ARGS__})

static inline void _load_method(
    env_t *env, bl_hashmap_t *ns, const char *extern_name, const char *method_name, bl_type_t *ret_t, int nargs, bl_arg_t args[nargs]
) {
    gcc_param_t *params[nargs];
    NEW_LIST(const char*, arg_name_list);
    NEW_LIST(bl_type_t*, arg_type_list);
    NEW_LIST(ast_t*, arg_default_list);
    for (int i = 0; i < nargs; i++) {
        params[i] = gcc_new_param(env->ctx, NULL, bl_type_to_gcc(env, args[i].t), args[i].name);
        APPEND(arg_name_list, args[i].name);
        APPEND(arg_type_list, args[i].t);
        APPEND(arg_default_list, args[i].default_val);
    }
    
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, bl_type_to_gcc(env, ret_t),
                                    extern_name, nargs, params, 0);
    bl_type_t *fn_type = Type(FunctionType, .arg_types=arg_type_list, .arg_names=arg_name_list, .arg_defaults=arg_default_list, .ret=ret_t);
    hset(ns, heap_str(method_name), new(binding_t, .type=fn_type, .func=func));
}

#define load_method(env,ns,ename,mname,ret,...) _load_method(env,ns,ename,mname,ret,\
                 sizeof((bl_arg_t[]){__VA_ARGS__})/sizeof(bl_arg_t),\
                 (bl_arg_t[]){__VA_ARGS__})

// Load a bunch of global (external) functions
static void load_global_functions(env_t *env)
{
    gcc_ctx_t *ctx = env->ctx;
    gcc_type_t *t_str = gcc_get_ptr_type(gcc_get_type(ctx, GCC_T_CHAR)),
               *t_int = gcc_get_type(ctx, GCC_T_INT),
               *t_int64 = gcc_get_type(ctx, GCC_T_INT64),
               *t_u32 = gcc_get_type(ctx, GCC_T_UINT32),
               *t_double = gcc_get_type(ctx, GCC_T_DOUBLE),
               *t_void = gcc_get_type(ctx, GCC_T_VOID),
               *t_void_ptr = gcc_get_type(ctx, GCC_T_VOID_PTR),
               *t_size = gcc_get_type(ctx, GCC_T_SIZE),
               *t_bool = gcc_get_type(ctx, GCC_T_BOOL),
               *t_file = gcc_get_type(ctx, GCC_T_FILE_PTR),
               *t_range = bl_type_to_gcc(env, Type(RangeType)),
               *t_bl_str = bl_type_to_gcc(env, Type(ArrayType, .item_type=Type(CharType)));

#define PARAM(type, name) gcc_new_param(ctx, NULL, type, name)
    load_global_func(env, t_void_ptr, "GC_malloc", PARAM(t_size, "size"));
    load_global_func(env, t_void_ptr, "GC_malloc_atomic", PARAM(t_size, "size"));
    load_global_func(env, t_void_ptr, "GC_realloc", PARAM(t_void_ptr, "data"), PARAM(t_size, "size"));
    load_global_func(env, t_void_ptr, "memcpy", PARAM(t_void_ptr, "dest"), PARAM(t_void_ptr, "src"), PARAM(t_size, "size"));
    load_global_func(env, t_file, "open_memstream", PARAM(gcc_get_ptr_type(t_str), "buf"), PARAM(gcc_get_ptr_type(t_size), "size"));
    load_global_func(env, t_void, "free", PARAM(t_void_ptr, "ptr"));
    load_global_func(env, t_int, "fwrite", PARAM(t_void_ptr, "data"), PARAM(t_size, "size"), PARAM(t_size, "nmemb"), PARAM(t_file, "file"));
    load_global_func(env, t_int, "fputs", PARAM(t_str, "str"), PARAM(t_file, "file"));
    load_global_func(env, t_int, "puts", PARAM(t_str, "str"));
    load_global_func(env, t_int, "fputc", PARAM(gcc_get_type(ctx, GCC_T_CHAR), "c"), PARAM(t_file, "file"));
    load_global_var_func(env, t_int, "fprintf", PARAM(t_file, "file"), PARAM(t_str, "format"));
    load_global_func(env, t_int, "fflush", PARAM(t_file, "file"));
    load_global_func(env, t_int, "fclose", PARAM(t_file, "file"));
    load_global_func(env, t_str, "heap_str", PARAM(t_str, "str"));
    load_global_func(env, t_str, "heap_strn", PARAM(t_str, "str"), PARAM(t_size, "length"));
    load_global_var_func(env, t_str, "heap_strf", PARAM(t_str, "fmt"));
    load_global_var_func(env, t_void, "fail", PARAM(t_str, "message"));
    load_global_func(env, t_double, "sane_fmod", PARAM(t_double, "num"), PARAM(t_double, "modulus"));
    load_global_func(env, t_int, "range_print", PARAM(t_range, "range"), PARAM(t_file, "file"), PARAM(t_void_ptr, "stack"));
    gcc_func_t *range_print = hget(env->global_funcs, "range_print", gcc_func_t*);
    hset(get_namespace(env, Type(RangeType)), "__print",
         new(binding_t, .func=range_print, .sym_name="range_print"));
    load_global_func(env, t_bl_str, "range_slice", PARAM(t_bl_str, "array"), PARAM(t_range, "range"), PARAM(t_size, "item_size"));
    load_global_func(env, t_void_ptr, "dlopen", PARAM(t_str, "filename"), PARAM(t_int, "flags"));
    load_global_func(env, t_void_ptr, "dlsym", PARAM(t_void_ptr, "handle"), PARAM(t_str, "symbol"));
    load_global_func(env, t_int, "dlclose", PARAM(t_void_ptr, "handle"));
    load_global_func(env, t_bl_str, "array_cow", PARAM(t_void_ptr, "array"), PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_bl_str, "array_flatten", PARAM(t_void_ptr, "array"), PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_bl_str, "array_insert", PARAM(t_void_ptr, "array"),
                     PARAM(t_void_ptr, "item"),
                     PARAM(t_int64, "index"),
                     PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_bl_str, "array_insert_all", PARAM(t_void_ptr, "array"),
                     PARAM(t_void_ptr, "other"),
                     PARAM(t_int64, "index"),
                     PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_bl_str, "array_remove", PARAM(t_void_ptr, "array"),
                     PARAM(t_int64, "index"),
                     PARAM(t_int64, "count"),
                     PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_bl_str, "array_sort", PARAM(t_void_ptr, "array"),
                     PARAM(t_void_ptr, "compare"),
                     PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_bl_str, "array_shuffle", PARAM(t_void_ptr, "array"), PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));

    // int halfsiphash(const void *in, const size_t inlen, const void *k, void *out, const size_t outlen);
    load_global_func(env, t_int, "halfsiphash", PARAM(t_void_ptr, "in"), PARAM(t_size, "inlen"), PARAM(t_void_ptr, "k"),
                     PARAM(t_void_ptr, "out"), PARAM(t_size, "outlen"));

    load_global_func(env, t_void_ptr, "bl_hashmap_get", PARAM(t_void_ptr, "table"), PARAM(t_void_ptr, "key_hash"),
                     PARAM(t_void_ptr, "key_cmp"), PARAM(t_size, "entry_size"), PARAM(t_void_ptr, "key"), PARAM(t_size, "value_offset"));
    load_global_func(env, t_void_ptr, "bl_hashmap_set", PARAM(t_void_ptr, "table"), PARAM(t_void_ptr, "key_hash"),
                     PARAM(t_void_ptr, "key_cmp"), PARAM(t_size, "entry_size"), PARAM(t_void_ptr, "key"),
                     PARAM(t_size, "value_offset"), PARAM(t_void_ptr, "value"));
    load_global_func(env, t_void, "bl_hashmap_remove", PARAM(t_void_ptr, "table"), PARAM(t_void_ptr, "key_hash"),
                     PARAM(t_void_ptr, "key_cmp"), PARAM(t_size, "entry_size"), PARAM(t_void_ptr, "key"));
    load_global_func(env, t_u32, "bl_hashmap_hash", PARAM(t_void_ptr, "table"), PARAM(t_void_ptr, "entry_hash"), PARAM(t_size, "entry_size"));
    load_global_func(env, t_u32, "bl_hashmap_len", PARAM(t_void_ptr, "table"));
    load_global_func(env, t_void, "bl_hashmap_mark_cow", PARAM(t_void_ptr, "table"));
    load_global_func(env, t_int, "bl_hashmap_compare", PARAM(t_void_ptr, "table1"), PARAM(t_void_ptr, "table2"), PARAM(t_void_ptr, "key_hash"),
                     PARAM(t_void_ptr, "key_compare"), PARAM(t_void_ptr, "value_compare"), PARAM(t_void_ptr, "entry_compare"),
                     PARAM(t_size, "entry_size"), PARAM(t_size, "value_offset"));
    load_global_func(env, t_u32, "hash_64bits", PARAM(t_void_ptr, "ptr"));
    load_global_func(env, t_u32, "compare_64bits", PARAM(t_void_ptr, "a"), PARAM(t_void_ptr, "b"));
#undef PARAM
}

static bl_type_t *define_string_type(env_t *env)
{
    bl_type_t *str_type = Type(ArrayType, .item_type=Type(CharType));
    bl_type_t *c_str_type = Type(PointerType, .pointed=Type(CStringCharType));
    gcc_rvalue_t *rval = gcc_str(env->ctx, "String");
    binding_t *binding = new(binding_t, .rval=rval, .type=Type(TypeType, .type=str_type));
    hset(env->global_bindings, "String", binding);

    bl_hashmap_t *ns = get_namespace(env, str_type);
    load_method(env, ns, "bl_string_uppercased", "uppercased", str_type, ARG("str",str_type,0));
    load_method(env, ns, "bl_string_capitalized", "capitalized", str_type, ARG("str",str_type,0));
    load_method(env, ns, "bl_string_capitalized", "capitalized", str_type, ARG("str",str_type,0));
    load_method(env, ns, "bl_string_titlecased", "titlecased", str_type, ARG("str",str_type,0));
    load_method(env, ns, "bl_string_quoted", "quoted", str_type,
                ARG("str",str_type,0),
                ARG("dsl", c_str_type, FakeAST(Nil, .type=FakeAST(Var, .name="Char"))),
                ARG("colorize", Type(BoolType), FakeAST(Bool, .b=false)));
    load_method(env, ns, "bl_string_starts_with", "starts_with", str_type, ARG("str",str_type,0), ARG("prefix",str_type,0));
    load_method(env, ns, "bl_string_ends_with", "ends_with", str_type, ARG("str",str_type,0), ARG("suffix",str_type,0));
    load_method(env, ns, "bl_string_trimmed", "trimmed", str_type,
                ARG("str",str_type,0),
                ARG("chars",str_type,FakeAST(StringJoin, .children=LIST(ast_t*,FakeAST(StringLiteral, .str=" \t\r\n")))),
                ARG("trim_left",Type(BoolType),FakeAST(Bool,.b=true)),
                ARG("trim_right",Type(BoolType),FakeAST(Bool,.b=true)));
    load_method(env, ns, "bl_string_replace", "replace", str_type,
                ARG("str",str_type,0), ARG("pattern",str_type,0), ARG("replacement",str_type,0), ARG("limit",INT_TYPE,FakeAST(Int,.i=-1,.precision=64)));

    bl_type_t *c_str = Type(PointerType, .pointed=Type(CStringCharType), .is_optional=true);
    load_method(env, ns, "c_string", "c_string", Type(PointerType, .pointed=Type(CStringCharType), .is_optional=false), ARG("str",str_type,0));
    load_method(env, ns, "from_c_string", "from_pointer", str_type, ARG("str",c_str,0));
    bl_hashmap_t *ns2 = get_namespace(env, c_str);
    assert(ns2 == get_namespace(env, c_str));
    load_method(env, ns2, "from_c_string", "as_string", str_type, ARG("str",c_str,0));
    bl_hashmap_t *ns3 = get_namespace(env, Type(PointerType, .pointed=Type(CStringCharType), .is_optional=false));
    load_method(env, ns3, "from_c_string", "as_string", str_type, ARG("str",c_str,0));
    return str_type;
}

static void define_num_types(env_t *env)
{
    bl_type_t *num64_type = Type(NumType, .bits=64);
    {
        gcc_rvalue_t *rval = gcc_str(env->ctx, "Num");
        binding_t *binding = new(binding_t, .rval=rval, .type=Type(TypeType, .type=num64_type));
        hset(env->global_bindings, "Num", binding);
    }

    bl_type_t *num32_type = Type(NumType, .bits=32);
    {
        gcc_rvalue_t *rval = gcc_str(env->ctx, "Num32");
        binding_t *binding = new(binding_t, .rval=rval, .type=Type(TypeType, .type=num32_type));
        hset(env->global_bindings, "Num32", binding);
    }

    bl_hashmap_t *ns64 = get_namespace(env, num64_type);
    bl_hashmap_t *ns32 = get_namespace(env, num32_type);

    load_method(env, ns64, "drand48", "random", num64_type);

    struct { const char *c_name, *bl_name; } unary_methods[] = {
        {"acos",0},{"asin",0},{"atan",0},{"cos",0},{"sin",0},{"tan",0},{"cosh",0},{"sinh",0},
        {"tanh",0},{"acosh",0},{"asinh",0},{"atanh",0},{"exp",0},{"log",0},{"log10",0},
        {"exp10",0},{"expm1",0}, {"log1p",0}, {"logb",0}, {"exp2",0}, {"log2",0},
        {"sqrt",0}, {"cbrt",0}, {"ceil",0}, {"fabs", "abs"}, {"floor",0},
        {"significand",0}, {"j0",0}, {"j1",0}, {"y0",0}, {"y1",0}, {"erf",0}, {"erfc",0},
        {"tgamma",0}, {"rint",0}, {"nextdown","next_lowest"}, {"nextup","next_highest"}, {"round",0}, {"trunc","truncate"},
        {"roundeven","round_even"},
    };
    for (size_t i = 0; i < sizeof(unary_methods)/sizeof(unary_methods[0]); i++) {
        const char *c_name = unary_methods[i].c_name;
        const char *alias = unary_methods[i].bl_name;
        if (!alias) alias = c_name;
        load_method(env, ns64, c_name, alias, num64_type, ARG("num",num64_type,0));
        load_method(env, ns32, heap_strf("%sf", c_name), alias, num32_type, ARG("num",num32_type,0));
    }

    struct { const char *c_name, *bl_name, *arg1, *arg2; } binary_methods[] = {
        {"atan2",NULL,"y","x"}, {"pow",NULL,"base","exponent"},
        {"hypot",NULL,"x","y"}, {"fmod","modulo","num","modulus"}, {"copysign","copy_sign","num","with_sign"},
        {"nextafter","next_toward","num","toward"}, {"remainder",NULL,"num","divisor"},
        {"fmax","max","x","y"}, {"fmin", "min","x","y"},
        {"fmaxmag","biggest","x","y"}, {"fminmag","smallest","x","y"},
        {"fdim","distance","x","y"},
    };
    for (size_t i = 0; i < sizeof(binary_methods)/sizeof(binary_methods[0]); i++) {
        const char *c_name = binary_methods[i].c_name;
        const char *alias = binary_methods[i].bl_name;
        if (!alias) alias = c_name;
        const char *arg1 = binary_methods[i].arg1;
        const char *arg2 = binary_methods[i].arg2;
        load_method(env, ns64, c_name, alias, num64_type, ARG(arg1,num64_type,0), ARG(arg2,num64_type,0));
        load_method(env, ns32, heap_strf("%sf", c_name), alias, num32_type, ARG(arg1,num32_type,0), ARG(arg2,num32_type,0));
    }

    const char *bool_methods[][2] = {
        {"isinf","is_infinite"}, {"finite","is_finite"}, {"isnan","is_nan"},
    };
    for (size_t i = 0; i < sizeof(bool_methods)/sizeof(bool_methods[0]); i++) {
        const char *c_name = bool_methods[i][0];
        const char *alias = bool_methods[i][1];
        if (!alias) alias = c_name;
        load_method(env, ns64, c_name, alias, Type(BoolType), ARG("num",num64_type,0));
        load_method(env, ns32, heap_strf("%sf", c_name), alias, Type(BoolType), ARG("num",num32_type,0));
    }

    static struct {const char *name; double val;} constants[] = {
        {"E", M_E}, {"Log2_E", M_LOG2E}, {"Log10_E", M_LOG10E}, {"Ln_2", M_LN2}, {"Ln_10", M_LN10},
        {"Pi", M_PI}, {"Tau", 2*M_PI}, {"HalfPi", M_PI_2}, {"QuarterPi", M_PI_4}, {"InversePi", M_1_PI},
        {"DoubleInversePi", M_2_PI}, {"DoubleInverseSqrtPi", M_2_SQRTPI}, {"Sqrt2", M_SQRT2}, {"InverseSqrt2", M_SQRT1_2},
        {"NaN", nan("")}, {"Infinity", 1./0.},
    };

    bl_type_t *str_t = Type(ArrayType, .item_type=Type(CharType));
    load_method(env, ns64, "bl_string_number_format", "format", str_t,
                ARG("num",num64_type,0), ARG("precision",INT_TYPE,FakeAST(Int,.i=6,.precision=64)));
    load_method(env, ns64, "bl_string_scientific_notation", "scientific", str_t,
                ARG("num",num64_type,0), ARG("precision",INT_TYPE,FakeAST(Int,.i=6,.precision=64)));

    load_method(env, ns32, "bl_string_number_format32", "format", str_t,
                ARG("num",num32_type,0), ARG("precision",INT_TYPE,FakeAST(Int,.i=6,.precision=64)));
    load_method(env, ns32, "bl_string_scientific_notation32", "scientific", str_t,
                ARG("num",num32_type,0), ARG("precision",INT_TYPE,FakeAST(Int,.i=6,.precision=64)));

    { // Num NaN and Infinity:
        gcc_type_t *gcc_num_t = bl_type_to_gcc(env, num64_type);
        bl_hashmap_t *ns = get_namespace(env, num64_type);
        for (size_t i = 0; i < sizeof(constants)/sizeof(constants[0]); i++)
            hset(ns, constants[i].name, new(binding_t, .type=num64_type,
                                            .rval=gcc_rvalue_from_double(env->ctx, gcc_num_t, constants[i].val)));
    }

    { // Num32 NaN and Infinity:
        gcc_type_t *gcc_num32_t = bl_type_to_gcc(env, num32_type);
        bl_hashmap_t *ns = get_namespace(env, num32_type);
        for (size_t i = 0; i < sizeof(constants)/sizeof(constants[0]); i++)
            hset(ns, constants[i].name, new(binding_t, .type=num32_type,
                                            .rval=gcc_rvalue_from_double(env->ctx, gcc_num32_t, constants[i].val)));
    }

    // oddballs: ldexp jn yn llogb lrint lround fma
}

static void define_int_types(env_t *env)
{
    { // Int64 methods
        bl_type_t *i64 = Type(IntType, .bits=64);
        bl_hashmap_t *ns = get_namespace(env, i64);
        gcc_type_t *gcc_i64 = bl_type_to_gcc(env, i64);

        load_method(env, ns, "labs", "abs", i64, ARG("i",i64,0));
        load_method(env, ns, "arc4random_uniform", "random", i64, ARG("max", Type(IntType, .bits=32), FakeAST(Int, .i=INT32_MAX, .precision=32)));

        hset(ns, "Min", new(binding_t, .type=i64, .rval=gcc_rvalue_from_long(env->ctx, gcc_i64, INT64_MIN)));
        hset(ns, "Max", new(binding_t, .type=i64, .rval=gcc_rvalue_from_long(env->ctx, gcc_i64, INT64_MAX)));
    }

    { // Int32 methods
        bl_type_t *i32 = Type(IntType, .bits=32);
        bl_hashmap_t *ns = get_namespace(env, i32);
        gcc_type_t *gcc_i32 = bl_type_to_gcc(env, i32);

        load_method(env, ns, "abs", "abs", i32, ARG("i",i32,0));
        load_method(env, ns, "arc4random_uniform", "random", i32, ARG("max", i32, FakeAST(Int, .i=INT32_MAX, .precision=32)));

        hset(ns, "Min", new(binding_t, .type=i32, .rval=gcc_rvalue_from_long(env->ctx, gcc_i32, INT32_MIN)));
        hset(ns, "Max", new(binding_t, .type=i32, .rval=gcc_rvalue_from_long(env->ctx, gcc_i32, INT32_MAX)));
    }

    { // Int16 methods
        bl_type_t *i16 = Type(IntType, .bits=16);
        bl_hashmap_t *ns = get_namespace(env, i16);
        gcc_type_t *gcc_i16 = bl_type_to_gcc(env, i16);
        load_method(env, ns, "arc4random_uniform", "random", i16, ARG("max", Type(IntType, .bits=32), FakeAST(Int, .i=INT16_MAX, .precision=32)));
        hset(ns, "Min", new(binding_t, .type=i16, .rval=gcc_rvalue_from_long(env->ctx, gcc_i16, INT16_MIN)));
        hset(ns, "Max", new(binding_t, .type=i16, .rval=gcc_rvalue_from_long(env->ctx, gcc_i16, INT16_MAX)));
    }

    { // Int8 methods
        bl_type_t *i8 = Type(IntType, .bits=8);
        bl_hashmap_t *ns = get_namespace(env, i8);
        gcc_type_t *gcc_i8 = bl_type_to_gcc(env, i8);
        load_method(env, ns, "arc4random_uniform", "random", i8, ARG("max", Type(IntType, .bits=32), FakeAST(Int, .i=INT8_MAX, .precision=32)));
        hset(ns, "Min", new(binding_t, .type=i8, .rval=gcc_rvalue_from_long(env->ctx, gcc_i8, INT8_MIN)));
        hset(ns, "Max", new(binding_t, .type=i8, .rval=gcc_rvalue_from_long(env->ctx, gcc_i8, INT8_MAX)));
    }

    // Stringifying methods
    bl_type_t *types[] = {Type(IntType, .bits=8), Type(IntType, .bits=16), Type(IntType, .bits=32), Type(IntType, .bits=64)};
    bl_type_t *str_t = Type(ArrayType, .item_type=Type(CharType));
    for (size_t i = 0; i < sizeof(types)/sizeof(types[0]); i++) {
        uint16_t bits = Match(types[i], IntType)->bits;
        const char* name = bits == 64 ? "Int" : heap_strf("Int%d", Match(types[i], IntType)->bits);
        hset(env->global_bindings, name,
                    new(binding_t, .rval=gcc_str(env->ctx, name), .type=Type(TypeType, .type=types[i])));
        bl_hashmap_t *ns = get_namespace(env, types[i]);
        load_method(env, ns, "bl_string_int_format", "format", str_t, ARG("i",types[i],0), ARG("digits",INT_TYPE,0));
        load_method(env, ns, "bl_string_hex", "hex", str_t, ARG("i",types[i],0),
                    ARG("digits",INT_TYPE,FakeAST(Int, .i=1, .precision=64)),
                    ARG("uppercase",Type(BoolType),FakeAST(Bool, .b=true)),
                    ARG("prefix",Type(BoolType),FakeAST(Bool, .b=true)));
        load_method(env, ns, "bl_string_octal", "octal", str_t, ARG("i",types[i],0),
                    ARG("digits",INT_TYPE,FakeAST(Int, .i=1, .precision=64)),
                    ARG("prefix",Type(BoolType),FakeAST(Bool, .b=true)));
    }
}

env_t *new_environment(gcc_ctx_t *ctx, jmp_buf *on_err, bl_file_t *f, bool debug)
{
    env_t *env = new(env_t,
        .ctx = ctx,
        .on_err = on_err,
        .file = f,
        .global_bindings = new(bl_hashmap_t),
        .bindings = new(bl_hashmap_t),
        .type_namespaces = new(bl_hashmap_t),
        .def_types = new(bl_hashmap_t),
        .global_funcs = new(bl_hashmap_t),
        .ast_functions = new(bl_hashmap_t),
        .debug = debug,
    );
    env->bindings->fallback = env->global_bindings;

    load_global_functions(env);

    bl_type_t *string_type = define_string_type(env);
    bl_type_t *say_type = Type(
        FunctionType,
        .arg_names=LIST(const char*, "str", "end"),
        .arg_types=LIST(bl_type_t*, string_type, string_type),
        .arg_defaults=LIST(ast_t*, NULL, FakeAST(StringLiteral, .str="\n")),
        .ret=Type(VoidType));

    gcc_param_t *gcc_say_params[] = {
        gcc_new_param(ctx, NULL, bl_type_to_gcc(env, string_type), "str"),
        gcc_new_param(ctx, NULL, bl_type_to_gcc(env, string_type), "end"),
    };
    gcc_func_t *say_func = gcc_new_func(ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, VOID), "say", 2, gcc_say_params, 0);
    gcc_rvalue_t *say_rvalue = gcc_get_func_address(say_func, NULL);
    hset(env->global_bindings, "say", new(binding_t, .rval=say_rvalue, .type=say_type));
    define_num_types(env);
#define DEFTYPE(t) hset(env->global_bindings, #t, new(binding_t, .rval=gcc_str(ctx, #t), .type=Type(TypeType, .type=Type(t##Type))));
    // Primitive types:
    DEFTYPE(Bool); DEFTYPE(Void); DEFTYPE(Abort);
    DEFTYPE(Char); DEFTYPE(CStringChar);
#undef DEFTYPE
    define_int_types(env);

    return env;
}

env_t *fresh_scope(env_t *env)
{
    env_t *fresh = GC_MALLOC(sizeof(env_t));
    *fresh = *env;
    fresh->bindings = new(bl_hashmap_t, .fallback=env->bindings);
    return fresh;
}

static void copy_global_bindings(bl_hashmap_t *dest, bl_hashmap_t *src)
{
    for (; src; src = src->fallback) {
        for (uint32_t i = 1; i <= src->count; i++) {
            auto entry = hnth(src, i, const char*, binding_t*);
            if (entry->value->visible_in_closures)
                hset(dest, entry->key, entry->value);
        }
    }
}

env_t *global_scope(env_t *env)
{
    env_t *fresh = GC_MALLOC(sizeof(env_t));
    *fresh = *env;
    fresh->bindings = new(bl_hashmap_t, .fallback=env->global_bindings);
    copy_global_bindings(fresh->bindings, env->bindings);
    return fresh;
}

void compiler_err(env_t *env, ast_t *ast, const char *fmt, ...)
{
    if (isatty(STDERR_FILENO))
        fputs("\x1b[31;7;1m", stderr);
    if (ast)
        fprintf(stderr, "%s:%ld.%ld: ", ast->span.file->filename, bl_get_line_number(ast->span.file, ast->span.start),
                bl_get_line_column(ast->span.file, ast->span.start));
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    if (isatty(STDERR_FILENO))
        fputs(" \x1b[m", stderr);
    fputs("\n\n", stderr);
    if (ast)
        fprint_span(stderr, ast->span, "\x1b[31;1m", 2, isatty(STDERR_FILENO));

    if (env->on_err)
        longjmp(*env->on_err, 1);

    raise(SIGABRT);
    exit(1);
}

binding_t *get_binding(env_t *env, const char *name)
{
    return hget(env->bindings, name, binding_t*);
}

binding_t *get_local_binding(env_t *env, const char *name)
{
    bl_hashmap_t *fallback = env->bindings->fallback;
    binding_t *b = hget(env->bindings, name, binding_t*);
    env->bindings->fallback = fallback;
    return b;
}

gcc_func_t *get_function(env_t *env, const char *name)
{
    return hget(env->global_funcs, name, gcc_func_t*);
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
            return get_from_namespace(env, Match(b->type, TypeType)->type, access->field);
        else
            return get_from_namespace(env, b->type, access->field);
    }
    default: compiler_err(env, ast, "I can't figure out at compile-time what this refers to");
    }
}

bl_hashmap_t *get_namespace(env_t *env, bl_type_t *t)
{
    bl_hashmap_t *ns = hget(env->type_namespaces, type_to_string(t), bl_hashmap_t*);
    if (!ns) {
        ns = new(bl_hashmap_t, .fallback=env->global_bindings);
        copy_global_bindings(ns, env->bindings);
        hset(env->type_namespaces, type_to_string(t), ns);
    }
    return ns;
}

env_t *get_type_env(env_t *env, bl_type_t *t)
{
    env_t *fresh = GC_MALLOC(sizeof(env_t));
    *fresh = *env;
    fresh->bindings = get_namespace(env, t);
    return fresh;
}

binding_t *get_from_namespace(env_t *env, bl_type_t *t, const char *name)
{
    return hget(get_namespace(env, t), name, binding_t*);
}

void set_in_namespace(env_t *env, bl_type_t *t, const char *name, void *value)
{
    hset(get_namespace(env, t), heap_str(name), value);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
