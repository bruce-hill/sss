// Code related to the ambient state/environment used in compilation
#include <libgccjit.h>
#include <limits.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "files.h"
#include "parse.h"
#include "types.h"
#include "typecheck.h"
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
    Table_str_set(&env->global->funcs, heap_str(name),
                  gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, t_ret, name, nargs, args, is_vararg));
}

typedef struct {
    const char *name;
    sss_type_t *t;
    ast_t *default_val;
} sss_arg_t;
#define ARG(...) ((sss_arg_t){__VA_ARGS__})

static inline void _load_method(
    env_t *env, table_t *ns, const char *extern_name, const char *method_name, sss_type_t *ret_t, int nargs, sss_arg_t args[nargs]
) {
    gcc_param_t *params[nargs];
    auto arg_name_list = EMPTY_ARRAY(const char*);
    auto arg_type_list = EMPTY_ARRAY(sss_type_t*);
    auto arg_default_list = EMPTY_ARRAY(ast_t*);
    for (int i = 0; i < nargs; i++) {
        params[i] = gcc_new_param(env->ctx, NULL, sss_type_to_gcc(env, args[i].t), args[i].name);
        append(arg_name_list, args[i].name);
        append(arg_type_list, args[i].t);
        append(arg_default_list, args[i].default_val);
    }
    
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, sss_type_to_gcc(env, ret_t),
                                    extern_name, nargs, params, 0);
    sss_type_t *fn_type = Type(FunctionType, .arg_types=arg_type_list, .arg_names=arg_name_list, .arg_defaults=arg_default_list, .ret=ret_t);
    Table_str_set(ns, heap_str(method_name), new(binding_t, .type=fn_type, .func=func));
}

sss_type_t *define_tagged_union(env_t *env, int tag_bits, const char *name, ARRAY_OF(sss_tagged_union_member_t) members)
{
    for (int64_t i = 0; i < LENGTH(members); i++) {
        auto member = ith(members, i);
        if (member.type && member.type->tag != StructType) {
            members[0][i].type = Type(StructType, .field_names=ARRAY(ith(members, i).name),
                                      .field_types=ARRAY(ith(members, i).type),
                                      .field_defaults=ARRAY((ast_t*)NULL));
        } else if (!member.type) {
            members[0][i].type = Type(StructType, .field_names=EMPTY_ARRAY(const char*), .field_types=EMPTY_ARRAY(sss_type_t*),
                                      .field_defaults=EMPTY_ARRAY(ast_t*));
        }
    }
    sss_type_t *t = Type(VariantType, .filename="<builtin>", .name=name, .variant_of=Type(TaggedUnionType, .tag_bits=tag_bits, .members=members));
    gcc_rvalue_t *rval = gcc_str(env->ctx, name);
    Table_str_set(&env->global->bindings, name, new(binding_t, .type=Type(TypeType, t), .rval=rval, .visible_in_closures=true));
    populate_tagged_union_constructors(env, t);
    return t;
}

typedef struct {
    const char *symbol, *sss_name, *type;
} builtin_binding_t;

static sss_type_t sss_str_t = {
    .tag=VariantType,
    .__data.VariantType={
        .name="Str", .variant_of=(sss_type_t[]){{
            .tag=ArrayType,
            .__data.ArrayType={
                .item_type=(sss_type_t[]){{.tag=CharType}}
            },
        }},
    },
};

static sss_type_t sss_c_str_t = {
    .tag=VariantType,
    .__data.VariantType={
        .name="CString", .variant_of=(sss_type_t[]){{
            .tag=PointerType,
            .__data.PointerType={
                .pointed=(sss_type_t[]){{.tag=CharType}},
                .is_optional=true,
            },
        }},
    },
};


// builtin_binding_t builtin_functions[] = {
//     {"GC_malloc", "func(size:UInt) @Memory"},
//     {"GC_malloc_atomic", "func(size:UInt) @Memory"},
//     {"GC_realloc", "func(data:?Memory, size:UInt) @Memory"},
//     {"memcpy", "func(dest:@Memory, src:@Memory, size:UInt) @Memory"},
//     {"getenv", "func(dest:@Memory, src:@Memory, size:UInt) @Memory"},
//     load_global_func(env, gcc_type(ctx, STRING), "getenv", PARAM(gcc_type(ctx, STRING), "name"));
//     load_global_func(env, t_int, "fwrite", PARAM(t_void_ptr, "data"), PARAM(t_size, "size"), PARAM(t_size, "nmemb"), PARAM(t_file, "file"));
//     load_global_func(env, t_int, "fputs", PARAM(t_str, "str"), PARAM(t_file, "file"));
//     load_global_func(env, t_int, "puts", PARAM(t_str, "str"));
//     load_global_func(env, t_int, "fputc", PARAM(gcc_get_type(ctx, GCC_T_CHAR), "c"), PARAM(t_file, "file"));
//     load_global_var_func(env, t_int, "fprintf", PARAM(t_file, "file"), PARAM(t_str, "format"));
//     load_global_var_func(env, t_int, "printf", PARAM(t_str, "format"));
//     load_global_func(env, t_int, "fflush", PARAM(t_file, "file"));
//     load_global_func(env, t_int, "fclose", PARAM(t_file, "file"));
//     load_global_func(env, t_int, "strcmp", PARAM(t_str, "str1"), PARAM(t_str, "str2"));
//     load_global_func(env, t_size, "strlen", PARAM(t_str, "str"));
//     load_global_func(env, t_str, "heap_str", PARAM(t_str, "str"));
//     load_global_func(env, t_str, "heap_strn", PARAM(t_str, "str"), PARAM(t_size, "length"));
//     load_global_var_func(env, t_str, "heap_strf", PARAM(t_str, "fmt"));
//     // Cord functions:
//     gcc_type_t *cord_t = gcc_get_type(ctx, GCC_T_STRING);
//     load_global_func(env, cord_t, "CORD_cat", PARAM(cord_t, "x"), PARAM(cord_t, "y"));
//     load_global_func(env, cord_t, "CORD_cat_char_star", PARAM(cord_t, "x"), PARAM(t_str, "y"), PARAM(t_size, "leny"));
//     load_global_func(env, cord_t, "CORD_cat_char", PARAM(cord_t, "x"), PARAM(t_char, "c"));
//     load_global_func(env, t_int, "CORD_cmp", PARAM(cord_t, "x"), PARAM(cord_t, "y"));
//     load_global_func(env, cord_t, "CORD_to_const_char_star", PARAM(cord_t, "x"));
//     load_global_func(env, cord_t, "CORD_to_char_star", PARAM(cord_t, "x"));
//     load_global_func(env, t_int, "CORD_put", PARAM(cord_t, "x"), PARAM(gcc_get_type(ctx, GCC_T_FILE_PTR), "f"));
//     load_global_var_func(env, t_int, "CORD_sprintf", PARAM(gcc_get_ptr_type(cord_t), "out"), PARAM(cord_t, "format"));
//     load_global_var_func(env, t_int, "CORD_fprintf", PARAM(t_file, "out"), PARAM(cord_t, "format"));

    // load_global_var_func(env, t_void, "fail", PARAM(t_str, "message"));
    // load_global_var_func(env, t_void, "sss_doctest", PARAM(t_str, "label"), PARAM(cord_t, "expr"), PARAM(t_str, "type"),
    //                      PARAM(t_bool, "use_color"), PARAM(t_str, "expected"), PARAM(t_str, "filename"), PARAM(t_int, "start"),
    //                      PARAM(t_int, "end"));
    // load_global_var_func(env, t_void, "exit", PARAM(gcc_get_type(ctx, GCC_T_INT), "status"));
    // gcc_func_t *exit_fn = Table_str_get(&env->global->funcs, "exit");
    // Table_str_set(&env->global->bindings, "exit", new(binding_t, .func=exit_fn, .sym_name="exit", .visible_in_closures=true, .type=Type(
    //     FunctionType,
    //     .arg_names=ARRAY((const char*)"status"),
    //     .arg_types=ARRAY(Type(IntType, .bits=32)),
    //     .arg_defaults=ARRAY(FakeAST(Int, .i=0, .precision=32)),
    //     .ret=Type(AbortType))));
    // load_global_func(env, t_double, "sane_fmod", PARAM(t_double, "num"), PARAM(t_double, "modulus"));
    // load_global_func(env, gcc_type(ctx, STRING), "range_to_cord", PARAM(t_range, "range"), PARAM(t_void_ptr, "stack"), PARAM(t_bool, "color"));
    // gcc_func_t *range_to_cord = Table_str_get(&env->global->funcs, "range_to_cord");
    // Table_str_set(get_namespace(env, Type(RangeType)), "__cord",
    //      new(binding_t, .func=range_to_cord, .sym_name="range_to_cord"));
    // load_global_func(env, t_bl_str, "range_slice", PARAM(t_bl_str, "array"), PARAM(t_range, "range"), PARAM(t_size, "item_size"));
    // load_global_func(env, t_void_ptr, "dlopen", PARAM(t_str, "filename"), PARAM(t_int, "flags"));
    // load_global_func(env, t_void_ptr, "dlsym", PARAM(t_void_ptr, "handle"), PARAM(t_str, "symbol"));
    // load_global_func(env, t_int, "dlclose", PARAM(t_void_ptr, "handle"));
// };

struct {
    sss_type_t sss_type;
    const char *type_name, *type_symbol;
    builtin_binding_t *bindings;
} builtin_types[] = {
    {{.tag=BoolType}, "Bool", "Bool_type", NULL},
    {{.tag=MemoryType}, "Memory", "Memory_type", NULL},
    {{.tag=RangeType}, "Range", "Range_type", NULL},

#define BUILTIN_INT(t, ...) \
    {{.tag=IntType, .__data.IntType={__VA_ARGS__}}, #t, #t "_type", (builtin_binding_t[]){ \
        {#t"__format", "format", "func(i:"#t", digits=0) Str"}, \
        {#t"__hex",    "hex", "func(i:"#t", digits=0, uppercase=yes, prefix=no) Str"}, \
        {#t"__octal",  "octal", "func(i:"#t", digits=0, prefix=no) Str"}, \
        {#t"__random", "random", "func() "#t}, \
        {#t"__min",    "min", #t}, \
        {#t"__max",    "max", #t}, \
        {NULL, NULL, NULL}, \
    }}
    BUILTIN_INT(Int, .bits=64), BUILTIN_INT(Int32, .bits=32), BUILTIN_INT(Int16, .bits=16), BUILTIN_INT(Int8, .bits=8),
    BUILTIN_INT(UInt, .bits=64, .is_unsigned=true), BUILTIN_INT(UInt32, .bits=32, .is_unsigned=true),
    BUILTIN_INT(UInt16, .bits=16, .is_unsigned=true), BUILTIN_INT(UInt8, .bits=8, .is_unsigned=true),
#undef BUILTIN_INT

    {{.tag=NumType, .__data.NumType={.bits=64}}, "Num", "Num_type", (builtin_binding_t[]){
        {"Num__format", "format", "func(n:Num, precision:Int) Str"},
        {"drand48", "random", "func() Num"},
        // Unary functions:
#define UNARY(fn) {#fn, #fn, "func(n:Num) Num"}
        UNARY(acos), UNARY(asin), UNARY(atan), UNARY(cos), UNARY(sin), UNARY(tan), UNARY(cosh), UNARY(sinh), UNARY(tanh),
        UNARY(acosh), UNARY(asinh), UNARY(atanh), UNARY(exp), UNARY(log), UNARY(log10), UNARY(exp10), UNARY(expm1),
        UNARY(log1p), UNARY(logb), UNARY(exp2), UNARY(log2), UNARY(sqrt), UNARY(cbrt), UNARY(ceil), UNARY(fabs),
        UNARY(floor), UNARY(significand), UNARY(j0), UNARY(j1), UNARY(y0), UNARY(y1), UNARY(erf), UNARY(erfc),
        UNARY(tgamma), UNARY(rint), UNARY(nextdown), UNARY(nextup), UNARY(round),
        UNARY(trunc), UNARY(roundeven),
#undef UNARY
        // Binary functions:
        {"atan2", "atan2", "func(y:Num, x:Num) Num"},
        {"pow", "pow", "func(base:Num, power:Num) Num"},
        {"hypot", "hypot", "func(x:Num, y:Num) Num"},
        {"copysign", "hypot", "func(magnitude:Num, sign:Num) Num"},
        {"nextafter", "nextafter", "func(x:Num, toward:Num) Num"},
        {"remainder", "remainder", "func(x:Num, divisor:Num) Num"},
        {"fmaxmag", "max_magnitude", "func(x:Num, y:Num) Num"},
        {"fminmag", "min_magnitude", "func(x:Num, y:Num) Num"},
        {"fdim", "distance", "func(x:Num, y:Num) Num"},
        // Predicates
        {"isinf", "isinf", "func(n:Num) Bool"},
        {"finite", "finite", "func(n:Num) Bool"},
        {"isnan", "isnan", "func(n:Num) Bool"},
        // Constants
        {"Num__e", "e", "Num"}, {"Num__log2e", "log2e", "Num"}, {"Num__ln2", "ln2", "Num"},
        {"Num__ln10", "ln10", "Num"}, {"Num__pi", "pi", "Num"}, {"Num__tau", "tau", "Num"},
        {"Num__half_pi", "half_pi", "Num"}, {"Num__quarter_pi", "quarter_pi", "Num"},
        {"Num__inverse_pi", "inverse_pi", "Num"}, {"Num__inverse_half_pi", "inverse_half_pi", "Num"},
        {"Num__2_sqrt_pi", "2_sqrt_pi", "Num"}, {"Num__sqrt2", "sqrt2", "Num"},
        {"Num__sqrt_half", "sqrt_half", "Num"}, {"Num__NaN", "NaN", "Num"}, {"Num__inf", "inf", "Num"},
        {NULL, NULL, NULL},
    }},

    {{.tag=NumType, .__data.NumType={.bits=32}}, "Num32", "Num32_type", (builtin_binding_t[]){
        {"Num32__format", "format", "func(n:Num32, precision:Int) Str"},
        {"Num32__random", "random", "func() Num32"},
        // Unary functions:
#define UNARY(fn) {#fn"f", #fn, "func(n:Num32) Num32"}
        UNARY(acos), UNARY(asin), UNARY(atan), UNARY(cos), UNARY(sin), UNARY(tan), UNARY(cosh), UNARY(sinh), UNARY(tanh),
        UNARY(acosh), UNARY(asinh), UNARY(atanh), UNARY(exp), UNARY(log), UNARY(log10), UNARY(exp10), UNARY(expm1),
        UNARY(log1p), UNARY(logb), UNARY(exp2), UNARY(log2), UNARY(sqrt), UNARY(cbrt), UNARY(ceil), UNARY(fabs),
        UNARY(floor), UNARY(significand), UNARY(j0), UNARY(j1), UNARY(y0), UNARY(y1), UNARY(erf), UNARY(erfc),
        UNARY(tgamma), UNARY(rint), UNARY(nextdown), UNARY(nextup), UNARY(round),
        UNARY(trunc), UNARY(roundeven),
#undef UNARY
        // Binary functions:
        {"atan2f", "atan2", "func(y:Num32, x:Num32) Num32"},
        {"powf", "pow", "func(base:Num32, power:Num32) Num32"},
        {"hypotf", "hypot", "func(x:Num32, y:Num32) Num32"},
        {"copysignf", "hypot", "func(magnitude:Num32, sign:Num32) Num32"},
        {"nextafterf", "nextafter", "func(x:Num32, toward:Num32) Num32"},
        {"remainderf", "remainder", "func(x:Num32, divisor:Num32) Num32"},
        {"fmaxmagf", "max_magnitude", "func(x:Num32, y:Num32) Num32"},
        {"fminmagf", "min_magnitude", "func(x:Num32, y:Num32) Num32"},
        {"fdimf", "distance", "func(x:Num32, y:Num32) Num32"},
        // Predicates
        {"Num32__isinf", "isinf", "func(n:Num32) Bool"},
        {"Num32__finite", "finite", "func(n:Num32) Bool"},
        {"Num32__isnan", "isnan", "func(n:Num32) Bool"},
        // Constants
        {"Num32__e", "e", "Num32"}, {"Num32__log2e", "log2e", "Num32"}, {"Num32__ln2", "ln2", "Num32"},
        {"Num32__ln10", "ln10", "Num32"}, {"Num32__pi", "pi", "Num32"}, {"Num32__tau", "tau", "Num32"},
        {"Num32__half_pi", "half_pi", "Num32"}, {"Num32__quarter_pi", "quarter_pi", "Num32"},
        {"Num32__inverse_pi", "inverse_pi", "Num32"}, {"Num32__inverse_half_pi", "inverse_half_pi", "Num32"},
        {"Num32__2_sqrt_pi", "2_sqrt_pi", "Num32"}, {"Num32__sqrt2", "sqrt2", "Num32"},
        {"Num32__sqrt_half", "sqrt_half", "Num32"}, {"Num32__NaN", "NaN", "Num32"}, {"Num32__inf", "inf", "Num32"},
        {NULL, NULL, NULL},
    }},

    {{.tag=CharType}, "Char", "Char_type", (builtin_binding_t[]){
        {"Char__toupper", "toupper", "func(c:Char) Char"},
        {"Char__tolower", "tolower", "func(c:Char) Char"},
#define CHARP(name) {"Char__"#name, #name, "func(c:Char) Bool"}
        CHARP(isalnum), CHARP(isalpha), CHARP(iscntrl), CHARP(isdigit), CHARP(isgraph), CHARP(islower),
        CHARP(isprint), CHARP(ispunct), CHARP(isspace), CHARP(isupper), CHARP(isxdigit), CHARP(isascii), CHARP(isblank),
#undef CHARP
        {NULL, NULL, NULL},
    }},

    {sss_str_t, "Str", "Str_type", (builtin_binding_t[]){
        {"Str__uppercased", "uppercased", "func(s:Str) Str"},
        {"Str__lowercased", "lowercased", "func(s:Str) Str"},
        {"Str__capitalized", "capitalized", "func(s:Str) Str"},
        {"Str__titlecased", "titlecased", "func(s:Str) Str"},
        {"Str__starts_with", "starts_with", "func(s:Str) Bool"},
        {"Str__ends_with", "ends_with", "func(s:Str) Bool"},
        {"Str__without_prefix", "without_prefix", "func(s:Str, prefix:Str) Str"},
        {"Str__without_suffix", "without_suffix", "func(s:Str, suffix:Str) Str"},
        {"Str__trimmed", "trimmed", "func(s:Str, trim_chars=[\\x20, \\n, \\r, \\t, \\v], trim_left=yes, trim_right=yes) Str"},
        {"Str__slice", "slice", "func(s:Str, first=1, stride=1, length=Int.max) Str"},
        {"Str__c_string", "c_string", "func(s:Str) CString"},
        {"Str__from_c_string", "from_c_string", "func(c:CString) Str"},
        {"Str__find", "find", "func(s:Str, target:Str) Str"},
        {"Str__replace", "replace", "func(s:Str, target:Str, replacement:Str, limit=Int.max) Str"},
        {"Str__quoted", "quoted", "func(s:Str) Str"},
        {"Str__split", "split", "func(s:Str, split_chars=[`, \\x20, \\n, \\r, \\t, \\v]) [Str]"},
        {"Str__join", "join", "func(glue:Str, pieces:[Str]) Str"},
        {NULL, NULL, NULL},
    }},

    {sss_c_str_t, "CString", "CString_type", (builtin_binding_t[]){
        {"Str__c_string", "from_str", "func(s:Str) CString"},
        {"Str__from_c_string", "as_str", "func(c:CString) Str"},
        {NULL, NULL, NULL},
    }},
};

#define load_method(env,ns,ename,mname,ret,...) _load_method(env,ns,ename,mname,ret,\
                 sizeof((sss_arg_t[]){__VA_ARGS__})/sizeof(sss_arg_t),\
                 (sss_arg_t[]){__VA_ARGS__})

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
               *t_char = gcc_get_type(ctx, GCC_T_CHAR),
               *t_file = gcc_get_type(ctx, GCC_T_FILE_PTR),
               *t_range = sss_type_to_gcc(env, Type(RangeType)),
               *t_bl_str = sss_type_to_gcc(env, Type(ArrayType, .item_type=Type(CharType)));

#define PARAM(type, name) gcc_new_param(ctx, NULL, type, name)
    load_global_func(env, t_void_ptr, "GC_malloc", PARAM(t_size, "size"));
    load_global_func(env, t_void_ptr, "GC_malloc_atomic", PARAM(t_size, "size"));
    load_global_func(env, t_void_ptr, "GC_realloc", PARAM(t_void_ptr, "data"), PARAM(t_size, "size"));
    load_global_func(env, t_void_ptr, "memcpy", PARAM(t_void_ptr, "dest"), PARAM(t_void_ptr, "src"), PARAM(t_size, "size"));
    load_global_func(env, t_file, "open_memstream", PARAM(gcc_get_ptr_type(t_str), "buf"), PARAM(gcc_get_ptr_type(t_size), "size"));
    load_global_func(env, t_void, "free", PARAM(t_void_ptr, "ptr"));
    load_global_func(env, gcc_type(ctx, STRING), "getenv", PARAM(gcc_type(ctx, STRING), "name"));
    load_global_func(env, t_int, "fwrite", PARAM(t_void_ptr, "data"), PARAM(t_size, "size"), PARAM(t_size, "nmemb"), PARAM(t_file, "file"));
    load_global_func(env, t_int, "fputs", PARAM(t_str, "str"), PARAM(t_file, "file"));
    load_global_func(env, t_int, "puts", PARAM(t_str, "str"));
    load_global_func(env, t_int, "fputc", PARAM(gcc_get_type(ctx, GCC_T_CHAR), "c"), PARAM(t_file, "file"));
    load_global_var_func(env, t_int, "fprintf", PARAM(t_file, "file"), PARAM(t_str, "format"));
    load_global_var_func(env, t_int, "printf", PARAM(t_str, "format"));
    load_global_func(env, t_int, "fflush", PARAM(t_file, "file"));
    load_global_func(env, t_int, "fclose", PARAM(t_file, "file"));
    load_global_func(env, t_int, "strcmp", PARAM(t_str, "str1"), PARAM(t_str, "str2"));
    load_global_func(env, t_size, "strlen", PARAM(t_str, "str"));
    load_global_func(env, t_str, "heap_str", PARAM(t_str, "str"));
    load_global_func(env, t_str, "heap_strn", PARAM(t_str, "str"), PARAM(t_size, "length"));
    load_global_var_func(env, t_str, "heap_strf", PARAM(t_str, "fmt"));
    // Cord functions:
    gcc_type_t *cord_t = gcc_get_type(ctx, GCC_T_STRING);
    load_global_func(env, cord_t, "CORD_cat", PARAM(cord_t, "x"), PARAM(cord_t, "y"));
    load_global_func(env, cord_t, "CORD_cat_char_star", PARAM(cord_t, "x"), PARAM(t_str, "y"), PARAM(t_size, "leny"));
    load_global_func(env, cord_t, "CORD_cat_char", PARAM(cord_t, "x"), PARAM(t_char, "c"));
    load_global_func(env, t_int, "CORD_cmp", PARAM(cord_t, "x"), PARAM(cord_t, "y"));
    load_global_func(env, cord_t, "CORD_to_const_char_star", PARAM(cord_t, "x"));
    load_global_func(env, cord_t, "CORD_to_char_star", PARAM(cord_t, "x"));
    load_global_func(env, t_int, "CORD_put", PARAM(cord_t, "x"), PARAM(gcc_get_type(ctx, GCC_T_FILE_PTR), "f"));
    load_global_var_func(env, t_int, "CORD_sprintf", PARAM(gcc_get_ptr_type(cord_t), "out"), PARAM(cord_t, "format"));
    load_global_var_func(env, t_int, "CORD_fprintf", PARAM(t_file, "out"), PARAM(cord_t, "format"));

    load_global_var_func(env, t_void, "fail", PARAM(t_str, "message"));
    load_global_var_func(env, t_void, "sss_doctest", PARAM(t_str, "label"), PARAM(cord_t, "expr"), PARAM(t_str, "type"),
                         PARAM(t_bool, "use_color"), PARAM(t_str, "expected"), PARAM(t_str, "filename"), PARAM(t_int, "start"),
                         PARAM(t_int, "end"));
    load_global_var_func(env, t_void, "exit", PARAM(gcc_get_type(ctx, GCC_T_INT), "status"));
    gcc_func_t *exit_fn = Table_str_get(&env->global->funcs, "exit");
    Table_str_set(&env->global->bindings, "exit", new(binding_t, .func=exit_fn, .sym_name="exit", .visible_in_closures=true, .type=Type(
        FunctionType,
        .arg_names=ARRAY((const char*)"status"),
        .arg_types=ARRAY(Type(IntType, .bits=32)),
        .arg_defaults=ARRAY(FakeAST(Int, .i=0, .precision=32)),
        .ret=Type(AbortType))));
    load_global_func(env, t_double, "sane_fmod", PARAM(t_double, "num"), PARAM(t_double, "modulus"));
    load_global_func(env, gcc_type(ctx, STRING), "range_to_cord", PARAM(t_range, "range"), PARAM(t_void_ptr, "stack"), PARAM(t_bool, "color"));
    gcc_func_t *range_to_cord = Table_str_get(&env->global->funcs, "range_to_cord");
    Table_str_set(get_namespace(env, Type(RangeType)), "__cord",
         new(binding_t, .func=range_to_cord, .sym_name="range_to_cord"));
    load_global_func(env, t_bl_str, "range_slice", PARAM(t_bl_str, "array"), PARAM(t_range, "range"), PARAM(t_size, "item_size"));
    load_global_func(env, t_void_ptr, "dlopen", PARAM(t_str, "filename"), PARAM(t_int, "flags"));
    load_global_func(env, t_void_ptr, "dlsym", PARAM(t_void_ptr, "handle"), PARAM(t_str, "symbol"));
    load_global_func(env, t_int, "dlclose", PARAM(t_void_ptr, "handle"));
    load_global_func(env, t_void, "array_cow", PARAM(t_void_ptr, "array"), PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_void, "array_flatten", PARAM(t_void_ptr, "array"), PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_void, "array_insert", PARAM(t_void_ptr, "array"),
                     PARAM(t_void_ptr, "item"),
                     PARAM(t_int64, "index"),
                     PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_void, "array_insert_all", PARAM(t_void_ptr, "array"),
                     PARAM(t_void_ptr, "other"),
                     PARAM(t_int64, "index"),
                     PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_void, "array_remove", PARAM(t_void_ptr, "array"),
                     PARAM(t_int64, "index"),
                     PARAM(t_int64, "count"),
                     PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_void, "array_sort", PARAM(t_void_ptr, "array"),
                     PARAM(t_void_ptr, "compare"),
                     PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_void, "array_shuffle", PARAM(t_void_ptr, "array"), PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));
    load_global_func(env, t_bl_str, "array_join", PARAM(t_void_ptr, "array"), PARAM(t_void_ptr, "glue"), PARAM(t_size, "item_size"), PARAM(t_bool, "atomic"));

    // int halfsiphash(const void *in, const size_t inlen, const void *k, void *out, const size_t outlen);
    load_global_func(env, t_int, "halfsiphash", PARAM(t_void_ptr, "in"), PARAM(t_size, "inlen"), PARAM(t_void_ptr, "k"),
                     PARAM(t_void_ptr, "out"), PARAM(t_size, "outlen"));

    load_global_func(env, t_void_ptr, "sss_hashmap_get", PARAM(t_void_ptr, "table"), PARAM(t_void_ptr, "key_hash"),
                     PARAM(t_void_ptr, "key_cmp"), PARAM(t_size, "entry_size"), PARAM(t_void_ptr, "key"), PARAM(t_size, "value_offset"));
    load_global_func(env, t_void_ptr, "sss_hashmap_get_raw", PARAM(t_void_ptr, "table"), PARAM(t_void_ptr, "key_hash"),
                     PARAM(t_void_ptr, "key_cmp"), PARAM(t_size, "entry_size"), PARAM(t_void_ptr, "key"), PARAM(t_size, "value_offset"));
    load_global_func(env, t_void_ptr, "sss_hashmap_set", PARAM(t_void_ptr, "table"), PARAM(t_void_ptr, "key_hash"),
                     PARAM(t_void_ptr, "key_cmp"), PARAM(t_size, "entry_size"), PARAM(t_void_ptr, "key"),
                     PARAM(t_size, "value_offset"), PARAM(t_void_ptr, "value"));
    load_global_func(env, t_void, "sss_hashmap_remove", PARAM(t_void_ptr, "table"), PARAM(t_void_ptr, "key_hash"),
                     PARAM(t_void_ptr, "key_cmp"), PARAM(t_size, "entry_size"), PARAM(t_void_ptr, "key"));
    load_global_func(env, t_u32, "sss_hashmap_hash", PARAM(t_void_ptr, "table"), PARAM(t_void_ptr, "entry_hash"), PARAM(t_size, "entry_size"));
    load_global_func(env, t_u32, "sss_hashmap_len", PARAM(t_void_ptr, "table"));
    load_global_func(env, t_void, "sss_hashmap_mark_cow", PARAM(t_void_ptr, "table"));
    load_global_func(env, t_int, "sss_hashmap_compare", PARAM(t_void_ptr, "table1"), PARAM(t_void_ptr, "table2"), PARAM(t_void_ptr, "key_hash"),
                     PARAM(t_void_ptr, "key_compare"), PARAM(t_void_ptr, "value_compare"),
                     PARAM(t_size, "entry_size"), PARAM(t_size, "value_offset"));
    load_global_func(env, t_u32, "hash_64bits", PARAM(t_void_ptr, "ptr"));
    load_global_func(env, t_u32, "compare_64bits", PARAM(t_void_ptr, "a"), PARAM(t_void_ptr, "b"));
#undef PARAM
}

env_t *new_environment(gcc_ctx_t *ctx, jmp_buf *on_err, sss_file_t *f, bool tail_calls)
{
    global_env_t *global = new(global_env_t);
    env_t *env = new(env_t,
        .ctx = ctx,
        .global=global,
        .on_err = on_err,
        .file = f,
        .file_bindings = new(table_t, .fallback=&global->bindings),
        .bindings = new(table_t),
        .tail_calls = tail_calls,
    );
    env->bindings->fallback = env->file_bindings;

    load_global_functions(env);

    gcc_type_t *type_gcc_type = sss_type_to_gcc(env, Type(TypeType));
    for (size_t i = 0; i < sizeof(builtin_types)/sizeof(builtin_types[0]); i++) {
        sss_type_t *t = &builtin_types[i].sss_type;
        binding_t *b = new(binding_t,
                           .type=Type(TypeType, t),
                           .rval=gcc_lvalue_address(
                               gcc_global(ctx, NULL, GCC_GLOBAL_IMPORTED, type_gcc_type, builtin_types[i].type_symbol),
                               NULL));
        Table_str_set(&env->global->bindings, builtin_types[i].type_name, b);
        for (int j = 0; builtin_types[i].bindings && builtin_types[i].bindings[j].symbol; i++) {
            auto member = builtin_types[i].bindings[j];
            ast_t *type_ast = parse_type(sss_spoof_file("<builtins>", member.type), NULL);
            sss_type_t *member_type = parse_type_ast(env, type_ast);
            gcc_type_t *member_gcc_type = sss_type_to_gcc(env, member_type);
            // if (member_type->tag == FunctionType) {
            // } else {
            gcc_lvalue_t *lval = gcc_global(ctx, NULL, GCC_GLOBAL_IMPORTED, member_gcc_type, member.symbol);
            set_in_namespace(env, t, member.sss_name, new(binding_t, .type=member_type, .rval=gcc_rval(lval)));
            // }
        }
    }

    return env;
}

env_t *fresh_scope(env_t *env)
{
    env_t *fresh = GC_MALLOC(sizeof(env_t));
    *fresh = *env;
    fresh->bindings = new(table_t, .fallback=env->bindings);
    return fresh;
}

env_t *scope_with_type(env_t *env, sss_type_t *t)
{
    env_t *fresh = GC_MALLOC(sizeof(env_t));
    *fresh = *env;
    fresh->bindings = new(table_t, .fallback=env->bindings);
    table_t *ns = get_namespace(env, t);
    if (t->tag == TaggedUnionType) {
        auto members = Match(t, TaggedUnionType)->members;
        if (LENGTH(members) > 0 && !Table_str_get(ns, ith(members, 0).name))
            populate_tagged_union_constructors(env, t);
    }
    for (uint32_t i = 1; i <= Table_length(ns); i++) {
        struct { const char *key; binding_t *value; } *entry = Table_entry(NULL, ns, i);
        if (!Table_str_get(fresh->bindings, entry->key))
            Table_str_set(fresh->bindings, entry->key, entry->value);
    }
    return fresh;
}

static void copy_global_bindings(table_t *dest, table_t *src)
{
    for (; src; src = src->fallback) {
        for (uint32_t i = 1; i <= Table_length(src); i++) {
            struct { const char *key; binding_t *value;} *entry = Table_str_entry(src, i);
            if (entry->value->visible_in_closures)
                Table_str_set(dest, entry->key, entry->value);
        }
    }
}

env_t *file_scope(env_t *env)
{
    env_t *fresh = GC_MALLOC(sizeof(env_t));
    *fresh = *env;
    fresh->bindings = new(table_t, .fallback=env->file_bindings);
    copy_global_bindings(fresh->bindings, env->bindings);
    return fresh;
}

void compiler_err(env_t *env, ast_t *ast, const char *fmt, ...)
{
    if (isatty(STDERR_FILENO) && !getenv("NO_COLOR"))
        fputs("\x1b[31;7;1m", stderr);
    if (ast && ast->file)
        fprintf(stderr, "%s:%ld.%ld: ", ast->file->relative_filename, sss_get_line_number(ast->file, ast->start),
                sss_get_line_column(ast->file, ast->start));
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    if (isatty(STDERR_FILENO) && !getenv("NO_COLOR"))
        fputs(" \x1b[m", stderr);
    fputs("\n\n", stderr);
    if (ast)
        fprint_span(stderr, ast->file, ast->start, ast->end, "\x1b[31;1m", 2, isatty(STDERR_FILENO) && !getenv("NO_COLOR"));

    if (env->on_err)
        longjmp(*env->on_err, 1);

    raise(SIGABRT);
    exit(1);
}

binding_t *get_binding(env_t *env, const char *name)
{
    return Table_str_get(env->bindings, name);
}

binding_t *get_local_binding(env_t *env, const char *name)
{
    table_t *fallback = env->bindings->fallback;
    binding_t *b = Table_str_get(env->bindings, name);
    env->bindings->fallback = fallback;
    return b;
}

gcc_func_t *get_function(env_t *env, const char *name)
{
    gcc_func_t *func = Table_str_get(&env->global->funcs, name);
    assert(func);
    return func;
}

binding_t *get_ast_binding(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case Var: {
        binding_t *b = get_binding(env, Match(ast, Var)->name);
        if (b) return b;
        const char *suggestion = spellcheck(env->bindings, Match(ast, Var)->name);
        if (suggestion)
            compiler_err(env, ast, "I can't figure out what this type refers to. Did you mean '%s'?", suggestion);
        else
            compiler_err(env, ast, "I can't figure out what this type refers to.");
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        binding_t *b = get_ast_binding(env, access->fielded);
        sss_type_t *t = b->type->tag == TypeType ? Match(b->type, TypeType)->type : b->type;
        table_t *ns = get_namespace(env, t);
        b = get_from_namespace(env, t, access->field);
        if (b) return b;

        const char *suggestion = spellcheck(ns, access->field);
        if (suggestion)
            compiler_err(env, ast, "There isn't a member on %T called '%s', did you mean '%s'?",
                         t, access->field, suggestion);
        else
            compiler_err(env, ast, "There isn't a member on %T called '%s'", t, access->field);
    }
    default: compiler_err(env, ast, "I can't figure out at compile-time what this refers to");
    }

}

table_t *get_namespace(env_t *env, sss_type_t *t)
{
    table_t *ns = Table_str_get(&env->global->type_namespaces, type_to_string(t));
    if (!ns) {
        ns = new(table_t, .fallback=env->file_bindings);
        Table_str_set(&env->global->type_namespaces, type_to_string(t), ns);

        sss_type_t *base_t = t;
        for (;;) {
            // if (base_t->tag == PointerType) base_t = Match(base_t, PointerType)->pointed;
            if (base_t->tag == VariantType) base_t = Match(base_t, VariantType)->variant_of;
            else break;
        }
    }
    return ns;
}

env_t *get_type_env(env_t *env, sss_type_t *t)
{
    env_t *fresh = GC_MALLOC(sizeof(env_t));
    *fresh = *env;
    fresh->bindings = get_namespace(env, t);
    return fresh;
}

binding_t *get_from_namespace(env_t *env, sss_type_t *t, const char *name)
{
    // Do lookup without fallbacks
    // (we don't want module.Struct.__cord to return module.__cord, even
    // though the namespace may have that set as a fallback so that code
    // module.Struct can reference things inside the module's namespace)
    table_t *ns = get_namespace(env, t);
    table_t *fallback = ns->fallback;
    ns->fallback = NULL;
    binding_t *b = Table_str_get(ns, name);
    ns->fallback = fallback;
    return b;
}

void set_in_namespace(env_t *env, sss_type_t *t, const char *name, void *value)
{
    Table_str_set(get_namespace(env, t), heap_str(name), value);
}

#define MIN3(a, b, c) ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))

static int word_distance(const char *s1, const char *s2) {
    // Levenshtein distance
    unsigned int s1len, s2len, x, y, lastdiag, olddiag;
    s1len = strlen(s1);
    s2len = strlen(s2);
    unsigned int column[s1len + 1];
    for (y = 1; y <= s1len; y++)
        column[y] = y;
    for (x = 1; x <= s2len; x++) {
        column[0] = x;
        for (y = 1, lastdiag = x - 1; y <= s1len; y++) {
            olddiag = column[y];
            column[y] = MIN3(column[y] + 1, column[y-1] + 1, lastdiag + (s1[y-1] == s2[x-1] ? 0 : 1));
            lastdiag = olddiag;
        }
    }
    return column[s1len];
}

const char *spellcheck(table_t *ns, const char *word)
{
    assert(!Table_str_get(ns, word));
    const char *closest = NULL;
    int closest_dist = 0;
    for (; ns; ns = ns->fallback) {
        for (uint32_t i = 1; i <= Table_length(ns); i++) {
            struct { const char *key; binding_t *value;} *entry = Table_str_entry(ns, i);
            int dist = word_distance(word, entry->key);
            if (closest == NULL || dist < closest_dist) {
                closest = entry->key;
                closest_dist = dist;
            }
        }
    }

    if (closest_dist <= (int)ceil((double)strlen(word) * 0.25))
        return closest;
    return NULL;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
