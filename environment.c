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

typedef struct {
    const char *name, *type;
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

static sss_type_t sss_cord_t = {
    .tag=VariantType,
    .__data.VariantType={
        .name="Cord", .variant_of=(sss_type_t[]){{
            .tag=PointerType,
            .__data.PointerType={
                .pointed=(sss_type_t[]){{.tag=CharType}},
                .is_optional=true,
            },
        }},
    },
};

struct {const char *symbol, *type; } builtin_functions[] = {
    {"GC_malloc", "func(size:Int)->@Memory"},
    {"GC_malloc_atomic", "func(size:Int)->@Memory"},
    {"GC_realloc", "func(data:?Memory, size:Int)->@Memory"},
    {"memcpy", "func(dest:@Memory, src:@Memory, size:Int)->@Memory"},
    {"getenv", "func(name:CString)->CString"},

    // Generic functions:
    {"generic_compare", "func(x:&(readonly)Memory, y:&(readonly)Memory, type:&(readonly)TypeInfo)->Int32"},
    {"generic_equal", "func(x:&(readonly)Memory, y:&(readonly)Memory, type:&(readonly)TypeInfo)->Bool"},
    {"generic_hash", "func(obj:&(readonly)Memory, type:&(readonly)TypeInfo)->Int32"},
    {"generic_cord", "func(obj:&(readonly)Memory, colorize:Bool, type:&(readonly)TypeInfo)->Cord"},
    {"Func__cord", "func(obj:&(readonly)Memory, colorize:Bool, type:&(readonly)TypeInfo)->Cord"},
    {"Type__cord", "func(obj:&(readonly)Memory, colorize:Bool, type:&(readonly)TypeInfo)->Cord"},
    {"Memory__cord", "func(obj:&(readonly)Memory, colorize:Bool, type:&(readonly)TypeInfo)->Cord"},
    {"Num__cord", "func(obj:&(readonly)Memory, colorize:Bool, type:&(readonly)TypeInfo)->Cord"},
    {"Num32__cord", "func(obj:&(readonly)Memory, colorize:Bool, type:&(readonly)TypeInfo)->Cord"},
    {"Int__cord", "func(obj:&(readonly)Memory, colorize:Bool, type:&(readonly)TypeInfo)->Cord"},
    {"Int32__cord", "func(obj:&(readonly)Memory, colorize:Bool, type:&(readonly)TypeInfo)->Cord"},
    {"Int16__cord", "func(obj:&(readonly)Memory, colorize:Bool, type:&(readonly)TypeInfo)->Cord"},
    {"Int8__cord", "func(obj:&(readonly)Memory, colorize:Bool, type:&(readonly)TypeInfo)->Cord"},

    // Builtins:
    {"builtin_say", "func(str:Str, end=\"\\n\")->Void"},
    {"builtin_last_err", "func()->Str"},
    {"builtin_doctest", "func(label:CString, expr:Cord, type:CString, use_color:Bool, expected:CString, filename:CString, start:Int32, end:Int32)->Void"},

    // Dynamic linking:
    {"dlopen", "func(filename:CString, flags:Int32)->?Memory"},
    {"dlsym", "func(handle:@Memory, symbol:CString)->?Memory"},
    {"dlclose", "func(handle:@Memory)->?Memory"},

    // Cord functions:
    {"CORD_cat", "func(x:Cord, y:Cord)->Cord"},
    {"CORD_cat_char_star", "func(x:Cord, y:CString, leny:Int)->Cord"},
    {"CORD_cat_char", "func(x:Cord, c:Char)->Cord"},
    {"CORD_cmp", "func(x:Cord, y:Cord)->Int32"},
    {"CORD_to_const_char_star", "func(c:Cord)->CString"},
    {"CORD_to_char_star", "func(c:Cord)->CString"},
    {"CORD_len", "func(c:Cord)->Int"},
};

struct {
    sss_type_t sss_type;
    const char *type_name, *type_symbol;
    builtin_binding_t *bindings;
} builtin_types[] = {
    {{.tag=BoolType}, "Bool", "Bool_type", NULL},
    {{.tag=MemoryType}, "Memory", "Memory_type", NULL},
    {{.tag=RangeType}, "Range", "Range_type", NULL},
    {{.tag=VoidType}, "Void", "Void_type", NULL},
    {{.tag=AbortType}, "Abort", "Abort_type", NULL},
    {{.tag=TypeInfoType}, "TypeInfo", "TypeInfo_type", NULL},

#define BUILTIN_INT(t, ...) \
    {{.tag=IntType, .__data.IntType={__VA_ARGS__}}, #t, #t "_type", (builtin_binding_t[]){ \
        {"min", #t}, \
        {"max", #t}, \
        {"abs", "func(i:"#t")->"#t}, \
        {"format", "func(i:"#t", digits=0)->Str"}, \
        {"hex", "func(i:"#t", digits=0, uppercase=yes, prefix=yes)->Str"}, \
        {"octal", "func(i:"#t", digits=0, prefix=yes)->Str"}, \
        {"random", "func(min=1, max=100)->"#t}, \
        {NULL, NULL}, \
    }}
    BUILTIN_INT(Int, .bits=64), BUILTIN_INT(Int32, .bits=32),
    BUILTIN_INT(Int16, .bits=16), BUILTIN_INT(Int8, .bits=8),
#undef BUILTIN_INT

    {{.tag=NumType, .__data.NumType={.bits=64}}, "Num", "Num_type", (builtin_binding_t[]){
        // Constants
        {"2_sqrt_pi", "Num"},
        {"NaN", "Num"},
        {"e", "Num"},
        {"half_pi", "Num"},
        {"inf", "Num"},
        {"inverse_half_pi", "Num"},
        {"inverse_pi", "Num"},
        {"ln10", "Num"},
        {"ln2", "Num"},
        {"log2e", "Num"},
        {"pi", "Num"},
        {"quarter_pi", "Num"},
        {"sqrt2", "Num"},
        {"sqrt_half", "Num"},
        {"tau", "Num"},
        // Nullary functions:
        {"random", "func()->Num"},
        // Predicates:
        {"isinf", "func(n:Num)->Bool"},
        {"finite", "func(n:Num)->Bool"},
        {"isnan", "func(n:Num)->Bool"},
        // Unary functions:
#define UNARY(fn) {#fn, "func(n:Num)->Num"}
        UNARY(abs), UNARY(acos), UNARY(acosh), UNARY(asin), UNARY(asinh), UNARY(atan), UNARY(atanh),
        UNARY(cbrt), UNARY(ceil), UNARY(cos), UNARY(cosh), UNARY(erf), UNARY(erfc), UNARY(exp),
        UNARY(exp10), UNARY(exp2), UNARY(expm1), UNARY(floor), UNARY(j0), UNARY(j1), UNARY(log),
        UNARY(log10), UNARY(log1p), UNARY(log2), UNARY(logb), UNARY(nextdown), UNARY(nextup),
        UNARY(rint), UNARY(round), UNARY(roundeven), UNARY(significand), UNARY(sin), UNARY(sinh),
        UNARY(sqrt), UNARY(tan), UNARY(tanh), UNARY(tgamma), UNARY(trunc), UNARY(y0), UNARY(y1),
#undef UNARY
        // Binary functions:
        {"atan2", "func(y:Num, x:Num)->Num"},
        {"copysign", "func(magnitude:Num, sign:Num)->Num"},
        {"distance", "func(x:Num, y:Num)->Num"},
        {"hypot", "func(x:Num, y:Num)->Num"},
        {"maxmag", "func(x:Num, y:Num)->Num"},
        {"minmag", "func(x:Num, y:Num)->Num"},
        {"mod", "func(x:Num, modulus:Num)->Num"},
        {"nextafter", "func(x:Num, toward:Num)->Num"},
        {"pow", "func(base:Num, power:Num)->Num"},
        {"remainder", "func(x:Num, divisor:Num)->Num"},
        // Other functions:
        {"format", "func(n:Num, precision=0x7fffffffffffffff)->Str"},
        {"scientific", "func(n:Num, precision=0x7fffffffffffffff)->Str"},
        {NULL, NULL},
    }},

    {{.tag=NumType, .__data.NumType={.bits=32}}, "Num32", "Num32_type", (builtin_binding_t[]){
        // Constants
        {"2_sqrt_pi", "Num32"},
        {"NaN", "Num32"},
        {"e", "Num32"},
        {"half_pi", "Num32"},
        {"inf", "Num32"},
        {"inverse_half_pi", "Num32"},
        {"inverse_pi", "Num32"},
        {"ln10", "Num32"},
        {"ln2", "Num32"},
        {"log2e", "Num32"},
        {"pi", "Num32"},
        {"quarter_pi", "Num32"},
        {"sqrt2", "Num32"},
        {"sqrt_half", "Num32"},
        {"tau", "Num32"},
        // Nullary functions:
        {"random", "func()->Num32"},
        // Predicates:
        {"isinf", "func(n:Num32)->Bool"},
        {"finite", "func(n:Num32)->Bool"},
        {"isnan", "func(n:Num32)->Bool"},
        // Unary functions:
#define UNARY(fn) {#fn, "func(n:Num32)->Num32"}
        UNARY(abs), UNARY(acos), UNARY(acosh), UNARY(asin), UNARY(asinh), UNARY(atan), UNARY(atanh),
        UNARY(cbrt), UNARY(ceil), UNARY(cos), UNARY(cosh), UNARY(erf), UNARY(erfc), UNARY(exp),
        UNARY(exp10), UNARY(exp2), UNARY(expm1), UNARY(floor), UNARY(j0), UNARY(j1), UNARY(log),
        UNARY(log10), UNARY(log1p), UNARY(log2), UNARY(logb), UNARY(nextdown), UNARY(nextup),
        UNARY(rint), UNARY(round), UNARY(roundeven), UNARY(significand), UNARY(sin), UNARY(sinh),
        UNARY(sqrt), UNARY(tan), UNARY(tanh), UNARY(tgamma), UNARY(trunc), UNARY(y0), UNARY(y1),
#undef UNARY
        // Binary functions:
        {"atan2", "func(y:Num32, x:Num32)->Num32"},
        {"copysign", "func(magnitude:Num32, sign:Num32)->Num32"},
        {"distance", "func(x:Num32, y:Num32)->Num32"},
        {"hypot", "func(x:Num32, y:Num32)->Num32"},
        {"maxmag", "func(x:Num32, y:Num32)->Num32"},
        {"minmag", "func(x:Num32, y:Num32)->Num32"},
        {"mod", "func(x:Num32, modulus:Num32)->Num32"},
        {"nextafter", "func(x:Num32, toward:Num32)->Num32"},
        {"pow", "func(base:Num32, power:Num32)->Num32"},
        {"remainder", "func(x:Num32, divisor:Num32)->Num32"},
        // Other functions:
        {"format", "func(n:Num32, precision=0x7fffffffffffffff)->Str"},
        {"scientific", "func(n:Num32, precision=0x7fffffffffffffff)->Str"},
        {NULL, NULL},
    }},

    {{.tag=CharType}, "Char", "Char_type", (builtin_binding_t[]){
        {"toupper", "func(c:Char)->Char"},
        {"tolower", "func(c:Char)->Char"},
#define CHARP(name) {#name, "func(c:Char)->Bool"}
        CHARP(isalnum), CHARP(isalpha), CHARP(iscntrl), CHARP(isdigit), CHARP(isgraph), CHARP(islower),
        CHARP(isprint), CHARP(ispunct), CHARP(isspace), CHARP(isupper), CHARP(isxdigit), CHARP(isascii), CHARP(isblank),
#undef CHARP
        {NULL, NULL},
    }},

    {sss_str_t, "Str", "Str_type", (builtin_binding_t[]){
        {"uppercased", "func(s:Str)->Str"},
        {"lowercased", "func(s:Str)->Str"},
        {"capitalized", "func(s:Str)->Str"},
        {"titlecased", "func(s:Str)->Str"},
        {"starts_with", "func(s:Str, prefix:Str)->Bool"},
        {"ends_with", "func(s:Str, suffix:Str)->Bool"},
        {"without_prefix", "func(s:Str, prefix:Str)->Str"},
        {"without_suffix", "func(s:Str, suffix:Str)->Str"},
        {"trimmed", "func(s:Str, trim_chars=[\\x20, \\n, \\r, \\t, \\v], trim_left=yes, trim_right=yes)->Str"},
        {"slice", "func(s:&Str, range:Range, readonly=no, _type=_typeinfo_(s))->Str"},
        {"c_string", "func(s:Str)->CString"},
        {"from_c_string", "func(c:CString)->Str"},
        {"find", "func(s:Str, target:Str)->Str"},
        {"replace", "func(s:Str, target:Str, replacement:Str, limit=0x7fffffffffffffff)->Str"},
        {"quoted", "func(s:Str)->Str"},
        {"split", "func(s:Str, split_chars=[\\x2C, \\x20, \\n, \\r, \\t, \\v])->[Str]"},
        {"join", "func(glue:Str, pieces:[Str])->Str"},

        {"equals", "func(x:&(readonly)Str, y:&(readonly)Str, _type=_typeinfo_(x[]))->Bool"},
        {"compare", "func(x:&(readonly)Str, y:&(readonly)Str, _type=_typeinfo_(x[]))->Int32"},
        {"hash", "func(s:&(readonly)Str, _type=_typeinfo_(s[]))->Int32"},
        {"cord", "func(s:&(readonly)Str, colorize:Bool, _type=_typeinfo_(s[]))->Cord"},
        {NULL, NULL},
    }},

    {sss_c_str_t, "CString", "CString_type", (builtin_binding_t[]){
        {"from_str", "func(s:Str)->CString"},
        {"as_str", "func(c:CString)->Str"},
        {NULL, NULL},
    }},

    {sss_cord_t, "Cord", "Cord_type", (builtin_binding_t[]){
        {NULL, NULL},
    }},
};

gcc_func_t *import_function(env_t *env, const char *name, sss_type_t *fn_t, bool keep)
{
    if (keep) {
        gcc_func_t *func = Table_str_get(&env->global->funcs, name);
        if (func) return func;
    }
    auto params = EMPTY_ARRAY(gcc_param_t*);
    auto fn = Match(fn_t, FunctionType);
    for (int64_t i = 0; i < LENGTH(fn->arg_types); i++) {
        gcc_type_t *arg_t = sss_type_to_gcc(env, ith(fn->arg_types, i));
        const char *arg_name = ith(fn->arg_names, i);
        append(params, gcc_new_param(env->ctx, NULL, arg_t, arg_name));
    }
    gcc_type_t *t_ret = sss_type_to_gcc(env, fn->ret);
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, t_ret, name, LENGTH(params), params[0], 0);
    if (keep)
        Table_str_set(&env->global->funcs, name, func);
    return func;
}

env_t *new_environment(gcc_ctx_t *ctx, jmp_buf *on_err, sss_file_t *f, bool tail_calls)
{
    global_env_t *global = new(global_env_t);
    env_t *env = new(env_t,
        .ctx = ctx,
        .global=global,
        .on_err = on_err,
        .file = f,
        .bindings = new(table_t, .fallback=&global->bindings),
        .tail_calls = tail_calls,
    );

    const int num_builtin_types = sizeof(builtin_types)/sizeof(builtin_types[0]);
    sss_type_t *namespace_types[num_builtin_types] = {};

    // Declare types first:
    for (size_t i = 0; i < num_builtin_types; i++) {
        sss_type_t *t = &builtin_types[i].sss_type;
        Table_str_set(&env->global->bindings, heap_strf("#type:%s", builtin_types[i].type_name), t);
        namespace_types[i] = Type(StructType, .field_names=ARRAY("type"), .field_types=ARRAY(Type(TypeInfoType)));
    }

    // Then add struct fields to the type namespace struct:
    for (size_t i = 0; i < num_builtin_types; i++) {
        auto ns_struct = Match(namespace_types[i], StructType);
        for (int j = 0; builtin_types[i].bindings && builtin_types[i].bindings[j].name; j++) {
            auto member = builtin_types[i].bindings[j];
            ast_t *type_ast = parse_type_str(member.type);
            if (!type_ast)
                compiler_err(env, NULL, "Couldn't parse builtin type for %s.%s: `%s`", builtin_types[i].type_name, member.name, member.type);
            sss_type_t *member_type = parse_type_ast(env, type_ast);
            append(ns_struct->field_names, member.name);
            append(ns_struct->field_types, member_type);
        }
    }

    // Then bind namespace structs for each type:
    for (size_t i = 0; i < num_builtin_types; i++) {
        sss_type_t *t = &builtin_types[i].sss_type;
        sss_type_t *ns_t = namespace_types[i];
        gcc_type_t *ns_gcc_t = sss_type_to_gcc(env, ns_t);
        gcc_lvalue_t *type_lval = gcc_global(ctx, NULL, GCC_GLOBAL_IMPORTED, ns_gcc_t, builtin_types[i].type_symbol);
        // Binding is a pointer to the namespace struct (so they can be imported more easily)
        binding_t *b = new(binding_t, .type=Type(PointerType, .pointed=ns_t),
                           .rval=gcc_lvalue_address(type_lval, NULL));
        Table_str_set(&env->global->bindings, builtin_types[i].type_name, b);
        gcc_field_t *info_field = gcc_get_field(gcc_type_as_struct(ns_gcc_t), 0);
        gcc_lvalue_t *info_lval = gcc_lvalue_access_field(type_lval, NULL, info_field);
        Table_str_set(&env->global->typeinfos, type_to_string_concise(t), new(typeinfo_lval_t, .type=t, .lval=info_lval));
        mark_typeinfo_lvalue_initialized(env, t);

        gcc_struct_t *ns_gcc_struct = gcc_type_as_struct(ns_gcc_t);

        // For each namespace struct member, bind its lval inside that type's namespace:
        auto ns_struct = Match(ns_t, StructType);
        for (int f = 1 /* skip 'type' */; f < LENGTH(ns_struct->field_names); f++) {
            const char *name = ith(ns_struct->field_names, f);
            sss_type_t *field_type = ith(ns_struct->field_types, f);
            gcc_field_t *field = gcc_get_field(ns_gcc_struct, f);
            gcc_lvalue_t *field_lval = gcc_lvalue_access_field(type_lval, NULL, field);
            set_in_namespace(env, t, name, new(binding_t, .type=field_type, .lval=field_lval, .rval=gcc_rval(field_lval)));
        }
    }

    // Load global functions:
    for (size_t i = 0; i < sizeof(builtin_functions)/sizeof(builtin_functions[0]); i++) {
        auto fn = builtin_functions[i];
        ast_t *type_ast = parse_type_str(fn.type);
        if (!type_ast)
            compiler_err(env, NULL, "Couldn't parse builtin type for %s: `%s`", fn.symbol, fn.type);
        sss_type_t *fn_type = parse_type_ast(env, type_ast);
        gcc_func_t *func = import_function(env, fn.symbol, fn_type, true);
        Table_str_set(&env->global->funcs, fn.symbol, func);
    }

    Table_str_set(&env->global->bindings, "say",
                  new(binding_t, .type=parse_type_ast(env, parse_type_str("func(str:Str, end=\"\\n\")->Void")),
                      .func=get_function(env, "builtin_say"),
                      .rval=gcc_get_func_address(get_function(env, "builtin_say"), NULL)));

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
    for (int64_t i = 1; i <= Table_length(ns); i++) {
        struct { const char *key; binding_t *value; } *entry = Table_entry(ns, i);
        if (!Table_str_get(fresh->bindings, entry->key))
            Table_str_set(fresh->bindings, entry->key, entry->value);
    }
    return fresh;
}

static void copy_global_bindings(table_t *dest, table_t *src)
{
    for (; src; src = src->fallback) {
        for (int64_t i = 1; i <= Table_length(src); i++) {
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
    fresh->bindings = new(table_t, .fallback=&env->global->bindings);
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

sss_type_t *get_type_by_name(env_t *env, const char *name)
{
    sss_type_t *t = Table_str_get(env->bindings, heap_strf("#type:%s", name));
    return t;
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
    if (!func)
        compiler_err(env, NULL, "Couldn't find function: %s", name);
    return func;
}

static inline void _load_method(env_t *env, table_t *ns, const char *name, const char *symbol, sss_type_t *t)
{
    gcc_func_t *func = import_function(env, symbol, t, false);
    Table_str_set(ns, name, new(binding_t, .type=t, .func=func, .rval=gcc_get_func_address(func, NULL)));
}

table_t *get_namespace(env_t *env, sss_type_t *t)
{
    while (t->tag == PointerType)
        t = Match(t, PointerType)->pointed;

    table_t *ns = Table_str_get(&env->global->type_namespaces, type_to_string(t));
    if (!ns) {
        ns = new(table_t, .fallback=env->bindings);
        Table_str_set(&env->global->type_namespaces, type_to_string(t), ns);

#define REF(x) Type(PointerType, .is_stack=true, .pointed=x)
#define RO_REF(x) Type(PointerType, .is_stack=true, .is_readonly=true, .pointed=x)
#define OPT(x) Type(PointerType, .is_optional=true, .pointed=x)
#define FN(names, types, defaults, ret_type) Type(FunctionType, .arg_names=names, .arg_types=types, .arg_defaults=defaults, .ret=ret_type)
#define TYPEINFO_DEREF(var) FakeAST(GetTypeInfo, FakeAST(Index, FakeAST(Var, var)))
#define NAMES(...) ARRAY((const char*)__VA_ARGS__)
#define TYPES(...) ARRAY((sss_type_t*)__VA_ARGS__)
#define DEFTS(...) ARRAY((ast_t*)__VA_ARGS__)
        sss_type_t *void_t = Type(VoidType);
        sss_type_t *typeinfo_ptr_t = Type(PointerType, .pointed=Type(TypeInfoType));
        if (t->tag == TableType) {
            sss_type_t *key_t = Match(t, TableType)->key_type;
            sss_type_t *value_t = Match(t, TableType)->value_type;
            _load_method(env, ns, "remove", "Table_remove",
                         FN(NAMES("t", "key", "_type"),
                            TYPES(REF(t), RO_REF(key_t), typeinfo_ptr_t),
                            DEFTS(NULL, NULL, TYPEINFO_DEREF("t")),
                            void_t));
            _load_method(env, ns, "set", "Table_set",
                         FN(NAMES("t", "key", "value", "_type"),
                            TYPES(REF(t), RO_REF(key_t), RO_REF(value_t), typeinfo_ptr_t),
                            DEFTS(NULL, NULL, NULL, TYPEINFO_DEREF("t")),
                            void_t));
            _load_method(env, ns, "reserve", "Table_reserve",
                         FN(NAMES("t", "key", "value", "_type"),
                            TYPES(REF(t), RO_REF(key_t), OPT(value_t), typeinfo_ptr_t),
                            DEFTS(NULL, NULL, NULL, TYPEINFO_DEREF("t")),
                            Type(PointerType, value_t)));
            _load_method(env, ns, "get", "Table_get",
                         FN(NAMES("t", "key", "_type"),
                            TYPES(RO_REF(t), RO_REF(key_t), typeinfo_ptr_t),
                            DEFTS(NULL, NULL, TYPEINFO_DEREF("t")),
                            OPT(value_t)));
            _load_method(env, ns, "get_raw", "Table_get_raw",
                         FN(NAMES("t", "key", "_type"),
                            TYPES(RO_REF(t), RO_REF(key_t), typeinfo_ptr_t),
                            DEFTS(NULL, NULL, TYPEINFO_DEREF("t")),
                            OPT(value_t)));
            _load_method(env, ns, "clear", "Table_get_clear",
                         FN(NAMES("t"), TYPES(REF(t)), DEFTS(NULL), void_t));
            _load_method(env, ns, "mark_copy_on_write", "Table_mark_copy_on_write",
                         FN(NAMES("t"), TYPES(REF(t)), DEFTS(NULL), void_t));
        } else if (t->tag == ArrayType) {
            sss_type_t *item_t = Match(t, ArrayType)->item_type;
            sss_type_t *i64 = Type(IntType, .bits=64);
            ast_t *item_size = FakeAST(SizeOf, FakeAST(Index, .indexed=FakeAST(Var, "array"), .index=FakeAST(Int, .precision=64, .i=1)));
            _load_method(env, ns, "insert", "Array_insert",
                         FN(NAMES("array", "item", "index", "_item_size"),
                            TYPES(REF(t), RO_REF(item_t), i64, i64),
                            DEFTS(NULL, NULL, FakeAST(Int, .precision=64, .i=0), item_size),
                            void_t));
            _load_method(env, ns, "insert_all", "Array_insert_all",
                         FN(NAMES("array", "to_insert", "index", "_item_size"),
                            TYPES(REF(t), t, i64, i64),
                            DEFTS(NULL, NULL, FakeAST(Int, .precision=64, .i=0), item_size),
                            void_t));
            _load_method(env, ns, "remove", "Array_remove",
                         FN(NAMES("array", "index", "count", "_item_size"),
                            TYPES(REF(t), i64, i64, i64),
                            DEFTS(NULL, FakeAST(Int, .precision=64, .i=-1), FakeAST(Int, .precision=64, .i=1), item_size),
                            void_t));
            _load_method(env, ns, "contains", "Array_contains",
                         FN(NAMES("array", "item", "_type"),
                            TYPES(RO_REF(t), RO_REF(item_t), typeinfo_ptr_t),
                            DEFTS(NULL, NULL, TYPEINFO_DEREF("array")),
                            Type(BoolType)));
            _load_method(env, ns, "compact", "Array_compact",
                         FN(NAMES("array", "item_size"), TYPES(REF(t), i64), DEFTS(NULL, item_size),
                            t));
            _load_method(env, ns, "sort", "Array_sort",
                         FN(NAMES("array", "_type"),
                            TYPES(REF(t), typeinfo_ptr_t),
                            DEFTS(NULL, TYPEINFO_DEREF("array")),
                            void_t));
            _load_method(env, ns, "shuffle", "Array_shuffle",
                         FN(NAMES("array", "_item_size"),
                            TYPES(REF(t), i64),
                            DEFTS(NULL, item_size),
                            void_t));
            _load_method(env, ns, "clear", "Array_clear",
                         FN(NAMES("array"),
                            TYPES(REF(t)),
                            DEFTS(NULL),
                            void_t));
            _load_method(env, ns, "slice", "Array_slice",
                         FN(NAMES("array", "range", "readonly", "_type"),
                            TYPES(REF(t), Type(RangeType), Type(BoolType), typeinfo_ptr_t),
                            DEFTS(NULL, NULL, FakeAST(Bool, false), TYPEINFO_DEREF("array")),
                            t));
        }
#undef REF
#undef RO_REF
#undef OPT
#undef FN
#undef TYPEINFO_DEREF
#undef NAMES
#undef TYPES
#undef DEFTS
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
    table_t *ns = get_namespace(env, t);
    // Do lookup without fallbacks
    // (we don't want module.Struct.__cord to return module.__cord, even
    // though the namespace may have that set as a fallback so that code
    // module.Struct can reference things inside the module's namespace)
    binding_t *b = Table_str_get_raw(ns, name);

    // If value isn't found, look up the value on the thing this type is a
    // variant of and convert that type to the variant type (e.g. on a DSL
    // variant of Str, bind "uppercased" to Str.uppercased, but with the type
    // `func(s:DSL)->DSL`)
    if (!b && t->tag == VariantType) {
        sss_type_t *base = Match(t, VariantType)->variant_of;
      next_base:;
        binding_t *base_b = get_from_namespace(env, base, name);
        if (base_b) {
            // If this is a variant of a type with a method, lazily include its methods
            // For example, `type DSL := Str` should get the method: uppercased:func(s:DSL)->DSL
            b = new(binding_t);
            *b = *base_b;
            b->type = replace_type(b->type, base, t);
            set_in_namespace(env, t, name, b);
        } else if (base->tag == VariantType) {
            base = Match(base, VariantType)->variant_of;
            goto next_base;
        }
    }
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
        for (int64_t i = 1; i <= Table_length(ns); i++) {
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
