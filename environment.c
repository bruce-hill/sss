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
    {"GC_malloc", "func(size:UInt)->@Memory"},
    {"GC_malloc_atomic", "func(size:UInt)->@Memory"},
    {"GC_realloc", "func(data:?Memory, size:UInt)->@Memory"},
    {"memcpy", "func(dest:@Memory, src:@Memory, size:UInt)->@Memory"},
    {"getenv", "func(name:CString)->CString"},

    // Generic functions:
    {"generic_compare", "func(x:&(read-only)Memory, y:&(read-only)Memory, type:&(read-only)Type)->Int32"},
    {"generic_equal", "func(x:&(read-only)Memory, y:&(read-only)Memory, type:&(read-only)Type)->Bool"},
    {"generic_hash", "func(obj:&(read-only)Memory, type:&(read-only)Type)->UInt32"},
    {"generic_cord", "func(obj:&(read-only)Memory, colorize:Bool, type:&(read-only)Type)->Cord"},
    {"Func_cord", "func(obj:&(read-only)Memory, colorize:Bool, type:&(read-only)Type)->Cord"},
    {"Type_cord", "func(obj:&(read-only)Memory, colorize:Bool, type:&(read-only)Type)->Cord"},

    // Builtins:
    {"builtin_say", "func(str:Str, end=\"\\n\")->Void"},
    {"builtin_last_err", "func()->Str"},
    {"builtin_doctest", "func(label:CString, expr:Cord, type:CString, use_color:Bool, expected:CString, filename:CString, start:Int32, end:Int32)->Void"},

    // Modulus:
    {"Num_mod", "func(n:Num, divisor:Num)->Num"},
    {"Num32_mod", "func(n:Num32, divisor:Num32)->Num32"},

    // Dynamic linking:
    {"dlopen", "func(filename:CString, flags:Int32)->?Memory"},
    {"dlsym", "func(handle:@Memory, symbol:CString)->?Memory"},
    {"dlclose", "func(handle:@Memory)->?Memory"},

    // Cord functions:
    {"CORD_cat", "func(x:Cord, y:Cord)->Cord"},
    {"CORD_cat_char_star", "func(x:Cord, y:CString, leny:UInt)->Cord"},
    {"CORD_cat_char", "func(x:Cord, c:Char)->Cord"},
    {"CORD_cmp", "func(x:Cord, y:Cord)->Int32"},
    {"CORD_to_const_char_star", "func(c:Cord)->CString"},
    {"CORD_to_char_star", "func(c:Cord)->CString"},
    {"CORD_len", "func(c:Cord)->UInt"},
};

struct {
    sss_type_t sss_type;
    const char *type_name, *type_symbol;
    builtin_binding_t *bindings;
} builtin_types[] = {
    {{.tag=BoolType}, "Bool", "Bool_type", NULL},
    {{.tag=MemoryType}, "Memory", "Memory_type", NULL},
    {{.tag=RangeType}, "Range", "Range_type", NULL},
    {{.tag=TypeType}, "Type", "Type_type", NULL},
    {{.tag=VoidType}, "Void", "Void_type", NULL},
    {{.tag=AbortType}, "Abort", "Abort_type", NULL},

#define BUILTIN_INT(t, ...) \
    {{.tag=IntType, .__data.IntType={__VA_ARGS__}}, #t, #t "_type", (builtin_binding_t[]){ \
        {#t"__format", "format", "func(i:"#t", digits=0)->Str"}, \
        {#t"__hex",    "hex", "func(i:"#t", digits=0, uppercase=yes, prefix=no)->Str"}, \
        {#t"__octal",  "octal", "func(i:"#t", digits=0, prefix=no)->Str"}, \
        {#t"__random", "random", "func()->"#t}, \
        {#t"__min",    "min", #t}, \
        {#t"__max",    "max", #t}, \
        {NULL, NULL, NULL}, \
    }}
    BUILTIN_INT(Int, .bits=64), BUILTIN_INT(Int32, .bits=32), BUILTIN_INT(Int16, .bits=16), BUILTIN_INT(Int8, .bits=8),
    BUILTIN_INT(UInt, .bits=64, .is_unsigned=true), BUILTIN_INT(UInt32, .bits=32, .is_unsigned=true),
    BUILTIN_INT(UInt16, .bits=16, .is_unsigned=true), BUILTIN_INT(UInt8, .bits=8, .is_unsigned=true),
#undef BUILTIN_INT

    {{.tag=NumType, .__data.NumType={.bits=64}}, "Num", "Num_type", (builtin_binding_t[]){
        {"Num__format", "format", "func(n:Num, precision:Int)->Str"},
        {"drand48", "random", "func()->Num"},
        // Unary functions:
#define UNARY(fn) {#fn, #fn, "func(n:Num)->Num"}
        UNARY(acos), UNARY(asin), UNARY(atan), UNARY(cos), UNARY(sin), UNARY(tan), UNARY(cosh), UNARY(sinh), UNARY(tanh),
        UNARY(acosh), UNARY(asinh), UNARY(atanh), UNARY(exp), UNARY(log), UNARY(log10), UNARY(exp10), UNARY(expm1),
        UNARY(log1p), UNARY(logb), UNARY(exp2), UNARY(log2), UNARY(sqrt), UNARY(cbrt), UNARY(ceil), UNARY(fabs),
        UNARY(floor), UNARY(significand), UNARY(j0), UNARY(j1), UNARY(y0), UNARY(y1), UNARY(erf), UNARY(erfc),
        UNARY(tgamma), UNARY(rint), UNARY(nextdown), UNARY(nextup), UNARY(round),
        UNARY(trunc), UNARY(roundeven),
#undef UNARY
        // Binary functions:
        {"atan2", "atan2", "func(y:Num, x:Num)->Num"},
        {"pow", "pow", "func(base:Num, power:Num)->Num"},
        {"hypot", "hypot", "func(x:Num, y:Num)->Num"},
        {"copysign", "hypot", "func(magnitude:Num, sign:Num)->Num"},
        {"nextafter", "nextafter", "func(x:Num, toward:Num)->Num"},
        {"remainder", "remainder", "func(x:Num, divisor:Num)->Num"},
        {"fmaxmag", "max_magnitude", "func(x:Num, y:Num)->Num"},
        {"fminmag", "min_magnitude", "func(x:Num, y:Num)->Num"},
        {"fdim", "distance", "func(x:Num, y:Num)->Num"},
        // Predicates
        {"isinf", "isinf", "func(n:Num)->Bool"},
        {"finite", "finite", "func(n:Num)->Bool"},
        {"isnan", "isnan", "func(n:Num)->Bool"},
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
        {"Num32__format", "format", "func(n:Num32, precision:Int)->Str"},
        {"Num32__random", "random", "func()->Num32"},
        // Unary functions:
#define UNARY(fn) {#fn"f", #fn, "func(n:Num32)->Num32"}
        UNARY(acos), UNARY(asin), UNARY(atan), UNARY(cos), UNARY(sin), UNARY(tan), UNARY(cosh), UNARY(sinh), UNARY(tanh),
        UNARY(acosh), UNARY(asinh), UNARY(atanh), UNARY(exp), UNARY(log), UNARY(log10), UNARY(exp10), UNARY(expm1),
        UNARY(log1p), UNARY(logb), UNARY(exp2), UNARY(log2), UNARY(sqrt), UNARY(cbrt), UNARY(ceil), UNARY(fabs),
        UNARY(floor), UNARY(significand), UNARY(j0), UNARY(j1), UNARY(y0), UNARY(y1), UNARY(erf), UNARY(erfc),
        UNARY(tgamma), UNARY(rint), UNARY(nextdown), UNARY(nextup), UNARY(round),
        UNARY(trunc), UNARY(roundeven),
#undef UNARY
        // Binary functions:
        {"atan2f", "atan2", "func(y:Num32, x:Num32)->Num32"},
        {"powf", "pow", "func(base:Num32, power:Num32)->Num32"},
        {"hypotf", "hypot", "func(x:Num32, y:Num32)->Num32"},
        {"copysignf", "hypot", "func(magnitude:Num32, sign:Num32)->Num32"},
        {"nextafterf", "nextafter", "func(x:Num32, toward:Num32)->Num32"},
        {"remainderf", "remainder", "func(x:Num32, divisor:Num32)->Num32"},
        {"fmaxmagf", "max_magnitude", "func(x:Num32, y:Num32)->Num32"},
        {"fminmagf", "min_magnitude", "func(x:Num32, y:Num32)->Num32"},
        {"fdimf", "distance", "func(x:Num32, y:Num32)->Num32"},
        // Predicates
        {"Num32__isinf", "isinf", "func(n:Num32)->Bool"},
        {"Num32__finite", "finite", "func(n:Num32)->Bool"},
        {"Num32__isnan", "isnan", "func(n:Num32)->Bool"},
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
        {"Char__toupper", "toupper", "func(c:Char)->Char"},
        {"Char__tolower", "tolower", "func(c:Char)->Char"},
#define CHARP(name) {"Char__"#name, #name, "func(c:Char)->Bool"}
        CHARP(isalnum), CHARP(isalpha), CHARP(iscntrl), CHARP(isdigit), CHARP(isgraph), CHARP(islower),
        CHARP(isprint), CHARP(ispunct), CHARP(isspace), CHARP(isupper), CHARP(isxdigit), CHARP(isascii), CHARP(isblank),
#undef CHARP
        {NULL, NULL, NULL},
    }},

    {sss_str_t, "Str", "Str_type", (builtin_binding_t[]){
        {"Str__uppercased", "uppercased", "func(s:Str)->Str"},
        {"Str__lowercased", "lowercased", "func(s:Str)->Str"},
        {"Str__capitalized", "capitalized", "func(s:Str)->Str"},
        {"Str__titlecased", "titlecased", "func(s:Str)->Str"},
        {"Str__starts_with", "starts_with", "func(s:Str, prefix:Str)->Bool"},
        {"Str__ends_with", "ends_with", "func(s:Str, suffix:Str)->Bool"},
        {"Str__without_prefix", "without_prefix", "func(s:Str, prefix:Str)->Str"},
        {"Str__without_suffix", "without_suffix", "func(s:Str, suffix:Str)->Str"},
        {"Str__trimmed", "trimmed", "func(s:Str, trim_chars=[\\x20, \\n, \\r, \\t, \\v], trim_left=yes, trim_right=yes)->Str"},
        {"Array__slice", "slice", "func(s:&Str, range:Range, readonly=no, _type=typeof(s))->Str"},
        {"Str__c_string", "c_string", "func(s:Str)->CString"},
        {"Str__from_c_string", "from_c_string", "func(c:CString)->Str"},
        {"Str__find", "find", "func(s:Str, target:Str)->Str"},
        {"Str__replace", "replace", "func(s:Str, target:Str, replacement:Str, limit=Int.max)->Str"},
        {"Str__quoted", "quoted", "func(s:Str)->Str"},
        {"Str__split", "split", "func(s:Str, split_chars=[\\x2C, \\x20, \\n, \\r, \\t, \\v])->[Str]"},
        {"Str__join", "join", "func(glue:Str, pieces:[Str])->Str"},

        {"Str__equal", "equals", "func(x:&(read-only)Str, y:&(read-only)Str, _type=typeof(x[]))->Bool"},
        {"Str__compare", "compare", "func(x:&(read-only)Str, y:&(read-only)Str, _type=typeof(x[]))->Int32"},
        {"Str__hash", "hash", "func(s:&(read-only)Str, _type=typeof(s[]))->UInt32"},
        {"Str__cord", "cord", "func(s:&(read-only)Str, colorize:Bool, _type=typeof(s[]))->Cord"},
        {NULL, NULL, NULL},
    }},

    {sss_c_str_t, "CString", "CString_type", (builtin_binding_t[]){
        {"Str__c_string", "from_str", "func(s:Str)->CString"},
        {"Str__from_c_string", "as_str", "func(c:CString)->Str"},
        {NULL, NULL, NULL},
    }},

    {sss_cord_t, "Cord", "Cord_type", (builtin_binding_t[]){
        {NULL, NULL, NULL},
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
        .file_bindings = new(table_t, .fallback=&global->bindings),
        .bindings = new(table_t),
        .tail_calls = tail_calls,
    );
    env->bindings->fallback = env->file_bindings;

    gcc_type_t *type_gcc_type = get_type_gcc_type(env);

    // Declare types first:
    for (size_t i = 0; i < sizeof(builtin_types)/sizeof(builtin_types[0]); i++) {
        sss_type_t *t = &builtin_types[i].sss_type;
        gcc_lvalue_t *type_lval = gcc_global(ctx, NULL, GCC_GLOBAL_IMPORTED, type_gcc_type, builtin_types[i].type_symbol);
        binding_t *b = new(binding_t,
                           .type=Type(TypeType, t),
                           .lval=type_lval,
                           .rval=gcc_rval(type_lval));
        Table_str_set(&env->global->bindings, builtin_types[i].type_name, b);
        Table_str_set(&env->global->type_lvals, type_to_string_concise(t), type_lval);
        mark_type_lvalue_initialized(env, t);
    }

    // Then declare type methods:
    for (size_t i = 0; i < sizeof(builtin_types)/sizeof(builtin_types[0]); i++) {
        sss_type_t *t = &builtin_types[i].sss_type;
        for (int j = 0; builtin_types[i].bindings && builtin_types[i].bindings[j].symbol; j++) {
            auto member = builtin_types[i].bindings[j];
            ast_t *type_ast = parse_type_str(member.type);
            if (!type_ast)
                compiler_err(env, NULL, "Couldn't parse builtin type for %s.%s: `%s`", builtin_types[i].type_name, member.sss_name, member.type);
            sss_type_t *member_type = parse_type_ast(env, type_ast);
            if (member_type->tag == FunctionType) {
                gcc_func_t *func = import_function(env, member.symbol, member_type, false);
                set_in_namespace(env, t, member.sss_name, new(binding_t, .type=member_type, .func=func, .rval=gcc_get_func_address(func, NULL)));
                Table_str_set(&env->global->funcs, member.symbol, func);
            } else {
                gcc_type_t *member_gcc_type = sss_type_to_gcc(env, member_type);
                gcc_lvalue_t *lval = gcc_global(ctx, NULL, GCC_GLOBAL_IMPORTED, member_gcc_type, member.symbol);
                set_in_namespace(env, t, member.sss_name, new(binding_t, .type=member_type, .rval=gcc_rval(lval)));
            }
        }
    }

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
    for (uint32_t i = 1; i <= Table_length(ns); i++) {
        struct { const char *key; binding_t *value; } *entry = Table_entry(ns, i);
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

sss_type_t *get_type_by_name(env_t *env, const char *name)
{
    binding_t *b = get_binding(env, name);
    return b && b->type->tag == TypeType ? Match(b->type, TypeType)->type : NULL;
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
        ns = new(table_t, .fallback=env->file_bindings);
        Table_str_set(&env->global->type_namespaces, type_to_string(t), ns);

#define REF(x) Type(PointerType, .is_stack=true, .pointed=x)
#define RO_REF(x) Type(PointerType, .is_stack=true, .is_readonly=true, .pointed=x)
#define OPT(x) Type(PointerType, .is_optional=true, .pointed=x)
#define FN(names, types, defaults, ret_type) Type(FunctionType, .arg_names=names, .arg_types=types, .arg_defaults=defaults, .ret=ret_type)
#define TYPEOF_DEREF(var) FakeAST(TypeOf, FakeAST(Index, FakeAST(Var, var)))
#define NAMES(...) ARRAY((const char*)__VA_ARGS__)
#define TYPES(...) ARRAY((sss_type_t*)__VA_ARGS__)
#define DEFTS(...) ARRAY((ast_t*)__VA_ARGS__)
        sss_type_t *void_t = Type(VoidType);
        sss_type_t *type_t = RO_REF(Type(TypeType, t));
        if (t->tag == TableType) {
            sss_type_t *key_t = Match(t, TableType)->key_type;
            sss_type_t *value_t = Match(t, TableType)->value_type;
            _load_method(env, ns, "remove", "Table_remove",
                         FN(NAMES("t", "key", "_type"),
                            TYPES(REF(t), RO_REF(key_t), type_t),
                            DEFTS(NULL, NULL, TYPEOF_DEREF("t")),
                            void_t));
            _load_method(env, ns, "set", "Table_set",
                         FN(NAMES("t", "key", "value", "_type"),
                            TYPES(REF(t), RO_REF(key_t), RO_REF(value_t), type_t),
                            DEFTS(NULL, NULL, NULL, TYPEOF_DEREF("t")),
                            void_t));
            _load_method(env, ns, "reserve", "Table_reserve",
                         FN(NAMES("t", "key", "value", "_type"),
                            TYPES(REF(t), RO_REF(key_t), OPT(value_t), type_t),
                            DEFTS(NULL, NULL, NULL, TYPEOF_DEREF("t")),
                            Type(PointerType, value_t)));
            _load_method(env, ns, "get", "Table_get",
                         FN(NAMES("t", "key", "_type"),
                            TYPES(RO_REF(t), RO_REF(key_t), type_t),
                            DEFTS(NULL, NULL, TYPEOF_DEREF("t")),
                            OPT(value_t)));
            _load_method(env, ns, "get_raw", "Table_get_raw",
                         FN(NAMES("t", "key", "_type"),
                            TYPES(RO_REF(t), RO_REF(key_t), type_t),
                            DEFTS(NULL, NULL, TYPEOF_DEREF("t")),
                            OPT(value_t)));
            _load_method(env, ns, "clear", "Table_get_clear",
                         FN(NAMES("t"), TYPES(REF(t)), DEFTS(NULL), void_t));
            _load_method(env, ns, "mark_copy_on_write", "Table_mark_copy_on_write",
                         FN(NAMES("t"), TYPES(REF(t)), DEFTS(NULL), void_t));
        } else if (t->tag == ArrayType) {
            sss_type_t *item_t = Match(t, ArrayType)->item_type;
            sss_type_t *i64 = Type(IntType, .bits=64);
            sss_type_t *size = Type(IntType, .is_unsigned=true, .bits=64);
            ast_t *item_size = FakeAST(SizeOf, FakeAST(Index, .indexed=FakeAST(Var, "array"), .index=FakeAST(Int, .precision=64, .i=1)));
            _load_method(env, ns, "insert", "Array_insert",
                         FN(NAMES("array", "item", "index", "_item_size"),
                            TYPES(REF(t), RO_REF(item_t), i64, size),
                            DEFTS(NULL, NULL, FakeAST(Int, .precision=64, .i=0), item_size),
                            void_t));
            _load_method(env, ns, "insert_all", "Array_insert_all",
                         FN(NAMES("array", "to_insert", "index", "_item_size"),
                            TYPES(REF(t), t, i64, size),
                            DEFTS(NULL, NULL, FakeAST(Int, .precision=64, .i=0), item_size),
                            void_t));
            _load_method(env, ns, "remove", "Array_remove",
                         FN(NAMES("array", "index", "count", "_item_size"),
                            TYPES(REF(t), i64, i64, size),
                            DEFTS(NULL, FakeAST(Int, .precision=64, .i=-1), FakeAST(Int, .precision=64, .i=1), item_size),
                            void_t));
            _load_method(env, ns, "contains", "Array_contains",
                         FN(NAMES("array", "item", "_type"),
                            TYPES(RO_REF(t), RO_REF(item_t), type_t),
                            DEFTS(NULL, NULL, TYPEOF_DEREF("array")),
                            Type(BoolType)));
            _load_method(env, ns, "sort", "Array_sort",
                         FN(NAMES("array", "_type"),
                            TYPES(REF(t), type_t),
                            DEFTS(NULL, TYPEOF_DEREF("array")),
                            void_t));
            _load_method(env, ns, "shuffle", "Array_shuffle",
                         FN(NAMES("array", "_item_size"),
                            TYPES(REF(t), size),
                            DEFTS(NULL, item_size),
                            void_t));
            _load_method(env, ns, "clear", "Array_clear",
                         FN(NAMES("array"),
                            TYPES(REF(t)),
                            DEFTS(NULL),
                            void_t));
            _load_method(env, ns, "slice", "Array_slice",
                         FN(NAMES("array", "range", "readonly", "_type"),
                            TYPES(REF(t), Type(RangeType), Type(BoolType), type_t),
                            DEFTS(NULL, NULL, FakeAST(Bool, false), TYPEOF_DEREF("array")),
                            t));
        }
#undef REF
#undef RO_REF
#undef OPT
#undef FN
#undef TYPEOF_DEREF
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
