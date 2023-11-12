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

struct {const char *symbol, *type; } builtin_functions[] = {
    {"GC_malloc", "func(size:UInt) @Memory"},
    {"GC_malloc_atomic", "func(size:UInt) @Memory"},
    {"GC_realloc", "func(data:?Memory, size:UInt) @Memory"},
    {"memcpy", "func(dest:@Memory, src:@Memory, size:UInt) @Memory"},
    {"getenv", "func(name:CString) CString"},
    // Cord functions:
    {"CORD_cat", "func(x:Cord, y:Cord)"},
    {"CORD_cat_char_star", "func(x:Cord, y:CString, leny:UInt64)"},
    {"CORD_cat_char", "func(x:Cord, c:Char)"},
    {"CORD_cmp", "func(x:Cord, y:Cord) Int32"},
    {"CORD_to_const_char_star", "func(x:Cord) CString"},
    {"CORD_to_char_star", "func(x:Cord) CString"},
    {"builtin_say", "func(str:Str, end=\"\\n\") Void"},
    {"builtin_last_err", "func() Str"},
    {"builtin_doctest", "func(label:CString, expr:Cord, type:CString, use_color:Bool, expected:CString, filename:CString, start:Int32, end:Int32) Void"},
    {"Num_mod", "func(n:Num, divisor:Num) Num"},
    {"Num32_mod", "func(n:Num32, divisor:Num32) Num32"},
    {"dlopen", "func(filename:CString, flags:Int32) ?Memory"},
    {"dlsym", "func(handle:@Memory, symbol:CString) ?Memory"},
    {"dlclose", "func(handle:@Memory) ?Memory"},
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

static inline gcc_func_t *load_func(env_t *env, const char *name, sss_type_t *t)
{
    auto params = EMPTY_ARRAY(gcc_param_t*);
    auto fn = Match(t, FunctionType);
    for (size_t i = 0; i < LENGTH(fn->arg_types); i++) {
        gcc_type_t *arg_t = sss_type_to_gcc(env, ith(fn->arg_types, i));
        const char *arg_name = ith(fn->arg_names, i);
        append(params, gcc_new_param(env->ctx, NULL, arg_t, arg_name));
    }
    gcc_type_t *t_ret = sss_type_to_gcc(env, fn->ret);
    return gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, t_ret, name, LENGTH(params), params[0], 0);
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
            if (member_type->tag == FunctionType) {
                gcc_func_t *func = load_func(env, member.symbol, member_type);
                set_in_namespace(env, t, member.sss_name, new(binding_t, .type=member_type, .func=func, .rval=gcc_get_func_address(func, NULL)));
            } else {
                gcc_type_t *member_gcc_type = sss_type_to_gcc(env, member_type);
                gcc_lvalue_t *lval = gcc_global(ctx, NULL, GCC_GLOBAL_IMPORTED, member_gcc_type, member.symbol);
                set_in_namespace(env, t, member.sss_name, new(binding_t, .type=member_type, .rval=gcc_rval(lval)));
            }
        }
    }

    for (size_t i = 0; i < sizeof(builtin_functions)/sizeof(builtin_functions[0]); i++) {
        auto fn = builtin_functions[i];
        ast_t *type_ast = parse_type(sss_spoof_file("<builtins>", fn.type), NULL);
        sss_type_t *fn_type = parse_type_ast(env, type_ast);
        gcc_func_t *func = load_func(env, fn.symbol, fn_type);
        Table_str_set(&env->global->funcs, fn.symbol, func);
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
