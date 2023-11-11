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

static sss_type_t *define_string_type(env_t *env, sss_type_t *str_type)
{
    const char *name = type_to_string(str_type);
    gcc_rvalue_t *rval = gcc_str(env->ctx, name);
    binding_t *binding = new(binding_t, .rval=rval, .type=Type(TypeType, .type=str_type));
    Table_str_set(&env->global->bindings, name, binding);

    table_t *ns = get_namespace(env, str_type);
    load_method(env, ns, "sss_string_uppercased", "uppercased", str_type, ARG("str",str_type,0));
    load_method(env, ns, "sss_string_lowercased", "lowercased", str_type, ARG("str",str_type,0));
    load_method(env, ns, "sss_string_capitalized", "capitalized", str_type, ARG("str",str_type,0));
    load_method(env, ns, "sss_string_titlecased", "titlecased", str_type, ARG("str",str_type,0));
    load_method(env, ns, "sss_string_quoted", "quoted", str_type,
                ARG("str",str_type,0),
                ARG("dsl", Type(PointerType, .pointed=Type(CStringCharType), .is_optional=true), FakeAST(Nil, .type=FakeAST(Var, .name="CStringChar"))),
                ARG("colorize", Type(BoolType), FakeAST(Bool, .b=false)));
    load_method(env, ns, "sss_string_starts_with", "starts_with", Type(BoolType), ARG("str",str_type,0), ARG("prefix",str_type,0));
    load_method(env, ns, "sss_string_ends_with", "ends_with", Type(BoolType), ARG("str",str_type,0), ARG("suffix",str_type,0));
    load_method(env, ns, "sss_string_without_prefix", "without_prefix", str_type, ARG("str",str_type,0), ARG("prefix",str_type,0));
    load_method(env, ns, "sss_string_without_suffix", "without_suffix", str_type, ARG("str",str_type,0), ARG("suffix",str_type,0));
    load_method(env, ns, "sss_string_trimmed", "trimmed", str_type,
                ARG("str",str_type,0),
                ARG("chars",str_type,FakeAST(StringJoin, .children=ARRAY(FakeAST(StringLiteral, .str=" \t\r\n")))),
                ARG("trim_left",Type(BoolType),FakeAST(Bool,.b=true)),
                ARG("trim_right",Type(BoolType),FakeAST(Bool,.b=true)));
    load_method(env, ns, "sss_string_replace", "replace", str_type,
                ARG("str",str_type,0), ARG("pattern",str_type,0), ARG("replacement",str_type,0), ARG("limit",INT_TYPE,FakeAST(Int,.i=-1,.precision=64)));
    load_method(env, ns, "sss_string_split", "split", Type(ArrayType, .item_type=str_type),
                ARG("str",str_type,0),
                ARG("separators", str_type, FakeAST(StringJoin, .children=ARRAY(FakeAST(StringLiteral, .str=" \t\r\n")))));

    // load_method(env, ns, "sss_string_find", "find",
    //             define_tagged_union(env, 8, "FindResult", ARRAY(
    //                 (sss_tagged_union_member_t){"Failure", 0, NULL}, {"Success", 1, Type(IntType, .bits=32)})),
    //             ARG("str",str_type,0),
    //             ARG("pattern",str_type,0));

    return str_type;
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

    sss_type_t *str_t = Type(ArrayType, .item_type=Type(CharType));
    table_t *str_ns = get_namespace(env, str_t);

    { // Define a type representing error codes from C
        auto error_members = EMPTY_ARRAY(sss_tagged_union_member_t);
        sss_tagged_union_member_t item = {.name="SUCCESS", .tag_value=0};
        append(error_members, item);
        item.name = "FAILURE";
        item.tag_value = -1;
        append(error_members, item);
        // These values are sadly platform-dependent, so they can't be hard-coded.
        for (int64_t i = 1; i < 256; i++) {
            item.name = strerrorname_np((int)i);
            item.tag_value = i;
            if (item.name)
                append(error_members, item);
        }
        sss_type_t *c_err = define_tagged_union(env, 32, "CError", error_members);
        Table_str_set(&env->global->bindings, "CError", new(binding_t, .rval=gcc_str(ctx, "CError"), .type=Type(TypeType, .type=c_err)));

        gcc_type_t *errno_ptr_gcc_t = gcc_get_ptr_type(sss_type_to_gcc(env, c_err));
        gcc_func_t *errno_loc_func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, errno_ptr_gcc_t, "__errno_location", 0, NULL, 0);
        gcc_rvalue_t *errno_ptr = gcc_callx(env->ctx, NULL, errno_loc_func);
        gcc_lvalue_t *errno_lval = gcc_rvalue_dereference(errno_ptr, NULL);
        Table_str_set(&env->global->bindings, "errno", new(binding_t, .rval=gcc_rval(errno_lval), .lval=errno_lval, .type=c_err));
    }

    sss_type_t *c_str = Type(PointerType, .pointed=Type(CStringCharType), .is_optional=true);
    load_method(env, str_ns, "c_string", "c_string", Type(PointerType, .pointed=Type(CStringCharType), .is_optional=false), ARG("str",str_t,0));
    load_method(env, str_ns, "from_c_string", "from_pointer", str_t, ARG("str",c_str,0));
    table_t *c_str_ns = get_namespace(env, c_str);
    assert(c_str_ns == get_namespace(env, c_str));
    load_method(env, c_str_ns, "from_c_string", "as_string", str_t, ARG("str",c_str,0));
    table_t *c_str_ptr_ns = get_namespace(env, Type(PointerType, .pointed=Type(CStringCharType), .is_optional=false));
    load_method(env, c_str_ptr_ns, "from_c_string", "as_string", str_t, ARG("str",c_str,0));

    sss_type_t *say_type = Type(
        FunctionType,
        .arg_names=ARRAY((const char*)"str", "end"),
        .arg_types=ARRAY(str_t, str_t),
        .arg_defaults=ARRAY((ast_t*)NULL, FakeAST(StringLiteral, .str="\n")),
        .ret=Type(VoidType));

    gcc_param_t *gcc_say_params[] = {
        gcc_new_param(ctx, NULL, sss_type_to_gcc(env, str_t), "str"),
        gcc_new_param(ctx, NULL, sss_type_to_gcc(env, str_t), "end"),
    };
    gcc_func_t *say_func = gcc_new_func(ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, VOID), "say", 2, gcc_say_params, 0);
    gcc_rvalue_t *say_rvalue = gcc_get_func_address(say_func, NULL);
    Table_str_set(&env->global->bindings, "say", new(binding_t, .func=say_func, .rval=say_rvalue, .type=say_type));
    sss_type_t *warn_type = Type(
        FunctionType,
        .arg_names=ARRAY((const char*)"str", "end", "colorize"),
        .arg_types=ARRAY(str_t, str_t, Type(BoolType)),
        .arg_defaults=ARRAY((ast_t*)NULL, FakeAST(StringLiteral, .str="\n"), FakeAST(Var, "USE_COLOR")),
        .ret=Type(VoidType));
    gcc_param_t *gcc_warn_params[] = {
        gcc_new_param(ctx, NULL, sss_type_to_gcc(env, str_t), "str"),
        gcc_new_param(ctx, NULL, sss_type_to_gcc(env, str_t), "end"),
        gcc_new_param(ctx, NULL, gcc_type(env->ctx, BOOL), "colorize"),
    };
    gcc_func_t *warn_func = gcc_new_func(ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, VOID), "warn", 3, gcc_warn_params, 0);
    Table_str_set(&env->global->bindings, "warn", new(binding_t, .func=warn_func, .rval=gcc_get_func_address(warn_func, NULL), .type=warn_type));
#define DEFTYPE(t) Table_str_set(&env->global->bindings, #t, new(binding_t, .rval=gcc_str(ctx, #t), .type=Type(TypeType, .type=Type(t##Type))));
    // Primitive types:
    DEFTYPE(Bool); DEFTYPE(Void); DEFTYPE(Abort); DEFTYPE(Memory);
    DEFTYPE(Char); DEFTYPE(CStringChar);
#undef DEFTYPE

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
        if (type_eq(base_t, Type(ArrayType, .item_type=Type(CharType))))
            (void)define_string_type(env, t);
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
