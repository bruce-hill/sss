// Compilation logic for Tables in this file
#include <assert.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>

#include "../ast.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

typedef struct {
    sss_type_t *table_type;
    gcc_rvalue_t *table_ptr;
} table_insert_info_t;

gcc_rvalue_t *table_entry_value_offset(env_t *env, sss_type_t *t)
{
    size_t key_size = gcc_sizeof(env, Match(t, TableType)->key_type);
    size_t value_align = gcc_alignof(env, Match(t, TableType)->value_type);
    size_t value_offset = key_size;
    if (value_align > 0 && value_offset % value_align != 0) value_offset = (value_offset - (value_offset % value_align) + value_align);
    return gcc_rvalue_size(env->ctx, value_offset);
}

void mark_table_cow(env_t *env, gcc_block_t **block, gcc_rvalue_t *table_ptr)
{
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, VOID_PTR), "t"),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED,
                                    gcc_type(env->ctx, VOID), "Table_mark_copy_on_write", 1, params, 0);
    gcc_eval(*block, NULL, gcc_callx(env->ctx, NULL, func, table_ptr));
}

gcc_lvalue_t *table_lvalue(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *table, ast_t *key_ast, bool autocreate)
{
    gcc_func_t *func = gcc_block_func(*block);
    sss_type_t *needed_key_t = Match(t, TableType)->key_type;
    gcc_type_t *needed_key_gcc_t = sss_type_to_gcc(env, needed_key_t);
    gcc_type_t *value_gcc_t = sss_type_to_gcc(env, Match(t, TableType)->value_type);

    gcc_rvalue_t *key_val = compile_expr(env, block, key_ast);
    if (!block) return NULL;
    if (!promote(env, get_type(env, key_ast), &key_val, needed_key_t))
        compiler_err(env, key_ast, "This key has type %T, but to work in this table, it needs type %T", get_type(env, key_ast), needed_key_t);

    gcc_lvalue_t *key_lval = gcc_local(func, NULL, needed_key_gcc_t, "_key");
    gcc_assign(*block, NULL, key_lval, key_val);

    gcc_lvalue_t *dest = gcc_local(func, NULL, gcc_get_ptr_type(value_gcc_t), "_dest");
    gcc_rvalue_t *type_ptr = get_type_rvalue(env, t);
    if (autocreate) {
        gcc_param_t *params[] = {
            gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, VOID_PTR), "type"),
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(sss_type_to_gcc(env, t)), "t"),
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(needed_key_gcc_t), "key"),
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(value_gcc_t), "value"),
        };
        gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED,
                                        gcc_get_ptr_type(value_gcc_t), "Table_set", sizeof(params)/sizeof(params[0]), params, 0);
        gcc_assign(*block, NULL, dest,
                   gcc_callx(env->ctx, NULL, func, type_ptr, table, gcc_lvalue_address(key_lval, NULL),
                             gcc_null(env->ctx, gcc_get_ptr_type(value_gcc_t))));
        return gcc_rvalue_dereference(gcc_rval(dest), NULL);
    } else {
        gcc_param_t *params[] = {
            gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, VOID_PTR), "type"),
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(sss_type_to_gcc(env, t)), "t"),
            gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(needed_key_gcc_t), "key"),
        };
        gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED,
                                        gcc_get_ptr_type(value_gcc_t), "Table_get_raw", sizeof(params)/sizeof(params[0]), params, 0);
        gcc_assign(*block, NULL, dest,
                   gcc_callx(env->ctx, NULL, func, type_ptr, table, gcc_lvalue_address(key_lval, NULL)));
        gcc_block_t *if_missing = gcc_new_block(func, fresh("if_missing")),
                    *done = gcc_new_block(func, fresh("done"));
        gcc_jump_condition(*block, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, gcc_rval(dest), gcc_null(env->ctx, gcc_get_ptr_type(value_gcc_t))),
                           if_missing, done);
        insert_failure(env, &if_missing, key_ast->file, key_ast->start, key_ast->end,
                       "Error: this table does not have the given key: %#s",
                       needed_key_t, gcc_rval(key_lval));

        *block = done;
        return gcc_rvalue_dereference(gcc_rval(dest), NULL);
    }
}

static binding_t *define_table_remove_method(env_t *env, sss_type_t *t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    sss_type_t *key_t = Match(t, TableType)->key_type;
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_t), fresh("table")),
        gcc_new_param(env->ctx, NULL, sss_type_to_gcc(env, key_t), fresh("key")),
    };
    gcc_rvalue_t *table = gcc_param_as_rvalue(params[0]);
    gcc_rvalue_t *key_val = gcc_param_as_rvalue(params[1]);

    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, gcc_type(env->ctx, VOID), fresh("remove"), 2, params, 0);
    gcc_block_t *block = gcc_new_block(func, fresh("remove"));

    gcc_type_t *key_gcc_t = sss_type_to_gcc(env, Match(t, TableType)->key_type);
    gcc_rvalue_t *key_ptr;
    if (key_val) {
        gcc_lvalue_t *key_lval = gcc_local(func, NULL, key_gcc_t, "_key");
        gcc_assign(block, NULL, key_lval, key_val);
        flatten_arrays(env, &block, Match(t, TableType)->key_type, gcc_lvalue_address(key_lval, NULL));
        key_ptr = gcc_lvalue_address(key_lval, NULL);
    } else {
        key_ptr = gcc_null(env->ctx, gcc_get_ptr_type(key_gcc_t));
    }

    gcc_func_t *hashmap_remove_fn = get_function(env, "sss_hashmap_remove");
    gcc_func_t *key_hash = get_hash_func(env, Match(t, TableType)->key_type);
    gcc_func_t *key_cmp = get_indirect_compare_func(env, Match(t, TableType)->key_type);
    gcc_rvalue_t *call = gcc_callx(
        env->ctx, NULL, hashmap_remove_fn,
        gcc_cast(env->ctx, NULL, table, gcc_type(env->ctx, VOID_PTR)),
        gcc_cast(env->ctx, NULL, gcc_get_func_address(key_hash, NULL), gcc_type(env->ctx, VOID_PTR)),
        gcc_cast(env->ctx, NULL, gcc_get_func_address(key_cmp, NULL), gcc_type(env->ctx, VOID_PTR)),
        gcc_rvalue_size(env->ctx, gcc_sizeof(env, table_entry_type(t))),
        key_ptr);
    gcc_eval(block, NULL, call);

    gcc_return_void(block, NULL);
    binding_t *b = new(binding_t, .func=func,
        .type=Type(FunctionType, .arg_names=ARRAY((const char*)"table", "key"),
                   .arg_types=ARRAY(Type(PointerType, .pointed=t, .is_stack=true), key_t),
                   .arg_defaults=ARRAY((ast_t*)NULL, NULL),
                   .ret=Type(VoidType)));
    set_in_namespace(env, t, "remove", b);
    return b;
}

binding_t *get_table_method(env_t *env, sss_type_t *t, const char *method_name)
{
    for (;;) {
        if (t->tag == PointerType) t = Match(t, PointerType)->pointed; 
        else if (t->tag == VariantType) t = Match(t, VariantType)->variant_of; 
        else break;
    }

    binding_t *b = get_from_namespace(env, t, method_name);
    if (b) return b;

    if (streq(method_name, "remove")) {
        return define_table_remove_method(env, t);
    } else {
        return NULL;
    }
}

static void add_table_entry(env_t *env, gcc_block_t **block, ast_t *entry, table_insert_info_t *info)
{
    if (entry->tag != TableEntry) {
        gcc_rvalue_t *val = compile_expr(env, block, entry);
        if (!block) return;
        if (val) gcc_eval(*block, NULL, val);
        return;
    }

    ast_t *key_ast = Match(entry, TableEntry)->key,
          *value_ast = Match(entry, TableEntry)->value;
    sss_type_t *raw_key_t = get_type(env, key_ast),
              *raw_value_t = get_type(env, value_ast);
    sss_type_t *needed_key_t = Match(info->table_type, TableType)->key_type,
              *needed_value_t = Match(info->table_type, TableType)->value_type;

    gcc_rvalue_t *key_val = compile_expr(env, block, key_ast);
    if (!*block) return;
    if (!promote(env, raw_key_t, &key_val, needed_key_t))
        compiler_err(env, key_ast, "This key was expected to be a %T, but was actually %T", needed_key_t, raw_key_t);

    gcc_rvalue_t *value_val = compile_expr(env, block, value_ast);
    if (!*block) return;
    if (!promote(env, raw_value_t, &value_val, needed_value_t))
        compiler_err(env, value_ast, "This value was expected to be a %T, but was actually %T", needed_value_t, raw_value_t);

    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *key_lval = gcc_local(func, NULL, sss_type_to_gcc(env, needed_key_t), "_key"),
                 *value_lval = gcc_local(func, NULL, sss_type_to_gcc(env, needed_value_t), "_value");
    gcc_assign(*block, NULL, key_lval, key_val);
    gcc_assign(*block, NULL, value_lval, value_val);

    gcc_func_t *hashmap_set_fn = get_function(env, "sss_hashmap_set");
    gcc_func_t *key_hash = get_hash_func(env, Match(info->table_type, TableType)->key_type);
    gcc_func_t *key_cmp = get_indirect_compare_func(env, Match(info->table_type, TableType)->key_type);
    gcc_eval(*block, NULL, gcc_callx(env->ctx, NULL, hashmap_set_fn,
                                     gcc_cast(env->ctx, NULL, info->table_ptr, gcc_type(env->ctx, VOID_PTR)),
                                     gcc_cast(env->ctx, NULL, gcc_get_func_address(key_hash, NULL), gcc_type(env->ctx, VOID_PTR)),
                                     gcc_cast(env->ctx, NULL, gcc_get_func_address(key_cmp, NULL), gcc_type(env->ctx, VOID_PTR)),
                                     gcc_rvalue_size(env->ctx, gcc_sizeof(env, table_entry_type(info->table_type))),
                                     gcc_lvalue_address(key_lval, NULL),
                                     table_entry_value_offset(env, info->table_type),
                                     gcc_lvalue_address(value_lval, NULL)));
}

// Returns an optional pointer to a value
gcc_rvalue_t *table_lookup_optional(env_t *env, gcc_block_t **block, ast_t *table_ast, ast_t *key_ast, gcc_rvalue_t **key_rval_out, bool raw)
{
    gcc_loc_t *loc = ast_loc(env, key_ast);
    sss_type_t *table_t = get_type(env, table_ast);
    gcc_rvalue_t *table = compile_expr(env, block, table_ast);
    while (table_t->tag == PointerType) {
        auto ptr = Match(table_t, PointerType);
        if (ptr->is_optional)
            compiler_err(env, table_ast, "This is an optional pointer, which can't be safely dereferenced.");

        table = gcc_rval(gcc_rvalue_dereference(table, loc));
        table_t = ptr->pointed;
    }
    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *table_var = gcc_local(func, loc, sss_type_to_gcc(env, table_t), "_table");
    gcc_assign(*block, loc, table_var, table);

    sss_type_t *key_t = Match(table_t, TableType)->key_type;
    sss_type_t *value_t = Match(table_t, TableType)->value_type;

    gcc_func_t *hashmap_get_fn = get_function(env, raw ? "sss_hashmap_get_raw" : "sss_hashmap_get");
    gcc_func_t *key_hash = get_hash_func(env, key_t);
    gcc_func_t *key_cmp = get_indirect_compare_func(env, key_t);

    sss_type_t *raw_key_t = get_type(env, key_ast);
    gcc_rvalue_t *key_val = compile_expr(env, block, key_ast);
    if (!promote(env, raw_key_t, &key_val, key_t))
        compiler_err(env, key_ast, "This key is a %T, but this table needs a key of type %T", raw_key_t, key_t);
    gcc_lvalue_t *key_lval = gcc_local(func, loc, sss_type_to_gcc(env, key_t), "_key");
    gcc_assign(*block, loc, key_lval, key_val);
    if (key_rval_out) *key_rval_out = gcc_rval(key_lval);
    flatten_arrays(env, block, key_t, gcc_lvalue_address(key_lval, loc));
    gcc_rvalue_t *val_ptr = gcc_callx(
        env->ctx, loc, hashmap_get_fn,
        gcc_cast(env->ctx, loc, gcc_lvalue_address(table_var, loc), gcc_type(env->ctx, VOID_PTR)),
        gcc_cast(env->ctx, loc, gcc_get_func_address(key_hash, loc), gcc_type(env->ctx, VOID_PTR)),
        gcc_cast(env->ctx, loc, gcc_get_func_address(key_cmp, loc), gcc_type(env->ctx, VOID_PTR)),
        gcc_rvalue_size(env->ctx, gcc_sizeof(env, table_entry_type(table_t))),
        gcc_lvalue_address(key_lval, loc),
        table_entry_value_offset(env, table_t));
    gcc_type_t *val_ptr_gcc_t = gcc_get_ptr_type(sss_type_to_gcc(env, value_t));
    val_ptr = gcc_cast(env->ctx, loc, val_ptr, val_ptr_gcc_t);

    gcc_lvalue_t *value_lval = gcc_local(func, loc, val_ptr_gcc_t, "_value");
    gcc_assign(*block, loc, value_lval, val_ptr);
    return gcc_rval(value_lval);
}

gcc_rvalue_t *compile_table(env_t *env, gcc_block_t **block, ast_t *ast, bool mark_cow)
{
    auto table = Match(ast, Table);
    sss_type_t *t = get_type(env, ast);
    if (Match(t, TableType)->key_type->tag == VoidType)
        compiler_err(env, ast, "Tables can't be defined with a Void key type");
    else if (Match(t, TableType)->value_type->tag == VoidType)
        compiler_err(env, ast, "Tables can't be defined with a Void value type");
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_func_t *func = gcc_block_func(*block);

    gcc_loc_t *loc = ast_loc(env, ast);
    gcc_lvalue_t *table_var = gcc_local(func, loc, gcc_t, "_table");

    gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
    gcc_assign(*block, loc, table_var,
        gcc_struct_constructor(env->ctx, loc, gcc_t, 1,
            (gcc_field_t*[]){gcc_get_field(gcc_struct, TABLE_COW_FIELD)},
            (gcc_rvalue_t*[]){gcc_rvalue_bool(env->ctx, mark_cow ? 1 : 0)}));

    env_t env2 = *env;
    env2.comprehension_callback = (void*)add_table_entry;
    table_insert_info_t info = {t, gcc_lvalue_address(table_var, loc)};
    env2.comprehension_userdata = &info;
    env = &env2;

    if (table->entries) {
        gcc_block_t *table_done = gcc_new_block(func, fresh("table_done"));
        foreach (table->entries, entry_ast, _) {
            gcc_block_t *entry_done = gcc_new_block(func, fresh("entry_done"));
            env2.loop_label = &(loop_label_t){
                .enclosing = env->loop_label,
                .names = ARRAY((const char*)"[]"),
                .skip_label = entry_done,
                .stop_label = table_done,
            };

            add_table_entry(env, block, *entry_ast, &info);

            if (*block)
                gcc_jump(*block, loc, entry_done);
            *block = entry_done;
        }
        if (*block)
            gcc_jump(*block, loc, table_done);
        *block = table_done;
    }

    if (table->fallback) {
        ast_t *fallback = table->fallback;
        sss_type_t *fallback_t = get_type(env, fallback);
        if (fallback_t->tag == PointerType) {
            compiler_err(env, fallback, "Fallback tables are not allowed to be pointers to mutable tables, only table values are allowed. \n"
                        "Use '[]' to dereference this value if you want to use it as a fallback.");
        } else {
            fallback = WrapAST(fallback, HeapAllocate, .value=fallback);
            fallback_t = get_type(env, fallback);
        }
        if (!type_eq(Match(fallback_t, PointerType)->pointed, t))
            compiler_err(env, fallback, "This fallback has type %T, which doesn't match the table's type: %T", fallback_t, t);

        gcc_struct_t *table_struct = gcc_type_if_struct(gcc_t);
        gcc_assign(*block, loc, gcc_lvalue_access_field(table_var, NULL, gcc_get_field(table_struct, TABLE_FALLBACK_FIELD)),
                   compile_expr(env, block, fallback));
    }

    if (table->default_value) {
        sss_type_t *default_t = get_type(env, table->default_value);
        sss_type_t *value_t = Match(t, TableType)->value_type;
        if (!type_is_a(default_t, value_t))
            compiler_err(env, table->default_value, "This default value has type %T, which doesn't match the table's value type: %T",
                        default_t, value_t);

        gcc_struct_t *table_struct = gcc_type_if_struct(gcc_t);
        gcc_assign(*block, loc, gcc_lvalue_access_field(table_var, NULL, gcc_get_field(table_struct, TABLE_DEFAULT_FIELD)),
                   compile_expr(env, block, WrapAST(table->default_value, HeapAllocate, .value=table->default_value)));
    }

    return gcc_rval(table_var);
}

void compile_table_cord_func(env_t *env, gcc_block_t **block, gcc_rvalue_t *obj, gcc_rvalue_t *rec, gcc_rvalue_t *color, sss_type_t *t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_func_t *cord_cat_fn = get_function(env, "CORD_cat");
    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *cord = gcc_local(func, NULL, gcc_type(env->ctx, STRING), "cord");
    gcc_assign(*block, NULL, cord, gcc_null(env->ctx, gcc_type(env->ctx, STRING)));

#define APPEND_CORD(b, to_append) gcc_assign(b, NULL, cord, gcc_callx(env->ctx, NULL, cord_cat_fn, gcc_rval(cord), to_append))
#define APPEND_LITERAL(b, str) gcc_assign(b, NULL, cord, gcc_callx(env->ctx, NULL, cord_cat_fn, gcc_rval(cord), gcc_str(env->ctx, str)))
#define APPEND_COLOR_LITERAL(block, str) maybe_append_cord(env, block, cord, color, str)

    sss_type_t *entry_t = table_entry_type(t);

    // i = 0
    gcc_lvalue_t *i = gcc_local(func, NULL, gcc_type(env->ctx, INT64), "_i");
    gcc_assign(*block, NULL, i, gcc_zero(env->ctx, gcc_type(env->ctx, INT64)));
    gcc_struct_t *table_struct = gcc_type_if_struct(gcc_t);
    gcc_rvalue_t *entries = gcc_rvalue_access_field(obj, NULL, gcc_get_field(table_struct, TABLE_ENTRIES_FIELD));
    gcc_rvalue_t *len = gcc_rvalue_access_field(obj, NULL, gcc_get_field(table_struct, TABLE_COUNT_FIELD));

    gcc_rvalue_t *len64 = gcc_cast(env->ctx, NULL, len, gcc_type(env->ctx, INT64));
    APPEND_COLOR_LITERAL(block, "\x1b[m");
    APPEND_LITERAL(*block, "{");

    gcc_block_t *add_comma = gcc_new_block(func, fresh("add_comma"));
    gcc_block_t *add_next_entry = gcc_new_block(func, fresh("next_entry"));
    gcc_block_t *done_with_entries = gcc_new_block(func, fresh("done_with_entries"));

    // entry_ptr = table.entries
    gcc_type_t *gcc_entry_t = sss_type_to_gcc(env, entry_t);
    gcc_lvalue_t *entry_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_entry_t), "_entry_ptr");
    gcc_assign(*block, NULL, entry_ptr, gcc_cast(env->ctx, NULL, entries, gcc_get_ptr_type(gcc_entry_t)));

    // if (i < len) goto add_next_entry;
    gcc_jump_condition(*block, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(i), len64),
                  add_next_entry, done_with_entries);

    // add_next_entry:
    // entry = *entry_ptr
    gcc_rvalue_t *entry = gcc_rval(gcc_jit_rvalue_dereference(gcc_rval(entry_ptr), NULL));
    // print(key) print("=>") print(value)
    sss_type_t *key_type = Match(t, TableType)->key_type;
    gcc_func_t *key_cord_fn = get_cord_func(env, key_type);
    gcc_struct_t *entry_struct = gcc_type_if_struct(sss_type_to_gcc(env, entry_t));
    gcc_rvalue_t *key = gcc_rvalue_access_field(entry, NULL, gcc_get_field(entry_struct, 0));
    APPEND_CORD(add_next_entry, gcc_callx(env->ctx, NULL, key_cord_fn, key, rec, color));
    gcc_block_t *rest_of_entry = add_next_entry;
    APPEND_COLOR_LITERAL(&rest_of_entry, "\x1b[0;33m");
    APPEND_LITERAL(rest_of_entry, "=>");
    APPEND_COLOR_LITERAL(&rest_of_entry, "\x1b[m");
    gcc_rvalue_t *value = gcc_rvalue_access_field(entry, NULL, gcc_get_field(entry_struct, 1));
    sss_type_t *value_type = Match(t, TableType)->value_type;
    gcc_func_t *value_cord_fn = get_cord_func(env, value_type);
    APPEND_CORD(rest_of_entry, gcc_callx(env->ctx, NULL, value_cord_fn, value, rec, color));
    
    // i += 1
    gcc_update(rest_of_entry, NULL, i, GCC_BINOP_PLUS, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
    // entry_ptr = &entry_ptr[1]
    gcc_assign(rest_of_entry, NULL, entry_ptr,
               gcc_lvalue_address(gcc_array_access(env->ctx, NULL, gcc_rval(entry_ptr), gcc_one(env->ctx, gcc_type(env->ctx, INT))), NULL));
    // if (i < len) goto add_comma;
    gcc_jump_condition(rest_of_entry, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(i), len64),
                  add_comma, done_with_entries);

    // add_comma:
    APPEND_COLOR_LITERAL(&add_comma, "\x1b[m");
    APPEND_LITERAL(add_comma, ", ");

    // goto add_next_entry;
    gcc_jump(add_comma, NULL, add_next_entry);

    // done_with_entries:
    gcc_block_t *add_fallback = gcc_new_block(func, fresh("add_fallback")),
                *check_default = gcc_new_block(func, fresh("check_default")),
                *add_default = gcc_new_block(func, fresh("add_default")),
                *done_with_metadata = gcc_new_block(func, fresh("done_with_metadata"));
    gcc_rvalue_t *fallback = gcc_rvalue_access_field(obj, NULL, gcc_get_field(table_struct, TABLE_FALLBACK_FIELD));
    gcc_rvalue_t *has_fallback = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, fallback, gcc_null(env->ctx, gcc_get_ptr_type(gcc_t)));
    gcc_jump_condition(done_with_entries, NULL, has_fallback, add_fallback, check_default);

    // Add fallback:
    APPEND_COLOR_LITERAL(&add_fallback, "\x1b[0;33m");
    APPEND_LITERAL(add_fallback, "; ");
    APPEND_COLOR_LITERAL(&add_fallback, "\x1b[32m");
    APPEND_LITERAL(add_fallback, "fallback=");
    APPEND_COLOR_LITERAL(&add_fallback, "\x1b[m");
    APPEND_CORD(add_fallback, gcc_callx(env->ctx, NULL, func, gcc_rval(gcc_rvalue_dereference(fallback, NULL)), rec, color));
    gcc_jump(add_fallback, NULL, check_default);

    // Check default
    sss_type_t *default_t = Match(t, TableType)->value_type;
    gcc_type_t *default_gcc_t = sss_type_to_gcc(env, default_t);
    gcc_rvalue_t *def = gcc_rvalue_access_field(obj, NULL, gcc_get_field(table_struct, TABLE_DEFAULT_FIELD));
    gcc_rvalue_t *has_default = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, def, gcc_null(env->ctx, gcc_get_ptr_type(default_gcc_t)));
    gcc_jump_condition(check_default, NULL, has_default, add_default, done_with_metadata);

    // Add default:
    APPEND_COLOR_LITERAL(&add_default, "\x1b[0;33m");
    APPEND_LITERAL(add_default, "; ");
    APPEND_COLOR_LITERAL(&add_default, "\x1b[32m");
    APPEND_LITERAL(add_default, "default=");
    APPEND_COLOR_LITERAL(&add_default, "\x1b[m");
    gcc_func_t *def_cord_fn = get_cord_func(env, default_t);
    APPEND_CORD(add_default, gcc_callx(env->ctx, NULL, def_cord_fn, gcc_rval(gcc_rvalue_dereference(def, NULL)), rec, color));
    gcc_jump(add_default, NULL, done_with_metadata);

    APPEND_COLOR_LITERAL(&done_with_metadata, "\x1b[m");
    APPEND_LITERAL(done_with_metadata, "}");

    gcc_return(done_with_metadata, NULL, gcc_rval(cord));
#undef APPEND_COLOR_LITERAL 
#undef APPEND_LITERAL 
#undef APPEND_CORD 
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
