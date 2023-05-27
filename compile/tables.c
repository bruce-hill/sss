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
    sss_type_t *table_t = Type(TableType, .key_type=Type(BoolType), .value_type=Type(BoolType));
    gcc_type_t *table_gcc_t = sss_type_to_gcc(env, table_t);
    gcc_struct_t *table_struct = gcc_type_if_struct(table_gcc_t);
    gcc_lvalue_t *cow = gcc_rvalue_dereference_field(
        gcc_cast(env->ctx, NULL, table_ptr, gcc_get_ptr_type(table_gcc_t)), NULL,
        gcc_get_field(table_struct, TABLE_COW_FIELD));
    gcc_assign(*block, NULL, cow, gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, BOOL), 1));
}

gcc_lvalue_t *table_lvalue(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *table, ast_t *key_ast)
{
    gcc_func_t *func = gcc_block_func(*block);
    sss_type_t *needed_key_t = Match(t, TableType)->key_type;
    gcc_type_t *needed_key_gcc_t = sss_type_to_gcc(env, needed_key_t);

    gcc_rvalue_t *key_val = compile_expr(env, block, key_ast);
    if (!block) return NULL;
    if (!promote(env, get_type(env, key_ast), &key_val, needed_key_t))
        compiler_err(env, key_ast, "This key has type %s, but to work in this table, it needs type %s",
                    type_to_string(get_type(env, key_ast)), type_to_string(needed_key_t));

    gcc_lvalue_t *key_lval = gcc_local(func, NULL, needed_key_gcc_t, "_key");
    gcc_assign(*block, NULL, key_lval, key_val);
    flatten_arrays(env, block, needed_key_t, gcc_lvalue_address(key_lval, NULL));

    gcc_func_t *hashmap_set_fn = get_function(env, "sss_hashmap_set");
    gcc_func_t *key_hash = get_hash_func(env, needed_key_t);
    gcc_func_t *key_cmp = get_indirect_compare_func(env, needed_key_t);
    gcc_rvalue_t *call = gcc_callx(
        env->ctx, NULL, hashmap_set_fn,
        gcc_cast(env->ctx, NULL, table, gcc_type(env->ctx, VOID_PTR)),
        gcc_cast(env->ctx, NULL, gcc_get_func_address(key_hash, NULL), gcc_type(env->ctx, VOID_PTR)),
        gcc_cast(env->ctx, NULL, gcc_get_func_address(key_cmp, NULL), gcc_type(env->ctx, VOID_PTR)),
        gcc_rvalue_size(env->ctx, gcc_sizeof(env, table_entry_type(t))),
        gcc_lvalue_address(key_lval, NULL),
        table_entry_value_offset(env, t),
        gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR)));

    gcc_type_t *value_gcc_t = sss_type_to_gcc(env, Match(t, TableType)->value_type);
    gcc_lvalue_t *dest = gcc_local(func, NULL, gcc_get_ptr_type(value_gcc_t), "_dest");
    gcc_assign(*block, NULL, dest, gcc_cast(env->ctx, NULL, call, gcc_get_ptr_type(value_gcc_t)));
    return gcc_rvalue_dereference(gcc_rval(dest), NULL);
}

void table_remove(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *table, gcc_rvalue_t *key_val)
{
    gcc_func_t *func = gcc_block_func(*block);
    gcc_type_t *key_gcc_t = sss_type_to_gcc(env, Match(t, TableType)->key_type);
    gcc_rvalue_t *key_ptr;
    if (key_val) {
        gcc_lvalue_t *key_lval = gcc_local(func, NULL, key_gcc_t, "_key");
        gcc_assign(*block, NULL, key_lval, key_val);
        flatten_arrays(env, block, Match(t, TableType)->key_type, gcc_lvalue_address(key_lval, NULL));
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
    gcc_eval(*block, NULL, call);
}

static void add_table_entry(env_t *env, gcc_block_t **block, ast_t *entry, table_insert_info_t *info)
{
    sss_type_t *raw_entry_t = get_type(env, entry);
    if (raw_entry_t->tag == GeneratorType) {
        gcc_rvalue_t *val = compile_expr(env, block, entry);
        assert(!val);
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
        compiler_err(env, key_ast, "This key was expected to be a %s, but was actually %s",
                    type_to_string(needed_key_t), type_to_string(raw_key_t));

    gcc_rvalue_t *value_val = compile_expr(env, block, value_ast);
    if (!*block) return;
    if (!promote(env, raw_value_t, &value_val, needed_value_t))
        compiler_err(env, value_ast, "This value was expected to be a %s, but was actually %s",
                    type_to_string(needed_value_t), type_to_string(raw_value_t));

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
        compiler_err(env, key_ast, "This key is a %s, but this table needs a key of type %s",
                    type_to_string(raw_key_t), type_to_string(key_t));
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
                .names = LIST(const char*, "[]"),
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
                        "Use '*' to dereference this value if you want to use it as a fallback.");
            // fallback = WrapAST(fallback, HeapAllocate, .value=WrapAST(fallback, Dereference, .value=fallback));
        } else {
            fallback = WrapAST(fallback, HeapAllocate, .value=fallback);
            fallback_t = get_type(env, fallback);
        }
        if (!type_eq(Match(fallback_t, PointerType)->pointed, t))
            compiler_err(env, fallback, "This fallback has type %s, which doesn't match the table's type: %s",
                        type_to_string(fallback_t), type_to_string(t));

        gcc_struct_t *table_struct = gcc_type_if_struct(gcc_t);
        gcc_assign(*block, loc, gcc_lvalue_access_field(table_var, NULL, gcc_get_field(table_struct, TABLE_FALLBACK_FIELD)),
                   compile_expr(env, block, fallback));
    }

    if (table->default_value) {
        sss_type_t *default_t = get_type(env, table->default_value);
        sss_type_t *value_t = Match(t, TableType)->value_type;
        if (!type_is_a(default_t, value_t))
            compiler_err(env, table->default_value, "This default value has type %s, which doesn't match the table's value type: %s",
                        type_to_string(default_t), type_to_string(value_t));

        gcc_struct_t *table_struct = gcc_type_if_struct(gcc_t);
        gcc_assign(*block, loc, gcc_lvalue_access_field(table_var, NULL, gcc_get_field(table_struct, TABLE_DEFAULT_FIELD)),
                   compile_expr(env, block, WrapAST(table->default_value, HeapAllocate, .value=table->default_value)));
    }

    return gcc_rval(table_var);
}

void compile_table_print_func(env_t *env, gcc_block_t **block, gcc_rvalue_t *obj, gcc_rvalue_t *file, gcc_rvalue_t *rec, gcc_rvalue_t *color, sss_type_t *t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_func_t *fputs_fn = get_function(env, "fputs");

#define WRITE_LITERAL(block, str) gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, fputs_fn, gcc_str(env->ctx, str), file))
#define COLOR_LITERAL(block, str) maybe_print_str(env, block, color, file, str)

    gcc_func_t *func = gcc_block_func(*block);

    sss_type_t *entry_t = table_entry_type(t);

    // i = 0
    gcc_lvalue_t *i = gcc_local(func, NULL, gcc_type(env->ctx, INT64), "_i");
    gcc_assign(*block, NULL, i, gcc_zero(env->ctx, gcc_type(env->ctx, INT64)));
    gcc_struct_t *table_struct = gcc_type_if_struct(gcc_t);
    gcc_rvalue_t *entries = gcc_rvalue_access_field(obj, NULL, gcc_get_field(table_struct, TABLE_ENTRIES_FIELD));
    gcc_rvalue_t *len = gcc_rvalue_access_field(obj, NULL, gcc_get_field(table_struct, TABLE_COUNT_FIELD));

    gcc_block_t *is_empty = gcc_new_block(func, fresh("is_empty")),
                *is_not_empty = gcc_new_block(func, fresh("is_not_empty"));
    gcc_jump_condition(*block, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, len, gcc_zero(env->ctx, gcc_type(env->ctx, UINT32))),
                       is_not_empty, is_empty);
    *block = is_empty;
    COLOR_LITERAL(block, "\x1b[m");
    WRITE_LITERAL(*block, "{");
    COLOR_LITERAL(block, "\x1b[0;2;36m");
    WRITE_LITERAL(*block, ":");
    COLOR_LITERAL(block, "\x1b[0;36m");
    WRITE_LITERAL(*block, type_to_string(Match(t, TableType)->key_type));
    COLOR_LITERAL(block, "\x1b[33m");
    WRITE_LITERAL(*block, "=>");
    COLOR_LITERAL(block, "\x1b[36m");
    WRITE_LITERAL(*block, type_to_string(Match(t, TableType)->value_type));
    COLOR_LITERAL(block, "\x1b[m");
    WRITE_LITERAL(*block, "}");
    gcc_return_void(*block, NULL);

    *block = is_not_empty;
    gcc_rvalue_t *len64 = gcc_cast(env->ctx, NULL, len, gcc_type(env->ctx, INT64));
    COLOR_LITERAL(block, "\x1b[m");
    WRITE_LITERAL(*block, "{");

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
    gcc_func_t *key_print = get_print_func(env, key_type);
    gcc_struct_t *entry_struct = gcc_type_if_struct(sss_type_to_gcc(env, entry_t));
    gcc_rvalue_t *key = gcc_rvalue_access_field(entry, NULL, gcc_get_field(entry_struct, 0));
    gcc_eval(add_next_entry, NULL, gcc_callx(env->ctx, NULL, key_print, key, file, rec, color));
    gcc_block_t *rest_of_entry = add_next_entry;
    COLOR_LITERAL(&rest_of_entry, "\x1b[0;33m");
    WRITE_LITERAL(rest_of_entry, "=>");
    COLOR_LITERAL(&rest_of_entry, "\x1b[m");
    gcc_rvalue_t *value = gcc_rvalue_access_field(entry, NULL, gcc_get_field(entry_struct, 1));
    sss_type_t *value_type = Match(t, TableType)->value_type;
    gcc_func_t *value_print = get_print_func(env, value_type);
    gcc_eval(rest_of_entry, NULL, gcc_callx(env->ctx, NULL, value_print, value, file, rec, color));
    
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
    COLOR_LITERAL(&add_comma, "\x1b[m");
    WRITE_LITERAL(add_comma, ", ");

    // goto add_next_entry;
    gcc_jump(add_comma, NULL, add_next_entry);

    // done_with_entries:
    gcc_block_t *print_fallback = gcc_new_block(func, fresh("print_fallback")),
                *check_default = gcc_new_block(func, fresh("check_default")),
                *print_default = gcc_new_block(func, fresh("print_default")),
                *done_with_metadata = gcc_new_block(func, fresh("done_with_metadata"));
    gcc_rvalue_t *fallback = gcc_rvalue_access_field(obj, NULL, gcc_get_field(table_struct, TABLE_FALLBACK_FIELD));
    gcc_rvalue_t *has_fallback = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, fallback, gcc_null(env->ctx, gcc_get_ptr_type(gcc_t)));
    gcc_jump_condition(done_with_entries, NULL, has_fallback, print_fallback, check_default);

    // Print fallback:
    COLOR_LITERAL(&print_fallback, "\x1b[0;33m");
    WRITE_LITERAL(print_fallback, "; ");
    COLOR_LITERAL(&print_fallback, "\x1b[32m");
    WRITE_LITERAL(print_fallback, "fallback=");
    COLOR_LITERAL(&print_fallback, "\x1b[m");
    gcc_eval(print_fallback, NULL, gcc_callx(env->ctx, NULL, func, gcc_rval(gcc_rvalue_dereference(fallback, NULL)), file, rec, color));
    gcc_jump(print_fallback, NULL, check_default);

    // Check default
    sss_type_t *default_t = Match(t, TableType)->value_type;
    gcc_type_t *default_gcc_t = sss_type_to_gcc(env, default_t);
    gcc_rvalue_t *def = gcc_rvalue_access_field(obj, NULL, gcc_get_field(table_struct, TABLE_DEFAULT_FIELD));
    gcc_rvalue_t *has_default = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, def, gcc_null(env->ctx, gcc_get_ptr_type(default_gcc_t)));
    gcc_jump_condition(check_default, NULL, has_default, print_default, done_with_metadata);

    // Print default:
    COLOR_LITERAL(&print_default, "\x1b[0;33m");
    WRITE_LITERAL(print_default, "; ");
    COLOR_LITERAL(&print_default, "\x1b[32m");
    WRITE_LITERAL(print_default, "default=");
    COLOR_LITERAL(&print_default, "\x1b[m");
    gcc_func_t *def_print_func = get_print_func(env, default_t);
    gcc_eval(print_default, NULL, gcc_callx(env->ctx, NULL, def_print_func, gcc_rval(gcc_rvalue_dereference(def, NULL)), file, rec, color));
    gcc_jump(print_default, NULL, done_with_metadata);

    COLOR_LITERAL(&done_with_metadata, "\x1b[m");
    WRITE_LITERAL(done_with_metadata, "}");

    gcc_return_void(done_with_metadata, NULL);
#undef WRITE_LITERAL 
#undef COLOR_LITERAL 
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
