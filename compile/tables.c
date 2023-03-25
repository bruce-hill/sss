// Compilation logic for Tables in this file
#include <assert.h>
#include <bhash.h>
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
    bl_type_t *table_type;
    gcc_rvalue_t *table_ptr;
} table_insert_info_t;

gcc_lvalue_t *table_set_loc(env_t *env, gcc_block_t **block, bl_type_t *t, gcc_rvalue_t *table, gcc_rvalue_t *key_val)
{
    gcc_type_t *entry_t = bl_type_to_gcc(env, table_entry_type(t));
    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *entry_lval = gcc_local(func, NULL, entry_t, fresh("entry"));

    gcc_lvalue_t *key_lval = gcc_local(func, NULL, bl_type_to_gcc(env, Match(t, TableType)->key_type), fresh("key"));
    gcc_assign(*block, NULL, key_lval, key_val);
    flatten_arrays(env, block, Match(t, TableType)->key_type, gcc_lvalue_address(key_lval, NULL));
    gcc_assign(*block, NULL, gcc_lvalue_access_field(entry_lval, NULL, gcc_get_field(gcc_type_if_struct(entry_t), 0)), gcc_rval(key_lval));

    gcc_func_t *hashmap_set_fn = hashmap_gets(env->global_funcs, "bl_hashmap_set");
    gcc_func_t *key_hash = get_hash_func(env, Match(t, TableType)->key_type);
    gcc_func_t *key_cmp = get_indirect_compare_func(env, Match(t, TableType)->key_type);
    gcc_rvalue_t *call = gcc_callx(
        env->ctx, NULL, hashmap_set_fn,
        gcc_cast(env->ctx, NULL, table, gcc_type(env->ctx, VOID_PTR)),
        gcc_cast(env->ctx, NULL, gcc_get_func_address(key_hash, NULL), gcc_type(env->ctx, VOID_PTR)),
        gcc_cast(env->ctx, NULL, gcc_get_func_address(key_cmp, NULL), gcc_type(env->ctx, VOID_PTR)),
        gcc_rvalue_size(env->ctx, gcc_sizeof(env, table_entry_type(t))),
        gcc_lvalue_address(entry_lval, NULL));

    gcc_field_t *value_field = gcc_get_field(gcc_type_if_struct(entry_t), 1);
    return gcc_rvalue_dereference_field(gcc_cast(env->ctx, NULL, call, gcc_get_ptr_type(entry_t)), NULL, value_field);
}

static void add_table_entry(env_t *env, gcc_block_t **block, ast_t *entry, table_insert_info_t *info)
{
    bl_type_t *entry_t = get_type(env, entry); // entry type
    if (entry_t->tag == GeneratorType) {
        gcc_rvalue_t *val = compile_expr(env, block, entry);
        assert(!val);
        return;
    }

    // This comes first, because the entry may short-circuit
    gcc_rvalue_t *entry_val = compile_expr(env, block, entry);
    if (!*block) return;

    bl_type_t *expected_entry_t = table_entry_type(info->table_type);
    // if (entry_t != expected_entry_t)
    //     if (!promote(env, entry_t, &entry_val, expected_entry_t))
    //         compile_err(env, entry, "I can't convert this type (%s) to %s", type_to_string(entry_t), type_to_string(expected_entry_t));

    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *entry_lval = gcc_local(func, NULL, bl_type_to_gcc(env, expected_entry_t), fresh("entry"));
    gcc_assign(*block, NULL, entry_lval, entry_val);

    gcc_func_t *hashmap_set_fn = hashmap_gets(env->global_funcs, "bl_hashmap_set");
    gcc_func_t *key_hash = get_hash_func(env, Match(info->table_type, TableType)->key_type);
    gcc_func_t *key_cmp = get_indirect_compare_func(env, Match(info->table_type, TableType)->key_type);
    gcc_eval(*block, NULL, gcc_callx(env->ctx, NULL, hashmap_set_fn,
                                     gcc_cast(env->ctx, NULL, info->table_ptr, gcc_type(env->ctx, VOID_PTR)),
                                     gcc_cast(env->ctx, NULL, gcc_get_func_address(key_hash, NULL), gcc_type(env->ctx, VOID_PTR)),
                                     gcc_cast(env->ctx, NULL, gcc_get_func_address(key_cmp, NULL), gcc_type(env->ctx, VOID_PTR)),
                                     gcc_rvalue_size(env->ctx, gcc_sizeof(env, expected_entry_t)),
                                     gcc_lvalue_address(entry_lval, NULL)));
}

// Returns an optional pointer to a value
gcc_rvalue_t *table_lookup_optional(env_t *env, gcc_block_t **block, ast_t *table_ast, ast_t *key)
{
    gcc_loc_t *loc = ast_loc(env, key);
    bl_type_t *table_t = get_type(env, table_ast);
    gcc_rvalue_t *table = compile_expr(env, block, table_ast);
    while (table_t->tag == PointerType) {
        auto ptr = Match(table_t, PointerType);
        if (ptr->is_optional)
            compile_err(env, table_ast, "This is an optional pointer, which can't be safely dereferenced.");

        table = gcc_rval(gcc_rvalue_dereference(table, loc));
        table_t = ptr->pointed;
    }
    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *table_var = gcc_local(func, loc, bl_type_to_gcc(env, table_t), fresh("table"));
    gcc_assign(*block, loc, table_var, table);

    bl_type_t *key_t = get_type(env, key);
    bl_type_t *expected_key_t = Match(table_t, TableType)->key_type;
    if (!type_is_a(key_t, expected_key_t))
        compile_err(env, key, "This key is a %s, which is the wrong type for this table (%s)", type_to_string(key_t), type_to_string(table_t));

    // void *bl_hashmap_get(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, const void *key, size_t entry_size_padded);
    gcc_func_t *hashmap_get_fn = hashmap_gets(env->global_funcs, "bl_hashmap_get");
    gcc_func_t *key_hash = get_hash_func(env, expected_key_t);
    gcc_func_t *key_cmp = get_indirect_compare_func(env, expected_key_t);
    bl_type_t *entry_t = table_entry_type(table_t);

    gcc_rvalue_t *key_val = compile_expr(env, block, key);
    gcc_lvalue_t *key_lval = gcc_local(func, loc, bl_type_to_gcc(env, expected_key_t), fresh("key"));
    gcc_assign(*block, loc, key_lval, key_val);
    flatten_arrays(env, block, key_t, gcc_lvalue_address(key_lval, loc));
    gcc_rvalue_t *entry = gcc_callx(
        env->ctx, loc, hashmap_get_fn,
        gcc_cast(env->ctx, loc, gcc_lvalue_address(table_var, loc), gcc_type(env->ctx, VOID_PTR)),
        gcc_cast(env->ctx, loc, gcc_get_func_address(key_hash, loc), gcc_type(env->ctx, VOID_PTR)),
        gcc_cast(env->ctx, loc, gcc_get_func_address(key_cmp, loc), gcc_type(env->ctx, VOID_PTR)),
        gcc_rvalue_size(env->ctx, gcc_sizeof(env, expected_key_t)),
        gcc_lvalue_address(key_lval, loc));
    gcc_type_t *entry_ptr_gcc_t = gcc_get_ptr_type(bl_type_to_gcc(env, entry_t));
    entry = gcc_cast(env->ctx, loc, entry, entry_ptr_gcc_t);

    gcc_lvalue_t *entry_lval = gcc_local(func, loc, entry_ptr_gcc_t, fresh("entry"));
    gcc_assign(*block, loc, entry_lval, entry);

    gcc_type_t *value_ptr_t = gcc_get_ptr_type(bl_type_to_gcc(env, Match(table_t, TableType)->value_type));
    gcc_lvalue_t *value_lval = gcc_local(func, loc, value_ptr_t, fresh("value"));

    gcc_block_t *missing_key = gcc_new_block(func, fresh("missing_key")),
                *found_key = gcc_new_block(func, fresh("found_key")),
                *done = gcc_new_block(func, fresh("done"));
    gcc_jump_condition(*block, loc,
                  gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, gcc_null(env->ctx, entry_ptr_gcc_t), gcc_rval(entry_lval)),
                  missing_key, found_key);

    gcc_assign(missing_key, loc, value_lval, gcc_null(env->ctx, value_ptr_t));
    gcc_jump(missing_key, loc, done);

    gcc_field_t *value_field = gcc_get_field(gcc_type_if_struct(bl_type_to_gcc(env, entry_t)), 1);
    gcc_assign(found_key, loc, value_lval, gcc_lvalue_address(gcc_rvalue_dereference_field(gcc_rval(entry_lval), loc, value_field), loc));
    gcc_jump(found_key, loc, done);

    *block = done;
    return gcc_rval(value_lval);
}

gcc_rvalue_t *compile_table(env_t *env, gcc_block_t **block, ast_t *ast)
{
    auto table = Match(ast, Table);
    bl_type_t *t = get_type(env, ast);
    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
    gcc_func_t *func = gcc_block_func(*block);

    gcc_loc_t *loc = ast_loc(env, ast);
    gcc_lvalue_t *table_var = gcc_local(func, loc, gcc_t, fresh("table"));

    gcc_assign(*block, loc, table_var, gcc_struct_constructor(env->ctx, loc, gcc_t, 0, NULL, NULL));

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
                .names = LIST(istr_t, intern_str("[]")),
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
    return gcc_rval(table_var);
}

void compile_table_print_func(env_t *env, gcc_block_t **block, gcc_rvalue_t *obj, gcc_rvalue_t *rec, gcc_rvalue_t *file, bl_type_t *t)
{
    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
    gcc_func_t *fputs_fn = hashmap_gets(env->global_funcs, "fputs");

#define WRITE_LITERAL(str) gcc_callx(env->ctx, NULL, fputs_fn, gcc_str(env->ctx, str), file)
#define ADD_WRITE(b, w) gcc_update(b, NULL, written_var, GCC_BINOP_PLUS, w)

    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *written_var = gcc_local(func, NULL, gcc_type(env->ctx, INT), fresh("written"));
    gcc_assign(*block, NULL, written_var, gcc_zero(env->ctx, gcc_type(env->ctx, INT)));

    bl_type_t *entry_t = table_entry_type(t);
    ADD_WRITE(*block, WRITE_LITERAL("{"));

    // i = 0
    gcc_lvalue_t *i = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("i"));
    gcc_assign(*block, NULL, i, gcc_zero(env->ctx, gcc_type(env->ctx, INT64)));
    gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
    gcc_rvalue_t *entries = gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, 0));
    gcc_rvalue_t *len = gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, 4));
    gcc_rvalue_t *len64 = gcc_cast(env->ctx, NULL, len, gcc_type(env->ctx, INT64));

    gcc_block_t *add_comma = gcc_new_block(func, fresh("add_comma"));
    gcc_block_t *add_next_entry = gcc_new_block(func, fresh("next_entry"));
    gcc_block_t *end = gcc_new_block(func, fresh("done"));

    // entry_ptr = array.entries
    gcc_type_t *gcc_entry_t = bl_type_to_gcc(env, entry_t);
    gcc_lvalue_t *entry_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_entry_t), fresh("entry_ptr"));
    gcc_assign(*block, NULL, entry_ptr, entries);

    // if (i < len) goto add_next_entry;
    gcc_jump_condition(*block, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(i), len64),
                  add_next_entry, end);

    // add_next_entry:
    // entry = *entry_ptr
    gcc_rvalue_t *entry = gcc_rval(gcc_jit_rvalue_dereference(gcc_rval(entry_ptr), NULL));
    // print("[") print(key) print("]=") print(value)
    bl_type_t *key_type = Match(t, TableType)->key_type;
    gcc_func_t *key_print = get_print_func(env, key_type);
    ADD_WRITE(add_next_entry, WRITE_LITERAL("["));
    gcc_struct_t *entry_struct = gcc_type_if_struct(bl_type_to_gcc(env, entry_t));
    gcc_rvalue_t *key = gcc_rvalue_access_field(entry, NULL, gcc_get_field(entry_struct, 0));
    ADD_WRITE(add_next_entry, gcc_callx(env->ctx, NULL, key_print, quote_string(env, key_type, key), file, rec));
    ADD_WRITE(add_next_entry, WRITE_LITERAL("]="));
    gcc_rvalue_t *value = gcc_rvalue_access_field(entry, NULL, gcc_get_field(entry_struct, 1));
    bl_type_t *value_type = Match(t, TableType)->value_type;
    gcc_func_t *value_print = get_print_func(env, value_type);
    ADD_WRITE(add_next_entry, gcc_callx(env->ctx, NULL, value_print, quote_string(env, value_type, value), file, rec));
    
    // i += 1
    gcc_update(add_next_entry, NULL, i, GCC_BINOP_PLUS, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
    // entry_ptr = &entry_ptr[1]
    gcc_assign(add_next_entry, NULL, entry_ptr,
               gcc_lvalue_address(gcc_array_access(env->ctx, NULL, gcc_rval(entry_ptr), gcc_one(env->ctx, gcc_type(env->ctx, INT))), NULL));
    // if (i < len) goto add_comma;
    gcc_jump_condition(add_next_entry, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(i), len64),
                  add_comma, end);

    // add_comma:
    ADD_WRITE(add_comma, WRITE_LITERAL(", "));

    // goto add_next_entry;
    gcc_jump(add_comma, NULL, add_next_entry);

    // end:
    ADD_WRITE(end, WRITE_LITERAL("}"));

    gcc_return(end, NULL, gcc_rval(written_var));
#undef WRITE_LITERAL 
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
