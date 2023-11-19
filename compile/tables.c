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
    return gcc_rvalue_int64(env->ctx, value_offset);
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
    gcc_rvalue_t *type_ptr = get_type_pointer(env, t);
    if (autocreate) {
        gcc_func_t *set_fn = get_from_namespace(env, t, "reserve")->func;
        gcc_assign(*block, NULL, dest,
                   gcc_callx(env->ctx, NULL, set_fn, table, gcc_lvalue_address(key_lval, NULL),
                             gcc_null(env->ctx, gcc_get_ptr_type(value_gcc_t)), type_ptr));
        return gcc_rvalue_dereference(gcc_rval(dest), NULL);
    } else {
        gcc_func_t *get_raw_fn = get_from_namespace(env, t, "get_raw")->func;
        gcc_assign(*block, NULL, dest,
                   gcc_callx(env->ctx, NULL, get_raw_fn, table, gcc_lvalue_address(key_lval, NULL), type_ptr));
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

    binding_t *set_fn_binding = get_from_namespace(env, info->table_type, "set");
    gcc_eval(*block, NULL, gcc_callx(env->ctx, NULL, set_fn_binding->func,
                                     info->table_ptr,
                                     gcc_lvalue_address(key_lval, NULL),
                                     gcc_lvalue_address(value_lval, NULL),
                                     get_type_pointer(env, info->table_type)));
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

    binding_t *get_fn_binding = get_from_namespace(env, table_t, raw ? "get_raw" : "get");

    sss_type_t *raw_key_t = get_type(env, key_ast);
    gcc_rvalue_t *key_val = compile_expr(env, block, key_ast);
    if (!promote(env, raw_key_t, &key_val, key_t))
        compiler_err(env, key_ast, "This key is a %T, but this table needs a key of type %T", raw_key_t, key_t);
    gcc_lvalue_t *key_lval = gcc_local(func, loc, sss_type_to_gcc(env, key_t), "_key");
    gcc_assign(*block, loc, key_lval, key_val);
    if (key_rval_out) *key_rval_out = gcc_rval(key_lval);
    gcc_rvalue_t *val_ptr = gcc_callx(
        env->ctx, loc, get_fn_binding->func,
        gcc_lvalue_address(table_var, loc),
        gcc_lvalue_address(key_lval, loc),
        get_type_pointer(env, table_t));
    gcc_type_t *val_ptr_gcc_t = gcc_get_ptr_type(sss_type_to_gcc(env, value_t));
    val_ptr = gcc_cast(env->ctx, loc, val_ptr, val_ptr_gcc_t);

    gcc_lvalue_t *value_lval = gcc_local(func, loc, val_ptr_gcc_t, "_value");
    gcc_assign(*block, loc, value_lval, val_ptr);
    return gcc_rval(value_lval);
}

gcc_rvalue_t *compile_table(env_t *env, gcc_block_t **block, ast_t *ast)
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
    gcc_assign(*block, loc, table_var, gcc_struct_constructor(env->ctx, loc, gcc_t, 0, NULL, NULL));

    // TODO: need to mark cow?

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

        gcc_struct_t *table_struct = gcc_type_as_struct(gcc_t);
        gcc_assign(*block, loc, gcc_lvalue_access_field(table_var, NULL, gcc_get_field(table_struct, TABLE_FALLBACK_FIELD)),
                   compile_expr(env, block, fallback));
    }

    if (table->default_value) {
        sss_type_t *default_t = get_type(env, table->default_value);
        sss_type_t *value_t = Match(t, TableType)->value_type;
        if (!type_is_a(default_t, value_t))
            compiler_err(env, table->default_value, "This default value has type %T, which doesn't match the table's value type: %T",
                        default_t, value_t);

        gcc_struct_t *table_struct = gcc_type_as_struct(gcc_t);
        gcc_assign(*block, loc, gcc_lvalue_access_field(table_var, NULL, gcc_get_field(table_struct, TABLE_DEFAULT_FIELD)),
                   compile_expr(env, block, WrapAST(table->default_value, HeapAllocate, .value=table->default_value)));
    }

    return gcc_rval(table_var);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
