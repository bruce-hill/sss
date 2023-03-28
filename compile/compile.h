#pragma once
#include <gc/gc.h>
#include <libgccjit.h>
#include <intern.h>
#include <bhash.h>
#include <signal.h>
#include <stdio.h>

#include "../ast.h"
#include "../environment.h"
#include "../types.h"
#include "../util.h"
#include "../files.h"
#include "libgccjit_abbrev.h"
#include "modules.h"

#define hashmap_gets(h, str) hashmap_get(h, intern_str(str))

#define foreach LIST_FOR
#define length LIST_LEN
#define ith LIST_ITEM
#define append APPEND

#define gcc_type(ctx, t) gcc_get_type(ctx, GCC_T_ ## t)
#define gcc_int64(ctx,i) gcc_rvalue_from_long(ctx, gcc_type(ctx, INT64), i)

#define ast_loc(env, ast) ((ast) && (ast)->span.start ? gcc_new_location((env)->ctx, (ast)->span.file->filename,\
        (int)bl_get_line_number((ast)->span.file, (ast)->span.start),\
        (int)bl_get_line_column((ast)->span.file, (ast)->span.start)) : NULL)

#define TABLE_ENTRIES_FIELD 0
#define TABLE_BUCKETS_FIELD 1
#define TABLE_FALLBACK_FIELD 2
#define TABLE_DEFAULT_FIELD 3
#define TABLE_CAPACITY_FIELD 4
#define TABLE_COUNT_FIELD 5
#define TABLE_LASTFREE_FIELD 6
#define TABLE_COW_FIELD 7

typedef struct {
    bl_type_t *key_type, *value_type;
    gcc_lvalue_t *key_lval, *value_lval;
} iterator_info_t;
typedef void (*loop_handler_t)(env_t *env, gcc_block_t **block, iterator_info_t *info, void *userdata);

// ============================== helpers.c ==============================
// Generate a fresh (unique) identifier
istr_t fresh(istr_t name);
// Data layout information:
ssize_t gcc_alignof(env_t *env, bl_type_t *bl_t);
ssize_t gcc_sizeof(env_t *env, bl_type_t *bl_t);
// Get the list of GCC fields in a union:
List(gcc_field_t*) get_union_fields(env_t *env, bl_type_t *t);
// Convert a blang type to the GCC JIT representation
gcc_type_t *bl_type_to_gcc(env_t *env, bl_type_t *t);
// Check whether a value is truthy or not
void check_truthiness(env_t *env, gcc_block_t **block, ast_t *obj, gcc_block_t *if_truthy, gcc_block_t *if_falsey);
// If a value is a string type, quote it:
gcc_rvalue_t *quote_string(env_t *env, bl_type_t *t, gcc_rvalue_t *val);
// Get a function to convert an object of a given type to a string
gcc_func_t *get_print_func(env_t *env, bl_type_t *t);
// Get a hash function for a type
gcc_func_t *get_hash_func(env_t *env, bl_type_t *t);
// Compare two values (returns [-1,0,1])
gcc_rvalue_t *compare_values(env_t *env, bl_type_t *t, gcc_rvalue_t *a, gcc_rvalue_t *b);
// Get a function to compare two values of a type
gcc_func_t *get_compare_func(env_t *env, bl_type_t *t);
// Get a function to compare two pointers to values of a type
gcc_func_t *get_indirect_compare_func(env_t *env, bl_type_t *t);
// Coerce two numbers into the larger representation
void coerce_numbers(env_t *env, bl_type_t **lhs_type, gcc_rvalue_t **lhs, bl_type_t **rhs_type, gcc_rvalue_t **rhs);
// A ternary expression (a ? b : c)
gcc_rvalue_t *ternary(gcc_block_t **block, gcc_rvalue_t *condition, gcc_type_t *gcc_t, gcc_rvalue_t *true_val, gcc_rvalue_t *false_val);
// Convert an AST into an lvalue
gcc_lvalue_t *get_lvalue(env_t *env, gcc_block_t **block, ast_t *ast, bool allow_slices);
// Apply optional/numeric promotion when possible
bool promote(env_t *env, bl_type_t *actual, gcc_rvalue_t **val, bl_type_t *needed);
// Compile all the deferred statements up to a given point
void insert_defers(env_t *env, gcc_block_t **block, defer_t *stop_at_defer);
// Ensure array is flat (stride == 1) for easy comparisons/hashes
void flatten_arrays(env_t *env, gcc_block_t **block, bl_type_t *t, gcc_rvalue_t *array);
void insert_failure(env_t *env, gcc_block_t **block, span_t span, const char *user_fmt, ...);

// ============================== program.c =============================
typedef void (*main_func_t)(int, char**);
main_func_t compile_file(gcc_ctx_t *ctx, jmp_buf *on_err, bl_file_t *f, ast_t *ast, bool debug, bool standalone, gcc_jit_result **result);

// ============================== expr.c ================================
gcc_rvalue_t *compile_constant(env_t *env, ast_t *ast);
gcc_rvalue_t *compile_expr(env_t *env, gcc_block_t **block, ast_t *ast);

// ============================== functions.c ===========================
void compile_function(env_t *env, gcc_func_t *func, ast_t *def);
gcc_func_t *get_function_def(env_t *env, ast_t *def, istr_t name, bool is_global);

// ============================== blocks.c ==============================
void compile_statement(env_t *env, gcc_block_t **block, ast_t *ast);
gcc_rvalue_t *compile_block_expr(env_t *env, gcc_block_t **block, ast_t *ast);
void compile_block_statement(env_t *env, gcc_block_t **block, ast_t *ast);
ast_t *globalize_decls(ast_t *block_ast);

// ============================== loops.c ================================
void compile_for_loop(env_t *env, gcc_block_t **block, ast_t *ast);
void compile_while_loop(env_t *env, gcc_block_t **block, istr_t loop_name, ast_t *condition, ast_t *body, ast_t *between);

// ============================== math.c ================================
gcc_rvalue_t *math_binop(env_t *env, gcc_block_t **block, ast_t *ast);
void math_update(env_t *env, gcc_block_t **block, ast_t *ast);
void math_update_rec(
    env_t *env, gcc_block_t **block, ast_t *ast, bl_type_t *lhs_t, gcc_lvalue_t *lhs,
    gcc_binary_op_e op, bl_type_t *rhs_t, gcc_rvalue_t *rhs);

// ============================== arrays.c ==============================
// Copy on write behavior:
typedef enum {ACCESS_READ, ACCESS_WRITE} access_type_e;
gcc_lvalue_t *array_index(env_t *env, gcc_block_t **block, ast_t *arr_ast, ast_t *index, index_type_e index_type, access_type_e access);
gcc_rvalue_t *array_slice(env_t *env, gcc_block_t **block, ast_t *arr_ast, ast_t *index, access_type_e access);
gcc_lvalue_t *array_capacity(env_t *env, gcc_rvalue_t *arr_ptr);
void mark_array_cow(env_t *env, gcc_block_t **block, gcc_rvalue_t *arr_ptr);
void check_cow(env_t *env, gcc_block_t **block, bl_type_t *arr_t, gcc_rvalue_t *arr);
gcc_rvalue_t *compile_array(env_t *env, gcc_block_t **block, ast_t *ast);
void compile_array_print_func(env_t *env, gcc_block_t **block, gcc_rvalue_t *obj, gcc_rvalue_t *rec, gcc_rvalue_t *file, bl_type_t *t);
gcc_rvalue_t *array_contains(env_t *env, gcc_block_t **block, ast_t *array, ast_t *member);

// ============================== tables.c ==============================
gcc_rvalue_t *table_lookup_optional(env_t *env, gcc_block_t **block, ast_t *table_ast, ast_t *key);
gcc_lvalue_t *table_lvalue(env_t *env, gcc_block_t **block, bl_type_t *t, gcc_rvalue_t *table, gcc_rvalue_t *key_val);
void table_remove(env_t *env, gcc_block_t **block, bl_type_t *t, gcc_rvalue_t *table, gcc_rvalue_t *key_val);
gcc_rvalue_t *compile_table(env_t *env, gcc_block_t **block, ast_t *ast);
void compile_table_print_func(env_t *env, gcc_block_t **block, gcc_rvalue_t *obj, gcc_rvalue_t *rec, gcc_rvalue_t *file, bl_type_t *t);

// ============================== ranges.c ==============================
gcc_rvalue_t *compile_range(env_t *env, gcc_block_t **block, ast_t *ast);
gcc_rvalue_t *range_len(env_t *env, gcc_type_t *gcc_t, gcc_rvalue_t *range);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
