#pragma once
#include <gc/gc.h>
#include <libgccjit.h>
#include <signal.h>
#include <stdio.h>

#include "../ast.h"
#include "../environment.h"
#include "../types.h"
#include "../util.h"
#include "../files.h"
#include "libgccjit_abbrev.h"

#define foreach LIST_FOR
#define length LIST_LEN
#define ith LIST_ITEM
#define append APPEND

#define gcc_type(ctx, t) gcc_get_type(ctx, GCC_T_ ## t)
#define gcc_int64(ctx,i) gcc_rvalue_from_long(ctx, gcc_type(ctx, INT64), i)

#define ast_loc(env, ast) ((ast) && (ast)->span.start ? gcc_new_location((env)->ctx, (ast)->span.file->filename,\
        (int)sss_get_line_number((ast)->span.file, (ast)->span.start),\
        (int)sss_get_line_column((ast)->span.file, (ast)->span.start)) : NULL)

#define TABLE_ENTRIES_FIELD 0
#define TABLE_BUCKETS_FIELD 1
#define TABLE_FALLBACK_FIELD 2
#define TABLE_DEFAULT_FIELD 3
#define TABLE_CAPACITY_FIELD 4
#define TABLE_COUNT_FIELD 5
#define TABLE_LASTFREE_FIELD 6
#define TABLE_COW_FIELD 7

#define ARRAY_DATA_FIELD 0
#define ARRAY_LENGTH_FIELD 1
#define ARRAY_STRIDE_FIELD 2
#define ARRAY_CAPACITY_FIELD 3

// ============================== helpers.c ==============================
// Generate a fresh (unique) identifier
const char *fresh(const char *name);
// Data layout information:
ssize_t gcc_alignof(env_t *env, sss_type_t *sss_t);
ssize_t gcc_sizeof(env_t *env, sss_type_t *sss_t);
// Get the type of a tag in a tagged union:
gcc_type_t *get_tag_type(env_t *env, sss_type_t *t);
// Get the type of a union in a tagged union:
gcc_type_t *get_union_type(env_t *env, sss_type_t *t);
// Convert an SSS type to the GCC JIT representation
gcc_type_t *sss_type_to_gcc(env_t *env, sss_type_t *t);
// Check whether a value is truthy or not
void check_truthiness(env_t *env, gcc_block_t **block, ast_t *obj, gcc_block_t *if_truthy, gcc_block_t *if_falsey);
// Maybe print a string
void maybe_print_str(env_t *env, gcc_block_t **block, gcc_rvalue_t *do_print, gcc_rvalue_t *file, const char *str);
// Get a function to convert an object of a given type to a string
gcc_func_t *get_print_func(env_t *env, sss_type_t *t);
// Get a hash function for a type
gcc_func_t *get_hash_func(env_t *env, sss_type_t *t);
// Compare two values (returns [-1,0,1])
gcc_rvalue_t *compare_values(env_t *env, sss_type_t *t, gcc_rvalue_t *a, gcc_rvalue_t *b);
// Get a function to compare two values of a type
gcc_func_t *get_compare_func(env_t *env, sss_type_t *t);
// Get a function to compare two pointers to values of a type
gcc_func_t *get_indirect_compare_func(env_t *env, sss_type_t *t);
// A ternary expression (a ? b : c)
gcc_rvalue_t *ternary(gcc_block_t **block, gcc_rvalue_t *condition, gcc_type_t *gcc_t, gcc_rvalue_t *true_val, gcc_rvalue_t *false_val);
// Convert an AST into an lvalue
gcc_lvalue_t *get_lvalue(env_t *env, gcc_block_t **block, ast_t *ast, bool allow_slices);
// Apply optional/numeric promotion when possible
bool promote(env_t *env, sss_type_t *actual, gcc_rvalue_t **val, sss_type_t *needed);
// Integer demotion in select cases:
bool demote_int_literals(ast_t **ast, sss_type_t *needed);
// Compile all the deferred statements up to a given point
void insert_defers(env_t *env, gcc_block_t **block, defer_t *stop_at_defer);
// Ensure array is flat (stride == 1) for easy comparisons/hashes
void insert_failure(env_t *env, gcc_block_t **block, span_t *span, const char *user_fmt, ...);

// ============================== program.c =============================
typedef void (*main_func_t)(int, char**);
main_func_t compile_file(gcc_ctx_t *ctx, jmp_buf *on_err, sss_file_t *f, ast_t *ast, bool debug, gcc_jit_result **result);

// ============================== expr.c ================================
gcc_rvalue_t *compile_constant(env_t *env, ast_t *ast);
gcc_rvalue_t *compile_expr(env_t *env, gcc_block_t **block, ast_t *ast);
gcc_rvalue_t *compile_len(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *obj);

// ============================== functions.c ===========================
void compile_function(env_t *env, gcc_func_t *func, ast_t *def);
gcc_func_t *get_function_def(env_t *env, ast_t *def, const char *name);

// ============================== blocks.c ==============================
gcc_func_t *prepare_use(env_t *env, ast_t *ast);
void populate_uses(env_t *env, ast_t *ast);
void predeclare_def_types(env_t *env, ast_t *def);
void predeclare_def_funcs(env_t *env, ast_t *def);
void populate_def_members(env_t *env, ast_t *def);
void compile_statement(env_t *env, gcc_block_t **block, ast_t *ast);
gcc_rvalue_t *compile_block_expr(env_t *env, gcc_block_t **block, ast_t *ast);
void compile_block_statement(env_t *env, gcc_block_t **block, ast_t *ast);

// ============================== loops.c ================================
void compile_for_loop(env_t *env, gcc_block_t **block, ast_t *ast);
void compile_while_loop(env_t *env, gcc_block_t **block, const char *loop_name, ast_t *condition, ast_t *body, ast_t *between);

// ============================== math.c ================================
gcc_rvalue_t *math_binop(env_t *env, gcc_block_t **block, ast_t *ast);
gcc_rvalue_t *math_update(env_t *env, gcc_block_t **block, ast_t *ast);
void math_update_rec(
    env_t *env, gcc_block_t **block, ast_t *ast, sss_type_t *lhs_t, gcc_lvalue_t *lhs,
    gcc_binary_op_e op, sss_type_t *rhs_t, gcc_rvalue_t *rhs);

// ============================== arrays.c ==============================
// Copy on write behavior:
typedef enum {ACCESS_READ, ACCESS_WRITE} access_type_e;
gcc_lvalue_t *array_index(env_t *env, gcc_block_t **block, ast_t *arr_ast, ast_t *index, bool unchecked, access_type_e access);
gcc_rvalue_t *array_slice(env_t *env, gcc_block_t **block, ast_t *arr_ast, ast_t *index, access_type_e access);
gcc_rvalue_t *array_field_slice(env_t *env, gcc_block_t **block, ast_t *ast, const char *field_name, access_type_e access);
gcc_lvalue_t *array_capacity(env_t *env, gcc_rvalue_t *arr_ptr);
void mark_array_cow(env_t *env, gcc_block_t **block, gcc_rvalue_t *arr_ptr);
void check_cow(env_t *env, gcc_block_t **block, sss_type_t *arr_t, gcc_rvalue_t *arr);
gcc_rvalue_t *compile_array(env_t *env, gcc_block_t **block, ast_t *ast, bool mark_cow);
void compile_array_print_func(env_t *env, gcc_block_t **block, gcc_rvalue_t *obj, gcc_rvalue_t *file, gcc_rvalue_t *rec, gcc_rvalue_t *color, sss_type_t *t);
gcc_rvalue_t *array_contains(env_t *env, gcc_block_t **block, ast_t *array, ast_t *member);
binding_t *get_array_method(env_t *env, sss_type_t *t, const char *method_name);
void flatten_arrays(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *array);

// ============================== tables.c ==============================
gcc_rvalue_t *table_entry_value_offset(env_t *env, sss_type_t *t);
gcc_rvalue_t *table_lookup_optional(env_t *env, gcc_block_t **block, ast_t *table_ast, ast_t *key_ast, gcc_rvalue_t **key_rval_out, bool raw);
gcc_lvalue_t *table_lvalue(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *table, ast_t *key_ast);
void table_remove(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *table, gcc_rvalue_t *key_val);
gcc_rvalue_t *compile_table(env_t *env, gcc_block_t **block, ast_t *ast, bool mark_cow);
void compile_table_print_func(env_t *env, gcc_block_t **block, gcc_rvalue_t *obj, gcc_rvalue_t *file, gcc_rvalue_t *rec, gcc_rvalue_t *color, sss_type_t *t);
void mark_table_cow(env_t *env, gcc_block_t **block, gcc_rvalue_t *table_ptr);

// ============================== ranges.c ==============================
gcc_rvalue_t *compile_range(env_t *env, gcc_block_t **block, ast_t *ast);
gcc_rvalue_t *range_len(env_t *env, gcc_type_t *gcc_t, gcc_rvalue_t *range);
gcc_rvalue_t *range_contains(env_t *env, gcc_block_t **block, ast_t *range, ast_t *member);

// ============================== match.c ===============================
typedef struct {
    gcc_block_t *match_block, *no_match_block;
    env_t *match_env;
} match_outcomes_t;
match_outcomes_t perform_conditional_match(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *val, ast_t *pattern);
const char *get_missing_pattern(env_t *env, sss_type_t *t, List(ast_t*) patterns);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
