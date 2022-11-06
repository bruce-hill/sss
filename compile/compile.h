#pragma once
#include <bp/files.h>
#include <gc/gc.h>
#include <libgccjit.h>
#include <intern.h>
#include <bhash.h>
#include <stdio.h>

#include "../ast.h"
#include "../types.h"
#include "../util.h"
#include "libgccjit_abbrev.h"

#define ERROR(env, ast, fmt, ...) { fprintf(stderr, "\x1b[31;7;1m" fmt "\x1b[m\n\n" __VA_OPT__(,) __VA_ARGS__); \
            highlight_match(stderr, env->file, (ast)->match, 2); \
            exit(1); }
#define hashmap_gets(h, str) hashmap_get(h, intern_str(str))

#define foreach LIST_FOR
#define length LIST_LEN
#define ith LIST_ITEM
#define append APPEND

#define gcc_type(ctx, t) gcc_get_type(ctx, GCC_T_ ## t)
#define gcc_int64(ctx,i) gcc_rvalue_from_long(ctx, gcc_type(ctx, INT64), i)

#define ast_loc(env, ast) gcc_new_location((env)->ctx, (env)->file->filename,\
        (int)get_line_number((env)->file, (ast)->match->start),\
        (int)get_line_column((env)->file, (ast)->match->start))

typedef struct loop_label_s {
    struct loop_label_s *enclosing;
    gcc_block_t *skip_label, *stop_label;
    istr_t name;
} loop_label_t;

typedef struct {
    gcc_ctx_t *ctx;
    file_t *file;
    hashmap_t *tostring_funcs;
    hashmap_t *bindings;
    hashmap_t *gcc_types;
    hashmap_t *global_funcs;
    loop_label_t *loop_label;
    bool debug;
} env_t;

typedef gcc_rvalue_t* (*ast_compiler_t)(env_t *env, gcc_block_t **block, ast_t *body, void *userdata);

// ============================== helpers.c ==============================
// Generate a fresh (unique) identifier
istr_t fresh(istr_t name);
// Convert a blang type to the GCC JIT representation
gcc_type_t *bl_type_to_gcc(env_t *env, bl_type_t *t);
// Get a map that contains only the global bindings
hashmap_t *global_bindings(hashmap_t *bindings);
// Check whether a value is truthy or not
void check_truthiness(env_t *env, gcc_block_t **block, ast_t *obj, gcc_block_t *if_truthy, gcc_block_t *if_falsey);
// Get a function to convert an object of a given type to a string
gcc_func_t *get_tostring_func(env_t *env, bl_type_t *t);
// Coerce two numbers into the larger representation
void coerce_numbers(env_t *env, bl_type_t *lhs_type, gcc_rvalue_t **lhs, bl_type_t *rhs_type, gcc_rvalue_t **rhs);
// Convert an AST into an lvalue
gcc_lvalue_t *get_lvalue(env_t *env, gcc_block_t **block, ast_t *ast);

// ============================== program.c ==============================
gcc_jit_result *compile_file(gcc_jit_context *ctx, file_t *f, ast_t *ast, bool debug);

// ============================== expr.c ==============================
gcc_rvalue_t *compile_expr(env_t *env, gcc_block_t **block, ast_t *ast);

// ============================== functions.c ==============================
void compile_function(env_t *env, gcc_func_t *func, ast_t *def);
gcc_func_t *get_function_def(env_t *env, ast_t *def, bool is_global);

// ============================== blocks.c ==============================
void compile_statement(env_t *env, gcc_block_t **block, ast_t *ast);
gcc_rvalue_t *compile_block(env_t *env, gcc_block_t **block, ast_t *ast, bool return_value);

// ============================== lists.c ==============================
gcc_rvalue_t *compile_list(env_t *env, gcc_block_t **block, ast_t *ast);
gcc_rvalue_t *compile_list_for_loop(env_t *env, gcc_block_t **block, ast_t *ast, ast_compiler_t body_compiler, void *userdata);

// ============================== ranges.c ==============================
gcc_rvalue_t *compile_range(env_t *env, gcc_block_t **block, ast_t *ast);
gcc_rvalue_t *compile_range_for_loop(env_t *env, gcc_block_t **block, ast_t *ast, ast_compiler_t body_compiler, void *userdata);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
