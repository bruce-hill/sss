// A compilation environment representing contextual state
#pragma once

#include <libgccjit.h>
#include <setjmp.h>
#include <stdbool.h>

#include "ast.h"
#include "files.h"
#include "compile/libgccjit_abbrev.h"
#include "libblang/hashmap.h"
#include "libblang/list.h"
#include "types.h"
#include "units.h"

typedef struct env_s env_t;

typedef struct defer_s {
    ast_t *body;
    env_t *environment;
    struct defer_s *next;
} defer_t;

typedef struct loop_label_s {
    struct loop_label_s *enclosing;
    gcc_block_t *skip_label, *stop_label;
    List(const char*) names;
    defer_t *deferred;
} loop_label_t;

typedef struct {
    gcc_jit_rvalue *rval;
    gcc_jit_lvalue *lval;
    bl_type_t *type;
    const char* sym_name;
    union {
        gcc_jit_rvalue *tag_rval;
        gcc_jit_function *func;
    };
    bool is_constant:1;
} binding_t;

typedef struct conversions_s {
    struct conversions_s *next;
    bl_type_t *src, *dest;
    gcc_func_t *func;
} conversions_t;

typedef struct export_t {
    const char* qualified_name;
    binding_t *binding;
} export_t;

typedef struct env_s {
    gcc_ctx_t *ctx;
    bl_file_t *file;
    jmp_buf *on_err;
    bl_hashmap_t *bindings; // name -> binding_t
    bl_hashmap_t *global_bindings; // name -> binding_t
    List(export_t*) exports; // Ordered list of exports
    bl_hashmap_t *gcc_types; // name -> bl_type
    bl_hashmap_t *union_fields; // name -> [gcc_field]
    bl_hashmap_t *global_funcs; // name -> func
    bl_hashmap_t *type_namespaces; // bl_type -> name -> binding_t
    bl_hashmap_t *tuple_types; // const char* -> bl_type_t
    bl_type_t *return_type;
    loop_label_t *loop_label;
    derived_units_t *derived_units;
    conversions_t *conversions;
    void (*comprehension_callback)(struct env_s *env, gcc_block_t **block, ast_t *item, void *userdata);
    void *comprehension_userdata;
    defer_t *deferred;
    bool debug, is_deferred;
} env_t;

__attribute__((noreturn, format(printf,3,4)))
void compiler_err(env_t *env, ast_t *ast, const char *fmt, ...);

env_t *new_environment(gcc_ctx_t *ctx, jmp_buf *on_err, bl_file_t *f, bool debug);
env_t *fresh_scope(env_t *env);
env_t *global_scope(env_t *env);
binding_t *get_binding(env_t *env, const char *name);
binding_t *get_local_binding(env_t *env, const char *name);
gcc_func_t *get_function(env_t *env, const char *name);
binding_t *get_ast_binding(env_t *env, ast_t *ast);
env_t *get_type_env(env_t *env, bl_type_t *t);
bl_hashmap_t *get_namespace(env_t *env, bl_type_t *t);
binding_t *get_from_namespace(env_t *env, bl_type_t *t, const char *name);
void set_in_namespace(env_t *env, bl_type_t *t, const char *name, void *value);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
