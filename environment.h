// A compilation environment representing contextual state
#pragma once

#include <libgccjit.h>
#include <setjmp.h>
#include <stdbool.h>

#include "ast.h"
#include "files.h"
#include "compile/libgccjit_abbrev.h"
#include "builtins/table.h"
#include "builtins/array.h"
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
    ARRAY_OF(const char*) names;
    defer_t *deferred;
} loop_label_t;

typedef struct {
    gcc_jit_rvalue *rval;
    gcc_jit_lvalue *lval;
    sss_type_t *type;
    const char* sym_name;
    union {
        gcc_jit_rvalue *tag_rval;
        gcc_jit_function *func;
    };
    bool is_constant:1;
    bool visible_in_closures:1;
} binding_t;

typedef struct {
    table_t bindings; // name -> binding_t*
    table_t funcs; // name -> func
    table_t type_namespaces; // type_string -> member_name -> binding_t*
    table_t type_lvals; // type_string -> gcc_lvalue_t*
    table_t def_types; // ast_t* -> binding_t*
    table_t ast_functions; // ast_t* -> func_context_t*
    table_t module_types; // inode -> sss_type_t*
} global_env_t;

typedef struct env_s {
    global_env_t *global;
    gcc_ctx_t *ctx;
    sss_file_t *file;
    jmp_buf *on_err;
    table_t *file_bindings; // name -> binding_t*
    table_t *bindings; // name -> binding_t
    sss_type_t *return_type;
    loop_label_t *loop_label;
    derived_units_t *derived_units;
    void (*comprehension_callback)(struct env_s *env, gcc_block_t **block, ast_t *item, void *userdata);
    void *comprehension_userdata;
    defer_t *deferred;
    bool tail_calls:1, is_deferred:1, should_mark_cow:1;
} env_t;

typedef struct {
    gcc_func_t* func;
    env_t env;
} func_context_t;

__attribute__((noreturn, format(printf,3,4)))
void compiler_err(env_t *env, ast_t *ast, const char *fmt, ...);

env_t *new_environment(gcc_ctx_t *ctx, jmp_buf *on_err, sss_file_t *f, bool tail_calls);
env_t *fresh_scope(env_t *env);
env_t *file_scope(env_t *env);
env_t *scope_with_type(env_t *env, sss_type_t *t);
binding_t *get_binding(env_t *env, const char *name);
sss_type_t *get_type_by_name(env_t *env, const char *name);
binding_t *get_local_binding(env_t *env, const char *name);
gcc_func_t *get_function(env_t *env, const char *name);
gcc_func_t *import_function(env_t *env, const char *name, sss_type_t *fn_t, bool keep);
binding_t *get_ast_binding(env_t *env, ast_t *ast);
env_t *get_type_env(env_t *env, sss_type_t *t);
table_t *get_namespace(env_t *env, sss_type_t *t);
binding_t *get_from_namespace(env_t *env, sss_type_t *t, const char *name);
void set_in_namespace(env_t *env, sss_type_t *t, const char *name, void *value);
const char *spellcheck(table_t *ns, const char *word);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
