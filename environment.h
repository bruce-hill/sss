// A compilation environment representing contextual state
#pragma once

#include <bhash.h>
#include <intern.h>
#include <libgccjit.h>
#include <setjmp.h>
#include <stdbool.h>

#include "ast.h"
#include "files.h"
#include "compile/libgccjit_abbrev.h"
#include "libblang/list.h"
#include "types.h"

typedef struct loop_label_s {
    struct loop_label_s *enclosing;
    gcc_block_t *skip_label, *stop_label;
    List(istr_t) names;
    unsigned int skip_reachable, stop_reachable;
} loop_label_t;

typedef struct {
    gcc_jit_rvalue *rval;
    gcc_jit_lvalue *lval;
    bl_type_t *type;
    union {
        struct {
            bl_type_t *type_value, *enum_type;
            gcc_jit_rvalue *tag_rval;
        };
        gcc_jit_function *func;
    };
    bool is_global:1, is_constant:1;
} binding_t;

typedef struct env_s {
    gcc_ctx_t *ctx;
    bl_file_t *file;
    jmp_buf *on_err;
    hashmap_t *print_funcs; // type -> func
    hashmap_t *cmp_funcs; // type -> func
    hashmap_t *bindings; // name -> binding_t
    hashmap_t *gcc_types; // name -> bl_type
    hashmap_t *global_funcs; // name -> func
    hashmap_t *type_namespaces; // bl_type -> name -> binding_t
    hashmap_t *tuple_types; // istr_t -> bl_type_t
    bl_type_t *return_type;
    loop_label_t *loop_label;
    void (*comprehension_callback)(struct env_s *env, gcc_block_t **block, ast_t *item, void *userdata);
    void *comprehension_userdata;
    bool debug;
} env_t;

__attribute__((noreturn, format(printf,3,4)))
void compile_err(env_t *env, ast_t *ast, const char *fmt, ...);

env_t *new_environment(gcc_ctx_t *ctx, jmp_buf *on_err, bl_file_t *f, bool debug);
binding_t *get_binding(env_t *env, const char *name);
binding_t *get_ast_binding(env_t *env, ast_t *ast);
hashmap_t *get_namespace(env_t *env, bl_type_t *t);
binding_t *get_from_namespace(env_t *env, bl_type_t *t, const char *name);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
