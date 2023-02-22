#pragma once
#include <stdint.h>

#include "../environment.h"

typedef struct {
    const char *name, *signature, *symbol;
    bool needs_deref;
    const char *docs;
} import_t;

typedef struct {
    import_t *imports;
    int32_t len, stride;
} import_list_t;

typedef import_list_t (load_fn_t)(void);

void load_module(env_t *env, gcc_block_t **block, ast_t *use);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
