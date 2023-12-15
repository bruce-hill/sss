// Logic for applying variable (and type) bindings
#pragma once

#include "ast.h"
#include "environment.h"
#include "builtins/table.h"

void bind_types(env_t *env, table_t *bindings, ast_t *ast);
void populate_types(env_t *env, table_t *bindings, ast_t *ast);
void bind_variables(env_t *env, table_t *bindings, ast_t *ast);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
