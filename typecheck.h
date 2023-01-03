#pragma once
#include <gc.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <bhash.h>

#include "ast.h"
#include "compile/compile.h"
#include "types.h"

bl_type_t *parse_type_ast(env_t *env, ast_t *ast);
bl_type_t *get_type(env_t *env, ast_t *ast);
bool is_discardable(env_t *env, ast_t *ast);
void check_discardable(env_t *env, ast_t *ast);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
