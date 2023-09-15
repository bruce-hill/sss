#pragma once
#include <gc.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "compile/compile.h"
#include "types.h"

sss_type_t *parse_type_ast(env_t *env, ast_t *ast);
sss_type_t *get_type(env_t *env, ast_t *ast);
sss_type_t *get_field_type(env_t *env, sss_type_t *t, const char *field_name);
sss_type_t *get_math_type(env_t *env, ast_t *ast, sss_type_t *lhs_t, ast_tag_e tag, sss_type_t *rhs_t);
bool is_discardable(env_t *env, ast_t *ast);
const char *get_missing_pattern(env_t *env, sss_type_t *t, List(ast_t*) patterns);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
