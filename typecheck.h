#pragma once
#include <gc.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "compile/compile.h"
#include "builtins/array.h"
#include "types.h"

sss_type_t *parse_type_ast(env_t *env, ast_t *ast);
sss_type_t *get_type(env_t *env, ast_t *ast);
sss_type_t *get_doctest_type(env_t *env, ast_t *ast);
sss_type_t *get_field_type(env_t *env, sss_type_t *t, const char *field_name);
sss_type_t *get_math_type(env_t *env, ast_t *ast, sss_type_t *lhs_t, ast_tag_e tag, sss_type_t *rhs_t);
bool is_discardable(env_t *env, ast_t *ast);
const char *get_missing_pattern(env_t *env, sss_type_t *t, ARRAY_OF(ast_t*) patterns);
sss_type_t *get_namespace_type(env_t *env, ast_t *namespace_ast, sss_type_t *type);
const char *get_module_name(const char *path);
sss_type_t *get_file_type(env_t *env, const char *path);
sss_type_t *get_iter_type(env_t *env, ast_t *iter);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
