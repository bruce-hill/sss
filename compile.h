#pragma once
#include "ast.h"
gcc_jit_result *compile_file(gcc_jit_context *ctx, file_t *f, ast_t *ast, bool debug);
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
