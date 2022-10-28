#pragma once
#include <bp/files.h>
#include "ast.h"
ast_t *parse(file_t *f);
void print_ast(ast_t *ast);
