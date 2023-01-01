#pragma once
#include <bp/files.h>
#include <setjmp.h>
#include "ast.h"
ast_t *parse(file_t *f, jmp_buf *on_err);
