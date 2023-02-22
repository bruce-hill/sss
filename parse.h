// Parse blang code
#pragma once

#include <setjmp.h>

#include "ast.h"
#include "files.h"

ast_t *parse_file(bl_file_t *file, jmp_buf *on_err);
ast_t *parse_type(bl_file_t *file, jmp_buf *on_err);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
