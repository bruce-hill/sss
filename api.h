#pragma once
#include "ast.h"
// Logic for generating a .sss.api file from a .sss file:

void generate_api_file(const char *sss_filename, ast_t *ast);


// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
