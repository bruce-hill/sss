#include <gc.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <bhash.h>
#include <bp/files.h>

#include "types.h"
#include "ast.h"

bl_type_t *get_type(file_t *f, hashmap_t *bindings, ast_t *ast);
void check_discardable(file_t *f, hashmap_t *bindings, ast_t *ast);
const char *base_type_for(bl_type_t *t);
const char *abi_type_for(bl_type_t *t);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
