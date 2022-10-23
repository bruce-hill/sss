#include <gc.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <bhash.h>

#include "types.h"
#include "ast.h"

bl_type_t *get_type(hashmap_t *bindings, ast_t *ast);

const char *get_base_type(hashmap_t *bindings, ast_t *ast);
const char *get_abi_type(hashmap_t *bindings, ast_t *ast);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
