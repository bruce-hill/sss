#include <gc.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "ast.h"
#include "util.h"

static inline bl_type_t *new_type(bl_type_t init) {
    bl_type_t *ret = new(bl_type_t);
    *ret = init;
    return ret;
}
#define TYPE(mykind, ...) (new_type((bl_type_t){.kind=mykind, __VA_ARGS__}))

bl_type_t *get_type(ast_t *ast)
{
    switch (ast->kind) {
        case Nil: return TYPE(NamedType, .name="Nil");
        case Bool: return TYPE(NamedType, .name="Bool");
        case Int: return TYPE(NamedType, .name="Int");
        case Num: return TYPE(NamedType, .name="Num");
        default: break;
    }
    return NULL;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
