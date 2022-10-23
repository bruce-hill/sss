#include <gc.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "ast.h"

static inline bl_type_t *new_type(typekind_e kind, ...) {
    va_list argp;
    va_start(argp, kind);
    bl_type_t *ret = GC_MALLOC(sizeof(bl_type_t));
    ret->kind = kind;
    switch (kind) {
    case NamedType: ret->name = va_arg(argp, const char*); break;
    case ListType: ret->item_type = va_arg(argp, bl_type_t*); break;
    case OptionalType: ret->nonnil = va_arg(argp, bl_type_t*); break;
    case TableType: {
        ret->key = va_arg(argp, bl_type_t*);
        ret->value = va_arg(argp, bl_type_t*);
        break;
    }
    case FunctionType: {
        ret->nargs = va_arg(argp, size_t);
        ret->args = va_arg(argp, bl_type_t**);
        ret->ret = va_arg(argp, bl_type_t*);
        break;
    }
    default: return NULL;
    }
    return ret;
}

bl_type_t *get_type(ast_t *ast)
{
    switch (ast->kind) {
        case Int: return new_type(NamedType, "Int");
        default: break;
    }
    return NULL;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
