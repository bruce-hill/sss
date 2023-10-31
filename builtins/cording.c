#include <gc.h>
#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/param.h>

#include "../SipHash/halfsiphash.h"
#include "../files.h"
#include "../span.h"
#include "../libsss/hashmap.h"
#include "string.h"
#include "types.h"

#ifndef auto
#define auto __auto_type
#endif

CORD generic_cord(const Type *type, const void *obj, bool colorize)
{
    switch (type->cord.tag) {
    case CordNotImplemented: {
        CORD c;
        CORD_sprintf(&c, "%s(?)", type->name);
        return c;
    }
    case CordFunction:
        return type->cord.__data.CordFunction.fn(type, obj, colorize);
    case CordNamed: {
        auto named = type->info.__data.NamedInfo;
        return CORD_cat(named.name, generic_cord(named.base, obj, colorize));
    }
    case CordPointer: {
        auto ptr_info = type->cord.__data.CordPointer;
        void *ptr = *(void**)obj;
        if (ptr == NULL)
            return ptr_info.null_str;
        return CORD_cat(ptr_info.sigil, generic_cord(type->info.__data.PointerInfo.pointed, ptr, colorize));
    }
    }
    return "";
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
