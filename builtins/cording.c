#include <gc.h>
#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/param.h>

#include "../SipHash/halfsiphash.h"
#include "../files.h"
#include "string.h"
#include "types.h"

#ifndef auto
#define auto __auto_type
#endif

CORD generic_cord(const void *obj, bool colorize, const Type *type)
{
    switch (type->cord.tag) {
    case CordNotImplemented: {
        CORD c;
        CORD_sprintf(&c, "%s(?)", type->name);
        return c;
    }
    case CordFunction:
        return type->cord.__data.CordFunction.fn(obj, colorize, type);
    case CordNamed: {
        auto named = type->info.__data.NamedInfo;
        return CORD_cat(named.name, generic_cord(obj, colorize, named.base));
    }
    case CordPointer: {
        auto ptr_info = type->cord.__data.CordPointer;
        void *ptr = *(void**)obj;
        if (ptr == NULL)
            return ptr_info.null_str;
        return CORD_cat(ptr_info.sigil, generic_cord(ptr, colorize, type->info.__data.PointerInfo.pointed));
    }
    }
    return "";
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
