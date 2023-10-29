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
        return type->cord.__data.CordFunction.fn(obj, colorize);
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
    case CordArray: {
        Type *item_type = type->info.__data.ArrayInfo.item;
        CORD c = "[";
        string_t *s = (string_t*)obj;
        for (unsigned long i = 0; i < s->length; i++) {
            if (i > 0)
                c = CORD_cat(c, ", ");
            CORD item_cord = generic_cord(item_type, &s->data[i*s->stride], colorize);
            c = CORD_cat(c, item_cord);
        }
        c = CORD_cat(c, "]");
        return c;
    }
    case CordTable: {
        auto table = type->info.__data.TableInfo;
        size_t entry_size = table.entry_size;
        size_t value_offset = table.value_offset;
        sss_hashmap_t *h = (sss_hashmap_t*)obj;
        CORD c = "{";
        for (uint32_t i = 1; i <= h->count; i++) {
            if (i > 1)
                c = CORD_cat(c, ", ");
            void *entry = sss_hashmap_nth(h, i, entry_size);
            c = CORD_cat(c, generic_cord(table.key, entry, colorize));
            c = CORD_cat(c, "=>");
            c = CORD_cat(c, generic_cord(table.value, entry + value_offset, colorize));
        }

        if (h->fallback) {
            c = CORD_cat(c, "; fallback=");
            c = CORD_cat(c, generic_cord(type, h->fallback, colorize));
        }

        if (h->default_value) {
            c = CORD_cat(c, "; default=");
            c = CORD_cat(c, generic_cord(table.value, h->default_value, colorize));
        }

        c = CORD_cat(c, "}");
        return c;
    }
    }
    return "";
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
