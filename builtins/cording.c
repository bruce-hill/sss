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
#include "cording.h"
#include "string.h"

#ifndef auto
#define auto __auto_type
#endif

CORD generic_cord(const void *obj, Cordable what, bool colorize)
{
    switch (what.tag) {
        case CordFunction:
            return what.__data.CordFunction.fn(obj, colorize);
        case CordNamed: {
            auto named = what.__data.CordNamed;
            return CORD_cat(named.name, generic_cord(obj, *named.base, colorize));
        }
        case CordPointer: {
            auto ptr_info = what.__data.CordPointer;
            void *ptr = *(void**)obj;
            if (ptr == NULL)
                return ptr_info.null_str;
            return CORD_cat(ptr_info.sigil, generic_cord(ptr, *ptr_info.pointed, colorize));
        }
        case CordArray: {
            auto item = what.__data.CordArray.item;
            CORD c = "[";
            string_t *s = (string_t*)obj;
            for (unsigned long i = 0; i < s->length; i++) {
                if (i > 0)
                    c = CORD_cat(c, ", ");
                CORD item_cord = generic_cord(&s->data[i*s->stride], *item, colorize);
                c = CORD_cat(c, item_cord);
            }
            c = CORD_cat(c, "]");
            return c;
        }
        case CordTable: {
            auto table = what.__data.CordTable;
            size_t entry_size = table.entry_size;
            size_t value_offset = table.value_offset;
            sss_hashmap_t *h = (sss_hashmap_t*)obj;
            CORD c = "{";
            for (uint32_t i = 1; i <= h->count; i++) {
                if (i > 1)
                    c = CORD_cat(c, ", ");
                void *entry = sss_hashmap_nth(h, i, entry_size);
                c = CORD_cat(c, generic_cord(entry, *table.key, colorize));
                c = CORD_cat(c, "=>");
                c = CORD_cat(c, generic_cord(entry + value_offset, *table.value, colorize));
            }

            if (h->fallback) {
                c = CORD_cat(c, "; fallback=");
                c = CORD_cat(c, generic_cord(h->fallback, what, colorize));
            }

            if (h->default_value) {
                c = CORD_cat(c, "; default=");
                c = CORD_cat(c, generic_cord(h->default_value, *table.value, colorize));
            }

            c = CORD_cat(c, "}");
            return c;
        }
    }
    return "";
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
