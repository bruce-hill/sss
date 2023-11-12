// Generic type constructor
#include <err.h>
#include <gc.h>
#include <string.h>
#include <stdalign.h>

#include "array.h"
#include "table.h"
#include "types.h"
#include "../util.h"
#include "../SipHash/halfsiphash.h"

extern const void *SSS_HASH_VECTOR;

uint32_t generic_hash(const void *obj, const Type *type)
{
    switch (type->tag) {
    case ArrayInfo: return Array_hash(obj, type);
    case TableInfo: return Table_hash(obj, type);
    case VTableInfo:
        if (type->__data.VTableInfo.hash)
            return type->__data.VTableInfo.hash(obj, type);
        // fallthrough
    default: {
        uint32_t hash;
        halfsiphash((void*)obj, type->size, SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
        return hash;
    }
    }
}

int32_t generic_compare(const void *x, const void *y, const Type *type)
{
    switch (type->tag) {
    case ArrayInfo: return Array_compare(x, y, type);
    case TableInfo: return Table_compare(x, y, type);
    case VTableInfo:
        if (type->__data.VTableInfo.compare)
            return type->__data.VTableInfo.compare(x, y, type);
        // fallthrough
    default: return (int32_t)memcmp((void*)x, (void*)y, type->size);
    }
}

bool generic_equal(const void *x, const void *y, const Type *type)
{
    switch (type->tag) {
    case ArrayInfo: return Array_equal(x, y, type);
    case TableInfo: return Table_equal(x, y, type);
    case VTableInfo:
        if (type->__data.VTableInfo.equal)
            return type->__data.VTableInfo.equal(x, y, type);
        // fallthrough
    default: return (generic_compare(x, y, type) == 0);
    }
}

CORD generic_cord(const void *obj, bool colorize, const Type *type)
{
    switch (type->tag) {
    case PointerInfo: {
        auto ptr_info = type->__data.PointerInfo;
        void *ptr = *(void**)obj;
        CORD c;
        if (ptr == NULL) {
            CORD_sprintf(&c, colorize ? "\x1b[34;1m!%s\x1b[m" : "!%s", ptr_info.pointed->name);
        } else {
            CORD_sprintf(&c, colorize ? "\x1b[34;1m%s%r\x1b[m" : "%s%r",
                         ptr_info.sigil, generic_cord(ptr, colorize, ptr_info.pointed));
        }
        return c;
    }
    case ArrayInfo: return Array_cord(obj, colorize, type);
    case TableInfo: return Table_cord(obj, colorize, type);
    case VTableInfo: return type->__data.VTableInfo.cord(obj, colorize, type);
    default: errx(1, "Unreachable");
    }
}

int32_t compare_data(const void *x, const void *y, const Type *type)
{
    return memcmp((void*)x, (void*)y, type->size);
}

bool equal_data(const void *x, const void *y, const Type *type)
{
    return (memcmp((void*)x, (void*)y, type->size) == 0);
}

static CORD Type_cord(Type **t, bool colorize, const Type *typetype)
{
    (void)typetype;
    if (!colorize)
        return CORD_from_char_star((*t)->name);
    CORD c;
    CORD_sprintf(&c, "\x1b[36;1m%s\x1b[m", (*t)->name);
    return c;
}

Type Type_type = {
    .name="Type",
    .tag=VTableInfo,
    .__data.VTableInfo={.cord=(void*)Type_cord},
};

Type *make_type(const char *name, size_t size, size_t align, void *compare_fn, void *equal_fn, void *hash_fn, void *cord_fn)
{
    return new(Type,
        .name=name,
        .size=size,
        .align=align,
        .tag=VTableInfo,
        .__data.VTableInfo={
            .compare=compare_fn,
            .equal=equal_fn,
            .hash=hash_fn,
            .cord=cord_fn,
        },
    );
}

Type *make_pointer_type(const char *sigil, Type *t)
{
    return new(Type,
        .name=heap_strf("%s%s", sigil, t->name),
        .size=sizeof(void*),
        .align=alignof(void*),
        .tag=PointerInfo,
        .__data.PointerInfo={sigil, t},
    );
}

Type *make_named_type(const char *name, Type *t)
{
    return new(Type,
        .name=name,
        .size=t->size,
        .align=t->align,
        .tag=t->tag,
        .__data=t->__data,
    );
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
