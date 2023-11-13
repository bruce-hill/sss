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

public uint32_t generic_hash(const void *obj, const Type *type)
{
    switch (type->tag) {
    case ArrayInfo: return Array_hash(obj, type);
    case TableInfo: return Table_hash(obj, type);
    case VTableInfo:
        if (type->VTableInfo.hash)
            return type->VTableInfo.hash(obj, type);
        // fallthrough
    default: {
        uint32_t hash;
        halfsiphash((void*)obj, type->size, SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
        return hash;
    }
    }
}

public int32_t generic_compare(const void *x, const void *y, const Type *type)
{
    switch (type->tag) {
    case ArrayInfo: return Array_compare(x, y, type);
    case TableInfo: return Table_compare(x, y, type);
    case VTableInfo:
        if (type->VTableInfo.compare)
            return type->VTableInfo.compare(x, y, type);
        // fallthrough
    default: return (int32_t)memcmp((void*)x, (void*)y, type->size);
    }
}

public bool generic_equal(const void *x, const void *y, const Type *type)
{
    switch (type->tag) {
    case ArrayInfo: return Array_equal(x, y, type);
    case TableInfo: return Table_equal(x, y, type);
    case VTableInfo:
        if (type->VTableInfo.equal)
            return type->VTableInfo.equal(x, y, type);
        // fallthrough
    default: return (generic_compare(x, y, type) == 0);
    }
}

public CORD generic_cord(const void *obj, bool colorize, const Type *type)
{
    switch (type->tag) {
    case PointerInfo: {
        auto ptr_info = type->PointerInfo;
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
    case VTableInfo: return type->VTableInfo.cord(obj, colorize, type);
    default: errx(1, "Invalid type tag: %d", type->tag);
    }
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

public Type Type_type = {
    .name="Type",
    .tag=VTableInfo,
    .VTableInfo={.cord=(void*)Type_cord},
};

public Type *make_type(const char *name, size_t size, size_t align, void *compare_fn, void *equal_fn, void *hash_fn, void *cord_fn)
{
    return new(Type,
        .name=name,
        .size=size,
        .align=align,
        .tag=VTableInfo,
        .VTableInfo={
            .compare=compare_fn,
            .equal=equal_fn,
            .hash=hash_fn,
            .cord=cord_fn,
        },
    );
}

public Type Void_type = {.name="Void", .size=0, .align=0};
public Type Abort_type = {.name="Abort", .size=0, .align=0};

public Type *make_pointer_type(const char *sigil, Type *t)
{
    return new(Type,
        .name=heap_strf("%s%s", sigil, t->name),
        .size=sizeof(void*),
        .align=alignof(void*),
        .tag=PointerInfo,
        .PointerInfo={sigil, t},
    );
}

public Type *make_named_type(const char *name, Type *t)
{
    Type *named = GC_MALLOC_ATOMIC(sizeof(Type));
    *named = *t;
    named->name = name;
    return named;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
