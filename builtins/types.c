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
        if (!type->VTableInfo.hash)
            goto hash_data;
        return type->VTableInfo.hash(obj, type);
    case PointerInfo:
    default: {
      hash_data:;
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
        if (!type->VTableInfo.compare)
            goto compare_data;
        return type->VTableInfo.compare(x, y, type);
    case PointerInfo:
    default:
      compare_data:
        return (int32_t)memcmp((void*)x, (void*)y, type->size);
    }
}

public bool generic_equal(const void *x, const void *y, const Type *type)
{
    switch (type->tag) {
    case ArrayInfo: return Array_equal(x, y, type);
    case TableInfo: return Table_equal(x, y, type);
    case VTableInfo:
        if (!type->VTableInfo.equal)
            goto use_generic_compare;
        return type->VTableInfo.equal(x, y, type);
    case PointerInfo:
    default:
      use_generic_compare:
        return (generic_compare(x, y, type) == 0);
    }
}

public CORD generic_cord(const void *obj, bool colorize, const Type *type)
{
    switch (type->tag) {
    case PointerInfo: {
        auto ptr_info = type->PointerInfo;
        static Type ptr_type = {.name="@Memory", .size=sizeof(void*), .align=alignof(void*)};
        static Type int_type = {.name="Int", .size=sizeof(int64_t), .align=alignof(int64_t)};
        static Type ptr_to_int_type = {
            .name="{@Memory=>Int}",
            .size=sizeof(table_t),
            .align=alignof(table_t),
            .tag=TableInfo,
            .TableInfo={.key=&ptr_type, .value=&int_type, .entry_size=16, .value_offset=8},
        };

        static int64_t zero = 0;
        static table_t recursion = {.default_value=&zero};

        CORD c;
        if (ptr_info.cyclic) {
            int64_t *found = Table_set(&recursion, obj, NULL, &ptr_to_int_type);
            if (*found) {
                CORD_sprintf(&c, colorize ? "\x1b[34;1m!%s\x1b[m" : "!%s", ptr_info.pointed->name);
                return c;
            } else {
                *found = 1;
            }
        }

        void *ptr = *(void**)obj;
        if (ptr == NULL) {
            CORD_sprintf(&c, colorize ? "\x1b[34;1m!%s\x1b[m" : "!%s", ptr_info.pointed->name);
        } else {
            CORD_sprintf(&c, colorize ? "\x1b[34;1m%s\x1b[m%r" : "%s%r",
                         ptr_info.sigil, generic_cord(ptr, colorize, ptr_info.pointed));
        }
        if (ptr_info.cyclic) {
            Table_remove(&recursion, obj, &ptr_to_int_type);
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
