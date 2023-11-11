// Generic type constructor
#include <gc.h>
#include <string.h>
#include <stdalign.h>

#include "types.h"
#include "../util.h"
#include "../SipHash/halfsiphash.h"

extern const void *SSS_HASH_VECTOR;

uint32_t hash_data(const void *obj, const Type *type)
{
    uint32_t hash;
    halfsiphash((void*)obj, type->size, SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
    return hash;
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
    .cord=(void*)Type_cord,
    .hash=hash_data,
    .compare=compare_data,
    .equal=equal_data,
    .bindings=(NamespaceBinding[]){{0}},
};

Type *make_type(const char *name, size_t size, size_t align, void *compare_fn, void *equal_fn, void *hash_fn, void *cord_fn, NamespaceBinding *bindings)
{
    return new(Type,
        .name=name,
        .info={NoInfo},
        .size=size,
        .align=align,
        .compare=compare_fn,
        .equal=equal_fn,
        .hash=hash_fn,
        .cord=cord_fn,
        .bindings=bindings,
    );
}

CORD Pointer_cord(const void **obj, bool colorize, Type *type)
{
    auto ptr_info = type->info.__data.PointerInfo;
    void *ptr = *(void**)obj;
    CORD c;
    if (ptr == NULL) {
        CORD_sprintf(&c, colorize ? "\x1b[34;1m!%s\x1b[m" : "!%s", ptr_info.pointed->name);
    } else {
        CORD_sprintf(&c, colorize ? "\x1b[34;1m%s%r\x1b[m" : "%s%r",
                     ptr_info.sigil, ptr_info.pointed->cord(ptr, colorize, ptr_info.pointed));
    }
    return c;
}

Type *make_pointer_type(const char *sigil, Type *t)
{
    return new(Type,
        .name=heap_strf("%s%s", sigil, t->name),
        .info={NoInfo},
        .size=sizeof(void*),
        .align=alignof(void*),
        .compare=compare_data,
        .equal=equal_data,
        .hash=hash_data,
        .cord=(void*)Pointer_cord,
    );
}

CORD Named_cord(const void *obj, bool colorize, Type *type)
{
    auto info = type->info.__data.NamedInfo;
    CORD c = info.base->cord(obj, colorize, info.base);
    CORD_sprintf(&c, colorize ? "\x1b[32;1m%s::\x1b[m%r" : "%s::%r", type->name, c);
    return c;
}

Type *make_named_type(const char *name, Type *t)
{
    return new(Type,
        .name=name,
        .info={NamedInfo, .__data.NamedInfo={name, t}},
        .size=t->size,
        .align=t->align,
        .compare=t->compare,
        .equal=t->equal,
        .hash=t->hash,
        .cord=(void*)Named_cord,
    );
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
