#include <gc.h>
#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/param.h>

#include "../SipHash/halfsiphash.h"
#include "table.h"
#include "types.h"
#include "string.h"

#ifndef auto
#define auto __auto_type
#endif

extern const void *SSS_HASH_VECTOR;

uint32_t generic_hash(const void *obj, const Type *type)
{
    switch (type->hash.tag) {
    case HashNotImplemented:
        return 0;
    case HashFunction:
        return type->hash.__data.HashFunction.fn(obj, type);
    case HashData: {
        uint32_t hash;
        halfsiphash((void*)obj, type->hash.__data.HashData.size, SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
        return hash;
    }
    }
    return 0;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
