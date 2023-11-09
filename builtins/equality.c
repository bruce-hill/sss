#include <gc.h>
#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/param.h>

#include "table.h"
#include "string.h"

#ifndef auto
#define auto __auto_type
#endif

bool generic_equals(const void *x, const void *y, const Type *type)
{
    switch (type->equality.tag) {
        case EqualityComparison:
            return (generic_compare(x, y, type) == 0);
        case EqualityFunction:
            return type->equality.__data.EqualityFunction.fn(x, y, type);
        case EqualityData:
            return memcmp(x, y, type->equality.__data.EqualityData.size);
    }
    return 0;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
