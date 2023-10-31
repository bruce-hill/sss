#include <gc.h>
#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/param.h>

#include "table.h"
#include "array.h"

#ifndef auto
#define auto __auto_type
#endif

int32_t generic_compare(const void *x, const void *y, const Type *type)
{
    switch (type->order.tag) {
        case OrderingFunction:
            return type->order.__data.OrderingFunction.fn(x, y, type);
        case OrderingData:
            return memcmp(x, y, type->order.__data.OrderingData.size);
    }
    return 0;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
