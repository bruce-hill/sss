#pragma once
#include <stdbool.h>
#include <stdint.h>
#include <gc/cord.h>

#include "string.h"

typedef struct {
    int64_t first, stride, last;
} range_t;

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
