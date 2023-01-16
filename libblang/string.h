#pragma once
#include <stdint.h>

typedef struct {
    const char *data;
    int32_t length, stride;
} string_t;

string_t bl_string_upper(string_t s);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
