#pragma once

#include <stdbool.h>
#include <stdint.h>

typedef struct Comparable {
    enum { CompareFunction, CompareData, CompareArray, CompareTable } tag;
    union {
        struct {
            int32_t (*fn)(const void*, const void*);
        } CompareFunction;
        struct {
            size_t size;
        } CompareData;
        struct {
            struct Comparable *item;
        } CompareArray;
        struct {
            size_t entry_size, value_offset;
            struct Comparable *key, *value;
        } CompareTable;
    } __data;
} Comparable;

#define CompareMethod(compare_tag, ...) ((Comparable){.tag=Compare##compare_tag, .__data.Compare##compare_tag={__VA_ARGS__}})

int32_t generic_compare(const void *x, const void *y, Comparable *what);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
