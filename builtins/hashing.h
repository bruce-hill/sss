#pragma once

#include <stdbool.h>
#include <stdint.h>

typedef struct Hashable {
    enum { HashFunction, HashData, HashArray, HashTable } tag;
    union {
        struct {
            uint32_t (*fn)(const void*);
        } HashFunction;
        struct {
            size_t size;
        } HashData;
        struct {
            struct Hashable *item;
        } HashArray;
        struct {
            size_t entry_size, value_offset;
            struct Hashable *key, *value;
        } HashTable;
    } __data;
} Hashable;

#define HashMethod(hash_tag, ...) ((Hashable){.tag=Hash##hash_tag, .__data.Hash##hash_tag={__VA_ARGS__}})

uint32_t generic_hash(const void *obj, Hashable *what);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
