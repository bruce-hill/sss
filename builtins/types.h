#pragma once
#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>

#include "datatypes.h"
#include "string.h"

struct Type;

typedef uint32_t (*hash_fn_t)(const void*, const struct Type*);
typedef int32_t (*compare_fn_t)(const void*, const void*, const struct Type*);
typedef bool (*equal_fn_t)(const void*, const void*, const struct Type*);
typedef CORD (*cord_fn_t)(const void*, bool, const struct Type*);

typedef struct Type {
    const char *name;
    int64_t size, align;
    struct { // Anonymous tagged union for convenience 
        enum { CustomInfo, PointerInfo, ArrayInfo, TableInfo, StructInfo, TaggedUnionInfo } tag;
        union {
            struct {
                equal_fn_t equal;
                compare_fn_t compare;
                hash_fn_t hash;
                cord_fn_t cord;
            } CustomInfo;
            struct {
                const char *sigil;
                struct Type *pointed;
                bool cyclic;
            } PointerInfo;
            struct {
                struct Type *item;
            } ArrayInfo;
            struct {
                struct Type *key, *value;
                int64_t entry_size, value_offset;
            } TableInfo;
            struct {
                array_t members; // [{name, type}]
            } StructInfo;
            struct {
                array_t members; // [{tag, name, type}]
            } TaggedUnionInfo;
        };
    };
} Type;

uint32_t generic_hash(const void *obj, const Type *type);
int32_t generic_compare(const void *x, const void *y, const Type *type);
bool generic_equal(const void *x, const void *y, const Type *type);
CORD generic_cord(const void *obj, bool colorize, const Type *type);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
