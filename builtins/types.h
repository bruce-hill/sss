#pragma once
#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>

#include "string.h"

typedef struct {
    const char *name, *type;
    void *value;
} NamespaceBinding;

struct Type;

typedef struct {
    enum { NoInfo, NamedInfo, PointerInfo, ArrayInfo, TableInfo } tag;
    union {
        struct {} NoInfo;
        struct {
            const char *name;
            struct Type *base;
        } NamedInfo;
        struct {
            const char *sigil;
            struct Type *pointed;
        } PointerInfo;
        struct {
            struct Type *item;
        } ArrayInfo;
        struct {
            struct Type *key, *value;
            size_t entry_size, value_offset;
        } TableInfo;
    } __data;
} TypeInfo;

typedef uint32_t (*hash_fn_t)(const void*, const struct Type*);
typedef int32_t (*compare_fn_t)(const void*, const void*, const struct Type*);
typedef bool (*equal_fn_t)(const void*, const void*, const struct Type*);
typedef CORD (*cord_fn_t)(const void*, bool, const struct Type*);

typedef struct Type {
    String_t name;
    TypeInfo info;
    size_t size, align;
    equal_fn_t equal;
    compare_fn_t compare;
    hash_fn_t hash;
    cord_fn_t cord;
    NamespaceBinding *bindings;
} Type;

uint32_t hash_data(const void *obj, const Type *type);
int32_t compare_data(const void *x, const void *y, const Type *type);
bool equal_data(const void *x, const void *y, const Type *type);
Type *make_type(const char *name, size_t size, size_t align, void *compare_fn, void *equal_fn, void *hash_fn, void *cord_fn, NamespaceBinding *bindings);
Type *make_named_type(const char *name, Type *t);
Type *make_pointer_type(const char *sigil, Type *t);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
