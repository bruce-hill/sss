#pragma once
#include <libgccjit.h>
#include <stdlib.h>

#include "ast.h"
#include "libblang/list.h"
#include "units.h"

typedef const struct bl_type_s bl_type_t;

typedef struct {
    const char *name;
    int64_t tag_value;
    bl_type_t *type;
} bl_tagged_union_member_t;

struct bl_type_s {
    enum {
        UnknownType,
        AbortType, VoidType,
        BoolType, CharType, CStringCharType,
        IntType, NumType,
        TypeType,
        RangeType,
        ArrayType,
        TableType,
        FunctionType,
        PointerType,
        GeneratorType,
        StructType,
        TaggedUnionType,
        ModuleType,
    } tag;

    union {
        struct {
        } UnknownType, AbortType, VoidType, BoolType, CharType, CStringCharType;
        struct {
            const char* units;
            uint16_t bits;
            bool is_unsigned;
        } IntType;
        struct {
            const char* units;
            uint16_t bits;
        } NumType;
        struct {
            bl_type_t *type;
        } TypeType;
        struct {} RangeType;
        struct {
            const char* dsl;
            bl_type_t *item_type;
        } ArrayType;
        struct {
            bl_type_t *key_type, *value_type;
        } TableType;
        struct {
            List(const char*) arg_names;
            List(bl_type_t*) arg_types;
            List(ast_t*) arg_defaults;
            bl_type_t *ret;
            void *env;
        } FunctionType;
        struct {
            bool is_optional;
            bl_type_t *pointed;
        } PointerType;
        struct {
            bl_type_t *generated;
        } GeneratorType;
        struct {
            const char* name;
            List(const char*) field_names;
            List(bl_type_t*) field_types;
            List(ast_t*) field_defaults;
            const char* units;
        } StructType;
        struct {
            const char* name;
            List(bl_tagged_union_member_t) members;
        } TaggedUnionType;
        struct {
            const char *path;
        } ModuleType;
    } __data;
};

#define Type(typetag, ...) new(bl_type_t, .tag=typetag, .__data.typetag={__VA_ARGS__})
#define INT_TYPE Type(IntType, .bits=64)
#define NUM_TYPE Type(NumType, .bits=64)

const char* type_to_string(bl_type_t *t);
bool type_eq(bl_type_t *a, bl_type_t *b);
bool type_is_a(bl_type_t *t, bl_type_t *req);
bl_type_t *type_or_type(bl_type_t *a, bl_type_t *b);
bl_type_t *value_type(bl_type_t *a);
bool is_integral(bl_type_t *t);
bool is_numeric(bl_type_t *t);
int numtype_priority(bl_type_t *t);
const char* type_units(bl_type_t *t);
bl_type_t *with_units(bl_type_t *t, const char* units);
bool is_orderable(bl_type_t *t);
bool has_heap_memory(bl_type_t *t);
bool can_promote(bl_type_t *actual, bl_type_t *needed);
bool can_leave_uninitialized(bl_type_t *t);
bl_type_t *table_entry_type(bl_type_t *table_t);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
