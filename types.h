#pragma once
#include <intern.h>
#include <libgccjit.h>
#include <stdlib.h>

#include "ast.h"
#include "libblang/list.h"
#include "units.h"

typedef const struct bl_type_s bl_type_t;

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
        FileType,
        GeneratorType,
        StructType,
        TagType,
        UnionType,
        TaggedUnionType,
    } tag;

    union {
        struct {
        } UnknownType, AbortType, VoidType, BoolType, CharType, CStringCharType, FileType;
        struct {
            istr_t units;
            uint16_t bits;
            bool is_unsigned;
        } IntType;
        struct {
            istr_t units;
            uint16_t bits;
        } NumType;
        struct {
            bl_type_t *type;
        } TypeType;
        struct {} RangeType;
        struct {
            istr_t dsl;
            bl_type_t *item_type;
        } ArrayType;
        struct {
            bl_type_t *key_type, *value_type;
        } TableType;
        struct {
            List(istr_t) arg_names;
            List(bl_type_t*) arg_types;
            List(ast_t*) arg_defaults;
            bl_type_t *ret;
        } FunctionType;
        struct {
            bool is_optional;
            bl_type_t *pointed;
        } PointerType;
        struct {
            bl_type_t *generated;
        } GeneratorType;
        struct {
            istr_t name;
            List(istr_t) field_names;
            List(bl_type_t*) field_types;
            List(ast_t*) field_defaults;
            istr_t units;
        } StructType;
        struct {
            istr_t name;
            List(istr_t) names;
            List(int64_t) values;
        } TagType;
        struct {
            List(istr_t) field_names;
            List(bl_type_t*) field_types;
        } UnionType;
        struct {
            istr_t name;
            bl_type_t *tag_type;
            bl_type_t *data;
        } TaggedUnionType;
    } __data;
};

#define Type(typetag, ...) ((bl_type_t*)intern_bytes(&(bl_type_t){.tag=typetag, .__data.typetag={__VA_ARGS__}}, sizeof(bl_type_t)))
#define INT_TYPE Type(IntType, .bits=64)
#define NUM_TYPE Type(NumType, .bits=64)

istr_t type_to_string(bl_type_t *t);
bool type_is_a(bl_type_t *t, bl_type_t *req);
bl_type_t *type_or_type(bl_type_t *a, bl_type_t *b);
bool is_integral(bl_type_t *t);
bool is_numeric(bl_type_t *t);
int numtype_priority(bl_type_t *t);
istr_t type_units(bl_type_t *t);
bl_type_t *with_units(bl_type_t *t, istr_t units);
bool is_orderable(bl_type_t *t);
bool has_heap_memory(bl_type_t *t);
bool can_promote(bl_type_t *actual, bl_type_t *needed);
bool can_leave_uninitialized(bl_type_t *t);
bl_type_t *table_entry_type(bl_type_t *table_t);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
