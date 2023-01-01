#pragma once
#include <bhash.h>
#include <intern.h>
#include <libgccjit.h>
#include <stdlib.h>

#include "ast.h"
#include "libblang/list.h"

typedef const struct bl_type_s bl_type_t;

struct bl_type_s {
    enum {
        UnknownType,
        AbortType, VoidType,
        BoolType, CharType,
        IntType, Int32Type, Int16Type, Int8Type,
        NumType, Num32Type,
        DSLType,
        NamedType,
        TypeType,
        RangeType,
        ArrayType,
        TableType,
        FunctionType,
        PointerType,
        GeneratorType,
        StructType,
        TagType,
        TaggedUnionType,
        UnionType,
    } tag;

    union {
        struct {
        } UnknownType, AbortType, VoidType, BoolType, CharType, IntType,
            Int32Type, Int16Type, Int8Type, NumType, Num32Type;
        struct {
            istr_t name;
        } DSLType, NamedType;
        struct {
        } TypeType, RangeType;
        struct {
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
        } StructType;
        struct {
            istr_t name;
            List(istr_t) names;
            List(int64_t) values;
        } TagType;
        struct {
            istr_t name;
            bl_type_t *tag_type;
            bl_type_t *data;
        } TaggedUnionType;
        struct {
            List(istr_t) field_names;
            List(bl_type_t*) field_types;
            List(gcc_jit_field*) fields;
        } UnionType;
    } __data;
};

#define Type(typetag, ...) ((bl_type_t*)intern_bytes(&(bl_type_t){.tag=typetag, .__data.typetag={__VA_ARGS__}}, sizeof(bl_type_t)))

istr_t type_to_string(bl_type_t *t);
bool type_is_a(bl_type_t *t, bl_type_t *req);
bl_type_t *type_or_type(bl_type_t *a, bl_type_t *b);
bool is_integral(bl_type_t *t);
bool is_numeric(bl_type_t *t);
int numtype_priority(bl_type_t *t);
bool has_pointer(bl_type_t *t);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
