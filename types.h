#pragma once
#include <bhash.h>
#include <intern.h>
#include <libgccjit.h>
#include <stdlib.h>

#include "libblang/list.h"

typedef enum {
    UnknownType,
    AbortType, VoidType,
    BoolType,
    IntType, Int32Type, Int16Type, Int8Type,
    NumType, Num32Type,
    StringType, DSLType,
    NamedType,
    TypeType,
    RangeType,
    ListType,
    TableType,
    FunctionType,
    OptionalType,
    StructType,
    TaggedUnionType,
    UnionType,
} typekind_e;

typedef const struct bl_type_s {
    typekind_e kind;
    union {
        istr_t name;
        const struct bl_type_s *item_type;
        const struct bl_type_s *nonnil;
        struct {
            const struct bl_type_s *key, *value;
        };
        struct {
            List(const struct bl_type_s*) args;
            const struct bl_type_s *ret;
        };
        struct {
            istr_t name;
            List(istr_t) field_names;
            List(const struct bl_type_s*) field_types;
        } struct_;
        struct {
            istr_t name;
            List(istr_t) tag_names;
            List(int64_t) tag_values;
            const struct bl_type_s *data;
        } tagged;
        struct {
            List(istr_t) field_names;
            List(const struct bl_type_s*) field_types;
            List(gcc_jit_field*) fields;
        } union_;
    };
} bl_type_t;

typedef struct {
    gcc_jit_rvalue *rval;
    gcc_jit_lvalue *lval;
    bl_type_t *type;
    union {
        struct {
            bl_type_t *type_value, *enum_type;
            hashmap_t *namespace;
        };
        gcc_jit_function *func;
    };
    bool is_global:1, is_constant:1;
} binding_t;

#define Type(mykind, ...) ((bl_type_t*)intern_bytes(&(bl_type_t){.kind=mykind, __VA_ARGS__}, sizeof(bl_type_t)))

istr_t type_to_string(bl_type_t *t);
bool type_is_a(bl_type_t *t, bl_type_t *req);
bl_type_t *type_or_type(bl_type_t *a, bl_type_t *b);
bool is_integral(bl_type_t *t);
bool is_numeric(bl_type_t *t);
int numtype_priority(bl_type_t *t);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
