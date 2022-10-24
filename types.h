#pragma once
#include <stdlib.h>
#include <intern.h>

#include "datastructures/list.h"

typedef enum {
    UnknownType,
    AbortType,
    NilType, BoolType,
    IntType, Int32Type, Int16Type, Int8Type,
    NumType, Num32Type,
    StringType,
    NamedType,
    TypeType,
    ListType,
    TableType,
    FunctionType,
    OptionalType,
} typekind_e;

typedef struct bl_type_s {
    typekind_e kind;
    union {
        istr_t name;
        struct bl_type_s *item_type;
        struct bl_type_s *nonnil;
        struct bl_type_s *type;
        struct {
            struct bl_type_s *key, *value;
        };
        struct {
            List(struct bl_type_s*) args;
            struct bl_type_s *ret;
        };
    };
} bl_type_t;

typedef struct {
    istr_t reg;
    bl_type_t *type;
} binding_t;

#define Type(mykind, ...) ((bl_type_t*)intern_bytes(&(bl_type_t){.kind=mykind, __VA_ARGS__}, sizeof(bl_type_t)))

istr_t type_to_string(bl_type_t *t);
bool type_is_a(bl_type_t *t, bl_type_t *req);
const char* nil_value(bl_type_t *t);
char base_type_for(bl_type_t *t);
char abi_type_for(bl_type_t *t);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
