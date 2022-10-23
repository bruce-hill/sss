#pragma once
#include <stdlib.h>
#include <intern.h>

#include "datastructures/list.h"

typedef enum {
    UnknownType,
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

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
