#pragma once
#include <intern.h>
#include <libgccjit.h>
#include <stdlib.h>

#include "libgccjit_abbrev.h"
#include "datastructures/list.h"

typedef enum {
    UnknownType,
    AbortType,
    NilType, BoolType,
    IntType, Int32Type, Int16Type, Int8Type,
    NumType, Num32Type,
    StringType, DSLType,
    NamedType,
    TypeType,
    ListType,
    TableType,
    FunctionType,
    OptionalType,
} typekind_e;

typedef const struct bl_type_s {
    typekind_e kind;
    union {
        istr_t name;
        const struct bl_type_s *item_type;
        const struct bl_type_s *nonnil;
        const struct bl_type_s *type;
        struct {
            const struct bl_type_s *key, *value;
        };
        struct {
            List(const struct bl_type_s*) args;
            const struct bl_type_s *ret;
        };
    };
} bl_type_t;

typedef struct {
    gcc_jit_rvalue *rval;
    gcc_jit_lvalue *lval;
    gcc_func_t *func;
    bl_type_t *type;
    bool is_global;
} binding_t;

#define Type(mykind, ...) ((bl_type_t*)intern_bytes(&(bl_type_t){.kind=mykind, __VA_ARGS__}, sizeof(bl_type_t)))

istr_t type_to_string(bl_type_t *t);
bool type_is_a(bl_type_t *t, bl_type_t *req);
bl_type_t *type_or_type(bl_type_t *a, bl_type_t *b);
bool is_numeric(bl_type_t *t);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
