#pragma once
#include <stdlib.h>

typedef enum {
    NamedType,
    ListType,
    TableType,
    FunctionType,
    OptionalType,
} typekind_e;

typedef struct bl_type_s {
    typekind_e kind;
    union {
        const char *name;
        struct bl_type_s *item_type;
        struct bl_type_s *nonnil;
        struct {
            struct bl_type_s *key, *value;
        };
        struct {
            size_t nargs;
            struct bl_type_s **args;
            struct bl_type_s *ret;
        };
    };
} bl_type_t;
