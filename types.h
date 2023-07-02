#pragma once
#include <libgccjit.h>
#include <printf.h>
#include <stdlib.h>

#include "ast.h"
#include "libsss/list.h"
#include "units.h"

typedef const struct sss_type_s sss_type_t;

typedef struct {
    const char *name;
    int64_t tag_value;
    sss_type_t *type;
} sss_tagged_union_member_t;

struct sss_type_s {
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
        VariantType,
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
            sss_type_t *type;
        } TypeType;
        struct {} RangeType;
        struct {
            sss_type_t *item_type;
        } ArrayType;
        struct {
            sss_type_t *key_type, *value_type;
        } TableType;
        struct {
            List(const char*) arg_names;
            List(sss_type_t*) arg_types;
            List(ast_t*) arg_defaults;
            sss_type_t *ret;
            void *env;
        } FunctionType;
        struct {
            sss_type_t *pointed;
            bool is_optional:1, is_stack:1;
        } PointerType;
        struct {
            sss_type_t *generated;
        } GeneratorType;
        struct {
            const char *name, *filename;
            List(const char*) field_names;
            List(sss_type_t*) field_types;
            List(ast_t*) field_defaults;
            const char* units;
        } StructType;
        struct {
            const char* name, *filename;
            List(sss_tagged_union_member_t) members;
            unsigned short int tag_bits;
        } TaggedUnionType;
        struct {
            const char *path;
        } ModuleType;
        struct {
            const char *name, *filename;
            sss_type_t *variant_of;
        } VariantType;
    } __data;
};

#define Type(typetag, ...) new(sss_type_t, .tag=typetag, .__data.typetag={__VA_ARGS__})
#define INT_TYPE Type(IntType, .bits=64)
#define NUM_TYPE Type(NumType, .bits=64)

int printf_type(FILE *stream, const struct printf_info *info, const void *const args[]);
int printf_type_size(const struct printf_info *info, size_t n, int argtypes[n], int size[n]);
const char* type_to_string_concise(sss_type_t *t);
const char* type_to_typeof_string(sss_type_t *t);
const char* type_to_string(sss_type_t *t);
bool type_eq(sss_type_t *a, sss_type_t *b);
bool type_is_a(sss_type_t *t, sss_type_t *req);
sss_type_t *type_or_type(sss_type_t *a, sss_type_t *b);
sss_type_t *value_type(sss_type_t *a);
bool is_integral(sss_type_t *t);
bool is_numeric(sss_type_t *t);
typedef enum {NUM_PRECISION_EQUAL, NUM_PRECISION_LESS, NUM_PRECISION_MORE, NUM_PRECISION_INCOMPARABLE} precision_cmp_e;
precision_cmp_e compare_precision(sss_type_t *a, sss_type_t *b);
const char* type_units(sss_type_t *t);
sss_type_t *with_units(sss_type_t *t, const char* units);
bool is_orderable(sss_type_t *t);
bool has_heap_memory(sss_type_t *t);
bool has_stack_memory(sss_type_t *t);
bool can_promote(sss_type_t *actual, sss_type_t *needed);
bool can_leave_uninitialized(sss_type_t *t);
bool can_have_cycles(sss_type_t *t);
sss_type_t *table_entry_type(sss_type_t *table_t);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
