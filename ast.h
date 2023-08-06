#pragma once
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <libgccjit.h>

#include "files.h"
#include "libsss/list.h"
#include "compile/libgccjit_abbrev.h"
#include "span.h"
#include "util.h"

#define NewAST(_file, _start, _end, ast_tag, ...) (new(ast_t, .span.file=_file, .span.start=_start, .span.end=_end,\
                                                     .tag=ast_tag, .__data.ast_tag={__VA_ARGS__}))
#define FakeAST(ast_tag, ...) (new(ast_t, .tag=ast_tag, .__data.ast_tag={__VA_ARGS__}))
#define WrapAST(ast, ast_tag, ...) (new(ast_t, .span=(ast)->span, .tag=ast_tag, .__data.ast_tag={__VA_ARGS__}))
#define StringAST(ast, _str) WrapAST(ast, StringLiteral, .str=heap_str(_str))

typedef enum {
    Unknown = 0,
    Nil, Bool, Var,
    Int, Num, Range, Char,
    StringLiteral, StringJoin, Interp,
    Declare, Assign,
    AddUpdate, SubtractUpdate, MultiplyUpdate, DivideUpdate,
    AndUpdate, OrUpdate, XorUpdate, ConcatenateUpdate,
    Add, Subtract, Multiply, Divide, Power, Modulus, Modulus1,
    LeftShift, RightShift,
    And, Or, Xor, Min, Max, Mix, Concatenate,
    Equal, NotEqual, Greater, GreaterEqual, Less, LessEqual,
    In, NotIn,
    Not, Negative, Len, Maybe,
    TypeOf, SizeOf,
    HeapAllocate, StackReference, Dereference, AssertNonNull,
    Array, Table, TableEntry,
    FunctionDef, Lambda,
    FunctionCall, KeywordArg,
    Block,
    Do, For, While, Repeat, If,
    Skip, Stop, Pass,
    Return,
    Fail,
    Extern,
    TypeArray, TypeTable, TypeStruct,
    TypeFunction, TypePointer,
    TypeMeasure, TypeTypeAST,
    Cast, Bitcast,
    Struct, StructDef,
    TaggedUnionDef, TypeTaggedUnion, TaggedUnionField,
    Index, FieldAccess,
    UnitDef, ConvertDef, VariantDef,
    Reduction,
    DocTest,
    Defer,
    With,
    Extend,
    Use,
    Delete,
    LinkerDirective,
    Variant,
    Using,
} ast_tag_e;

#define NUM_AST_TAGS (Using + 1)

typedef struct ast_s ast_t;

typedef struct {
    List(const char*) names;
    List(ast_t*) types;
    List(ast_t*) defaults;
} args_t;

struct ast_s {
    ast_tag_e tag;
    span_t span;
    union {
        struct {
        } Unknown;
        struct {
            ast_t *type;
        } Nil;
        struct {
            bool b;
        } Bool;
        struct {
            const char *name;
        } Var;
        struct {
            int64_t i;
            uint8_t precision;
            bool is_unsigned;
            const char * units;
        } Int;
        struct {
            double n;
            uint8_t precision;
            const char *units;
        } Num;
        struct {
            ast_t *first, *last, *step;
        } Range;
        struct {
            char c;
        } Char;
        struct {
            const char *str;
        } StringLiteral;
        struct {
            List(ast_t*) children;
        } StringJoin;
        struct {
            ast_t *value;
            bool labelled:1, colorize:1, quote_string:1;
        } Interp;
        struct {
            ast_t *var, *value;
            bool is_global;
        } Declare;
        struct {
            List(ast_t*) targets;
            List(ast_t*) values;
        } Assign; 
        struct {
            ast_t *lhs, *rhs;
        } AddUpdate, SubtractUpdate, MultiplyUpdate, DivideUpdate,
            AndUpdate, OrUpdate, XorUpdate, ConcatenateUpdate,
            Add, Subtract, Multiply, Divide, Power, Modulus, Modulus1,
            LeftShift, RightShift, And, Or, Xor, Concatenate,
            Equal, NotEqual, Greater, GreaterEqual, Less, LessEqual;
        struct {
            ast_t *lhs, *rhs, *key;
        } Min, Max, Mix;
        struct {
            ast_t *value;
        } Not, Negative, Len, Maybe, TypeOf, SizeOf, HeapAllocate, StackReference, Dereference, AssertNonNull;
        struct {
            ast_t *type;
            List(ast_t*) items;
        } Array;
        struct {
            ast_t *key_type, *value_type, *fallback, *default_value;
            List(ast_t*) entries;
        } Table;
        struct {
            ast_t *key, *value;
        } TableEntry;
        struct {
            const char *name;
            args_t args;
            ast_t *ret_type;
            ast_t *body;
            ast_t *cache;
            bool is_inline;
        } FunctionDef;
        struct {
            args_t args;
            ast_t *body;
        } Lambda;
        struct {
            ast_t *fn;
            List(ast_t*) args;
            ast_t *extern_return_type;
        } FunctionCall;
        struct {
            const char *name;
            ast_t *arg;
        } KeywordArg;
        struct {
            List(ast_t*) statements;
            // Whether to keep using the existing scope instead of creating a new one:
            bool keep_scope;
        } Block;
        struct {
            const char *label;
            ast_t *body, *else_body;
        } Do;
        struct {
            ast_t *index, *value, *iter, *first, *body, *between, *empty;
        } For;
        struct {
            ast_t *condition, *body, *between;
        } While;
        struct {
            ast_t *body, *between;
        } Repeat;
        struct {
            ast_t *subject;
            List(ast_t*) patterns;
            List(ast_t*) blocks;
        } If;
        struct {
            const char *target;
        } Skip, Stop;
        struct {} Pass;
        struct {
            ast_t *value;
        } Return;
        struct {
            ast_t *message;
        } Fail;
        struct {
            const char *name;
            ast_t *type;
            bool address;
        } Extern;
        struct {
            ast_t *item_type;
        } TypeArray;
        struct {
            ast_t *key_type, *value_type;
        } TypeTable;
        struct {
            const char *name;
            args_t members;
        } TypeStruct;
        struct {
            args_t args;
            ast_t *ret_type;
        } TypeFunction;
        struct {
            ast_t *pointed;
            bool is_optional:1, is_stack:1;
        } TypePointer;
        struct {
            ast_t *type;
            const char *units;
        } TypeMeasure;
        struct {
            ast_t *type;
        } TypeTypeAST;
        struct {
            ast_t *value, *type;
        } Cast, Bitcast;
        struct {
            ast_t *type;
            List(ast_t *) members;
            const char *units;
        } Struct;
        struct {
            const char *name;
            List(const char*) field_names;
            List(ast_t*) field_types;
            List(ast_t*) field_defaults;
            List(ast_t*) definitions;
        } StructDef;
        struct {
            const char *name;
            List(const char*) tag_names;
            List(int64_t) tag_values;
            List(args_t) tag_args;
            List(ast_t*) definitions;
            unsigned short int tag_bits;
        } TaggedUnionDef, TypeTaggedUnion;
        struct {
            const char *name;
            ast_t *value;
        } TaggedUnionField;
        struct {
            ast_t *indexed, *index;
            bool unchecked;
        } Index;
        struct {
            ast_t *fielded;
            const char *field;
        } FieldAccess;
        struct {
            ast_t *derived, *base;
        } UnitDef;
        struct {
            const char *var;
            ast_t *source_type, *target_type, *body;
        } ConvertDef;
        struct {
            const char *name;
            ast_t *variant_of;
            ast_t *body;
        } VariantDef;
        struct {
            ast_t *iter, *combination, *fallback;
        } Reduction;
        struct {
            ast_t *expr;
            const char *output;
            bool skip_source:1;
        } DocTest;
        struct {
            ast_t *body;
        } Defer;
        struct {
            ast_t *var, *expr, *cleanup, *body;
        } With;
        struct {
            ast_t *type, *body;
        } Extend;
        struct {
            const char *path;
            sss_file_t *file;
            bool main_program;
        } Use;
        struct {
            ast_t *value;
        } Delete;
        struct {
            ast_t *member, *container;
        } In, NotIn;
        struct {
            List(const char*) directives;
        } LinkerDirective;
        struct {
            ast_t *type, *value;
        } Variant;
        struct {
            List(ast_t*) used;
            ast_t *body;
        } Using;
    } __data;
};

const char *ast_to_str(ast_t *ast);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
