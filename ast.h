#pragma once
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <libgccjit.h>

#include "files.h"
#include "builtins/array.h"
#include "compile/libgccjit_abbrev.h"
#include "util.h"

#define NewAST(_file, _start, _end, ast_tag, ...) (new(ast_t, .file=_file, .start=_start, .end=_end,\
                                                     .tag=ast_tag, .__data.ast_tag={__VA_ARGS__}))
#define FakeAST(ast_tag, ...) (new(ast_t, .tag=ast_tag, .__data.ast_tag={__VA_ARGS__}))
#define WrapAST(ast, ast_tag, ...) (new(ast_t, .file=(ast)->file, .start=(ast)->start, .end=(ast)->end, .tag=ast_tag, .__data.ast_tag={__VA_ARGS__}))
#define StringAST(ast, _str) WrapAST(ast, StringLiteral, .str=heap_str(_str))

typedef enum {
    Unknown = 0,
    Nil, Bool, Var, Wildcard,
    Int, Num, Range, Char,
    StringLiteral, StringJoin, Interp,
    Predeclare, Declare, Assign,
    AddUpdate, SubtractUpdate, MultiplyUpdate, DivideUpdate,
    AndUpdate, OrUpdate, XorUpdate, ConcatenateUpdate,
    Add, Subtract, Multiply, Divide, Power, Modulus, Modulus1,
    LeftShift, RightShift,
    And, Or, Xor, Min, Max, Mix, Concatenate,
    Equal, NotEqual, Greater, GreaterEqual, Less, LessEqual,
    In, NotIn,
    Not, Negative,
    TypeOf, SizeOf,
    HeapAllocate, StackReference,
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
    TypeTaggedUnion,
    Cast, Bitcast,
    Struct,
    TaggedUnionField,
    TypeDef,
    Index, FieldAccess,
    UnitDef, ConvertDef,
    Reduction,
    DocTest,
    Defer,
    With,
    Extend,
    Use,
    LinkerDirective,
    Variant,
    Using,
} ast_tag_e;

#define NUM_AST_TAGS (Using + 1)

typedef struct ast_s ast_t;

typedef struct {
    ARRAY_OF(const char*) names;
    ARRAY_OF(ast_t*) types;
    ARRAY_OF(ast_t*) defaults;
} args_t;

struct ast_s {
    ast_tag_e tag;
    sss_file_t *file;
    const char *start, *end;
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
        } Var, Wildcard;
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
            ARRAY_OF(ast_t*) children;
        } StringJoin;
        struct {
            ast_t *value;
            bool labelled:1, colorize:1, quote_string:1;
        } Interp;
        struct {
            ast_t *var, *type;
        } Predeclare;
        struct {
            ast_t *var, *value;
            bool is_global;
        } Declare;
        struct {
            ARRAY_OF(ast_t*) targets;
            ARRAY_OF(ast_t*) values;
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
        } Not, Negative, TypeOf, SizeOf, HeapAllocate, StackReference;
        struct {
            ast_t *type;
            ARRAY_OF(ast_t*) items;
        } Array;
        struct {
            ast_t *key_type, *value_type, *fallback, *default_value;
            ARRAY_OF(ast_t*) entries;
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
            ARRAY_OF(ast_t*) args;
            ast_t *extern_return_type;
        } FunctionCall;
        struct {
            const char *name;
            ast_t *arg;
        } KeywordArg;
        struct {
            ARRAY_OF(ast_t*) statements;
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
            ARRAY_OF(ast_t*) patterns;
            ARRAY_OF(ast_t*) blocks;
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
            args_t members;
        } TypeStruct;
        struct {
            args_t args;
            ast_t *ret_type;
        } TypeFunction;
        struct {
            ast_t *pointed;
            bool is_optional:1, is_stack:1, is_readonly:1;
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
            ARRAY_OF(ast_t *) members;
            const char *units;
        } Struct;
        struct {
            ARRAY_OF(const char*) tag_names;
            ARRAY_OF(int64_t) tag_values;
            ARRAY_OF(args_t) tag_args;
        } TypeTaggedUnion;
        struct {
            const char *name;
            ast_t *value;
        } TaggedUnionField;
        struct {
            const char *name;
            ast_t *type;
            ARRAY_OF(ast_t*) definitions;
        } TypeDef;
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
            ast_t *member, *container;
        } In, NotIn;
        struct {
            ARRAY_OF(const char*) directives;
        } LinkerDirective;
        struct {
            ast_t *type, *value;
        } Variant;
        struct {
            ARRAY_OF(ast_t*) used;
            ast_t *body;
        } Using;
    } __data;
};

const char *ast_to_str(ast_t *ast);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
