#pragma once
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <intern.h>

#include "libblang/list.h"
#include "span.h"
#include "util.h"

#define NewAST(_file, _start, _end, ast_tag, ...) (new(ast_t, .span.file=_file, .span.start=_start, .span.end=_end,\
                                                     .tag=ast_tag, .__data.ast_tag={__VA_ARGS__}))
#define FakeAST(ast_tag, ...) (new(ast_t, .tag=ast_tag, .__data.ast_tag={__VA_ARGS__}))
#define WrapAST(ast, ast_tag, ...) (new(ast_t, .span=(ast)->span, .tag=ast_tag, .__data.ast_tag={__VA_ARGS__}))
#define StringAST(ast, _str) WrapAST(ast, StringJoin, .children=LIST(ast_t*, WrapAST(ast, StringLiteral, .str=intern_str(_str))))

typedef enum {
    Unknown = 0,
    Nil, Bool, Var,
    Int, Num, Range, Char,
    StringLiteral, StringJoin, Interp,
    Declare, Assign,
    AddUpdate, SubtractUpdate, MultiplyUpdate, DivideUpdate,
    AndUpdate, OrUpdate,
    Add, Subtract, Multiply, Divide, Power, Modulus,
    And, Or, Xor,
    Equal, NotEqual, Greater, GreaterEqual, Less, LessEqual,
    Not, Negative, Len, Maybe,
    TypeOf, SizeOf,
    HeapAllocate, Dereference,
    Array, Table, TableEntry,
    FunctionDef, Lambda,
    FunctionCall, KeywordArg,
    Block,
    Do, Using, If, For, While, Repeat, When,
    Skip, Stop,
    Return,
    Fail,
    Extern,
    TypeArray, TypeTable, TypeStruct,
    TypeFunction, TypePointer, TypeOptional,
    TypeMeasure, TypeDSL,
    Cast, Bitcast,
    Struct, StructDef, StructField,
    TaggedUnionDef, TypeTaggedUnion, TaggedUnionField,
    Index, FieldAccess,
    UnitDef, ConvertDef,
    Reduction,
    DocTest,
    Defer,
    With,
    Extend,
    Ellipsis,
} ast_tag_e;

#define NUM_AST_TAGS (Ellipsis + 1)

typedef struct ast_s ast_t;

typedef struct {
    ast_t *var, *tag, *body;
} ast_case_t;

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
            istr_t name;
        } Var;
        struct {
            int64_t i;
            uint8_t precision;
            istr_t units;
        } Int;
        struct {
            double n;
            uint8_t precision;
            istr_t units;
        } Num;
        struct {
            ast_t *first, *last, *step;
        } Range;
        struct {
            char c;
        } Char;
        struct {
            istr_t str;
        } StringLiteral;
        struct {
            istr_t dsl;
            List(ast_t*) children;
        } StringJoin;
        struct {
            istr_t name;
            ast_t *str;
        } DSL;
        struct {
            ast_t *value;
            bool labelled;
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
            AndUpdate, OrUpdate,
            Add, Subtract, Multiply, Divide, Power, Modulus,
            And, Or, Xor,
            Equal, NotEqual, Greater, GreaterEqual, Less, LessEqual;
        struct {
            ast_t *value;
        } Not, Negative, Len, Maybe, TypeOf, SizeOf, HeapAllocate, Dereference;
        struct {
            ast_t *type;
            List(ast_t*) items;
        } Array;
        struct {
            ast_t *key_type, *value_type;
            List(ast_t*) entries;
        } Table;
        struct {
            ast_t *key, *value;
        } TableEntry;
        struct {
            istr_t name;
            List(istr_t) arg_names;
            List(ast_t*) arg_types;
            List(ast_t*) arg_defaults;
            ast_t *ret_type;
            ast_t *body;
            bool is_exported;
            bool is_inline;
        } FunctionDef;
        struct {
            List(istr_t) arg_names;
            List(ast_t*) arg_types;
            ast_t *body;
        } Lambda;
        struct {
            ast_t *fn;
            List(ast_t*) args;
        } FunctionCall;
        struct {
            istr_t name;
            ast_t *arg;
        } KeywordArg;
        struct {
            List(ast_t*) statements;
        } Block;
        struct {
            istr_t label;
            ast_t *body, *else_body;
        } Do;
        struct {
            List(ast_t*) vars;
            ast_t *body;
        } Using;
        struct {
            ast_t *condition, *body, *else_body;
        } If;
        struct {
            ast_t *key, *value, *iter, *first, *body, *between, *empty;
        } For;
        struct {
            ast_t *condition, *body, *between;
        } While;
        struct {
            ast_t *body, *between;
        } Repeat;
        struct {
            ast_t *subject;
            List(ast_case_t) cases;
            ast_t *default_body;
        } When;
        struct {
            istr_t target;
        } Skip, Stop;
        struct {
            ast_t *value;
        } Return;
        struct {
            ast_t *message;
        } Fail;
        struct {
            istr_t name, bl_name;
            ast_t *type;
        } Extern;
        struct {
            ast_t *item_type;
        } TypeArray;
        struct {
            ast_t *key_type, *val_type;
        } TypeTable;
        struct {
            istr_t name;
            List(istr_t) member_names;
            List(ast_t*) member_types;
        } TypeStruct;
        struct {
            List(istr_t) arg_names;
            List(ast_t*) arg_types;
            ast_t *ret_type;
        } TypeFunction;
        struct {
            ast_t *pointed;
        } TypePointer;
        struct {
            ast_t *type;
        } TypeOptional;
        struct {
            ast_t *type;
            istr_t units;
        } TypeMeasure;
        struct {
            istr_t name;
        } TypeDSL;
        struct {
            ast_t *value, *type;
        } Cast, Bitcast;
        struct {
            ast_t *type;
            List(ast_t *) members;
            istr_t units;
        } Struct;
        struct {
            istr_t name;
            List(istr_t) field_names;
            List(ast_t*) field_types;
            List(ast_t*) field_defaults;
            List(ast_t*) definitions;
        } StructDef;
        struct {
            istr_t name;
            ast_t *value;
        } StructField;
        struct {
            istr_t name;
            List(istr_t) tag_names;
            List(int64_t) tag_values;
            List(ast_t *) tag_types;
        } TaggedUnionDef, TypeTaggedUnion;
        struct {
            istr_t name;
            ast_t *value;
        } TaggedUnionField;
        struct {
            ast_t *indexed, *index;
            bool unchecked;
        } Index;
        struct {
            ast_t *fielded;
            istr_t field;
        } FieldAccess;
        struct {
            ast_t *derived, *base;
        } UnitDef;
        struct {
            istr_t var;
            ast_t *source_type, *target_type, *body;
        } ConvertDef;
        struct {
            ast_t *iter, *combination, *fallback;
        } Reduction;
        struct {
            ast_t *expr;
            istr_t output;
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
        struct {} Ellipsis;
    } __data;
};

const char *ast_to_str(ast_t *ast);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
