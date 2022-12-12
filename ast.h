#pragma once
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <bp/match.h>
#include <intern.h>

#include "libblang/list.h"

typedef enum {
    Unknown = 0,
    Nil, Bool, Var,
    Int, Num, Range,
    StringLiteral, StringJoin, DSL, Interp,
    Declare, Assign,
    AddUpdate, SubtractUpdate, MultiplyUpdate, DivideUpdate,
    AndUpdate, OrUpdate,
    Add, Subtract, Multiply, Divide, Power, Modulus,
    And, Or, Xor,
    Equal, NotEqual, Greater, GreaterEqual, Less, LessEqual,
    Not, Negative, Len, Maybe,
    TypeOf, SizeOf,
    List, Table,
    FunctionDef, MethodDef, Lambda,
    FunctionCall, KeywordArg,
    Block,
    Do, If, For, While, Repeat, When,
    Skip, Stop,
    Return,
    Fail,
    Extern,
    TypeList, TypeTable,
    TypeFunction, TypeOption,
    Cast, As,
    Struct, StructDef, StructFieldDef, StructField,
    EnumDef, EnumField,
    Index, FieldAccess, FieldName,
} ast_tag_e;

#define NUM_TYPES (FieldName + 1)

typedef struct ast_s ast_t;

const char *get_ast_tag_name(ast_tag_e tag);

typedef struct {
    ast_t *condition, *body;
} ast_clause_t;

typedef struct {
    List(ast_t*) cases;
    ast_t *body;
} ast_cases_t;


struct ast_s {
    ast_tag_e tag;
    match_t *match;
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
        } Int;
        struct {
            double n;
        } Num;
        struct {
            ast_t *first, *last, *step;
        } Range;
        struct {
            istr_t str;
        } StringLiteral;
        struct {
            List(ast_t*) children;
        } StringJoin;
        struct {
            istr_t name;
            ast_t *str;
        } DSL;
        struct {
            ast_t *value;
        } Interp;
        struct {
            istr_t name;
            ast_t *value;
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
        } Not, Negative, Len, Maybe, TypeOf, SizeOf;
        struct {
            ast_t *type;
            List(ast_t*) items;
        } List;
        struct {
            ast_t *key_type, *value_type;
            List(ast_t*) items;
        } Table;
        struct {
            istr_t name;
            List(istr_t) arg_names;
            List(ast_t*) arg_types;
            ast_t *ret_type;
            ast_t *body;
        } FunctionDef;
        struct {
            istr_t name, self;
            List(istr_t) arg_names;
            List(ast_t*) arg_types;
            ast_t *ret_type;
            ast_t *body;
        } MethodDef;
        struct {
            List(istr_t) arg_names;
            List(ast_t*) arg_types;
            ast_t *ret_type;
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
            List(ast_t*) blocks;
        } Do;
        struct {
            List(ast_clause_t) clauses;
            ast_t *else_body;
        } If;
        struct {
            istr_t key, value;
            ast_t *iter, *body, *between;
        } For;
        struct {
            ast_t *condition, *body, *between;
        } While;
        struct {
            ast_t *body, *between;
        } Repeat;
        struct {
            ast_t *subject;
            List(ast_cases_t) cases;
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
            istr_t name;
            ast_t *type;
        } Extern;
        struct {
            ast_t *item_type;
        } TypeList;
        struct {
            ast_t *key_type, *val_type;
        } TypeTable;
        struct {
            List(istr_t) arg_names;
            List(ast_t*) arg_types;
            ast_t *ret_type;
        } TypeFunction;
        struct {
            ast_t *nonnil;
        } TypeOption;
        struct {
            ast_t *value, *type;
        } Cast, As;
        struct {
            ast_t *type;
            List(ast_t *) members;
        } Struct;
        struct {
            istr_t name;
            List(ast_t *) members;
        } StructDef;
        struct {
            List(istr_t) names;
            ast_t *type;
        } StructFieldDef;
        struct {
            istr_t name;
            ast_t *value;
        } StructField;
        struct {
            istr_t name;
            List(istr_t) tag_names;
            List(int64_t) tag_values;
            List(ast_t *) tag_types;
        } EnumDef;
        struct {
            istr_t name;
            ast_t *value;
        } EnumField;
        struct {
            ast_t *indexed, *index;
        } Index;
        struct {
            istr_t field;
            ast_t *fielded;
        } FieldAccess;
        struct {
            istr_t name;
        } FieldName;
    } __data;
};
