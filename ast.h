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
    Do, If, For, While, Repeat,
    Skip, Stop,
    Return,
    Fail,
    Extern,
    TypeName,
    TypeList, TypeTable,
    TypeFunction, TypeOption,
    Cast, As,
    Struct, StructDef, StructFieldDef, StructField,
    Index, FieldName,
    NUM_TYPES,
} astkind_e;

const char *get_ast_kind_name(astkind_e kind);

typedef struct {
    struct ast_s *condition, *body;
} ast_clause_t;

typedef struct ast_s {
    astkind_e kind;
    match_t *match;
    union {
        bool b;
        int64_t i;
        double n;
        istr_t str;
        struct ast_s *child;
        List(struct ast_s*) children;
        // struct {
        //     List(struct ast_s*) items;
        //     struct ast_s *type;
        // } list;
        struct { // Infix
            struct ast_s *lhs, *rhs;
        };
        struct { // Cast/As
            struct ast_s *expr, *type;
        };
        struct { // Index
            struct ast_s *indexed, *index;
        };
        struct { // Multiple assignment
            List(struct ast_s*) lhs;
            List(struct ast_s*) rhs;
        } multiassign;
        struct { // Function def/lambda
            istr_t name, self;
            List(istr_t) arg_names;
            List(struct ast_s*) arg_types;
            struct ast_s *ret_type;
            struct ast_s *body;
        } fn;
        struct { // Function
            struct ast_s *fn;
            List(struct ast_s*) args;
        } call;
        struct {
            istr_t name;
            struct ast_s *text;
        } dsl;
        struct {
            istr_t name;
            struct ast_s *value;
        } named;
        struct { // If
            List(ast_clause_t) clauses;
            struct ast_s *else_body;
        };
        struct { // While/Repeat
            struct ast_s *condition, *body, *between;
        } loop;
        struct {
            struct ast_s *key, *value, *iter, *body, *between;
        } for_loop;
        struct { // Lists
            struct ast_s *type;
            List(struct ast_s*) items;
        } list;
        struct {
            struct ast_s *first, *last, *step;
        } range;
        struct {
            List(istr_t) names;
            struct ast_s *type;
        } fields;
        struct {
            istr_t name;
            List(struct ast_s *) members;
        } struct_;
    };
} ast_t;
