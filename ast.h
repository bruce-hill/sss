#pragma once
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <bp/match.h>
#include <intern.h>

#include "datastructures/list.h"

typedef enum {
    Unknown = 0,
    Nil, Bool, Var,
    Int, Num,
    StringLiteral, StringJoin, DSL, Interp,
    Declare, Assign,
    AddUpdate, SubtractUpdate, MultiplyUpdate, DivideUpdate,
    AndUpdate, OrUpdate,
    Add, Subtract, Multiply, Divide, Power, Modulus,
    And, Or, Xor,
    Equal, NotEqual, Greater, GreaterEqual, Less, LessEqual,
    Not, Negative, Len,
    List, Table,
    FunctionDef, Lambda,
    FunctionCall, KeywordArg,
    Block,
    If, For, While, Repeat,
    Skip, Stop,
    Return,
    Fail,
    TypeName,
    TypeList, TypeTable,
    TypeFunction, TypeOption,
    Cast, As,
    NUM_TYPES,
} astkind_e;

const char *get_ast_kind_name(astkind_e kind);

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
        struct { // Infix
            struct ast_s *lhs, *rhs;
        };
        struct { // Cast/As
            struct ast_s *expr, *type;
        };
        struct { // Multiple assignment
            List(struct ast_s*) lhs;
            List(struct ast_s*) rhs;
        } multiassign;
        struct { // Function def/lambda
            istr_t name;
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
    };
} ast_t;
