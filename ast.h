#pragma once
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

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
    TypeName,
    TypeList, TypeTable,
    TypeFunction, TypeOption,
} astkind_e;

typedef struct ast_s {
    astkind_e kind;
    union {
        bool b;
        int64_t i;
        double n;
        const char *str;
        struct ast_s *child;
        struct {
            size_t nchildren;
            struct ast_s **children;
        };
        struct { // Infix
            struct ast_s *lhs, *rhs;
        };
        struct { // Function def/lambda
            size_t nargs;
            const char **arg_names;
            struct ast_s **arg_types;
            struct ast_s *ret_type;
            struct ast_s *body;
        } fn;
        struct { // Function
            struct ast_s *fn;
            size_t nargs;
            struct ast_s **args;
        } call;
        struct {
            const char *name;
            struct ast_s *text;
        } dsl;
        struct {
            const char *name;
            struct ast_s *value;
        } named;
    };
} ast_t;
