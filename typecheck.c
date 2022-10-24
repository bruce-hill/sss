#include <bhash.h>
#include <gc.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "typecheck.h"
#include "types.h"
#include "util.h"

#define TYPE_ERR(f, ast, fmt, ...) do { \
    fprintf(stderr, "\x1b[31;7;1m" fmt "\x1b[m\n\n" __VA_OPT__(,) __VA_ARGS__); \
    highlight_match(f, (ast)->match); \
    exit(1); } while (0)

static inline bool type_is_a(bl_type_t *t, bl_type_t *req)
{
    if (t == req) return true;
    if (req->kind == OptionalType && (t->kind == NilType || type_is_a(t, req->nonnil)))
        return true;
    return false;
}

bl_type_t *get_type(file_t *f, hashmap_t *bindings, ast_t *ast)
{
    switch (ast->kind) {
        case Nil: return Type(NilType);
        case Bool: return Type(BoolType);
        case Int: return Type(IntType);
        case Num: return Type(NumType);
        case StringJoin: case StringLiteral: return Type(StringType);
        case Var: {
            binding_t *binding = hashmap_get(bindings, ast->str);
            if (binding) {
                return binding->type;
            } else {
                return Type(UnknownType);
            }
        }
        case KeywordArg: return get_type(f, bindings, ast->named.value);
        case FunctionCall: {
            bl_type_t *fn_type = get_type(f, bindings, ast->call.fn);
            if (fn_type->kind != FunctionType) {
                TYPE_ERR(f, ast->call.fn, "This function is a %s and not a function", type_to_string(fn_type));
            }
            int64_t max_args = LIST_LEN(fn_type->args);
            int64_t min_args = max_args;
            while (min_args > 0 && LIST_ITEM(fn_type->args, min_args-1)->kind == OptionalType)
                --min_args;
            int64_t len_args = LIST_LEN(ast->call.args);
            if (len_args < min_args) {
                TYPE_ERR(f, ast, "Expected to get at least %ld arguments but only got %ld", min_args, len_args);
            } else if (len_args > max_args) {
                TYPE_ERR(f, LIST_ITEM(ast->call.args, max_args), "Too many arguments provided to this function call. Everything from here on is too much.");
            }
            for (int64_t i = 0; i < len_args; i++) {
                ast_t *arg = LIST_ITEM(ast->call.args, i);
                bl_type_t *arg_t = get_type(f, bindings, arg);
                if (!type_is_a(arg_t, LIST_ITEM(fn_type->args, i))) {
                    TYPE_ERR(f, arg, "This argument has the wrong type. Expected %s but got %s", type_to_string(LIST_ITEM(fn_type->args, i)), type_to_string(arg_t));
                }
            }
            return fn_type->ret;
        }

        default: break;
    }
    TYPE_ERR(f, ast, "Couldn't figure out type for %s:", get_ast_kind_name(ast->kind));
}

const char *get_base_type(file_t *f, hashmap_t *bindings, ast_t *ast)
{
    bl_type_t *t = get_type(f, bindings, ast);
    switch (t->kind) {
    case BoolType: case Int8Type: case Int16Type: case Int32Type: return "w";
    case NumType: return "d";
    case Num32Type: return "s";
    default: return "l";
    }
}

const char *get_abi_type(file_t *f, hashmap_t *bindings, ast_t *ast)
{
    bl_type_t *t = get_type(f, bindings, ast);
    switch (t->kind) {
    case BoolType: case Int8Type: return "b";
    case Int16Type: return "h";
    case Int32Type: return "w";
    case NumType: return "d";
    case Num32Type: return "s";
    default: return "l";
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
