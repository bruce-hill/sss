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
    highlight_match(stderr, f, (ast)->match); \
    exit(1); } while (0)

static bl_type_t *get_clause_type(file_t *f, hashmap_t *bindings, ast_t *condition, ast_t *body)
{
    if (condition->kind == Declare) {
        hashmap_t *body_bindings = hashmap_new();
        body_bindings->fallback = bindings;
        bl_type_t *t = get_type(f, bindings, condition);
        assert(t);
        binding_t b = {.type=t};
        hashmap_set(body_bindings, condition->lhs->str, &b);
        return get_type(f, body_bindings, body);
    } else {
        return get_type(f, bindings, body);
    }
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
                TYPE_ERR(f, ast, "Couldn't figure out what type %s refers to", ast->str);
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
        case Block: {
            ast_t *last = LIST_ITEM(ast->children, LIST_LEN(ast->children)-1);
            return get_type(f, bindings, last);
        }
        case Declare: return get_type(f, bindings, ast->rhs);
        case Assign: return Type(NilType);
        case Return: case Fail: case Stop: case Skip: return Type(AbortType);

        case Cast: {
            bl_type_t *t = get_type(f, bindings, ast->type);
            return t->type;
        }

        case TypeName: {
            binding_t *binding = hashmap_get(bindings, ast->str);
            if (!binding) {
                TYPE_ERR(f, ast, "This type name is not defined");
            }
            if (binding->type->kind != TypeType)
                TYPE_ERR(f, ast, "This is not a type, it's a %s", type_to_string(binding->type));
            return binding->type;
        }

        case TypeList: {
            bl_type_t *item_t = get_type(f, bindings, ast->child);
            if (!item_t) TYPE_ERR(f, ast->child, "This item type is not defined");
            if (item_t->kind != TypeType)
                TYPE_ERR(f, ast, "This is not a type, it's a %s", type_to_string(item_t));
            return Type(TypeType, .type=Type(ListType, .item_type=item_t->type));
        }

        case TypeOption: {
            bl_type_t *item_t = get_type(f, bindings, ast->child);
            if (!item_t) TYPE_ERR(f, ast->child, "This option type is not defined");
            if (item_t->kind != TypeType)
                TYPE_ERR(f, ast, "This is not a type, it's a %s", type_to_string(item_t));
            return Type(TypeType, .type=Type(OptionalType, .nonnil=item_t->type));
        }

        case Add: case Subtract: case Divide: case Multiply: case Power: case Modulus: {
            bl_type_t *t1 = get_type(f, bindings, ast->lhs);
            bl_type_t *t2 = get_type(f, bindings, ast->rhs);
            if (t1 == t2) {
                if (is_numeric(t1))
                    return t1;
                else if (t1->kind == DSLType || t1->kind == StringType)
                    return t1;
            } else if (is_numeric(t1)) {
                TYPE_ERR(f, ast->rhs, "This value is type %s, which can't be added to something with type %s",
                         type_to_string(t2), type_to_string(t1));
            } else if (is_numeric(t2)) {
                TYPE_ERR(f, ast->lhs, "This value is type %s, which can't be added to something with type %s",
                         type_to_string(t1), type_to_string(t2));
            }
            TYPE_ERR(f, ast, "Math operations are not supported between %s and %s",
                     type_to_string(t1), type_to_string(t2));
        }

        case Less: case LessEqual: case Greater: case GreaterEqual: {
            bl_type_t *lhs_t = get_type(f, bindings, ast->lhs);
            bl_type_t *rhs_t = get_type(f, bindings, ast->rhs);
            if (lhs_t == rhs_t && (lhs_t->kind == StringType || lhs_t->kind == DSLType))
                return Type(BoolType);
            else if (lhs_t == rhs_t && is_numeric(lhs_t))
                return Type(BoolType);
            else
                TYPE_ERR(f, ast, "Ordered comparison is not supported for %s and %s", type_to_string(lhs_t), type_to_string(rhs_t));
        }

        case Equal: case NotEqual: {
            bl_type_t *lhs_t = get_type(f, bindings, ast->lhs);
            bl_type_t *rhs_t = get_type(f, bindings, ast->rhs);
            if (type_is_a(lhs_t, rhs_t) || type_is_a(rhs_t, lhs_t))
                return Type(BoolType);
            else
                TYPE_ERR(f, ast, "These two values have incompatible types: %s vs %s", type_to_string(lhs_t), type_to_string(rhs_t));
        }

        case FunctionDef: case Lambda: {
            NEW_LIST(bl_type_t*, args);
            for (int64_t i = 0; i < LIST_LEN(ast->fn.arg_types); i++) {
                ast_t *arg_def = LIST_ITEM(ast->fn.arg_types, i);
                bl_type_t *t = get_type(f, bindings, arg_def);
                if (t->kind != TypeType) TYPE_ERR(f, arg_def, "Expected a type here, not %s", type_to_string(t));
                APPEND(args, t->type);
            }

            bl_type_t *ret = NULL;
            if (ast->kind == FunctionDef) {
                if (ast->fn.ret_type) {
                    ret = get_type(f, bindings, ast->fn.ret_type);
                    if (ret->kind != TypeType) TYPE_ERR(f, ast->fn.ret_type, "Expected a type here");
                    ret = ret->type;
                }
            } else {
                hashmap_t *body_bindings = hashmap_new();
                // TODO: strip closure bindings, but allow globals
                // body_bindings->fallback = bindings;
                for (int64_t i = 0; i < LIST_LEN(ast->fn.arg_types); i++) {
                    hashmap_set(body_bindings, LIST_ITEM(ast->fn.arg_names, i), new(binding_t, .type=LIST_ITEM(args, i)));
                }
                ret = get_type(f, body_bindings, ast->fn.body->child);
            }
            if (ret == NULL) ret = Type(NilType);
            return Type(FunctionType, .args=args, .ret=ret);
        }

        case If: {
            bl_type_t *t = NULL;
            LIST_FOR (ast->clauses, clause, _) {
                bl_type_t *clause_t = get_clause_type(f, bindings, clause->condition, clause->body);
                t = type_or_type(t, clause_t);
                if (!t)
                    TYPE_ERR(f, clause->body,
                             "This block has a type %s, which is incompatible with earlier blocks of type %s",
                             type_to_string(clause_t), type_to_string(t));
            }
            if (ast->else_body) {
                bl_type_t *else_type = get_type(f, bindings, ast->else_body);
                t = type_or_type(t, else_type);
                if (!t)
                    TYPE_ERR(f, ast->else_body,
                             "This block has a type %s, which is incompatible with earlier blocks of type %s",
                             type_to_string(else_type), type_to_string(t));
            } else {
                t = type_or_type(t, Type(NilType));
            }
            return t;
        }

        case While: {
            bl_type_t *t = get_clause_type(f, bindings, ast->loop.condition, ast->loop.body);
            assert(t);
            if (t->kind == OptionalType || t->kind == NilType) return t;
            else return Type(OptionalType, .nonnil=t);
        }

        default: break;
    }
    TYPE_ERR(f, ast, "Couldn't figure out type for %s", get_ast_kind_name(ast->kind));
}

void check_discardable(file_t *f, hashmap_t *bindings, ast_t *ast)
{
    if (ast->kind == Declare || ast->kind == Block || ast->kind == FunctionDef)
        return;
    bl_type_t *t = get_type(f, bindings, ast);
    if (!(t->kind == NilType || t->kind == AbortType)) {
        TYPE_ERR(f, ast, "This value has a return type of %s but the value is being ignored", type_to_string(t));
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
