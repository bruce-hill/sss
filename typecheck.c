// Logic for getting a Blang type from an AST node
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
    highlight_match(stderr, f, (ast)->match, 2); \
    exit(1); } while (0)

static bl_type_t *get_clause_type(file_t *f, hashmap_t *bindings, ast_t *condition, ast_t *body)
{
    if (condition && condition->kind == Declare) {
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

static bl_type_t *parse_type(file_t *f, hashmap_t *bindings, ast_t *ast)
{
    switch (ast->kind) {
    case TypeName: {
        binding_t *binding = hashmap_get(bindings, ast->str);
        if (!binding) {
            TYPE_ERR(f, ast, "This type name is not defined");
        }
        return binding->type->type;
    }

    case TypeList: {
        bl_type_t *item_t = parse_type(f, bindings, ast->child);
        if (!item_t) TYPE_ERR(f, ast->child, "This item type is not defined");
        return Type(ListType, .item_type=item_t);
    }

    case TypeOption: {
        bl_type_t *item_t = parse_type(f, bindings, ast->child);
        if (!item_t) TYPE_ERR(f, ast->child, "This option type is not defined");
        return Type(OptionalType, .nonnil=item_t);
    }
    case TypeFunction: {
        bl_type_t *ret_t = parse_type(f, bindings, ast->fn.ret_type);
        NEW_LIST(bl_type_t*, arg_types);
        LIST_FOR (ast->fn.arg_types, arg_t, _) {
            bl_type_t *bl_arg_t = parse_type(f, bindings, *arg_t);
            APPEND(arg_types, bl_arg_t);
        }
        return Type(FunctionType, .args=arg_types, .ret=ret_t);
    }
    default: TYPE_ERR(f, ast, "This is not a Type value");
    }
}

bl_type_t *get_type(file_t *f, hashmap_t *bindings, ast_t *ast)
{
    switch (ast->kind) {
        case Nil: {
            bl_type_t *nonnil = parse_type(f, bindings, ast->child);
            return nonnil->kind == OptionalType ? nonnil : Type(OptionalType, .nonnil=nonnil);
        }
        case Bool: {
            return Type(BoolType);
        }
        case Int: {
            return Type(IntType);
        }
        case Num: {
            return Type(NumType);
        }
        case TypeOf: {
            bl_type_t *t = get_type(f, bindings, ast->child);
            return Type(TypeType, .type=t);
        }
        case Maybe: {
            bl_type_t *nonnil = get_type(f, bindings, ast->child);
            return nonnil->kind == OptionalType ? nonnil : Type(OptionalType, .nonnil=nonnil);
        }
        case Range: {
            return Type(RangeType);
        }
        case StringJoin: case StringLiteral: {
            return Type(StringType);
        }
        case Var: {
            binding_t *binding = hashmap_get(bindings, ast->str);
            if (binding) {
                return binding->type;
            } else {
                TYPE_ERR(f, ast, "Couldn't figure out what type %s refers to", ast->str);
            }
        }
        case Len: {
            return Type(IntType);
        }
        case List: {
            if (ast->list.type)
                return parse_type(f, bindings, ast->list.type);

            bl_type_t *item_type = NULL;
            for (int64_t i = 0; i < LIST_LEN(ast->list.items); i++) {
                ast_t *item = LIST_ITEM(ast->list.items, i);
                bl_type_t *t2;
                switch (item->kind) {
                case For: {
                    t2 = get_type(f, bindings, item->for_loop.body);
                    break;
                }
                case While: case Repeat: {
                    t2 = get_type(f, bindings, item->loop.body);
                    break;
                }
                case If: {
                    TYPE_ERR(f, item, "Conditional items in lists not yet supported");
                }
                default: {
                    t2 = get_type(f, bindings, item);
                    break;
                }
                }
                bl_type_t *merged = item_type ? type_or_type(item_type, t2) : t2;
                if (!merged)
                    TYPE_ERR(f, LIST_ITEM(ast->list.items, i),
                             "This list item has type %s, which is different from earlier items which have type %s",
                             type_to_string(t2),  type_to_string(item_type));
                item_type = merged;
            }
            return Type(ListType, .item_type=item_type);
        }
        case KeywordArg: {
            return get_type(f, bindings, ast->named.value);
        }
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
        case Declare: {
            return get_type(f, bindings, ast->rhs);
        }
        case Extern: {
            return Type(VoidType);
        }
        case Assign: {
            return Type(VoidType);
        }
        case Return: case Fail: case Stop: case Skip: {
            return Type(AbortType);
        }
        case Cast: case As: {
            return parse_type(f, bindings, ast->type);
        }
        case TypeName: case TypeList: case TypeOption: case TypeFunction: {
            bl_type_t *t = parse_type(f, bindings, ast);
            return Type(TypeType, .type=t);
        }

        case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate:
        case Add: case Subtract: case Divide: case Multiply: case Power: case Modulus: {
            bl_type_t *t1 = get_type(f, bindings, ast->lhs);
            bl_type_t *t2 = get_type(f, bindings, ast->rhs);

            if (AddUpdate <= ast->kind && ast->kind <= DivideUpdate) {
                if (is_numeric(t1) && is_numeric(t2) && numtype_priority(t1) >= numtype_priority(t2))
                    return t1;
            }

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
            else if (is_numeric(lhs_t) && is_numeric(rhs_t))
                return Type(BoolType);
            else
                TYPE_ERR(f, ast, "Ordered comparison is not supported for %s and %s", type_to_string(lhs_t), type_to_string(rhs_t));
        }

        case Not: {
            return Type(BoolType);
        }

        case Equal: case NotEqual: {
            bl_type_t *lhs_t = get_type(f, bindings, ast->lhs);
            bl_type_t *rhs_t = get_type(f, bindings, ast->rhs);
            if (type_is_a(lhs_t, rhs_t) || type_is_a(rhs_t, lhs_t))
                return Type(BoolType);
            else if (is_numeric(lhs_t) && is_numeric(rhs_t))
                return Type(BoolType);
            else
                TYPE_ERR(f, ast, "These two values have incompatible types: %s vs %s", type_to_string(lhs_t), type_to_string(rhs_t));
        }

        case FunctionDef: case Lambda: {
            NEW_LIST(bl_type_t*, args);
            for (int64_t i = 0; i < LIST_LEN(ast->fn.arg_types); i++) {
                ast_t *arg_def = LIST_ITEM(ast->fn.arg_types, i);
                bl_type_t *t = parse_type(f, bindings, arg_def);
                APPEND(args, t);
            }

            bl_type_t *ret = NULL;
            if (ast->kind == FunctionDef) {
                if (ast->fn.ret_type) {
                    ret = parse_type(f, bindings, ast->fn.ret_type);
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
            if (ret == NULL) ret = Type(VoidType);
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
                if (t->kind != OptionalType)
                    t = Type(OptionalType, .nonnil=t);
            }
            return t;
        }

        case While: case Repeat: case For: {
            return Type(VoidType);
        }

        default: break;
    }
    TYPE_ERR(f, ast, "Couldn't figure out type for %s", get_ast_kind_name(ast->kind));
}

void check_discardable(file_t *f, hashmap_t *bindings, ast_t *ast)
{
    switch (ast->kind) {
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate:
    case Assign: case Declare: case Block: case FunctionDef:
        return;
    default: {
        bl_type_t *t = get_type(f, bindings, ast);
        if (t->kind == OptionalType)
            t = t->nonnil;

        if (!(t->kind == VoidType || t->kind == AbortType)) {
            TYPE_ERR(f, ast, "This value has a return type of %s but the value is being ignored", type_to_string(t));
        }
    }
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
