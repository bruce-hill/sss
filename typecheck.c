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
    if (condition && condition->tag == Declare) {
        hashmap_t *body_bindings = hashmap_new();
        body_bindings->fallback = bindings;
        bl_type_t *t = get_type(f, bindings, condition);
        assert(t);
        binding_t b = {.type=t};
        auto decl = Match(condition, Declare);
        hashmap_set(body_bindings, decl->name, &b);
        return get_type(f, body_bindings, body);
    } else {
        return get_type(f, bindings, body);
    }
}

binding_t *get_binding(hashmap_t *bindings, ast_t *ast)
{
    switch (ast->tag) {
    case Var: {
        binding_t *binding = hashmap_get(bindings, Match(ast, Var)->name);
        return binding ? binding : NULL;
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        binding_t *ns_binding = get_binding(bindings, access->fielded);
        if (!ns_binding || !ns_binding->namespace) return NULL;
        binding_t *binding = hashmap_get(ns_binding->namespace, access->field);
        return binding ? binding : NULL;
    }
    default: return NULL;
    }
}

hashmap_t *get_namespace(hashmap_t *bindings, ast_t *ast)
{
    binding_t *binding = get_binding(bindings, ast);
    return binding ? binding->namespace : NULL;
}

bl_type_t *parse_type(file_t *f, hashmap_t *bindings, ast_t *ast)
{
    switch (ast->tag) {
    case Var: case FieldAccess: {
        binding_t *b = get_binding(bindings, ast);
        if (!b || b->type->tag != TypeType)
            TYPE_ERR(f, ast, "I don't know any type with this name.");
        return b ? b->type_value : NULL;
    }

    case TypeList: {
        ast_t *item_type = Match(ast, TypeList)->item_type;
        bl_type_t *item_t = parse_type(f, bindings, item_type);
        if (!item_t) TYPE_ERR(f, item_type, "I can't figure out what this type is.");
        return Type(ListType, .item_type=item_t);
    }

    case TypeOption: {
        ast_t *nonnil = Match(ast, TypeOption)->nonnil;
        bl_type_t *nonnil_t = parse_type(f, bindings, nonnil);
        if (!nonnil_t) TYPE_ERR(f, nonnil, "I can't figure out what this type is.");
        return Type(OptionalType, .nonnil=nonnil_t);
    }
    case TypeFunction: {
        auto fn = Match(ast, TypeFunction);
        bl_type_t *ret_t = parse_type(f, bindings, fn->ret_type);
        NEW_LIST(bl_type_t*, arg_types);
        LIST_FOR (fn->arg_types, arg_t, _) {
            bl_type_t *bl_arg_t = parse_type(f, bindings, *arg_t);
            APPEND(arg_types, bl_arg_t);
        }
        return Type(FunctionType, .arg_types=arg_types, .ret=ret_t);
    }
    default: TYPE_ERR(f, ast, "This is not a Type value");
    }
}

bl_type_t *get_type(file_t *f, hashmap_t *bindings, ast_t *ast)
{
    switch (ast->tag) {
    case Nil: {
        bl_type_t *nonnil = parse_type(f, bindings, Match(ast, Nil)->type);
        return nonnil->tag == OptionalType ? nonnil : Type(OptionalType, .nonnil=nonnil);
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
        return Type(TypeType);
    }
    case SizeOf: {
        return Type(IntType);
    }
    case Maybe: {
        bl_type_t *nonnil = get_type(f, bindings, Match(ast, Maybe)->value);
        return nonnil->tag == OptionalType ? nonnil : Type(OptionalType, .nonnil=nonnil);
    }
    case Range: {
        return Type(RangeType);
    }
    case StringJoin: case StringLiteral: {
        return Type(StringType);
    }
    case Var: {
        istr_t name = Match(ast, Var)->name;
        binding_t *binding = hashmap_get(bindings, name);
        if (binding) {
            return binding->type;
        } else {
            TYPE_ERR(f, ast, "I can't figure out what \"%s\" refers to", name);
        }
    }
    case Len: {
        return Type(IntType);
    }
    case List: {
        auto list = Match(ast, List);
        if (list->type)
            return parse_type(f, bindings, list->type);

        bl_type_t *item_type = NULL;
        for (int64_t i = 0; i < LIST_LEN(list->items); i++) {
            ast_t *item = LIST_ITEM(list->items, i);
            bl_type_t *t2;
            switch (item->tag) {
            case For: {
                auto for_loop = Match(item, For);
                bl_type_t *iter_t = get_type(f, bindings, for_loop->iter);
                if (iter_t->tag == OptionalType) iter_t = Match(iter_t, OptionalType)->nonnil;
                hashmap_t *loop_bindings = hashmap_new();
                loop_bindings->fallback = bindings;
                switch (iter_t->tag) {
                case ListType: {
                    auto list_t = Match(iter_t, ListType);
                    if (for_loop->key)
                        hashmap_set(loop_bindings, for_loop->key, new(binding_t, .type=Type(IntType)));
                    if (for_loop->value)
                        hashmap_set(loop_bindings, for_loop->value, new(binding_t, .type=list_t->item_type));
                    break;
                }
                case RangeType: {
                    if (for_loop->key)
                        hashmap_set(loop_bindings, for_loop->key, new(binding_t, .type=Type(IntType)));
                    if (for_loop->value)
                        hashmap_set(loop_bindings, for_loop->value, new(binding_t, .type=Type(IntType)));
                    break;
                }
                default:
                    TYPE_ERR(f, for_loop->iter, "I don't know how to iterate over %s values like this", type_to_string(iter_t));
                    break;
                }
                t2 = get_type(f, loop_bindings, for_loop->body);
                break;
            }
            case While: {
                auto loop = Match(item, While);
                t2 = get_type(f, bindings, loop->body);
                break;
            }
            case Repeat: {
                auto loop = Match(item, Repeat);
                t2 = get_type(f, bindings, loop->body);
                break;
            }
            default: {
                t2 = get_type(f, bindings, item);
                break;
            }
            }
            bl_type_t *merged = item_type ? type_or_type(item_type, t2) : t2;
            if (!merged)
                TYPE_ERR(f, LIST_ITEM(list->items, i),
                         "This list item has type %s, which is different from earlier list items which have type %s",
                         type_to_string(t2),  type_to_string(item_type));
            item_type = merged;
        }
        return Type(ListType, .item_type=item_type);
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        bl_type_t *fielded_t = get_type(f, bindings, access->fielded);
        bl_type_t *nonnil = (fielded_t->tag == OptionalType) ? Match(fielded_t, OptionalType)->nonnil : fielded_t;
        switch (nonnil->tag) {
        case StructType: {
            auto struct_t = Match(nonnil, StructType);
            for (int64_t i = 0, len = LIST_LEN(struct_t->field_names); i < len; i++) {
                if (LIST_ITEM(struct_t->field_names, i) == access->field) {
                    if (nonnil != fielded_t)
                        TYPE_ERR(f, access->fielded, "This value may be nil, so accessing members on it is unsafe.");
                    return LIST_ITEM(struct_t->field_types, i);
                }
            }
            goto class_lookup;
        }
        case TaggedUnionType: {
            auto union_t = Match(Match(fielded_t, TaggedUnionType)->data, UnionType);
            for (int64_t i = 0, len = LIST_LEN(union_t->field_names); i < len; i++) {
                if (LIST_ITEM(union_t->field_names, i) == access->field) {
                    if (nonnil != fielded_t)
                        TYPE_ERR(f, access->fielded, "This value may be nil, so accessing members on it is unsafe.");
                    return LIST_ITEM(union_t->field_types, i);
                }
            }
            goto class_lookup;
        }
        case TypeType: {
            hashmap_t *ns = get_namespace(bindings, access->fielded);
            assert(ns);
            binding_t *binding = hashmap_get(ns, access->field);
            if (binding)
                return binding->type;
            else
                TYPE_ERR(f, ast, "I can't find anything called %s on type %s", access->field, type_to_string(fielded_t));
        }
        default: {
          class_lookup:;
            binding_t *type_binding = hashmap_get(bindings, nonnil);
            binding_t *binding = (type_binding && type_binding->namespace) ? hashmap_get(type_binding->namespace, access->field) : NULL;
            if (binding)
                return binding->type;
            else
                TYPE_ERR(f, ast, "I can't find any field or method called \"%s\" on type %s", access->field, type_to_string(fielded_t));
        }
        }
    }
    case Index: {
        auto indexing = Match(ast, Index);
        bl_type_t *indexed_t = get_type(f, bindings, indexing->indexed);
        switch (indexed_t->tag) {
        case ListType: {
            bl_type_t *index_t = get_type(f, bindings, indexing->index);
            switch (index_t->tag) {
            case IntType: case Int32Type: case Int16Type: case Int8Type: break;
            default: TYPE_ERR(f, indexing->index, "I only know how to index lists using integers, not %s", type_to_string(index_t));
            }
            return Match(indexed_t, ListType)->item_type;
        }
        // TODO: support accessing fields by integer like (Vec{3,4})[1] --> 3
        // TODO: support ranges like (99..123)[5]
        default: {
            TYPE_ERR(f, ast, "I don't know how to index %s values", type_to_string(indexed_t));
        }
        }
    }
    case KeywordArg: {
        return get_type(f, bindings, Match(ast, KeywordArg)->arg);
    }
    case FunctionCall: {
        auto call = Match(ast, FunctionCall);
        bl_type_t *fn_type_t = get_type(f, bindings, call->fn);
        if (fn_type_t->tag != FunctionType) {
            TYPE_ERR(f, call->fn, "You're calling a value of type %s and not a function", type_to_string(fn_type_t));
        }
        auto fn_type = Match(fn_type_t, FunctionType);
        // int64_t max_args = LIST_LEN(fn_type->arg_types);
        // int64_t min_args = max_args;
        // while (min_args > 0 && LIST_ITEM(fn_type->arg_types, min_args-1)->tag == OptionalType)
        //     --min_args;
        // int64_t len_args = LIST_LEN(call->args);
        // int64_t num_selfs = 0;
        // if (call->fn->tag == FieldAccess) {
        //     // Insert "self" argument
        //     auto access = Match(call->fn, FieldAccess);
        //     ast_t *self = access->fielded;
        //     bl_type_t *self_t = get_type(f, bindings, self);
        //     if (self_t->tag != TypeType) {
        //         bl_type_t *expected = LIST_ITEM(fn_type->arg_types, 0);
        //         if (is_numeric(self_t) && is_numeric(expected) && numtype_priority(self_t) < numtype_priority(expected))
        //             self_t = expected;
        //         if (!type_is_a(self_t, expected)) {
        //             TYPE_ERR(f, self, "I was expecting this argument to be a %s, but this value is a %s",
        //                      type_to_string(expected), type_to_string(self_t));
        //         }
        //         num_selfs += 1;
        //     }
        // }
        // if (num_selfs + len_args < min_args) {
        //     TYPE_ERR(f, ast, "I expected this function to have at least %ld arguments, but there's only %ld arguments here.", min_args, num_selfs + len_args);
        // } else if (num_selfs + len_args > max_args) {
        //     TYPE_ERR(f, LIST_ITEM(call->args, max_args), "I was only expecting %ld arguments for this function call, but everything from here on is too much.",
        //              max_args);
        // }
        // for (int64_t i = 0; i < len_args; i++) {
        //     ast_t *arg = LIST_ITEM(call->args, i);
        //     bl_type_t *arg_t = get_type(f, bindings, arg);
        //     bl_type_t *expected = LIST_ITEM(fn_type->arg_types, num_selfs + i);
        //     if (is_numeric(arg_t) && is_numeric(expected) && numtype_priority(arg_t) < numtype_priority(expected))
        //         arg_t = expected;
        //     if (!type_is_a(arg_t, expected)) {
        //         TYPE_ERR(f, arg, "I was expecting this argument to be a %s, but this value is a %s",
        //                  type_to_string(expected), type_to_string(arg_t));
        //     }
        // }
        return fn_type->ret;
    }
    case Block: {
        auto block = Match(ast, Block);
        ast_t *last = LIST_ITEM(block->statements, LIST_LEN(block->statements)-1);
        return get_type(f, bindings, last);
    }
    case Do: {
        auto do_ = Match(ast, Do);
        return get_type(f, bindings, LIST_ITEM(do_->blocks, 0));
    }
    case Declare: {
        return get_type(f, bindings, Match(ast, Declare)->value);
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
    case Cast: {
        return parse_type(f, bindings, Match(ast, Cast)->type);
    }
    case As: {
        return parse_type(f, bindings, Match(ast, As)->type);
    }
    case TypeList: case TypeOption: case TypeFunction: {
        return Type(TypeType);
    }
    case Negative: {
        bl_type_t *t = get_type(f, bindings, Match(ast, Negative)->value);
        if (!is_numeric(t))
            TYPE_ERR(f, ast, "I only know how to negate numeric types, not %s", type_to_string(t));
        return t;
    }
    case And: {
        auto and_ = Match(ast, And);
        bl_type_t *lhs_t = get_type(f, bindings, and_->lhs),
                  *rhs_t = get_type(f, bindings, and_->rhs);

        if (lhs_t->tag == BoolType && rhs_t->tag == BoolType)
            return lhs_t;
        else if (lhs_t->tag == BoolType && rhs_t->tag == AbortType)
            return lhs_t;
        else if (rhs_t->tag == AbortType)
            return lhs_t;
        else if (lhs_t->tag == OptionalType && rhs_t == Match(lhs_t, OptionalType)->nonnil)
            return lhs_t;
        else if (is_integral(lhs_t) && is_integral(rhs_t))
            return numtype_priority(lhs_t) >= numtype_priority(rhs_t) ? lhs_t : rhs_t;

        TYPE_ERR(f, ast, "I can't figure out the type of this `and` expression because the left side is a %s, but the right side is a %s.",
                 type_to_string(lhs_t), type_to_string(rhs_t));
    }
    case Or: {
        auto or_ = Match(ast, Or);
        bl_type_t *lhs_t = get_type(f, bindings, or_->lhs),
                  *rhs_t = get_type(f, bindings, or_->rhs);

        if (lhs_t->tag == BoolType && rhs_t->tag == BoolType)
            return lhs_t;
        else if (lhs_t->tag == BoolType && rhs_t->tag == AbortType)
            return lhs_t;
        else if (is_integral(lhs_t) && is_integral(rhs_t))
            return numtype_priority(lhs_t) >= numtype_priority(rhs_t) ? lhs_t : rhs_t;

        if (lhs_t->tag == OptionalType) {
            auto lhs_option = Match(lhs_t, OptionalType);
            if (rhs_t == lhs_option->nonnil || rhs_t->tag == AbortType)
                return lhs_option->nonnil;
            else if (rhs_t == lhs_t)
                return lhs_t;
        }
        TYPE_ERR(f, ast, "I can't figure out the type of this `or` expression because the left side is a %s, but the right side is a %s.",
                 type_to_string(lhs_t), type_to_string(rhs_t));
    }
    case Xor: {
        auto xor = Match(ast, Xor);
        bl_type_t *lhs_t = get_type(f, bindings, xor->lhs),
                  *rhs_t = get_type(f, bindings, xor->rhs);

        if (lhs_t->tag == BoolType && rhs_t->tag == BoolType)
            return lhs_t;
        else if (is_integral(lhs_t) && is_integral(rhs_t))
            return numtype_priority(lhs_t) >= numtype_priority(rhs_t) ? lhs_t : rhs_t;

        TYPE_ERR(f, ast, "I can't figure out the type of this `xor` expression because the left side is a %s, but the right side is a %s.",
                 type_to_string(lhs_t), type_to_string(rhs_t));
    }
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate:
    case Add: case Subtract: case Divide: case Multiply: case Power: case Modulus: {
        // Unsafe! These types *should* have the same fields and this saves a lot of duplicate code:
        ast_t *lhs = ast->__data.Add.lhs,
              *rhs = ast->__data.Add.rhs;
        // Okay safe again
        bl_type_t *t1 = get_type(f, bindings, lhs),
                  *t2 = get_type(f, bindings, rhs);

        if (AddUpdate <= ast->tag && ast->tag <= DivideUpdate) {
            if (is_numeric(t1) && is_numeric(t2) && numtype_priority(t1) >= numtype_priority(t2))
                return t1;
        } else {
            if (is_numeric(t1) && is_numeric(t2))
                return numtype_priority(t1) >= numtype_priority(t2) ? t1 : t2;
        }

        if (t1 == t2) {
            if (is_numeric(t1))
                return t1;
            else if (t1->tag == DSLType || t1->tag == StringType)
                return t1;
        } else if (is_numeric(t1)) {
            TYPE_ERR(f, ast, "I only know how to do math operations on numeric types, but the right side of this operation is a %s.",
                     type_to_string(t2));
        } else if (is_numeric(t2)) {
            TYPE_ERR(f, ast, "I only know how to do math operations on numeric types, but the left side of this operation is a %s.",
                     type_to_string(t1));
        }
        TYPE_ERR(f, ast, "I only know how to do math operations between numeric types, not between a %s and a %s",
                 type_to_string(t1), type_to_string(t2));
    }

    case Less: case LessEqual: case Greater: case GreaterEqual: {
        return Type(BoolType);
    }

    case Not: {
        bl_type_t *t = get_type(f, bindings, Match(ast, Not)->value);
        if (t->tag == BoolType || is_integral(t))
            return t;
        TYPE_ERR(f, ast, "I only know what `not` means for Bools and integers, but this is a %s", type_to_string(t)); 
    }

    case Equal: case NotEqual: {
        ast_t *lhs, *rhs;
        if (ast->tag == Equal) {
            lhs = Match(ast, Equal)->lhs, rhs = Match(ast, Equal)->rhs;
        } else {
            lhs = Match(ast, NotEqual)->lhs, rhs = Match(ast, NotEqual)->rhs;
        }
        bl_type_t *lhs_t = get_type(f, bindings, lhs);
        bl_type_t *rhs_t = get_type(f, bindings, rhs);
        if (type_is_a(lhs_t, rhs_t) || type_is_a(rhs_t, lhs_t))
            return Type(BoolType);
        else if (is_numeric(lhs_t) && is_numeric(rhs_t))
            return Type(BoolType);
        else
            TYPE_ERR(f, ast, "I only know how to compare values that have the same type, but this comparison is between a %s and a %s",
                     type_to_string(lhs_t), type_to_string(rhs_t));
    }

    case Lambda: {
        auto lambda = Match(ast, Lambda);
        NEW_LIST(istr_t, arg_names);
        NEW_LIST(bl_type_t*, arg_types);
        for (int64_t i = 0; i < LIST_LEN(lambda->arg_types); i++) {
            ast_t *arg_def = LIST_ITEM(lambda->arg_types, i);
            bl_type_t *t = parse_type(f, bindings, arg_def);
            APPEND(arg_names, LIST_ITEM(lambda->arg_names, i));
            APPEND(arg_types, t);
        }

        // Include only global bindings:
        hashmap_t *body_bindings = hashmap_new();
        for (hashmap_t *h = bindings; h; h = h->fallback) {
            for (istr_t key = NULL; (key = hashmap_next(h, key)); ) {
                binding_t *val = hashmap_get_raw(h, key);
                assert(val);
                if (val->is_global)
                    hashmap_set(body_bindings, key, val);
            }
        }

        for (int64_t i = 0; i < LIST_LEN(lambda->arg_types); i++) {
            hashmap_set(body_bindings, LIST_ITEM(arg_names, i), new(binding_t, .type=LIST_ITEM(arg_types, i)));
        }
        bl_type_t *ret = get_type(f, body_bindings, Match(lambda->body, Return)->value);
        return Type(FunctionType, .arg_names=arg_names, .arg_types=arg_types, .ret=ret);
    }

    case FunctionDef: {
        auto def = Match(ast, FunctionDef);
        NEW_LIST(istr_t, arg_names);
        NEW_LIST(bl_type_t*, arg_types);
        for (int64_t i = 0; i < LIST_LEN(def->arg_types); i++) {
            ast_t *arg_def = LIST_ITEM(def->arg_types, i);
            bl_type_t *t = parse_type(f, bindings, arg_def);
            APPEND(arg_names, LIST_ITEM(def->arg_names, i));
            APPEND(arg_types, t);
        }

        bl_type_t *ret = def->ret_type ? parse_type(f, bindings, def->ret_type) : Type(VoidType);
        return Type(FunctionType, .arg_names=arg_names, .arg_types=arg_types, .ret=ret);
    }

    case StructDef: case EnumDef: {
        return Type(VoidType);
    }

    case Struct: {
        auto struct_ = Match(ast, Struct);
        if (struct_->type) {
            binding_t *b = get_binding(bindings, struct_->type);
            if (b->enum_type)
                return b->enum_type;
            else
                return b->type_value;
        }
        TYPE_ERR(f, ast, "I haven't implemented anonymous structs yet");
        // TODO: anonymous structs

        //             istr_t name = ast->struct_.name;
        //             NEW_LIST(istr_t, field_names);
        //             NEW_LIST(bl_type_t*, field_types);
        //             LIST_FOR (ast->struct_.members, member, _) {
        //                 if ((*member)->tag == StructField) {
        //                     APPEND(field_names, (*member)->named.name);
        //                     APPEND(field_types, get_type(f, bindings, (*member)->named.value));
        //                 } else {
        //                     assert(false);
        //                 }
        //             }
        //             return Type(StructType, .struct_.name=name, .struct_.field_names=field_names, .struct_.field_types=field_types);
    }

    case If: {
        bl_type_t *t = NULL;
        auto if_ = Match(ast, If);
        LIST_FOR (if_->clauses, clause, _) {
            bl_type_t *clause_t = get_clause_type(f, bindings, clause->condition, clause->body);
            bl_type_t *t2 = type_or_type(t, clause_t);
            if (!t2)
                TYPE_ERR(f, clause->body,
                         "I was expecting this block to have a %s value (based on earlier clauses), but it actually has a %s value.",
                         type_to_string(t), type_to_string(clause_t));
            t = t2;
        }
        if (if_->else_body) {
            bl_type_t *else_type = get_type(f, bindings, if_->else_body);
            bl_type_t *t2 = type_or_type(t, else_type);
            if (!t2)
                TYPE_ERR(f, if_->else_body,
                         "I was expecting this block to have a %s value (based on earlier clauses), but it actually has a %s value.",
                         type_to_string(t), type_to_string(else_type));
            t = t2;
        } else {
            if (t->tag != OptionalType)
                t = Type(OptionalType, .nonnil=t);
        }
        return t;
    }

    case When: {
        bl_type_t *t = NULL;
        auto when = Match(ast, When);
        LIST_FOR (when->cases, case_, _) {
            bl_type_t *case_t = get_type(f, bindings, (case_)->body);
            bl_type_t *t2 = type_or_type(t, case_t);
            if (!t2)
                TYPE_ERR(f, (case_)->body,
                         "I was expecting this block to have a %s value (based on earlier clauses), but it actually has a %s value.",
                         type_to_string(t), type_to_string(case_t));
            t = t2;
        }
        if (when->default_body) {
            bl_type_t *else_type = get_type(f, bindings, when->default_body);
            bl_type_t *t2 = type_or_type(t, else_type);
            if (!t2)
                TYPE_ERR(f, when->default_body,
                         "I was expecting this block to have a %s value (based on earlier clauses), but it actually has a %s value.",
                         type_to_string(t), type_to_string(else_type));
            t = t2;
        } else {
            if (t->tag != OptionalType)
                t = Type(OptionalType, .nonnil=t);
        }
        return t;
    }

    case While: case Repeat: case For: {
        return Type(VoidType);
    }

    default: break;
    }
    TYPE_ERR(f, ast, "I can't figure out what type a %s is", get_ast_tag_name(ast->tag));
}

void check_discardable(file_t *f, hashmap_t *bindings, ast_t *ast)
{
    switch (ast->tag) {
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate:
    case Assign: case Declare: case Block: case FunctionDef: case StructDef:
        return;
    default: {
        bl_type_t *t = get_type(f, bindings, ast);
        if (t->tag == OptionalType)
            t = Match(t, OptionalType)->nonnil;

        if (!(t->tag == VoidType || t->tag == AbortType)) {
            TYPE_ERR(f, ast, "This value has a return type of %s but the value is being ignored. If you want to intentionally ignore it, assign the value to a variable called \"_\".",
                     type_to_string(t));
        }
    }
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
