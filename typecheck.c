// Logic for getting a Blang type from an AST node
#include <bhash.h>
#include <gc.h>
#include <stdarg.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

#include "ast.h"
#include "environment.h"
#include "typecheck.h"
#include "types.h"
#include "util.h"

static bl_type_t *get_clause_type(env_t *env, ast_t *condition, ast_t *body)
{
    if (condition && condition->tag == Declare) {
        hashmap_t *body_bindings = hashmap_new();
        body_bindings->fallback = env->bindings;
        bl_type_t *t = get_type(env, condition);
        assert(t);
        binding_t b = {.type=t};
        auto decl = Match(condition, Declare);
        hashmap_set(body_bindings, decl->name, &b);
        return get_type(env, body);
    } else {
        return get_type(env, body);
    }
}

bl_type_t *parse_type(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case Var: {
        binding_t *b = hashmap_get(env->bindings, Match(ast, Var)->name);
        if (!b || b->type->tag != TypeType)
            compile_err(env, ast, "I don't know any type with this name.");
        return b->type_value;
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        bl_type_t *fielded_t = parse_type(env, access->fielded);
        binding_t *b = get_from_namespace(env, fielded_t, access->field);
        if (!b || b->type->tag != TypeType)
            compile_err(env, ast, "I don't know any type with this name.");
        return b->type_value;
    }

    case TypeArray: {
        ast_t *item_type = Match(ast, TypeArray)->item_type;
        bl_type_t *item_t = parse_type(env, item_type);
        if (!item_t) compile_err(env, item_type, "I can't figure out what this type is.");
        return Type(ArrayType, .item_type=item_t);
    }

    case TypePointer: {
        auto ptr = Match(ast, TypePointer);
        if (ptr->pointed->tag == TypeOptional) {
            bl_type_t *pointed_t = parse_type(env, Match(ptr->pointed, TypeOptional)->type);
            return Type(PointerType, .is_optional=true, .pointed=pointed_t);
        } else {
            bl_type_t *pointed_t = parse_type(env, ptr->pointed);
            return Type(PointerType, .is_optional=false, .pointed=pointed_t);
        }
    }

    case TypeOptional: {
        auto opt = Match(ast, TypeOptional);
        bl_type_t *t = parse_type(env, opt->type);
        if (t->tag != PointerType)
            compile_err(env, ast, "I only know how to do optional types for pointers like @%s (because NULL is used to represent the absence of a value), "
                        "but this type isn't a pointer", type_to_string(t));
        return Type(PointerType, .is_optional=true, .pointed=Match(t, PointerType)->pointed);
    }

    case TypeFunction: {
        auto fn = Match(ast, TypeFunction);
        bl_type_t *ret_t = parse_type(env, fn->ret_type);
        NEW_LIST(bl_type_t*, arg_types);
        LIST_FOR (fn->arg_types, arg_t, _) {
            bl_type_t *bl_arg_t = parse_type(env, *arg_t);
            APPEND(arg_types, bl_arg_t);
        }
        return Type(FunctionType, .arg_types=arg_types, .ret=ret_t);
    }
    default: compile_err(env, ast, "This is not a Type value");
    }
}

bl_type_t *get_type(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case Nil: {
        bl_type_t *pointed = parse_type(env, Match(ast, Nil)->type);
        return Type(PointerType, .is_optional=true, .pointed=pointed);
    }
    case Bool: {
        return Type(BoolType);
    }
    case Int: {
        switch (Match(ast, Int)->precision) {
        case 64: return Type(IntType);
        case 32: return Type(Int32Type);
        case 16: return Type(Int16Type);
        case 8: return Type(Int8Type);
        default: compile_err(env, ast, "Unsupported precision");
        }
    }
    case Num: {
        switch (Match(ast, Num)->precision) {
        case 64: return Type(NumType);
        case 32: return Type(Num32Type);
        default: compile_err(env, ast, "Unsupported precision");
        }
    }
    case TypeOf: {
        return Type(TypeType);
    }
    case SizeOf: {
        return Type(IntType);
    }
    case HeapAllocate: {
        bl_type_t *pointed = get_type(env, Match(ast, HeapAllocate)->value);
        return Type(PointerType, .is_optional=false, .pointed=pointed);
    }
    case Dereference: {
        bl_type_t *pointer_t = get_type(env, Match(ast, Dereference)->value);
        if (pointer_t->tag != PointerType)
            compile_err(env, ast, "You're attempting to dereference something that isn't a pointer (it's a %s)",
                        type_to_string(pointer_t));
        auto ptr = Match(pointer_t, PointerType);
        if (ptr->is_optional)
            compile_err(env, ast, "You're attempting to dereference a pointer whose type indicates it could be nil");
        return ptr->pointed;
    }
    case Maybe: {
        ast_t *value = Match(ast, Maybe)->value;
        bl_type_t *pointed = get_type(env, value);
        if (pointed->tag != PointerType)
            compile_err(env, value, "This value isn't a pointer type, so it doesn't make sense to say it's optional. "
                        "You can use `?@` to make it a potentially nil pointer to a heap allocated value.");
        pointed = Match(pointed, PointerType)->pointed;
        return Type(PointerType, .is_optional=true, .pointed=pointed);
    }
    case Range: {
        return Type(RangeType);
    }
    case Interp: {
        return get_type(env, Match(ast, Interp)->value);
    }
    case StringJoin: case StringLiteral: {
        return Type(ArrayType, .item_type=Type(CharType));
    }
    case Var: {
        istr_t name = Match(ast, Var)->name;
        binding_t *binding = get_binding(env, name);
        if (binding) {
            return binding->type;
        } else {
            compile_err(env, ast, "I can't figure out what \"%s\" refers to", name);
        }
    }
    case Len: {
        return Type(IntType);
    }
    case Array: {
        auto list = Match(ast, Array);
        if (list->type)
            return parse_type(env, list->type);

        bl_type_t *item_type = NULL;
        for (int64_t i = 0; i < LIST_LEN(list->items); i++) {
            ast_t *item = LIST_ITEM(list->items, i);
            bl_type_t *t2 = get_type(env, item);
            while (t2->tag == GeneratorType)
                t2 = Match(t2, GeneratorType)->generated;
            bl_type_t *merged = item_type ? type_or_type(item_type, t2) : t2;
            if (!merged)
                compile_err(env, LIST_ITEM(list->items, i),
                            "This list item has type %s, which is different from earlier list items which have type %s",
                            type_to_string(t2),  type_to_string(item_type));
            item_type = merged;
        }
        return Type(ArrayType, .item_type=item_type);
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        bl_type_t *fielded_t = get_type(env, access->fielded);
        bool is_optional = (fielded_t->tag == PointerType) ? Match(fielded_t, PointerType)->is_optional : false;
        bl_type_t *value_t = (fielded_t->tag == PointerType) ? Match(fielded_t, PointerType)->pointed : fielded_t;
        switch (value_t->tag) {
        case StructType: {
            auto struct_t = Match(value_t, StructType);
            for (int64_t i = 0, len = LIST_LEN(struct_t->field_names); i < len; i++) {
                if (LIST_ITEM(struct_t->field_names, i) == access->field) {
                    if (is_optional)
                        compile_err(env, access->fielded, "This value may be nil, so accessing members on it is unsafe.");
                    return LIST_ITEM(struct_t->field_types, i);
                }
            }
            goto class_lookup;
        }
        case TaggedUnionType: {
            auto union_t = Match(Match(fielded_t, TaggedUnionType)->data, UnionType);
            for (int64_t i = 0, len = LIST_LEN(union_t->field_names); i < len; i++) {
                if (LIST_ITEM(union_t->field_names, i) == access->field) {
                    if (is_optional)
                        compile_err(env, access->fielded, "This value may be nil, so accessing members on it is unsafe.");
                    return LIST_ITEM(union_t->field_types, i);
                }
            }
            goto class_lookup;
        }
        case TypeType: {
            binding_t *type_binding = get_ast_binding(env, access->fielded);
            if (!type_binding || type_binding->type->tag != TypeType || !type_binding->type_value)
                compile_err(env, access->fielded, "Something went wrong with looking up this type");
            binding_t *binding = get_from_namespace(env, type_binding->type_value, access->field);
            if (binding)
                return binding->type;
            else
                compile_err(env, ast, "I can't find anything called %s on type %s", access->field, type_to_string(fielded_t));
        }
        default: {
          class_lookup:;
            binding_t *binding = get_from_namespace(env, value_t, access->field);
            if (binding)
                return binding->type;
            else
                compile_err(env, ast, "I can't find any field or method called \"%s\" on type %s", access->field, type_to_string(fielded_t));
        }
        }
    }
    case Index: {
        auto indexing = Match(ast, Index);
        bl_type_t *indexed_t = get_type(env, indexing->indexed);
        switch (indexed_t->tag) {
        case ArrayType: {
            bl_type_t *index_t = get_type(env, indexing->index);
            switch (index_t->tag) {
            case RangeType: return indexed_t;
            case IntType: case Int32Type: case Int16Type: case Int8Type: case CharType:
                return Match(indexed_t, ArrayType)->item_type;
            default: compile_err(env, indexing->index, "I only know how to index lists using integers, not %s", type_to_string(index_t));
            }
        }
        // TODO: support ranges like (99..123)[5]
        // TODO: support slicing arrays like ([1,2,3,4])[2..10]
        default: {
            compile_err(env, ast, "I don't know how to index %s values", type_to_string(indexed_t));
        }
        }
    }
    case KeywordArg: {
        return get_type(env, Match(ast, KeywordArg)->arg);
    }
    case FunctionCall: {
        auto call = Match(ast, FunctionCall);
        bl_type_t *fn_type_t = get_type(env, call->fn);
        if (fn_type_t->tag != FunctionType) {
            compile_err(env, call->fn, "You're calling a value of type %s and not a function", type_to_string(fn_type_t));
        }
        auto fn_type = Match(fn_type_t, FunctionType);
        return fn_type->ret;
    }
    case Block: {
        auto block = Match(ast, Block);
        ast_t *last = LIST_ITEM(block->statements, LIST_LEN(block->statements)-1);
        return get_type(env, last);
    }
    case Do: {
        auto do_ = Match(ast, Do);
        return get_type(env, LIST_ITEM(do_->blocks, 0));
    }
    case Declare: {
        return Type(VoidType);
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
        return parse_type(env, Match(ast, Cast)->type);
    }
    case As: {
        return parse_type(env, Match(ast, As)->type);
    }
    case TypeArray: case TypePointer: case TypeFunction: {
        return Type(TypeType);
    }
    case Negative: {
        bl_type_t *t = get_type(env, Match(ast, Negative)->value);
        if (!is_numeric(t))
            compile_err(env, ast, "I only know how to negate numeric types, not %s", type_to_string(t));
        return t;
    }
    case And: {
        auto and_ = Match(ast, And);
        bl_type_t *lhs_t = get_type(env, and_->lhs),
                  *rhs_t = get_type(env, and_->rhs);

        if (lhs_t->tag == BoolType && rhs_t->tag == BoolType) {
            return lhs_t;
        } else if (lhs_t->tag == BoolType && rhs_t->tag == AbortType) {
            return lhs_t;
        } else if (rhs_t->tag == AbortType) {
            return lhs_t;
        } else if (lhs_t->tag == PointerType && rhs_t->tag == PointerType) {
            auto lhs_ptr = Match(lhs_t, PointerType);
            auto rhs_ptr = Match(rhs_t, PointerType);
            if (lhs_ptr->pointed == rhs_ptr->pointed)
                return Type(PointerType, .pointed=lhs_ptr->pointed, .is_optional=lhs_ptr->is_optional || rhs_ptr->is_optional);
        } else if (is_integral(lhs_t) && is_integral(rhs_t)) {
            return numtype_priority(lhs_t) >= numtype_priority(rhs_t) ? lhs_t : rhs_t;
        }

        compile_err(env, ast, "I can't figure out the type of this `and` expression because the left side is a %s, but the right side is a %s.",
                    type_to_string(lhs_t), type_to_string(rhs_t));
    }
    case Or: {
        auto or_ = Match(ast, Or);
        bl_type_t *lhs_t = get_type(env, or_->lhs),
                  *rhs_t = get_type(env, or_->rhs);

        if (lhs_t->tag == BoolType && rhs_t->tag == BoolType)
            return lhs_t;
        else if (lhs_t->tag == BoolType && rhs_t->tag == AbortType)
            return lhs_t;
        else if (is_integral(lhs_t) && is_integral(rhs_t))
            return numtype_priority(lhs_t) >= numtype_priority(rhs_t) ? lhs_t : rhs_t;

        if (lhs_t->tag == PointerType) {
            auto lhs_ptr = Match(lhs_t, PointerType);
            if (rhs_t->tag == AbortType) {
                return Type(PointerType, .pointed=lhs_ptr->pointed, .is_optional=false);
            } else if (rhs_t->tag == PointerType) {
                auto rhs_ptr = Match(rhs_t, PointerType);
                if (rhs_ptr->pointed == lhs_ptr->pointed)
                    return Type(PointerType, .pointed=lhs_ptr->pointed, .is_optional=lhs_ptr->is_optional && rhs_ptr->is_optional);
            }
        }
        compile_err(env, ast, "I can't figure out the type of this `or` expression because the left side is a %s, but the right side is a %s.",
                    type_to_string(lhs_t), type_to_string(rhs_t));
    }
    case Xor: {
        auto xor = Match(ast, Xor);
        bl_type_t *lhs_t = get_type(env, xor->lhs),
                  *rhs_t = get_type(env, xor->rhs);

        if (lhs_t->tag == BoolType && rhs_t->tag == BoolType)
            return lhs_t;
        else if (is_integral(lhs_t) && is_integral(rhs_t))
            return numtype_priority(lhs_t) >= numtype_priority(rhs_t) ? lhs_t : rhs_t;

        compile_err(env, ast, "I can't figure out the type of this `xor` expression because the left side is a %s, but the right side is a %s.",
                    type_to_string(lhs_t), type_to_string(rhs_t));
    }
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate: {
        return Type(VoidType);
    }
    case Add: case Subtract: case Divide: case Multiply: case Power: case Modulus: {
        // Unsafe! These types *should* have the same fields and this saves a lot of duplicate code:
        ast_t *lhs = ast->__data.Add.lhs,
              *rhs = ast->__data.Add.rhs;
        // Okay safe again
        bl_type_t *t1 = get_type(env, lhs),
                  *t2 = get_type(env, rhs);

        if (AddUpdate <= ast->tag && ast->tag <= DivideUpdate) {
            if (is_numeric(t1) && is_numeric(t2) && numtype_priority(t1) >= numtype_priority(t2))
                return t1;
        } else {
            if (is_numeric(t1) && is_numeric(t2))
                return numtype_priority(t1) >= numtype_priority(t2) ? t1 : t2;
        }

        if (t1 == t2)
            return t1;

        compile_err(env, ast, "I don't know how to do math operations between %s and %s",
                    type_to_string(t1), type_to_string(t2));
    }

    case Less: case LessEqual: case Greater: case GreaterEqual: {
        return Type(BoolType);
    }

    case Not: {
        bl_type_t *t = get_type(env, Match(ast, Not)->value);
        if (t->tag == BoolType || is_integral(t))
            return t;
        compile_err(env, ast, "I only know what `not` means for Bools and integers, but this is a %s", type_to_string(t)); 
    }

    case Equal: case NotEqual: {
        ast_t *lhs, *rhs;
        if (ast->tag == Equal) {
            lhs = Match(ast, Equal)->lhs, rhs = Match(ast, Equal)->rhs;
        } else {
            lhs = Match(ast, NotEqual)->lhs, rhs = Match(ast, NotEqual)->rhs;
        }
        bl_type_t *lhs_t = get_type(env, lhs);
        bl_type_t *rhs_t = get_type(env, rhs);
        if (type_is_a(lhs_t, rhs_t) || type_is_a(rhs_t, lhs_t))
            return Type(BoolType);
        else if (is_numeric(lhs_t) && is_numeric(rhs_t))
            return Type(BoolType);
        else
            compile_err(env, ast, "I only know how to compare values that have the same type, but this comparison is between a %s and a %s",
                        type_to_string(lhs_t), type_to_string(rhs_t));
    }

    case Lambda: {
        auto lambda = Match(ast, Lambda);
        NEW_LIST(istr_t, arg_names);
        NEW_LIST(bl_type_t*, arg_types);
        for (int64_t i = 0; i < LIST_LEN(lambda->arg_types); i++) {
            ast_t *arg_def = LIST_ITEM(lambda->arg_types, i);
            bl_type_t *t = parse_type(env, arg_def);
            APPEND(arg_names, LIST_ITEM(lambda->arg_names, i));
            APPEND(arg_types, t);
        }

        // Include only global bindings:
        hashmap_t *body_bindings = hashmap_new();
        for (hashmap_t *h = env->bindings; h; h = h->fallback) {
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
        env_t lambda_env = *env;
        lambda_env.bindings = body_bindings;
        bl_type_t *ret = get_type(&lambda_env, Match(lambda->body, Return)->value);
        return Type(FunctionType, .arg_names=arg_names, .arg_types=arg_types, .ret=ret);
    }

    case FunctionDef: {
        auto def = Match(ast, FunctionDef);
        NEW_LIST(istr_t, arg_names);
        NEW_LIST(bl_type_t*, arg_types);
        NEW_LIST(ast_t*, arg_defaults);
        for (int64_t i = 0; i < LIST_LEN(def->arg_types); i++) {
            ast_t *arg_def = LIST_ITEM(def->arg_types, i);
            APPEND(arg_names, LIST_ITEM(def->arg_names, i));
            if (arg_def) {
                APPEND(arg_types, parse_type(env, arg_def));
                APPEND(arg_defaults, NULL);
            } else {
                ast_t *default_val = LIST_ITEM(def->arg_defaults, i);
                APPEND(arg_types, get_type(env, default_val));
                APPEND(arg_defaults, default_val);
            }
        }

        bl_type_t *ret = def->ret_type ? parse_type(env, def->ret_type) : Type(VoidType);
        return Type(FunctionType, .arg_names=arg_names, .arg_types=arg_types, .arg_defaults=arg_defaults, .ret=ret);
    }

    case StructDef: case EnumDef: {
        return Type(VoidType);
    }

    case Struct: {
        auto struct_ = Match(ast, Struct);
        if (!struct_->type)
            compile_err(env, ast, "I haven't implemented anonymous structs yet");
        binding_t *b = get_ast_binding(env, struct_->type);
        if (!b)
            compile_err(env, struct_->type, "I can't figure out this type");
        if (b->enum_type)
            return b->enum_type;
        else if (b->type_value)
            return b->type_value;
        else
            compile_err(env, ast, "There isn't any kind of struct like this");
        // return parse_type(env, struct_->type);
    }

    case If: {
        bl_type_t *t = NULL;
        auto if_ = Match(ast, If);
        LIST_FOR (if_->clauses, clause, _) {
            bl_type_t *clause_t = get_clause_type(env, clause->condition, clause->body);
            bl_type_t *t2 = type_or_type(t, clause_t);
            if (!t2)
                compile_err(env, clause->body,
                            "I was expecting this block to have a %s value (based on earlier clauses), but it actually has a %s value.",
                            type_to_string(t), type_to_string(clause_t));
            t = t2;
        }
        if (if_->else_body) {
            bl_type_t *else_type = get_type(env, if_->else_body);
            bl_type_t *t2 = type_or_type(t, else_type);
            if (!t2)
                compile_err(env, if_->else_body,
                            "I was expecting this block to have a %s value (based on earlier clauses), but it actually has a %s value.",
                            type_to_string(t), type_to_string(else_type));
            t = t2;
        } else {
            if (t->tag == VoidType)
                return t;
            t = Type(GeneratorType, .generated=t);
        }
        return t;
    }

    case When: {
        bl_type_t *t = NULL;
        auto when = Match(ast, When);
        LIST_FOR (when->cases, case_, _) {
            bl_type_t *case_t = get_type(env, (case_)->body);
            bl_type_t *t2 = type_or_type(t, case_t);
            if (!t2)
                compile_err(env, (case_)->body,
                            "I was expecting this block to have a %s value (based on earlier clauses), but it actually has a %s value.",
                            type_to_string(t), type_to_string(case_t));
            t = t2;
        }
        if (when->default_body) {
            bl_type_t *else_type = get_type(env, when->default_body);
            bl_type_t *t2 = type_or_type(t, else_type);
            if (!t2)
                compile_err(env, when->default_body,
                            "I was expecting this block to have a %s value (based on earlier clauses), but it actually has a %s value.",
                            type_to_string(t), type_to_string(else_type));
            t = t2;
        } else {
            if (t->tag == VoidType)
                return t;
            t = Type(GeneratorType, .generated=t);
        }
        return t;
    }

    case While: {
        return Type(GeneratorType, .generated=get_type(env, Match(ast, While)->body));
    }
    case Repeat: {
        return Type(GeneratorType, .generated=get_type(env, Match(ast, Repeat)->body));
    }
    case For: {
        auto for_loop = Match(ast, For);
        bl_type_t *iter_t = get_type(env, for_loop->iter);
        if (iter_t->tag == PointerType) iter_t = Match(iter_t, PointerType)->pointed;
        hashmap_t *loop_bindings = hashmap_new();
        loop_bindings->fallback = env->bindings;
        switch (iter_t->tag) {
        case ArrayType: {
            auto list_t = Match(iter_t, ArrayType);
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
        case StructType: {
            // TODO: check for .next field
            if (for_loop->key)
                hashmap_set(loop_bindings, for_loop->key, new(binding_t, .type=Type(IntType)));
            if (for_loop->value)
                hashmap_set(loop_bindings, for_loop->value, new(binding_t, .type=Type(PointerType, .pointed=iter_t, .is_optional=false)));
            break;
        }
        default:
            compile_err(env, for_loop->iter, "I don't know how to iterate over %s values like this", type_to_string(iter_t));
            break;
        }
        env_t loop_env = *env;
        loop_env.bindings = loop_bindings;
        bl_type_t *t = get_type(&loop_env, for_loop->body);
        return Type(GeneratorType, .generated=t);
    }

    default: break;
    }
    compile_err(env, ast, "I can't figure out what type a %s is", get_ast_tag_name(ast->tag));
}

void check_discardable(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate:
    case Assign: case Declare: case Block: case FunctionDef: case StructDef:
        return;
    default: {
        bl_type_t *t = get_type(env, ast);
        bool was_generator = (t->tag == GeneratorType);
        while (t->tag == GeneratorType) t = Match(t, GeneratorType)->generated;
        if (!(t->tag == VoidType || t->tag == AbortType)) {
            if (was_generator)
                compile_err(env, ast, "This expression can produce a value of type %s but the value is being ignored. If you want to intentionally ignore the value, assign the body of the block to a variable called \"_\".",
                            type_to_string(t));
            else
                compile_err(env, ast, "This expression has a type of %s but the value is being ignored. If you want to intentionally ignore it, assign the value to a variable called \"_\".",
                            type_to_string(t));
        }
    }
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
