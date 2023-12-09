// Logic for getting an SSS type from an AST node
#include <gc.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <sys/stat.h>

#include "args.h"
#include "ast.h"
#include "environment.h"
#include "parse.h"
#include "typecheck.h"
#include "types.h"
#include "util.h"

// Cache of type string -> tuple type
static table_t tuple_types = {0};

sss_type_t *parse_type_ast(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case Var: {
        const char *name = Match(ast, Var)->name;
        sss_type_t *t = get_type_by_name(env, name);
        if (t) return t;
        compiler_err(env, ast, "I don't know a type with the name '%s'", name);
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        sss_type_t *fielded_t = parse_type_ast(env, access->fielded);
        sss_type_t *t = (sss_type_t*)get_from_namespace(env, fielded_t, heap_strf("#type:%s", access->field));
        if (!t)
            compiler_err(env, ast, "I don't know any type with this name.");
        return t;
    }
    case TypeArray: {
        ast_t *item_type = Match(ast, TypeArray)->item_type;
        sss_type_t *item_t = parse_type_ast(env, item_type);
        if (!item_t) compiler_err(env, item_type, "I can't figure out what this type is.");
        if (has_stack_memory(item_t))
            compiler_err(env, item_type, "Arrays can't have stack references because the array may outlive the stack frame.");
        return Type(ArrayType, .item_type=item_t);
    }
    case TypeTable: {
        ast_t *key_type_ast = Match(ast, TypeTable)->key_type;
        sss_type_t *key_type = parse_type_ast(env, key_type_ast);
        if (!key_type) compiler_err(env, key_type_ast, "I can't figure out what type this is.");
        if (has_stack_memory(key_type))
            compiler_err(env, key_type_ast, "Tables can't have stack references because the array may outlive the stack frame.");
        ast_t *val_type_ast = Match(ast, TypeTable)->value_type;
        sss_type_t *val_type = parse_type_ast(env, val_type_ast);
        if (!val_type) compiler_err(env, val_type_ast, "I can't figure out what type this is.");
        if (has_stack_memory(val_type))
            compiler_err(env, val_type_ast, "Tables can't have stack references because the array may outlive the stack frame.");
        return Type(TableType, .key_type=key_type, .value_type=val_type);
    }
    case TypePointer: {
        auto ptr = Match(ast, TypePointer);
        sss_type_t *pointed_t = parse_type_ast(env, ptr->pointed);
        if (pointed_t->tag == VoidType)
            compiler_err(env, ast, "Void pointers are not supported in SSS. You probably meant '!Memory'");
        return Type(PointerType, .is_optional=ptr->is_optional, .pointed=pointed_t, .is_stack=ptr->is_stack, .is_readonly=ptr->is_readonly);
    }
    case TypeFunction: {
        auto fn = Match(ast, TypeFunction);
        sss_type_t *ret_t = parse_type_ast(env, fn->ret_type);
        if (has_stack_memory(ret_t))
            compiler_err(env, fn->ret_type, "Functions are not allowed to return stack references, because the reference may no longer exist on the stack.");
        auto arg_names = EMPTY_ARRAY(const char*);
        auto arg_defaults = EMPTY_ARRAY(ast_t*);
        auto arg_types = EMPTY_ARRAY(sss_type_t*);
        env_t *default_arg_env = file_scope(env);
        default_arg_env->bindings = new(table_t, .fallback=default_arg_env->bindings);
        for (int64_t i = 0; i < LENGTH(fn->args.types); i++) {
            append(arg_names, ith(fn->args.names, i));
            sss_type_t *arg_t;
            if (ith(fn->args.types, i)) {
                arg_t = parse_type_ast(default_arg_env, ith(fn->args.types, i));
                append(arg_types, arg_t);
                append(arg_defaults, NULL);
            } else {
                arg_t = get_type(default_arg_env, ith(fn->args.defaults, i));
                append(arg_types, arg_t);
                append(arg_defaults, ith(fn->args.defaults, i));
            }
            Table_str_set(default_arg_env->bindings, ith(fn->args.names, i), new(binding_t, .type=arg_t));
        }
        return Type(FunctionType, .arg_names=arg_names, .arg_types=arg_types, .arg_defaults=arg_defaults, .ret=ret_t, .env=file_scope(env));
    }
    case TypeStruct: {
        auto struct_ = Match(ast, TypeStruct);
        auto member_names = EMPTY_ARRAY(const char*);
        auto member_types = EMPTY_ARRAY(sss_type_t*);
        sss_type_t *t = Type(StructType, .field_names=member_names, .field_types=member_types, .field_defaults=struct_->members.defaults);
        for (int64_t i = 0, len = LENGTH(struct_->members.types); i < len; i++) {
            const char *member_name = ith(struct_->members.names, i);
            append(member_names, member_name);
            ast_t *type_ast = ith(struct_->members.types, i);
            sss_type_t *member_t = type_ast ? parse_type_ast(env, type_ast) : get_type(env, ith(struct_->members.defaults, i));
            if (has_stack_memory(member_t))
                compiler_err(env, ith(struct_->members.types, i), "Structs can't have stack memory because the struct may outlive the stack frame.");
            append(member_types, member_t);
        }
        sss_type_t *memoized = Table_str_get(&tuple_types, type_to_string(t));
        if (memoized) {
            t = memoized;
        } else {
            Table_str_set(&tuple_types, type_to_string(t), t);
        }
        return t;
    }
    case TypeTaggedUnion: {
        auto tu = Match(ast, TypeTaggedUnion);
        auto members = EMPTY_ARRAY(sss_tagged_union_member_t);
        for (int64_t i = 0, len = LENGTH(tu->tag_names); i < len; i++) {
            args_t args = ith(tu->tag_args, i);
            sss_type_t *member_t = parse_type_ast(env, WrapAST(ast, TypeStruct, .members=args));
            if (member_t && has_stack_memory(member_t))
                compiler_err(env, ast, "Tagged unions can't hold stack memory because the tagged union may outlive the stack frame.");
            sss_tagged_union_member_t member = {
                .name=ith(tu->tag_names, i),
                .tag_value=ith(tu->tag_values, i),
                .type=member_t,
            };
            append(members, member);
        }
        return Type(TaggedUnionType, .members=members);
    }
    default: compiler_err(env, ast, "This is not a Type value");
    }
}

sss_type_t *get_iter_type(env_t *env, ast_t *iter)
{
    sss_type_t *iter_t = get_type(env, iter);
    for (;;) {
        if (iter_t->tag == PointerType) iter_t = Match(iter_t, PointerType)->pointed;
        // else if (iter_t->tag == VariantType) iter_t = Match(iter_t, VariantType)->variant_of;
        else break;
    }
    sss_type_t *base_t = base_variant(iter_t);
    switch (base_t->tag) {
    case ArrayType: return Match(base_t, ArrayType)->item_type;
    case TableType: return table_entry_type(base_t);
    case RangeType: return INT_TYPE;
    case StructType: {
        auto struct_ = Match(base_t, StructType);
        for (int64_t i = 0; i < LENGTH(struct_->field_names); i++) {
            if (streq(ith(struct_->field_names, i), "next")
                && type_eq(ith(struct_->field_types, i), Type(PointerType, .pointed=iter_t, .is_optional=true)))
                return Type(PointerType, .pointed=iter_t, .is_optional=false);
        }
        compiler_err(env, iter, "I don't know how to iterate over %T structs that don't have a .next member", iter_t);
    }
    case GeneratorType: return Match(base_t, GeneratorType)->generated;
    default:
        compiler_err(env, iter, "I don't know how to iterate over %T values like this", iter_t);
        break;
    }
}

sss_type_t *get_math_type(env_t *env, ast_t *ast, sss_type_t *lhs_t, ast_tag_e tag, sss_type_t *rhs_t)
{
    // Dereference:
    while (lhs_t->tag == PointerType)
        lhs_t = Match(lhs_t, PointerType)->pointed;
    while (rhs_t->tag == PointerType)
        rhs_t = Match(rhs_t, PointerType)->pointed;

    if (type_eq(lhs_t, rhs_t)) {
        return lhs_t;
    } else if (is_numeric(lhs_t) && is_numeric(rhs_t)) {
        sss_type_t *t = type_or_type(lhs_t, rhs_t);
        if (lhs_t->tag == VariantType && rhs_t->tag == VariantType)
            compiler_err(env, ast, "The two operands in this math operation have different types: %T vs %T", lhs_t, rhs_t);
        else if (lhs_t->tag == VariantType && rhs_t->tag != VariantType && (tag == Multiply || tag == Divide)
                 && (compare_precision(lhs_t, rhs_t) == NUM_PRECISION_EQUAL || compare_precision(lhs_t, rhs_t) == NUM_PRECISION_MORE))
            t = lhs_t;
        else if (rhs_t->tag == VariantType && lhs_t->tag != VariantType && tag == Multiply
                 && (compare_precision(rhs_t, lhs_t) == NUM_PRECISION_EQUAL || compare_precision(rhs_t, lhs_t) == NUM_PRECISION_MORE))
            t = rhs_t;

        if (!t) {
            if (lhs_t->tag == VariantType || rhs_t->tag == VariantType)
                compiler_err(env, ast, "This math operation between %T and %T is not supported", lhs_t, rhs_t);
            else
                compiler_err(env, ast, "The result of a math operation between %T and %T can't always fit in either type.", lhs_t, rhs_t);
        }
        return t;
    } else if (is_numeric(lhs_t) && (base_variant(rhs_t)->tag == StructType || base_variant(rhs_t)->tag == ArrayType)) {
        return rhs_t;
    } else if (is_numeric(rhs_t) && (base_variant(lhs_t)->tag == StructType || base_variant(lhs_t)->tag == ArrayType)) {
        return lhs_t;
    } else if (lhs_t->tag == BoolType && (base_variant(rhs_t)->tag == StructType || base_variant(rhs_t)->tag == ArrayType) && (tag == And || tag == Or || tag == Xor)) {
        return rhs_t;
    } else if (rhs_t->tag == BoolType && (base_variant(lhs_t)->tag == StructType || base_variant(lhs_t)->tag == ArrayType) && (tag == And || tag == Or || tag == Xor)) {
        return lhs_t;
    } else {
        compiler_err(env, ast, "I don't know how to do math operations between %T and %T", lhs_t, rhs_t);
    }
}

sss_type_t *get_field_type(env_t *env, sss_type_t *t, const char *field_name)
{
    sss_type_t *original_t = t;
    for (;;) {
        if (t->tag == PointerType) t = Match(t, PointerType)->pointed;
        else if (t->tag == VariantType) t = Match(t, VariantType)->variant_of;
        else break;
    }

    switch (t->tag) {
    case StructType: {
        auto struct_t = Match(t, StructType);
        for (int64_t i = 0, len = LENGTH(struct_t->field_names); i < len; i++) {
            const char *struct_field = ith(struct_t->field_names, i);
            if (!struct_field) struct_field = heap_strf("_%ld", i+1);
            if (streq(struct_field, field_name)) {
                return ith(struct_t->field_types, i);
            }
        }
        goto class_lookup;
    }
    case TaggedUnionType: {
        auto tagged = Match(t, TaggedUnionType);
        foreach (tagged->members, member, _) {
            if (streq(field_name, member->name))
                return member->type;
        }
        goto class_lookup;
    }
    case ArrayType: {
        if (streq(field_name, "length"))
            return INT_TYPE;

        auto array = Match(t, ArrayType);
        sss_type_t *item_t = array->item_type;

        // TODO: support other things like pointers
        if (base_variant(item_t)->tag == StructType) {
            // vecs.x ==> [v.x for v in vecs]
            auto struct_ = Match(base_variant(item_t), StructType);
            for (int64_t i = 0, len = LENGTH(struct_->field_names); i < len; i++) {
                const char *struct_field = ith(struct_->field_names, i);
                if (!struct_field) struct_field = heap_strf("_%ld", i+1);
                if (streq(struct_field, field_name)) {
                    return Type(ArrayType, .item_type=ith(struct_->field_types, i));
                }
            }
        }
        goto class_lookup;
    }
    case RangeType: {
        if (streq(field_name, "first") || streq(field_name, "last") || streq(field_name, "step") || streq(field_name, "length"))
            return INT_TYPE;
        goto class_lookup;
    }
    case TableType: {
        if (streq(field_name, "entries"))
            return Type(ArrayType, .item_type=table_entry_type(t));
        else if (streq(field_name, "length"))
            return INT_TYPE;
        else if (streq(field_name, "default"))
            return Type(PointerType, .pointed=Match(t, TableType)->value_type, .is_optional=true, .is_readonly=true);
        else if (streq(field_name, "fallback"))
            return Type(PointerType, .pointed=t, .is_optional=true, .is_readonly=true);
        else if (streq(field_name, "keys"))
            return Type(ArrayType, .item_type=Match(t, TableType)->key_type);
        else if (streq(field_name, "values"))
            return Type(ArrayType, .item_type=Match(t, TableType)->value_type);

        goto class_lookup;
    }
    default: break;
    }

  class_lookup:;
    t = original_t;
    for (;;) {
        binding_t *b = get_from_namespace(env, t, field_name);
        if (b) return b->type;

        if (t->tag == PointerType) t = Match(t, PointerType)->pointed;
        else if (t->tag == VariantType) t = Match(t, VariantType)->variant_of;
        else break;
    }
    return NULL;
}

static sss_type_t *generate(sss_type_t *t)
{
    if (t->tag == VoidType)
        return t;
    else
        return Type(GeneratorType, .generated=t);
}

sss_type_t *get_doctest_type(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case Return: {
        auto ret = Match(ast, Return);
        return ret->value ? get_doctest_type(env, ret->value) : Type(VoidType);
    }
    case Assign: {
        auto assign = Match(ast, Assign);
        auto members = EMPTY_ARRAY(ast_t*);
        for (int64_t i = 0; i < LENGTH(assign->targets); i++) {
            append(members, WrapAST(ith(assign->targets, i), KeywordArg, .name=heap_strf("_%d", i+1), .arg=ith(assign->targets, i)));
            // ast_t *target = ith(assign->targets, i);
            // append(members, WrapAST(target, KeywordArg, .name=heap_strf("%W", target), .arg=target));
        }
        return get_doctest_type(env, WrapAST(ast, Struct, .members=members));
    }
    case AddUpdate: case SubtractUpdate: case MultiplyUpdate: case DivideUpdate: case AndUpdate: case OrUpdate:
    case XorUpdate: case ConcatenateUpdate: case Declare: {
        // UNSAFE: this assumes all these types have the same layout:
        ast_t *lhs_ast = ast->__data.AddUpdate.lhs;
        // END UNSAFE
        return get_doctest_type(env, lhs_ast);
    }
    default:
        return get_type(env, ast);
    }
}

sss_type_t *get_type(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case Nil: {
        sss_type_t *pointed = parse_type_ast(env, Match(ast, Nil)->type);
        return Type(PointerType, .is_optional=true, .pointed=pointed);
    }
    case Bool: {
        return Type(BoolType);
    }
    case Int: {
        auto i = Match(ast, Int);
        switch (i->precision) {
        case 64: case 32: case 16: case 8:
            return Type(IntType, .bits=i->precision);
        default: compiler_err(env, ast, "Unsupported precision");
        }
    }
    case Char: return Type(CharType);
    case Num: {
        auto n = Match(ast, Num);
        switch (n->precision) {
        case 64: return Type(NumType, .bits=64);
        case 32: return Type(NumType, .bits=32);
        default: compiler_err(env, ast, "Unsupported precision");
        }
    }
    case GetTypeInfo: {
        return Type(PointerType, .pointed=Type(TypeInfoType));
    }
    case SizeOf: {
        return Type(IntType, .bits=64);
    }
    case HeapAllocate: {
        sss_type_t *pointed = get_type(env, Match(ast, HeapAllocate)->value);
        if (has_stack_memory(pointed))
            compiler_err(env, ast, "Stack references cannot be moved to the heap because they may outlive the stack frame they were created in.");
        return Type(PointerType, .is_optional=false, .pointed=pointed);
    }
    case StackReference: {
        ast_t *value = Match(ast, StackReference)->value;
        sss_type_t *pointed_t = get_type(env, Match(ast, StackReference)->value);
        bool is_stack = true;
        // References to heap members/indexes are heap pointers, e.g. v := @Vec{1,2}; &v.x
        switch (value->tag) {
        case FieldAccess: {
            sss_type_t *fielded_t = get_type(env, Match(value, FieldAccess)->fielded);
            is_stack = fielded_t->tag == PointerType ? Match(fielded_t, PointerType)->is_stack : true;
            break;
        }
        case Index: {
            sss_type_t *indexed_t = get_type(env, Match(value, Index)->indexed);
            is_stack = indexed_t->tag == PointerType ? Match(indexed_t, PointerType)->is_stack : true;
            break;
        }
        default: break;
        }
        return Type(PointerType, .pointed=pointed_t, .is_stack=is_stack);
    }
    case Range: {
        return Type(RangeType);
    }
    case StringJoin: case Interp: case StringLiteral: {
        return get_type_by_name(env, "Str");
    }
    case Var: {
        const char* name = Match(ast, Var)->name;
        binding_t *binding = get_binding(env, name);
        if (!binding) {
            const char *suggestion = spellcheck(env->bindings, name);
            if (suggestion)
                compiler_err(env, ast, "I don't know what this variable is referring to. Did you mean '%s'?", suggestion); 
            else
                compiler_err(env, ast, "I don't know what this variable is referring to."); 
            compiler_err(env, ast, "I don't know what \"%s\" refers to", name);
        }
        return binding->type;
    }
    case Wildcard: {
        compiler_err(env, ast, "Wildcards can only be used inside 'matches' expressions, they can't be used as values");
    }
    case Array: {
        auto array = Match(ast, Array);
        sss_type_t *item_type = NULL;
        if (array->type) {
            item_type = parse_type_ast(env, array->type);
        } else if (array->items) {
            for (int64_t i = 0; i < LENGTH(array->items); i++) {
                ast_t *item = ith(array->items, i);
                sss_type_t *t2 = get_type(env, item);
                while (t2->tag == GeneratorType)
                    t2 = Match(t2, GeneratorType)->generated;
                sss_type_t *merged = item_type ? type_or_type(item_type, t2) : t2;
                if (!merged)
                    compiler_err(env, ith(array->items, i),
                                "This array item has type %s, which is different from earlier array items which have type %s",
                                type_to_string(t2),  type_to_string(item_type));
                item_type = merged;
            }
        } else {
            compiler_err(env, ast, "I can't figure out what type this array has because it has no members or explicit type");
        }
        if (has_stack_memory(item_type))
            compiler_err(env, ast, "Arrays cannot hold stack references, because the array may outlive the stack frame the reference was created in.");
        return Type(ArrayType, .item_type=item_type);
    }
    case Table: {
        auto table = Match(ast, Table);
        sss_type_t *key_type = NULL, *value_type = NULL;
        if (table->key_type && table->value_type) {
            key_type = parse_type_ast(env, table->key_type);
            value_type = parse_type_ast(env, table->value_type);
        } else {
            if (table->default_value)
                value_type = get_type(env, table->default_value);
            for (int64_t i = 0; i < LENGTH(table->entries); i++) {
                ast_t *entry = ith(table->entries, i);
                sss_type_t *entry_t = get_type(env, entry);
                while (entry_t->tag == GeneratorType)
                    entry_t = Match(entry_t, GeneratorType)->generated;

                sss_type_t *key_t = ith(Match(entry_t, StructType)->field_types, 0);
                sss_type_t *key_merged = key_type ? type_or_type(key_type, key_t) : key_t;
                if (!key_merged)
                    compiler_err(env, ith(table->entries, i),
                                "This table entry has type %s, which is different from earlier table entries which have type %s",
                                type_to_string(key_t),  type_to_string(key_type));
                key_type = key_merged;

                sss_type_t *value_t = ith(Match(entry_t, StructType)->field_types, 1);
                sss_type_t *val_merged = value_type ? type_or_type(value_type, value_t) : value_t;
                if (!val_merged)
                    compiler_err(env, ith(table->entries, i),
                                "This table entry has type %s, which is different from earlier table entries which have type %s",
                                type_to_string(value_t),  type_to_string(value_type));
                value_type = val_merged;
            }
        }
        if (has_stack_memory(key_type) || has_stack_memory(value_type))
            compiler_err(env, ast, "Tables cannot hold stack references because the table may outlive the reference's stack frame.");
        return Type(TableType, .key_type=key_type, .value_type=value_type);
    }
    case TableEntry: {
        auto entry = Match(ast, TableEntry);
        sss_type_t *t = Type(StructType, .field_names=ARRAY("key", "value"),
                            .field_types=ARRAY(get_type(env, entry->key), get_type(env, entry->value)));
        sss_type_t *memoized = Table_str_get(&tuple_types, type_to_string(t));
        if (memoized) {
            t = memoized;
        } else {
            Table_str_set(&tuple_types, type_to_string(t), t);
        }
        return t;
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        sss_type_t *fielded_t = get_type(env, access->fielded);
        sss_type_t *field_t = get_field_type(env, fielded_t, access->field);
        if (!field_t)
            compiler_err(env, ast, "I can't find anything called %s on the type %T", access->field, fielded_t);
        return field_t;
    }
    case Index: {
        auto indexing = Match(ast, Index);
        sss_type_t *indexed_t = get_type(env, indexing->indexed);
        if (indexed_t->tag == PointerType && !indexing->index) {
            auto ptr = Match(indexed_t, PointerType);
            if (ptr->is_optional)
                compiler_err(env, ast, "You're attempting to dereference a pointer whose type indicates it could be nil");
            return ptr->pointed;
        }

        sss_type_t *base_value_t = base_value_type(indexed_t);

        switch (base_value_t->tag) {
        case ArrayType: {
            if (!indexing->index) return indexed_t;
            sss_type_t *index_t = get_type(env, indexing->index);
            switch (index_t->tag) {
            case RangeType: return indexed_t;
            case IntType: case CharType:
                return Match(base_value_t, ArrayType)->item_type;
            default: compiler_err(env, indexing->index, "I only know how to index lists using integers, not %T", index_t);
            }
        }
        case TableType: {
            return Match(base_value_t, TableType)->value_type;
        }
        // TODO: support ranges like (99..123)[5]
        // TODO: support slicing arrays like ([1,2,3,4])[2..10]
        default: {
            compiler_err(env, ast, "I don't know how to index %T values", indexed_t);
        }
        }
    }
    case KeywordArg: {
        return get_type(env, Match(ast, KeywordArg)->arg);
    }
    case FunctionCall: {
        auto call = Match(ast, FunctionCall);
        if (call->extern_return_type)
            return parse_type_ast(env, call->extern_return_type);
        sss_type_t *fn_type_t = get_type(env, call->fn);
        auto fn_type = Match(fn_type_t, FunctionType);
        return fn_type->ret;
    }
    case Block: {
        auto block = Match(ast, Block);
        if (LENGTH(block->statements) == 0)
            return Type(VoidType);
        ast_t *last = ith(block->statements, LENGTH(block->statements)-1);
        // Early out if the type is knowable without any context from the block:
        switch (last->tag) {
        case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate: case ConcatenateUpdate:
        case AndUpdate: case OrUpdate: case XorUpdate:
        case Assign: case Declare: case FunctionDef: case TypeDef:
            return Type(VoidType);
        default: break;
        }

        env = fresh_scope(env);

        // Function defs are visible in the entire block (allowing corecursive funcs)
        foreach (block->statements, stmt, _) {
            predeclare_def_funcs(env, *stmt);
        }

        for (int64_t i = 0, len = LENGTH(block->statements); i < len-1; i++) {
            ast_t *stmt = ith(block->statements, i);
            switch (stmt->tag) {
            case Declare: {
                auto decl = Match(stmt, Declare);
                sss_type_t *t;
                if (decl->value->tag == Use) {
                    sss_type_t *file_t = get_file_type(env, Match(decl->value, Use)->path);
                    Table_str_set(env->bindings, heap_strf("#type:%s", Match(decl->var, Var)->name), file_t);
                    t = Type(PointerType, .pointed=file_t);                                                                     
                } else {
                    t = get_type(env, decl->value);
                }
                Table_str_set(env->bindings, Match(decl->var, Var)->name, new(binding_t, .type=t, .visible_in_closures=decl->is_public));
                break;
            }
            default:
                // TODO: bind structs/tagged unions in block typechecking
                break;
            }
        }
        return get_type(env, last);
    }
    case Do: {
        auto do_ = Match(ast, Do);
        sss_type_t *t = get_type(env, do_->body);
        if (do_->else_body) {
            sss_type_t *else_t = get_type(env, do_->else_body);
            sss_type_t *t2 = type_or_type(t, else_t);
            if (!t2)
                compiler_err(env, do_->else_body, "I was expecting this 'else' block to have a %T value (based on the preceding 'do'), but it actually has a %T value.",
                             t, else_t);
            t = t2;
        } else if (do_->label) {
            t = generate(t);
        }
        return t;
    }
    case Extern: {
        sss_type_t *t = parse_type_ast(env, Match(ast, Extern)->type);
        return Match(ast, Extern)->address ? Type(PointerType, .pointed=t, .is_optional=false) : t;
    }
    case Declare: case Assign: case DocTest: case LinkerDirective: {
        return Type(VoidType);
    }
    case Use: {
        const char *path = Match(ast, Use)->path;
        return Type(PointerType, .pointed=get_file_type(env, path));
    }
    case Return: case Fail: case Stop: case Skip: {
        return Type(AbortType);
    }
    case Pass: return Type(VoidType);
    case Cast: {
        return parse_type_ast(env, Match(ast, Cast)->type);
    }
    case Bitcast: {
        return parse_type_ast(env, Match(ast, Bitcast)->type);
    }
    case TypeArray: case TypeTable: case TypeStruct: case TypePointer: case TypeFunction:
    case TypeTaggedUnion: {
        compiler_err(env, ast, "Attempt to get the type of this term, which is already a type annotation");
    }
    case Negative: {
        sss_type_t *t = get_type(env, Match(ast, Negative)->value);
        if (!is_numeric(t))
            compiler_err(env, ast, "I only know how to get negatives of numeric types, not %T", t);
        return t;
    }
    case And: {
        auto and_ = Match(ast, And);
        sss_type_t *lhs_t = get_type(env, and_->lhs),
                  *rhs_t = get_type(env, and_->rhs);

        if (lhs_t->tag == BoolType && rhs_t->tag == BoolType) {
            return lhs_t;
        } else if (lhs_t->tag == BoolType && rhs_t->tag == AbortType) {
            return lhs_t;
        } else if (rhs_t->tag == AbortType) {
            return lhs_t;
        } else if (lhs_t->tag == TaggedUnionType && type_eq(lhs_t, rhs_t)) {
            return lhs_t;
        } else if (lhs_t->tag == PointerType && rhs_t->tag == PointerType) {
            auto lhs_ptr = Match(lhs_t, PointerType);
            auto rhs_ptr = Match(rhs_t, PointerType);
            if (type_eq(lhs_ptr->pointed, rhs_ptr->pointed))
                return Type(PointerType, .pointed=lhs_ptr->pointed, .is_optional=lhs_ptr->is_optional || rhs_ptr->is_optional,
                            .is_readonly=lhs_ptr->is_readonly || rhs_ptr->is_readonly);
        } else {
            return get_math_type(env, ast, lhs_t, ast->tag, rhs_t);
        }
        compiler_err(env, ast, "I can't figure out the type of this `and` expression because the left side is a %T, but the right side is a %T",
                     lhs_t, rhs_t);
    }
    case Or: {
        auto or_ = Match(ast, Or);
        sss_type_t *lhs_t = get_type(env, or_->lhs),
                  *rhs_t = get_type(env, or_->rhs);

        if (lhs_t->tag == BoolType && rhs_t->tag == BoolType)
            return lhs_t;
        else if (lhs_t->tag == BoolType && rhs_t->tag == AbortType)
            return lhs_t;
        else if (lhs_t->tag == TaggedUnionType && type_eq(lhs_t, rhs_t))
            return lhs_t;
        else if (is_integral(lhs_t) && is_integral(rhs_t)) {
            sss_type_t *t = type_or_type(lhs_t, rhs_t);
            if (!t)
                compiler_err(env, ast, "I can't have a type that is either %T or %T", lhs_t, rhs_t);
            return t;
        }

        if (lhs_t->tag == PointerType) {
            auto lhs_ptr = Match(lhs_t, PointerType);
            if (rhs_t->tag == AbortType) {
                return Type(PointerType, .pointed=lhs_ptr->pointed, .is_optional=false, .is_readonly=lhs_ptr->is_readonly);
            } else if (rhs_t->tag == PointerType) {
                auto rhs_ptr = Match(rhs_t, PointerType);
                if (type_eq(rhs_ptr->pointed, lhs_ptr->pointed))
                    return Type(PointerType, .pointed=lhs_ptr->pointed, .is_optional=lhs_ptr->is_optional && rhs_ptr->is_optional,
                                .is_readonly=lhs_ptr->is_readonly || rhs_ptr->is_readonly);
            }
        } else {
            return get_math_type(env, ast, lhs_t, ast->tag, rhs_t);
        }
        compiler_err(env, ast, "I can't figure out the type of this `or` expression because the left side is a %T, but the right side is a %T",
                     lhs_t, rhs_t);
    }
    case Xor: {
        auto xor = Match(ast, Xor);
        sss_type_t *lhs_t = get_type(env, xor->lhs),
                  *rhs_t = get_type(env, xor->rhs);

        if (lhs_t->tag == BoolType && rhs_t->tag == BoolType) {
            return lhs_t;
        } else if (lhs_t->tag == TaggedUnionType && type_eq(lhs_t, rhs_t)) {
            return lhs_t;
        } else if (is_integral(lhs_t) && is_integral(rhs_t)) {
            sss_type_t *t = type_or_type(lhs_t, rhs_t);
            if (!t)
                compiler_err(env, ast, "I can't have a type that is either %T or %T", lhs_t, rhs_t);
            return t;
        } else {
            return get_math_type(env, ast, lhs_t, ast->tag, rhs_t);
        }

        compiler_err(env, ast, "I can't figure out the type of this `xor` expression because the left side is a %T, but the right side is a %T",
                     lhs_t, rhs_t);
    }
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate: case ConcatenateUpdate:
    case AndUpdate: case OrUpdate: case XorUpdate:
        return Type(VoidType);

    case Add: case Subtract: case Divide: case Multiply: case Power: case Modulus: case Modulus1: case LeftShift: case RightShift: {
        // Unsafe! These types *should* have the same fields and this saves a lot of duplicate code:
        ast_t *lhs = ast->__data.Add.lhs, *rhs = ast->__data.Add.rhs;
        // Okay safe again

        sss_type_t *lhs_t = get_type(env, lhs), *rhs_t = get_type(env, rhs);
        return get_math_type(env, ast, lhs_t, ast->tag, rhs_t);
    }
    case Concatenate: {
        auto concat = Match(ast, Concatenate);
        sss_type_t *lhs_t = get_type(env, concat->lhs),
                  *rhs_t = get_type(env, concat->rhs);
        if (!type_eq(lhs_t, rhs_t))
            compiler_err(env, ast, "The type on the left side of this concatenation doesn't match the right side: %T vs. %T",
                         lhs_t, rhs_t);
        sss_type_t *base_t = lhs_t;
        while (base_t->tag == VariantType) base_t = Match(base_t, VariantType)->variant_of;
        if (base_t->tag != ArrayType)
            compiler_err(env, ast, "Only array/string value types support concatenation, not %T", lhs_t);
        return lhs_t;
    }
    case Less: case LessEqual: case Greater: case GreaterEqual: case In: case NotIn: {
        return Type(BoolType);
    }
    case Min: case Max: case Mix: {
        // Unsafe! These types *should* have the same fields and this saves a lot of duplicate code:
        ast_t *lhs = ast->__data.Min.lhs, *rhs = ast->__data.Min.rhs;
        // Okay safe again

        sss_type_t *lhs_t = get_type(env, lhs), *rhs_t = get_type(env, rhs);
        sss_type_t *t = type_or_type(lhs_t, rhs_t);
        if (!t)
            compiler_err(env, ast, "The two sides of this operation are not compatible: %T vs %T", lhs_t, rhs_t);
        return t;
    }

    case Not: {
        sss_type_t *t = get_type(env, Match(ast, Not)->value);
        if (base_variant(t)->tag == TaggedUnionType)
            return t;
        else if (base_variant(t)->tag == BoolType || is_integral(t))
            return t;
        else if (base_variant(t)->tag == PointerType && Match(base_variant(t), PointerType)->is_optional)
            return Type(BoolType);
        compiler_err(env, ast, "I only know what `not` means for Bools, Ints, and Enums, but this is a %T", t); 
    }

    case Equal: case NotEqual: {
        ast_t *lhs, *rhs;
        if (ast->tag == Equal) {
            lhs = Match(ast, Equal)->lhs, rhs = Match(ast, Equal)->rhs;
        } else {
            lhs = Match(ast, NotEqual)->lhs, rhs = Match(ast, NotEqual)->rhs;
        }
        sss_type_t *lhs_t = get_type(env, lhs);
        sss_type_t *rhs_t = get_type(env, rhs);
        if (type_is_a(lhs_t, rhs_t) || type_is_a(rhs_t, lhs_t))
            return Type(BoolType);
        else if (is_numeric(lhs_t) && is_numeric(rhs_t))
            return Type(BoolType);
        else
            compiler_err(env, ast, "I only know how to compare values that have the same type, but this comparison is between %T and %T",
                         lhs_t, rhs_t);
    }

    case Lambda: {
        auto lambda = Match(ast, Lambda);
        auto arg_names = EMPTY_ARRAY(const char*);
        auto arg_types = EMPTY_ARRAY(sss_type_t*);
        for (int64_t i = 0; i < LENGTH(lambda->args.types); i++) {
            ast_t *arg_def = ith(lambda->args.types, i);
            sss_type_t *t = parse_type_ast(env, arg_def);
            const char* arg_name = ith(lambda->args.names, i);
            append(arg_names, arg_name);
            append(arg_types, t);
        }

        // Include only global bindings:
        env_t *lambda_env = file_scope(env);
        for (int64_t i = 0; i < LENGTH(lambda->args.types); i++) {
            Table_str_set(lambda_env->bindings, ith(arg_names, i), new(binding_t, .type=ith(arg_types, i)));
        }
        sss_type_t *ret = get_type(lambda_env, lambda->body);
        if (has_stack_memory(ret))
            compiler_err(env, ast, "Functions can't return stack references because the reference may outlive its stack frame.");
        return Type(FunctionType, .arg_names=arg_names, .arg_types=arg_types, .ret=ret);
    }

    case FunctionDef: {
        auto def = Match(ast, FunctionDef);
        auto arg_names = EMPTY_ARRAY(const char*);
        auto arg_types = EMPTY_ARRAY(sss_type_t*);
        auto arg_defaults = EMPTY_ARRAY(ast_t*);

        // In order to allow default values to reference other arguments (e.g. `def foo(x:Foo, y=x)`)
        // we need to create scoped bindings for them here:
        env_t *default_arg_env = file_scope(env);
        default_arg_env->bindings = new(table_t, .fallback=default_arg_env->bindings);
        for (int64_t i = 0; i < LENGTH(def->args.types); i++) {
            ast_t *arg_type_def = ith(def->args.types, i);
            if (!arg_type_def) continue;
            sss_type_t *arg_type = parse_type_ast(env, arg_type_def);
            Table_str_set(default_arg_env->bindings, ith(def->args.names, i), new(binding_t, .type=arg_type));
        }
        
        for (int64_t i = 0; i < LENGTH(def->args.types); i++) {
            ast_t *arg_def = ith(def->args.types, i);
            const char* arg_name = ith(def->args.names, i);
            append(arg_names, arg_name);
            if (arg_def) {
                sss_type_t *arg_type = parse_type_ast(env, arg_def);
                append(arg_types, arg_type);
                ast_t *default_val = NULL;
                append(arg_defaults, default_val);
            } else {
                ast_t *default_val = ith(def->args.defaults, i);
                sss_type_t *arg_type = get_type(default_arg_env, default_val);
                append(arg_types, arg_type);
                append(arg_defaults, default_val);
                Table_str_set(default_arg_env->bindings, ith(def->args.names, i), new(binding_t, .type=arg_type));
            }
        }

        sss_type_t *ret = def->ret_type ? parse_type_ast(env, def->ret_type) : Type(VoidType);
        if (has_stack_memory(ret))
            compiler_err(env, def->ret_type, "Functions can't return stack references because the reference may outlive its stack frame.");
        return Type(FunctionType, .arg_names=arg_names, .arg_types=arg_types, .arg_defaults=arg_defaults, .ret=ret, .env=default_arg_env);
    }

    case TypeDef: case ConvertDef: {
        return Type(VoidType);
    }

    case Struct: {
        auto struct_ = Match(ast, Struct);
        if (!struct_->type) {
            auto field_names = EMPTY_ARRAY(const char*);
            auto field_types = EMPTY_ARRAY(sss_type_t*);
            foreach (struct_->members, member, _) {
                if ((*member)->tag != KeywordArg)
                    compiler_err(env, *member, "Anonymous structs must have names for each field");
                auto field = Match(*member, KeywordArg);
                append(field_names, field->name);
                sss_type_t *field_type = get_type(env, field->arg);
                if (has_stack_memory(field_type))
                    compiler_err(env, field->arg, "Structs aren't allowed to have stack references because the struct may outlive the reference's stack frame.");
                append(field_types, field_type);
            }

            sss_type_t *t = Type(StructType, .field_names=field_names, .field_types=field_types);
            sss_type_t *memoized = Table_str_get(&tuple_types, type_to_string(t));
            if (memoized) {
                t = memoized;
            } else {
                Table_str_set(&tuple_types, type_to_string(t), t);
            }
            return t;
        }

        sss_type_t *t = parse_type_ast(env, struct_->type);
        if (t == NULL)
            compiler_err(env, ast, "There isn't any kind of struct like this");

        return t;
    }

    case If: {
        auto if_ = Match(ast, If);
        sss_type_t *subject_t;
        if (if_->subject->tag == Declare) {
            subject_t = get_type(env, Match(if_->subject, Declare)->value);
            env = fresh_scope(env);
        } else {
            subject_t = get_type(env, if_->subject);
        }
        sss_type_t *t = NULL;
        for (int64_t i = 0; i < LENGTH(if_->patterns); i++) {
            sss_type_t *case_t = get_type(env, ith(if_->blocks, i));
            sss_type_t *t2 = type_or_type(t, case_t);
            if (!t2)
                compiler_err(env, ith(if_->blocks, i),
                            "I was expecting this block to have a %s value (based on earlier clauses), but it actually has a %s value.",
                            type_to_string(t), type_to_string(case_t));
            t = t2;
        }
        if (get_missing_pattern(env, subject_t, if_->patterns) && t->tag != GeneratorType)
            t = Type(GeneratorType, .generated=t);
        return t;
    }

    case While: {
        return generate(get_type(env, Match(ast, While)->body));
    }
    case Repeat: {
        return generate(get_type(env, Match(ast, Repeat)->body));
    }
    case For: {
        auto for_loop = Match(ast, For);
        sss_type_t *index_type = INT_TYPE,
                  *value_type = get_iter_type(env, for_loop->iter);

        env_t *loop_env = fresh_scope(env);
        if (for_loop->index) {
            Table_str_set(loop_env->bindings, Match(for_loop->index, Var)->name, new(binding_t, .type=index_type));
        }
        if (for_loop->value) {
            Table_str_set(loop_env->bindings, Match(for_loop->value, Var)->name, new(binding_t, .type=value_type));
        }
        
        if (for_loop->first)
            return generate(get_type(loop_env, for_loop->first));
        else if (for_loop->body)
            return generate(get_type(loop_env, for_loop->body));
        else if (for_loop->between)
            return generate(get_type(loop_env, for_loop->between));
        else if (for_loop->empty)
            return generate(get_type(loop_env, for_loop->empty));
        else
            compiler_err(env, ast, "I can't figure out the type of this 'for' loop");
    }
    case Reduction: {
        env = fresh_scope(env);
        auto reduction = Match(ast, Reduction);
        sss_type_t *item_type = get_iter_type(env, reduction->iter);
        Table_str_set(env->bindings, "x.0", new(binding_t, .type=item_type));
        Table_str_set(env->bindings, "y.0", new(binding_t, .type=item_type));
        sss_type_t *combo_t = get_type(env, reduction->combination);
        if (!can_promote(item_type, combo_t))
            compiler_err(env, ast, "This reduction expression has type %T, but it's iterating over %T values, so I wouldn't know what to produce if there was only one value.",
                         combo_t, item_type);

        if (reduction->fallback) {
            sss_type_t *fallback_t = get_type(env, reduction->fallback);
            if (!can_promote(fallback_t, combo_t))
                compiler_err(env, ast, "This reduction expression has type %T, but the fallback has type %T", combo_t, fallback_t);
        }
        return combo_t;
    }
    case Defer: {
        return Type(VoidType);
    }
    case With: {
        auto with = Match(ast, With);
        if (with->var) {
            env = fresh_scope(env);
            Table_str_set(env->bindings, Match(with->var, Var)->name, new(binding_t, .type=get_type(env, with->expr)));
        }
        return get_type(env, with->body);
    }
    case Using: { // using expr *[; expr] body
        auto using = Match(ast, Using);
        env = fresh_scope(env);
        foreach (using->used, used, _) {
            sss_type_t *t = get_type(env, *used);
            auto fields = EMPTY_ARRAY(const char*);
            for (;;) {
                if (t->tag == PointerType) {
                    if (Match(t, PointerType)->is_optional)
                        compiler_err(env, *used, "This value might be null, so it's not safe to use its fields");
                    t = Match(t, PointerType)->pointed;
                } else if (t->tag == VariantType) {
                    t = Match(t, VariantType)->variant_of;
                } else if (t->tag == StructType) {
                    fields = Match(t, StructType)->field_names;
                    break;
                } else {
                    compiler_err(env, *used, "I'm sorry, but 'using' isn't supported for %T types yet", t);
                }
            }
            foreach (fields, field, _) {
                ast_t *shim = WrapAST(*used, FieldAccess, .fielded=*used, .field=*field);
                Table_str_set(env->bindings, *field, new(binding_t, .type=get_type(env, shim)));
            }
        }
        return get_type(env, using->body);
    }
    case Variant: {
        auto variant = Match(ast, Variant);
        return parse_type_ast(env, variant->type);
    }
    case Namespace: {
        return get_namespace_type(env, ast, false);
    }
    case Extend: return Type(VoidType);
    default: break;
    }
    compiler_err(env, ast, "I can't figure out the type of: %s", ast_to_str(ast));
}

bool is_discardable(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate: case ConcatenateUpdate:
    case Assign: case Declare: case FunctionDef: case TypeDef: case Use: case Namespace:
        return true;
    default: break;
    }
    sss_type_t *t = get_type(env, ast);
    while (t->tag == GeneratorType) t = Match(t, GeneratorType)->generated;
    return (t->tag == VoidType || t->tag == AbortType);
}

const char *get_missing_pattern(env_t *env, sss_type_t *t, ARRAY_OF(ast_t*) patterns)
{
    foreach (patterns, pat, _) {
        if ((*pat)->tag == Wildcard)
            return NULL;
    }

    if (t->tag == TaggedUnionType) {
        table_t member_handlers = {0};
        auto members = Match(t, TaggedUnionType)->members;
        for (int64_t i = 0; i < LENGTH(members); i++) {
            auto member = ith(members, i);
            auto list = EMPTY_ARRAY(ast_t*);
            Table_str_set(&member_handlers, member.name, list);
        }

        foreach (patterns, pat, _) {
            if ((*pat)->tag == FunctionCall) {
                ast_t *fn = Match((*pat), FunctionCall)->fn;
                if (fn->tag == Var) {
                    const char *name = Match(fn, Var)->name;
                    ARRAY_OF(ast_t*) handlers = Table_str_get(&member_handlers, name);
                    if (handlers) {
                        auto args = Match(*pat, FunctionCall)->args;
                        ast_t *m_pat = WrapAST(*pat, Struct, .members=args);
                        append(handlers, m_pat);
                    }
                }
            } else if ((*pat)->tag == Var) {
                const char *name = Match(*pat, Var)->name;
                ARRAY_OF(ast_t*) handlers = Table_str_get(&member_handlers, name);
                if (handlers)
                    append(handlers, *pat);
            }
        }

        const char *unhandled = NULL;
        for (int64_t i = 0; i < LENGTH(members); i++) {
            auto member = ith(members, i);
            ARRAY_OF(ast_t*) handlers = Table_str_get(&member_handlers, member.name);
            if (LENGTH(handlers) == 0) {
                if (unhandled)
                    unhandled = heap_strf("%s, nor is %s.%s",
                                          unhandled, type_to_string(t), member.name);
                else
                    unhandled = heap_strf("The tagged union member %s.%s is not handled",
                                          type_to_string(t), member.name);
            } else if (member.type) {
                foreach (handlers, h, _) {
                    if ((*h)->tag == Var)
                        goto handled;
                }
                const char *missing = get_missing_pattern(env, member.type, handlers);
                if (!missing) continue;
                if (unhandled)
                    unhandled = heap_strf("%s, also for %s.%s(...): %s",
                                          unhandled, type_to_string(t), member.name, missing);
                else
                    unhandled = heap_strf("Among the patterns for %s.%s(...): %s",
                                          type_to_string(t), member.name, missing);
              handled: continue;
            }
        }
        return unhandled;
    } else if (t->tag == PointerType) {
        auto ptr = Match(t, PointerType);
        if (ptr->is_optional) {
            bool handled_null = false;
            foreach (patterns, pat, _) {
                if ((*pat)->tag == Nil
                    || ((*pat)->tag == Var && !get_binding(env, Match(*pat, Var)->name))) {
                    handled_null = true;
                    break;
                }
            }
            if (!handled_null) return "The null value is not handled";
        }

        auto value_handlers = EMPTY_ARRAY(ast_t*);
        foreach (patterns, pat, _) {
            if ((*pat)->tag == HeapAllocate)
                append(value_handlers, Match(*pat, HeapAllocate)->value);
            else if ((*pat)->tag == StackReference)
                append(value_handlers, Match(*pat, StackReference)->value);
        }
        return get_missing_pattern(env, ptr->pointed, value_handlers);
    } else if (t->tag == BoolType) {
        bool cases_handled[2] = {false, false};
        foreach (patterns, pat, _) {
            if ((*pat)->tag != Bool) continue;
            cases_handled[(int)Match(*pat, Bool)->b] = true;
        }
        if (!cases_handled[0])
            return "'no' is not handled";
        else if (!cases_handled[1])
            return "'yes' is not handled";
        else
            return NULL;
    } else if (is_numeric(t)) {
        return "some numbers are not handled";
    } else if (t->tag == CharType) {
        return "some numbers are not handled";
    } else if (t->tag == StructType) {
        auto field_names = Match(t, StructType)->field_names;
        auto field_type_list = Match(t, StructType)->field_types;
        table_t field_types = {0};
        for (int64_t i = 0; i < LENGTH(field_names); i++) {
            auto name = ith(field_names, i);
            if (!name) continue;
            auto type = ith(field_type_list, i);
            Table_str_set(&field_types, name, type);
        }

        foreach (patterns, pat, _) {
            if ((*pat)->tag != Struct) continue;
            auto struct_ = Match(*pat, Struct);
            table_t named_members = {0};
            foreach (struct_->members, member, _) {
                if ((*member)->tag != KeywordArg) continue;
                auto memb = Match(*member, KeywordArg);
                if (!memb->name) continue;
                Table_str_set(&named_members, memb->name, memb->arg);
            }
            foreach (struct_->members, member, _) {
                if ((*member)->tag == KeywordArg && Match(*member, KeywordArg)->name)
                    continue;
                for (int64_t i = 0; i < LENGTH(field_names); i++) {
                    const char *name = ith(field_names, i);
                    if (name && !Table_str_get(&named_members, name)) {
                        Table_str_set(&named_members, name, *member);
                        break;
                    }
                }
            }

            const char *missing = NULL;
            for (int64_t i = 1; i <= Table_length(&named_members); i++) {
                struct {const char *key; ast_t *value;} *entry = Table_str_entry(&named_members, i);
                sss_type_t *type = Table_str_get(&field_types, entry->key);
                if (!type) continue;
                missing = get_missing_pattern(env, type, ARRAY(entry->value));
                if (missing) break;
            }
            if (!missing) return NULL;
        }
    }
    return heap_strf("I can't prove that every %s case in this 'when' block is handled by an 'is' clause. Please add a wildcard clause like: 'else ...'",
                     type_to_string(t));
}

sss_type_t *get_namespace_type(env_t *env, ast_t *namespace_ast, sss_type_t *type)
{
    auto statements = Match(namespace_ast, Namespace)->statements;

    // Function defs are visible in the entire block (allowing corecursive funcs)
    foreach (statements, stmt, _) {
        predeclare_def_funcs(env, *stmt);
    }

    auto field_names = EMPTY_ARRAY(const char*);
    auto field_types = EMPTY_ARRAY(sss_type_t*);

    if (type) {
        append(field_names, "type");
        append(field_types, Type(TypeInfoType));

        if (base_variant(type)->tag == TaggedUnionType) {
            // Add enum flag/constructors:
            auto tagged = Match(base_variant(type), TaggedUnionType);
            foreach (tagged->members, member, _) {
                append(field_names, member->name);
                auto fields = Match(member->type, StructType);
                if (LENGTH(fields->field_types) == 0) {
                    append(field_types, type);
                } else {
                    sss_type_t *constructor_t = Type(FunctionType, .arg_names=fields->field_names,
                                                     .arg_types=fields->field_types, .arg_defaults=fields->field_defaults,
                                                     .ret=type, .env=env);
                    append(field_types, constructor_t);
                }
            }
        }
    }

    for (int64_t i = 0, len = LENGTH(statements); i < len; i++) {
        ast_t *stmt = ith(statements, i);
      doctest_inner:
        switch (stmt->tag) {
        case Declare: {
            auto decl = Match(stmt, Declare);
            const char *name = Match(decl->var, Var)->name;
            sss_type_t *t = get_type(env, decl->value);
            append(field_names, name);
            append(field_types, t);
            set_binding(env, name, new(binding_t, .type=t));
            break;
        }
        case TypeDef: {
            auto def = Match(stmt, TypeDef);
            append(field_names, def->name);
            sss_type_t *def_type = get_type_by_name(env, def->name);
            sss_type_t *ns_t = get_namespace_type(env, def->namespace, def_type);
            set_binding(env, def->name, new(binding_t, .type=ns_t));
            append(field_types, ns_t);
            break;
        }
        case FunctionDef: {
            auto def = Match(stmt, FunctionDef);
            append(field_names, def->name);
            sss_type_t *t = get_binding(env, def->name)->type;
            append(field_types, t);
            set_binding(env, def->name, new(binding_t, .type=t));
            break;
        }
        case DocTest: {
            stmt = Match(stmt, DocTest)->expr;
            goto doctest_inner;
        }
        default:
            // TODO: bind structs/tagged unions in block typechecking
            break;
        }
    }
    return Type(StructType, .field_names=field_names, .field_types=field_types);
}

const char *get_module_name(const char *path)
{
    const char *base = strrchr(path, '/')+1;
    if (isdigit(*base))
        --base; // This will put the pointer onto the slash, which will get turned into a leading underscore;
    size_t len = strcspn(base, ".");
    char *name = GC_MALLOC_ATOMIC(len + 1);
    memcpy(name, base, len);
    name[len] = '\0';
    for (int i = 0; name[i]; i++) {
        if (!isalnum(name[i]) && name[i] != '_')
            name[i] = '_';
    }
    return name;
}

sss_type_t *get_file_type(env_t *env, const char *path)
{
    struct stat file_info;
    const char *sss_path = strlen(path) > 4 && streq(path + strlen(path) - 4, ".sss") ? path : heap_strf("%s.sss", path);
    if (stat(sss_path, &file_info) == -1)
        compiler_err(env, NULL, "I can't find the file %s", sss_path);

    const char *name = get_module_name(path);
    sss_type_t *type = Table_str_get(&env->global->module_types, path);
    if (type) return type;

    sss_file_t *f = sss_load_file(sss_path);
    ast_t *ast = parse_file(f, env->on_err);
    type = Type(VariantType, .name=name, .variant_of=get_namespace_type(env, ast, false));
    Table_str_set(&env->global->module_types, path, type);
    return type;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
