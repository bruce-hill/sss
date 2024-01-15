// Logic for getting an SSS type from an AST node
#include <gc.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <sys/stat.h>

#include "args.h"
#include "bindings.h"
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
    case TypeVar: {
        sss_type_t *t = Match(ast, TypeVar)->type;
        if (t) return t;
        t = Table_str_get(&env->global->types, Match(ast, TypeVar)->name);
        if (t) return t;
        compiler_err(env, ast, "I don't know a type with the name '%s'", Match(ast, TypeVar)->name);
    }
    // case FieldAccess: {
    //     auto access = Match(ast, FieldAccess);
    //     sss_type_t *fielded_t = parse_type_ast(env, access->fielded);
    //     sss_type_t *t = (sss_type_t*)get_from_namespace(env, fielded_t, heap_strf("#type:%s", access->field));
    //     if (!t)
    //         compiler_err(env, ast, "I don't know any type with this name.");
    //     return t;
    // }
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
        auto args = EMPTY_ARRAY(ast_t*);
        auto arg_defaults = EMPTY_ARRAY(ast_t*);
        auto arg_types = EMPTY_ARRAY(sss_type_t*);
        for (int64_t i = 0; i < LENGTH(fn->args.types); i++) {
            append(args, ith(fn->args.args, i));
            sss_type_t *arg_t;
            if (ith(fn->args.types, i)) {
                arg_t = parse_type_ast(env, ith(fn->args.types, i));
                append(arg_types, arg_t);
                append(arg_defaults, NULL);
            } else {
                arg_t = get_type(env, ith(fn->args.defaults, i));
                append(arg_types, arg_t);
                append(arg_defaults, ith(fn->args.defaults, i));
            }
        }
        return Type(FunctionType, .args=args, .arg_types=arg_types, .arg_defaults=arg_defaults, .ret=ret_t);
    }
    case TypeStruct: {
        auto struct_ = Match(ast, TypeStruct);
        auto member_names = EMPTY_ARRAY(const char*);
        auto member_types = EMPTY_ARRAY(sss_type_t*);
        sss_type_t *t = Type(StructType, .field_names=member_names, .field_types=member_types, .field_defaults=struct_->members.defaults);
        for (int64_t i = 0, len = LENGTH(struct_->members.types); i < len; i++) {
            auto arg = ith(struct_->members.args, i);
            const char *member_name = arg ? Match(arg, Var)->name : NULL;
            if (!member_name)
                member_name = heap_strf("_%ld", i+1);
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

sss_type_t *get_math_type(env_t *env, ast_t *ast, sss_type_t *lhs_t, operator_e op, sss_type_t *rhs_t)
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
        else if (lhs_t->tag == VariantType && rhs_t->tag != VariantType && (op == OP_MULT || op == OP_DIVIDE)
                 && (compare_precision(lhs_t, rhs_t) == NUM_PRECISION_EQUAL || compare_precision(lhs_t, rhs_t) == NUM_PRECISION_MORE))
            t = lhs_t;
        else if (rhs_t->tag == VariantType && lhs_t->tag != VariantType && op == OP_MULT
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
    } else if (lhs_t->tag == BoolType && (base_variant(rhs_t)->tag == StructType || base_variant(rhs_t)->tag == ArrayType) && (op == OP_AND || op == OP_OR || op == OP_XOR)) {
        return rhs_t;
    } else if (rhs_t->tag == BoolType && (base_variant(lhs_t)->tag == StructType || base_variant(lhs_t)->tag == ArrayType) && (op == OP_AND || op == OP_OR || op == OP_XOR)) {
        return lhs_t;
    } else {
        compiler_err(env, ast, "I don't know how to do math operations between %T and %T", lhs_t, rhs_t);
    }
}

static ARRAY_OF(ast_t*) _names_to_args(ARRAY_OF(const char*) names)
{
    auto args = EMPTY_ARRAY(ast_t*);
    foreach (names, name, _)
        append(args, FakeAST(Var, .name=*name));
    return args;
}

static sss_type_t *get_array_field_type(env_t *env, sss_type_t *t, const char *name)
{
    (void)env;
#define REF(x) Type(PointerType, .is_stack=true, .pointed=x)
#define RO_REF(x) Type(PointerType, .is_stack=true, .is_readonly=true, .pointed=x)
#define OPT(x) Type(PointerType, .is_optional=true, .pointed=x)
#define FN(names, types, defaults, ret_type) Type(FunctionType, .args=_names_to_args(names), .arg_types=types, .arg_defaults=defaults, .ret=ret_type)
#define TYPEINFO_DEREF(var, t) FakeAST(GetTypeInfo, FakeAST(Index, FakeAST(Var, var, .binding=new(binding_t, .type=t))))
#define NAMES(...) ARRAY((const char*)__VA_ARGS__)
#define TYPES(...) ARRAY((sss_type_t*)__VA_ARGS__)
#define DEFAULTS(...) ARRAY((ast_t*)__VA_ARGS__)
    sss_type_t *item_t = Match(t, ArrayType)->item_type;
    sss_type_t *i64 = Type(IntType, .bits=64);
    sss_type_t *void_t = Type(VoidType);
    sss_type_t *typeinfo_ptr_t = Type(PointerType, .pointed=Type(TypeInfoType));
    ast_t *item_size = FakeAST(SizeOf, FakeAST(Index, .indexed=FakeAST(Var, "array", .binding=new(binding_t, .type=t)), .index=FakeAST(Int, .precision=64, .i=1)));

    if (streq(name, "length")) {
        return INT_TYPE;
    } else if (streq(name, "insert")) {
        return FN(NAMES("array", "item", "index", "_item_size"),
                  TYPES(REF(t), RO_REF(item_t), i64, i64),
                  DEFAULTS(NULL, NULL, FakeAST(Int, .precision=64, .i=0), item_size),
                  void_t);
    } else if (streq(name, "insert_all")) {
        return FN(NAMES("array", "to_insert", "index", "_item_size"),
                  TYPES(REF(t), t, i64, i64),
                  DEFAULTS(NULL, NULL, FakeAST(Int, .precision=64, .i=0), item_size),
                  void_t);
    } else if (streq(name, "remove")) {
        return FN(NAMES("array", "index", "count", "_item_size"),
                  TYPES(REF(t), i64, i64, i64),
                  DEFAULTS(NULL, FakeAST(Int, .precision=64, .i=-1), FakeAST(Int, .precision=64, .i=1), item_size),
                  void_t);
    } else if (streq(name, "contains")) {
        return FN(NAMES("array", "item", "_type"),
                  TYPES(RO_REF(t), RO_REF(item_t), typeinfo_ptr_t),
                  DEFAULTS(NULL, NULL, TYPEINFO_DEREF("array", t)),
                  Type(BoolType));
    } else if (streq(name, "compact")) {
        return FN(NAMES("array", "item_size"), TYPES(REF(t), i64), DEFAULTS(NULL, item_size), t);
    } else if (streq(name, "sort")) {
        return FN(NAMES("array", "_type"),
                  TYPES(REF(t), typeinfo_ptr_t),
                  DEFAULTS(NULL, TYPEINFO_DEREF("array", t)),
                  void_t);
    } else if (streq(name, "shuffle")) {
        return FN(NAMES("array", "_item_size"),
                  TYPES(REF(t), i64),
                  DEFAULTS(NULL, item_size),
                  void_t);
    } else if (streq(name, "clear")) {
        return FN(NAMES("array"), TYPES(REF(t)), DEFAULTS(NULL), void_t);
    } else if (streq(name, "slice")) {
        return FN(NAMES("array", "range", "readonly", "_type"),
                  TYPES(REF(t), Type(RangeType), Type(BoolType), typeinfo_ptr_t),
                  DEFAULTS(NULL, NULL, FakeAST(Bool, false), TYPEINFO_DEREF("array", t)),
                  t);
    }
    return NULL;
}

static sss_type_t *get_table_field_type(env_t *env, sss_type_t *t, const char *name)
{
    (void)env;
    sss_type_t *key_t = Match(t, TableType)->key_type;
    sss_type_t *value_t = Match(t, TableType)->value_type;
    sss_type_t *void_t = Type(VoidType);
    sss_type_t *typeinfo_ptr_t = Type(PointerType, .pointed=Type(TypeInfoType));
    if (streq(name, "entries")) {
        return Type(ArrayType, .item_type=table_entry_type(t));
    } else if (streq(name, "length")) {
        return INT_TYPE;
    } else if (streq(name, "default")) {
        return Type(PointerType, .pointed=Match(t, TableType)->value_type, .is_optional=true, .is_readonly=true);
    } else if (streq(name, "fallback")) {
        return Type(PointerType, .pointed=t, .is_optional=true, .is_readonly=true);
    } else if (streq(name, "keys")) {
        return Type(ArrayType, .item_type=Match(t, TableType)->key_type);
    } else if (streq(name, "values")) {
        return Type(ArrayType, .item_type=Match(t, TableType)->value_type);
    } else if (streq(name, "remove")) {
        return FN(NAMES("t", "key", "_type"),
                  TYPES(REF(t), RO_REF(key_t), typeinfo_ptr_t),
                  DEFAULTS(NULL, NULL, TYPEINFO_DEREF("t", t)),
                  void_t);
    } else if (streq(name, "set")) {
        return FN(NAMES("t", "key", "value", "_type"),
                  TYPES(REF(t), RO_REF(key_t), RO_REF(value_t), typeinfo_ptr_t),
                  DEFAULTS(NULL, NULL, NULL, TYPEINFO_DEREF("t", t)),
                  void_t);
    } else if (streq(name, "reserve")) {
        return FN(NAMES("t", "key", "value", "_type"),
                  TYPES(REF(t), RO_REF(key_t), OPT(value_t), typeinfo_ptr_t),
                  DEFAULTS(NULL, NULL, NULL, TYPEINFO_DEREF("t", t)),
                  Type(PointerType, value_t));
    } else if (streq(name, "get")) {
        return FN(NAMES("t", "key", "_type"),
                  TYPES(RO_REF(t), RO_REF(key_t), typeinfo_ptr_t),
                  DEFAULTS(NULL, NULL, TYPEINFO_DEREF("t", t)),
                  OPT(value_t));
    } else if (streq(name, "get_raw")) {
        return FN(NAMES("t", "key", "_type"),
                  TYPES(RO_REF(t), RO_REF(key_t), typeinfo_ptr_t),
                  DEFAULTS(NULL, NULL, TYPEINFO_DEREF("t", t)),
                  OPT(value_t));
    } else if (streq(name, "clear")) {
        return FN(NAMES("t"), TYPES(REF(t)), DEFAULTS(NULL), void_t);
    } else if (streq(name, "mark_copy_on_write")) {
        return FN(NAMES("t"), TYPES(REF(t)), DEFAULTS(NULL), void_t);
    }
    return NULL;
}

static sss_type_t *get_value_field_type(env_t *env, sss_type_t *t, const char *field_name)
{
    t = base_value_type(t);
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
        return NULL;
    }
    case TaggedUnionType: {
        auto tagged = Match(t, TaggedUnionType);
        foreach (tagged->members, member, _) {
            if (streq(field_name, member->name))
                return member->type;
        }
        return NULL;
    }
    case ArrayType: {
        return get_array_field_type(env, t, field_name);
    }
    case RangeType: {
        if (streq(field_name, "first") || streq(field_name, "last") || streq(field_name, "step") || streq(field_name, "length"))
            return INT_TYPE;
        return NULL;
    }
    case TableType: {
        return get_table_field_type(env, t, field_name);
    }
    default: return NULL;
    }
}

sss_type_t *get_field_type(env_t *env, sss_type_t *t, const char *field_name)
{
    sss_type_t *field_t = get_value_field_type(env, t, field_name);
    if (field_t) return field_t;
    for (;;) {
        binding_t *b = get_from_namespace(env, t, field_name);
        if (b) return b->type;

        switch (t->tag) {
        case PointerType: t = Match(t, PointerType)->pointed; break;
        case VariantType: t = Match(t, VariantType)->variant_of; break;
        default: return NULL;
        }
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
    case Declare: {
        return get_doctest_type(env, Match(ast, Declare)->value);
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
    case UpdateAssign: {
        return get_doctest_type(env, Match(ast, UpdateAssign)->lhs);
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
        return Table_str_get(&env->global->types, "Str");
    }
    case Var: {
        auto var = Match(ast, Var);
        if (var->binding) return var->binding->type;

            // const char *suggestion = spellcheck(env->bindings, name);
            // if (suggestion)
            //     compiler_err(env, ast, "I don't know what this variable is referring to. Did you mean '%s'?", suggestion); 
            // else
            //     compiler_err(env, ast, "I don't know what this variable is referring to."); 
        compiler_err(env, ast, "I don't know what \"%s\" refers to", var->name);
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
        case UpdateAssign: case Assign: case Declare: case FunctionDef: case TypeDef:
            return Type(VoidType);
        default: break;
        }

        // Function defs are visible in the entire block (allowing corecursive funcs)
        foreach (block->statements, stmt, _) {
            predeclare_def_funcs(env, *stmt);
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
    case TypeArray: case TypeTable: case TypeStruct: case TypePointer: case TypeFunction:
    case TypeTaggedUnion: {
        compiler_err(env, ast, "Attempt to get the type of this term, which is already a type annotation");
    }
    case UnaryOp: {
        auto unop = Match(ast, UnaryOp);
        sss_type_t *t = get_type(env, unop->value);
        if (unop->op == OP_NEGATIVE) {
            if (!is_numeric(t))
                compiler_err(env, ast, "I only know how to get negatives of numeric types, not %T", t);
            return t;
        } else if (unop->op == OP_NOT) {
            if (base_variant(t)->tag == TaggedUnionType)
                return t;
            else if (base_variant(t)->tag == BoolType || is_integral(t))
                return t;
            else if (base_variant(t)->tag == PointerType && Match(base_variant(t), PointerType)->is_optional)
                return Type(BoolType);
            compiler_err(env, ast, "I only know what `not` means for Bools, Ints, and Enums, but this is a %T", t); 
        }
        break;
    }
    case BinaryOp: {
        auto binop = Match(ast, BinaryOp);
        sss_type_t *lhs_t = get_type(env, binop->lhs),
                  *rhs_t = get_type(env, binop->rhs);

        switch (binop->op) {
        case OP_AND: {
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
                return get_math_type(env, ast, lhs_t, binop->op, rhs_t);
            }
            compiler_err(env, ast, "I can't figure out the type of this `and` expression because the left side is a %T, but the right side is a %T",
                         lhs_t, rhs_t);
        }
        case OP_OR: {
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
                return get_math_type(env, ast, lhs_t, binop->op, rhs_t);
            }
            compiler_err(env, ast, "I can't figure out the type of this `or` expression because the left side is a %T, but the right side is a %T",
                         lhs_t, rhs_t);
        }
        case OP_XOR: {
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
                return get_math_type(env, ast, lhs_t, binop->op, rhs_t);
            }

            compiler_err(env, ast, "I can't figure out the type of this `xor` expression because the left side is a %T, but the right side is a %T",
                         lhs_t, rhs_t);
        }
        case OP_CONCAT: {
            if (!type_eq(lhs_t, rhs_t))
                compiler_err(env, ast, "The type on the left side of this concatenation doesn't match the right side: %T vs. %T",
                             lhs_t, rhs_t);
            sss_type_t *base_t = lhs_t;
            while (base_t->tag == VariantType) base_t = Match(base_t, VariantType)->variant_of;
            if (base_t->tag != ArrayType)
                compiler_err(env, ast, "Only array/string value types support concatenation, not %T", lhs_t);
            return lhs_t;
        }
        case OP_IN: case OP_NOT_IN: {
            return Type(BoolType);
        }
        default: {
            return get_math_type(env, ast, lhs_t, binop->op, rhs_t);
        }
        }
    }
    case UpdateAssign:
        return Type(VoidType);

    case Comparison: {
        auto comparison = Match(ast, Comparison);
        sss_type_t *lhs_t = get_type(env, comparison->lhs);
        sss_type_t *rhs_t = get_type(env, comparison->rhs);
        if (type_is_a(lhs_t, rhs_t) || type_is_a(rhs_t, lhs_t))
            return Type(BoolType);
        else if (is_numeric(lhs_t) && is_numeric(rhs_t))
            return Type(BoolType);
        else
            compiler_err(env, ast, "I only know how to compare values that have the same type, but this comparison is between %T and %T",
                         lhs_t, rhs_t);
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

    case Lambda: {
        auto lambda = Match(ast, Lambda);
        auto args = EMPTY_ARRAY(ast_t*);
        auto arg_types = EMPTY_ARRAY(sss_type_t*);
        for (int64_t i = 0; i < LENGTH(lambda->args.types); i++) {
            ast_t *arg_def = ith(lambda->args.types, i);
            sss_type_t *t = parse_type_ast(env, arg_def);
            append(arg_types, t);
            append(args, ith(lambda->args.args, i));
        }

        sss_type_t *ret = get_type(env, lambda->body);
        if (has_stack_memory(ret))
            compiler_err(env, ast, "Functions can't return stack references because the reference may outlive its stack frame.");
        return Type(FunctionType, .args=args, .arg_types=arg_types, .ret=ret);
    }

    case FunctionDef: {
        auto def = Match(ast, FunctionDef);
        auto args = EMPTY_ARRAY(ast_t*);
        auto arg_types = EMPTY_ARRAY(sss_type_t*);
        auto arg_defaults = EMPTY_ARRAY(ast_t*);

        for (int64_t i = 0; i < LENGTH(def->args.types); i++) {
            ast_t *arg_def = ith(def->args.types, i);
            ast_t *arg = ith(def->args.args, i);
            append(args, arg);
            if (arg_def) {
                sss_type_t *arg_type = parse_type_ast(env, arg_def);
                append(arg_types, arg_type);
                ast_t *default_val = NULL;
                append(arg_defaults, default_val);
            } else {
                ast_t *default_val = ith(def->args.defaults, i);
                sss_type_t *arg_type = get_type(env, default_val);
                append(arg_types, arg_type);
                append(arg_defaults, default_val);
            }
        }

        sss_type_t *ret = def->ret_type ? parse_type_ast(env, def->ret_type) : Type(VoidType);
        if (has_stack_memory(ret))
            compiler_err(env, def->ret_type, "Functions can't return stack references because the reference may outlive its stack frame.");
        return Type(FunctionType, .args=args, .arg_types=arg_types, .arg_defaults=arg_defaults, .ret=ret);
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
        if (for_loop->first)
            return generate(get_type(env, for_loop->first));
        else if (for_loop->body)
            return generate(get_type(env, for_loop->body));
        else if (for_loop->between)
            return generate(get_type(env, for_loop->between));
        else if (for_loop->empty)
            return generate(get_type(env, for_loop->empty));
        else
            compiler_err(env, ast, "I can't figure out the type of this 'for' loop");
    }
    case Reduction: {
        auto reduction = Match(ast, Reduction);
        sss_type_t *item_type = get_iter_type(env, reduction->iter);
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
        return get_type(env, with->body);
    }
    case Using: { // using expr *[; expr] body
        auto using = Match(ast, Using);
        foreach (using->used, used, _) {
            compiler_err(env, ast, "'using' is not implemented right now");
        }
        return get_type(env, using->body);
    }
    case Variant: {
        auto variant = Match(ast, Variant);
        return parse_type_ast(env, variant->type);
    }
    default: break;
    }
    compiler_err(env, ast, "I can't figure out the type of: %s", ast_to_str(ast));
}

bool is_discardable(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case UpdateAssign: case Assign: case Declare: case FunctionDef: case TypeDef: case Use:
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
                if ((*pat)->tag == Nil || (*pat)->tag == Wildcard) {
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
    auto statements = Match(namespace_ast, Block)->statements;

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
                    auto args = EMPTY_ARRAY(ast_t*);
                    for (int64_t i = 0; i < LENGTH(fields->field_names); i++) {
                        append(args, FakeAST(Var, .name=ith(fields->field_names, i), .binding=new(binding_t, .type=ith(fields->field_types, i))));
                    }
                    sss_type_t *constructor_t = Type(FunctionType, .args=args,
                                                     .arg_types=fields->field_types, .arg_defaults=fields->field_defaults,
                                                     .ret=type);
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
            break;
        }
        case TypeDef: {
            auto def = Match(stmt, TypeDef);
            const char *name = Match(def->name, TypeVar)->name;
            append(field_names, name);
            sss_type_t *def_type = Match(def->name, TypeVar)->type;
            sss_type_t *ns_t = get_namespace_type(env, def->namespace, def_type);
            append(field_types, ns_t);
            break;
        }
        case FunctionDef: {
            auto def = Match(stmt, FunctionDef);
            auto var = Match(def->name, Var);
            append(field_names, var->name);
            append(field_types, var->binding->type);
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

typedef struct {
    sss_file_t *file;
    env_t *env;
    ast_t *ast;
} parsed_file_info_t;

static parsed_file_info_t *get_file_info(env_t *env, const char *path)
{
    static table_t cache = {0};

    struct stat file_stat;
    const char *sss_path = strlen(path) > 4 && streq(path + strlen(path) - 4, ".sss") ? path : heap_strf("%s.sss", path);
    if (stat(sss_path, &file_stat) == -1)
        compiler_err(env, NULL, "I can't find the file %s", sss_path);

    parsed_file_info_t *file_info = Table_str_get(&cache, path);
    if (file_info) return file_info;

    sss_file_t *f = sss_load_file(sss_path);
    file_info = new(parsed_file_info_t, .file=f);
    Table_str_set(&cache, path, file_info);
    file_info->env = new(env_t);
    *file_info->env = *env;
    file_info->env->file = f;

    file_info->ast = parse_file(f, env->on_err);
    table_t *type_bindings = new(table_t, .fallback=&env->global->types);
    bind_types(env, type_bindings, file_info->ast);
    populate_types(env, type_bindings, file_info->ast);
    bind_variables(env, new(table_t, .fallback=&env->global->bindings), file_info->ast);
    // sss_type_t *ns_t = get_namespace_type(env, ast, NULL);
    return file_info;
}

sss_type_t *get_file_type(env_t *env, const char *path)
{
    auto info = get_file_info(env, path);
    return get_namespace_type(info->env, info->ast, NULL);
}

// static void _load_file_types(env_t *src_env, table_t *src_bindings, env_t *dest_env, table_t *dest_namespace)
// {
//     for (int64_t i = 1; i <= Table_length(src_bindings); i++) {
//         struct {const char *key; sss_type_t *type;} *entry = Table_str_entry(src_bindings, i);
//         if (strncmp(entry->key, "#type:", strlen("#type:")) != 0)
//             continue;
//         Table_str_set(dest_namespace, entry->key, entry->type);
//         _load_file_types(src_env, get_namespace(src_env, entry->type),
//                          dest_env, get_namespace(dest_env, entry->type));
//     }
// }

// void load_file_types(env_t *env, const char *name, const char *path)
// {
//     sss_type_t *t = Type(VariantType, .variant_of=Type(VoidType), .name=name);
//     Table_str_set(env->bindings, heap_strf("#type:%s", name), t);
//     table_t *namespace = get_namespace(env, t);
//     namespace->fallback = NULL;

    // auto info = get_file_info(env, path);
    // _load_file_types(info->env, info->env->bindings, env, namespace);
// }

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
