// Helper functions used for compiling
#include <assert.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <gc/cord.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdalign.h>
#include <stdint.h>

#include "../ast.h"
#include "../builtins/array.h"
#include "../builtins/table.h"
#include "../builtins/range.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"
#include "compile.h"
#include "libgccjit_abbrev.h"

static table_t opaque_structs = {0};

const char *fresh(const char *name)
{
    static table_t seen = {0};
    char *tmp = (char*)heap_str(name);
    for (size_t i = 0; i < strlen(tmp); i++) {
        if (!isalpha(name[i]) && !isdigit(name[i]) && name[i] != '_')
            tmp[i] = '_';
    }
    name = (const char*)tmp;
    int64_t count = (int64_t)Table_str_get(&seen, name);
    const char *ret = heap_strf("%s__%ld", name, count++);
    Table_str_set(&seen, name, (void*)(count+1));
    return ret;
}

// Kinda janky, but libgccjit doesn't have this function built in
ssize_t gcc_alignof(env_t *env, sss_type_t *sss_t)
{
    switch (sss_t->tag) {
    case ArrayType: return alignof(array_t);
    case TableType: return sizeof(void*);
    case StructType: {
        ssize_t align = 0;
        auto struct_type = Match(sss_t, StructType);
        foreach (struct_type->field_types, ftype, _) {
            if (type_eq(*ftype, sss_t))
                compiler_err(env, NULL, "The struct %T recursively contains itself, which would be infinitely large. If you want to reference other %T structs, use a pointer or an array.",
                             sss_t, sss_t);
            ssize_t field_align = gcc_alignof(env, *ftype);
            if (field_align > align) align = field_align;
        }
        return align;
    }
    case TaggedUnionType: {
        auto tagged = Match(sss_t, TaggedUnionType);
        ssize_t align = tagged->tag_bits/8;
        foreach (tagged->members, member, _) {
            if (!member->type) continue;
            ssize_t member_align = gcc_alignof(env, member->type);
            if (member_align > align) align = member_align;
        }
        return align;
    }
    case VariantType: return gcc_alignof(env, Match(sss_t, VariantType)->variant_of);
    case ModuleType: return 1;
    case VoidType: case MemoryType: return 1;
    default:
        return gcc_sizeof(env, sss_t);
    }
}

// Kinda janky, but libgccjit doesn't have this function built in, except for integer types:
ssize_t gcc_sizeof(env_t *env, sss_type_t *sss_t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, sss_t);
    if (gcc_type_is_integral(gcc_t))
        return gcc_type_size(gcc_t);

    switch (sss_t->tag) {
    case ArrayType: return sizeof (array_t);
    case TableType: return sizeof (table_t);
    case RangeType: return sizeof (range_t);
    case BoolType: return sizeof(bool);
    case TypeType: return sizeof(char*);
    case NumType: return Match(sss_t, NumType)->bits / 8;
    case FunctionType:
    case PointerType: return sizeof(void*);
    case StructType: {
        ssize_t size = 0;
        ssize_t max_align = 0;
        auto struct_type = Match(sss_t, StructType);
        foreach (struct_type->field_types, ftype, _) {
            if (type_eq(*ftype, sss_t))
                compiler_err(env, NULL, "The struct %T recursively contains itself, which would be infinitely large. If you want to reference other %T structs, use a pointer or an array.",
                             sss_t, sss_t);
            ssize_t field_align = gcc_alignof(env, *ftype);
            if (field_align > 1 && size % field_align)
                size += field_align - (size % field_align); // padding
            size += gcc_sizeof(env, *ftype);
            if (field_align > max_align) max_align = field_align;
        }
        if (max_align > 1 && size % max_align)
            size += max_align - (size % max_align); // padding
        return size;
    }
    case TaggedUnionType: {
        auto tagged = Match(sss_t, TaggedUnionType);
        ssize_t size = tagged->tag_bits / 8;
        ssize_t union_align = size;
        ssize_t union_size = 0;
        foreach (tagged->members, member, _) {
            if (!member->type) continue;
            ssize_t member_align = gcc_alignof(env, member->type);
            if (member_align > union_align) union_align = member_align;
            ssize_t member_size = gcc_sizeof(env, member->type);
            if (member_size > union_size) union_size = member_size;
        }
        // Padding for union:
        if (size % union_align != 0) size = (size - (size % union_align)) + union_align;
        size += union_size;
        return size;
    }
    case VariantType: return gcc_sizeof(env, Match(sss_t, VariantType)->variant_of);
    case ModuleType: return 0;
    case VoidType: case MemoryType: return 0;
    default: compiler_err(env, NULL, "gcc_sizeof() isn't implemented for %T", sss_t);
    }
}

gcc_type_t *get_tag_type(env_t *env, sss_type_t *t)
{
    auto tagged = Match(base_variant(t), TaggedUnionType);
    switch (tagged->tag_bits) {
    case 64: return gcc_type(env->ctx, INT64);
    case 32: return gcc_type(env->ctx, INT32);
    case 16: return gcc_type(env->ctx, INT16);
    case 8: return gcc_type(env->ctx, INT8);
    default: compiler_err(env, NULL, "Unsupported tagged enum size: %d\n", tagged->tag_bits); 
    }
}

gcc_type_t *get_union_type(env_t *env, sss_type_t *t)
{
    t = base_variant(t);
    static table_t cache = {0};
    gcc_type_t *gcc_t = Table_str_get(&cache, type_to_string(t));
    if (gcc_t) return gcc_t;
    auto tagged = Match(base_variant(t), TaggedUnionType);
    auto fields = EMPTY_ARRAY(gcc_field_t*);
    foreach (tagged->members, member, _) {
        if (member->type && Table_str_get(&opaque_structs, type_to_string(member->type)))
            compiler_err(env, NULL, "The tagged union %T recursively contains itself, which could be infinitely large. If you want to reference other %T values, use a pointer or an array.",
                         t, t);
        gcc_type_t *gcc_ft = member->type ? sss_type_to_gcc(env, member->type)
            : gcc_struct_as_type(gcc_new_struct_type(env->ctx, NULL, member->name, 0, NULL)); // empty struct
        gcc_field_t *field = gcc_new_field(env->ctx, NULL, gcc_ft, member->name);
        append(fields, field);
    }
    gcc_type_t *union_gcc_t = gcc_union(env->ctx, NULL, "data_union", LENGTH(fields), fields[0]);
    Table_str_set(&cache, type_to_string(t), union_gcc_t);
    return union_gcc_t;
}

static gcc_type_t *get_type_gcc_type(env_t *env)
{
    static gcc_type_t *type_gcc_type = NULL;
    if (type_gcc_type != NULL)
        return type_gcc_type;

#define FIELD(name, type) gcc_new_field(env->ctx, NULL, type, name)
#define STRUCT(name, ...) gcc_struct_as_type(gcc_new_struct_type(env->ctx, NULL, name, sizeof((gcc_field_t*[]){__VA_ARGS__})/sizeof(gcc_field_t*), (gcc_field_t*[]){__VA_ARGS__}))
#define UNION(name, ...) gcc_union(env->ctx, NULL, name, sizeof((gcc_field_t*[]){__VA_ARGS__})/sizeof(gcc_field_t*), (gcc_field_t*[]){__VA_ARGS__})

    gcc_struct_t *type_struct = gcc_opaque_struct(env->ctx, NULL, "Type");
    type_gcc_type = gcc_struct_as_type(type_struct);
    
    gcc_type_t *type_info_union_t = UNION(
        "TypeInfo",
        FIELD("NamedInfo", STRUCT(
                "NamedInfo",
                FIELD("name", gcc_type(env->ctx, STRING)), 
                FIELD("base", gcc_get_ptr_type(type_gcc_type)))),
        FIELD("PointerInfo", STRUCT(
                "PointerInfo",
                FIELD("sigil", gcc_type(env->ctx, STRING)), 
                FIELD("pointed", gcc_get_ptr_type(type_gcc_type)))),
        FIELD("ArrayInfo", STRUCT(
                "ArrayInfo",
                FIELD("item", gcc_get_ptr_type(type_gcc_type)))),
        FIELD("TableInfo", STRUCT(
                "TableInfo",
                FIELD("key", gcc_get_ptr_type(type_gcc_type)),
                FIELD("value", gcc_get_ptr_type(type_gcc_type)),
                FIELD("entry_size", gcc_type(env->ctx, SIZE)),
                FIELD("value_offset", gcc_type(env->ctx, SIZE)))));

    gcc_field_t *fields[] = { 
        FIELD("name", gcc_type(env->ctx, STRING)),
        FIELD("info", STRUCT("TypeInfo", FIELD("tag", gcc_type(env->ctx, INT32)), FIELD("__data", type_info_union_t))),
        FIELD("size", gcc_type(env->ctx, SIZE)),
        FIELD("align", gcc_type(env->ctx, SIZE)),
        FIELD("equal", gcc_type(env->ctx, VOID_PTR)),
        FIELD("compare", gcc_type(env->ctx, VOID_PTR)),
        FIELD("hash", gcc_type(env->ctx, VOID_PTR)),
        FIELD("cord", gcc_type(env->ctx, VOID_PTR)),
        FIELD("bindings", gcc_type(env->ctx, VOID_PTR)),
    };
#undef UNION
#undef STRUCT
#undef FIELD
    gcc_set_fields(type_struct, NULL, sizeof(fields)/sizeof(fields[0]), fields);
    return type_gcc_type;
}


// This must be memoized because GCC JIT doesn't do structural equality
gcc_type_t *sss_type_to_gcc(env_t *env, sss_type_t *t)
{
    static table_t cache = {0};
    t = with_units(t, NULL);
    const char *cache_key = type_to_string(t);
    gcc_type_t *gcc_t = Table_str_get(&cache, cache_key);
    if (gcc_t) return gcc_t;

    const char *name = NULL;

  // proceed_with_name:;

    switch (t->tag) {
    case IntType: {
        if (Match(t, IntType)->is_unsigned) {
            switch (Match(t, IntType)->bits) {
            case 64: gcc_t = gcc_type(env->ctx, UINT64); break;
            case 32: gcc_t = gcc_type(env->ctx, UINT32); break;
            case 16: gcc_t = gcc_type(env->ctx, UINT16); break;
            case 8: gcc_t = gcc_type(env->ctx, UINT8); break;
            case 0: gcc_t = gcc_type(env->ctx, UINT32); break;
            default: compiler_err(env, NULL, "I couldn't get a GCC type for an unsigned integer with %d bits", Match(t, IntType)->bits);
            }
        } else {
            switch (Match(t, IntType)->bits) {
            case 64: gcc_t = gcc_type(env->ctx, INT64); break;
            case 32: gcc_t = gcc_type(env->ctx, INT32); break;
            case 16: gcc_t = gcc_type(env->ctx, INT16); break;
            case 8: gcc_t = gcc_type(env->ctx, INT8); break;
            case 0: gcc_t = gcc_type(env->ctx, INT); break;
            default: compiler_err(env, NULL, "I couldn't get a GCC type for an integer with %d bits", Match(t, IntType)->bits);
            }
        }
        break;
    }
    case CharType: case CStringCharType: gcc_t = gcc_type(env->ctx, CHAR); break;
    case BoolType: gcc_t = gcc_type(env->ctx, BOOL); break;
    case NumType: gcc_t = Match(t, NumType)->bits == 32 ? gcc_type(env->ctx, FLOAT) : gcc_type(env->ctx, DOUBLE); break;
    case VoidType: case MemoryType: case AbortType: gcc_t = gcc_type(env->ctx, VOID); break;
    case PointerType: {
        gcc_t = sss_type_to_gcc(env, Match(t, PointerType)->pointed);
        gcc_t = gcc_get_ptr_type(gcc_t);
        break;
    }
    case RangeType: {
        gcc_type_t *i64 = gcc_type(env->ctx, INT64);
        gcc_field_t *fields[3] = {
            gcc_new_field(env->ctx, NULL, i64, "first"),
            gcc_new_field(env->ctx, NULL, i64, "step"),
            gcc_new_field(env->ctx, NULL, i64, "last"),
        };
        gcc_struct_t *range = gcc_new_struct_type(env->ctx, NULL, "Range", 3, fields);
        gcc_t = gcc_struct_as_type(range);
        break;
    }
    case ArrayType: {
        sss_type_t *item_type = Match(t, ArrayType)->item_type;
        gcc_field_t *fields[] = {
            [ARRAY_DATA_FIELD]=gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(sss_type_to_gcc(env, item_type)), "items"),
            [ARRAY_LENGTH_FIELD]=gcc_bitfield(env->ctx, NULL, gcc_type(env->ctx, INT64), 42, "LENGTH"),
            [ARRAY_CAPACITY_FIELD]=gcc_bitfield(env->ctx, NULL, gcc_type(env->ctx, UINT8), 4, "free"),
            [ARRAY_COW_FIELD]=gcc_bitfield(env->ctx, NULL, gcc_type(env->ctx, UINT8), 1, "copy_on_write"),
            [ARRAY_ATOMIC_FIELD]=gcc_bitfield(env->ctx, NULL, gcc_type(env->ctx, UINT8), 1, "atomic"),
            [ARRAY_STRIDE_FIELD]=gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT16), "stride"),
        };
        gcc_struct_t *array = gcc_new_struct_type(env->ctx, NULL, fresh("Array"), sizeof(fields)/sizeof(fields[0]), fields);
        gcc_t = gcc_struct_as_type(array);
        break;
    }
    case TableType: {
        gcc_type_t *u32 = gcc_type(env->ctx, UINT32);
        auto table = Match(t, TableType);
        if (table->key_type->tag == VoidType || table->value_type->tag == VoidType)
            compiler_err(env, NULL, "Tables can't hold Void types");

        gcc_field_t *bucket_fields[] = {
            gcc_new_field(env->ctx, NULL, u32, "index1"),
            gcc_new_field(env->ctx, NULL, u32, "next1"),
        };
        gcc_struct_t *bucket = gcc_new_struct_type(env->ctx, NULL, "Bucket", 2, bucket_fields);

        gcc_struct_t *gcc_struct = gcc_opaque_struct(env->ctx, NULL, "Table");
        gcc_field_t *fields[] = {
            [TABLE_ENTRIES_FIELD]=gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, VOID_PTR), "entries"),
            [TABLE_BUCKETS_FIELD]=gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(gcc_struct_as_type(bucket)), "buckets"),
            [TABLE_FALLBACK_FIELD]=gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(gcc_struct_as_type(gcc_struct)), "fallback"),
            [TABLE_DEFAULT_FIELD]=gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(sss_type_to_gcc(env, table->value_type)), "default_value"),
            [TABLE_CAPACITY_FIELD]=gcc_new_field(env->ctx, NULL, u32, "capacity"),
            [TABLE_COUNT_FIELD]=gcc_new_field(env->ctx, NULL, u32, "count"),
            [TABLE_LASTFREE_FIELD]=gcc_new_field(env->ctx, NULL, u32, "lastfree_index1"),
            [TABLE_COW_FIELD]=gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, BOOL), "copy_on_write"),
        };
        gcc_set_fields(gcc_struct, NULL, sizeof(fields)/sizeof(fields[0]), fields);
        gcc_t = gcc_struct_as_type(gcc_struct);
        break;
    }
    case FunctionType: {
        auto arg_types = EMPTY_ARRAY(gcc_type_t*);
        auto fn = Match(t, FunctionType);
        foreach (fn->arg_types, arg_t, _) {
            if ((*arg_t)->tag == VoidType)
                compiler_err(env, NULL, "Function has a Void argument type: %T", t);
            append(arg_types, sss_type_to_gcc(env, *arg_t));
        }
        gcc_type_t *ret_type = sss_type_to_gcc(env, fn->ret);
        gcc_t = gcc_new_func_type(env->ctx, NULL, ret_type, LENGTH(arg_types), arg_types[0], 0);
        break;
    }
    case StructType: {
        auto struct_t = Match(t, StructType);
        const char *t_str = type_to_string(t);
        gcc_type_t *opaque = Table_str_get(&opaque_structs, t_str);
        if (opaque) return opaque;
        gcc_struct_t *gcc_struct = gcc_opaque_struct(env->ctx, NULL, name ? name : "Tuple");
        gcc_t = gcc_struct_as_type(gcc_struct);
        Table_str_set(&cache, t_str, gcc_t);
        Table_str_set(&opaque_structs, t_str, gcc_t);

        auto fields = EMPTY_ARRAY(gcc_field_t*);
        for (int64_t i = 0; i < LENGTH(struct_t->field_types); i++) {
            sss_type_t *sss_ft = ith(struct_t->field_types, i);
            if (Table_str_get(&opaque_structs, type_to_string(sss_ft)))
                compiler_err(env, NULL, "The struct %T recursively contains itself, which would be infinitely large. If you want to reference other %T structs, use a pointer or an array.",
                             t, t);
            gcc_type_t *gcc_ft = sss_type_to_gcc(env, sss_ft);
            assert(gcc_ft);
            const char *field_name = struct_t->field_names ? ith(struct_t->field_names, i) : NULL;
            if (!field_name) field_name = heap_strf("%_ld", i);
            gcc_field_t *field = gcc_new_field(env->ctx, NULL, gcc_ft, field_name);
            append(fields, field);
        }
        gcc_set_fields(gcc_struct, NULL, LENGTH(fields), fields[0]);
        gcc_t = gcc_struct_as_type(gcc_struct);
        Table_str_remove(&opaque_structs, t_str);
        break;
    }
    case TaggedUnionType: {
        const char *t_str = type_to_string(t);
        gcc_type_t *opaque = Table_str_get(&opaque_structs, t_str);
        if (opaque) return opaque;
        gcc_struct_t *gcc_struct = gcc_opaque_struct(env->ctx, NULL, name ? name : "TaggedUnion");
        gcc_t = gcc_struct_as_type(gcc_struct);
        Table_str_set(&cache, t_str, gcc_t);
        Table_str_set(&opaque_structs, t_str, gcc_t);
        gcc_set_fields(gcc_struct, NULL, 2, (gcc_field_t*[]){
            gcc_new_field(env->ctx, NULL, get_tag_type(env, t), "tag"),
            gcc_new_field(env->ctx, NULL, get_union_type(env, t), "__data"),
        });
        Table_str_remove(&opaque_structs, t_str);
        break;
    }
    case TypeType: {
        cache_key = "Type";
        gcc_t = Table_str_get(&cache, cache_key);
        if (gcc_t) return gcc_t;
        gcc_t = gcc_get_ptr_type(get_type_gcc_type(env));
        break;
    }
    case GeneratorType: {
        auto generator = Match(t, GeneratorType);
        switch (generator->generated->tag) {
        case AbortType: case VoidType: case MemoryType: return gcc_type(env->ctx, VOID);
        default: goto unknown_gcc_type;
        }
    }
    case ModuleType: {
        gcc_t = gcc_struct_as_type(gcc_new_struct_type(env->ctx, NULL, "Module", 0, NULL));
        break;
    }
    case VariantType: {
        return sss_type_to_gcc(env, Match(t, VariantType)->variant_of);
        // name = Match(t, VariantType)->name;
        // t = Match(t, VariantType)->variant_of;
        // goto proceed_with_name;
    }
    default: {
      unknown_gcc_type:
        compiler_err(env, NULL, "The following SSS type doesn't have a GCC type: %T", t);
    }
    }

    Table_str_set(&cache, cache_key, gcc_t);
    return gcc_t;
}

bool demote_int_literals(ast_t **ast, sss_type_t *needed)
{
    if ((*ast)->tag != Int || needed->tag != IntType) return false;

    auto needed_int = Match(needed, IntType);
    auto int_ast = Match(*ast, Int);
    if (streq(needed_int->units, int_ast->units)) {
        *ast = WrapAST(*ast, Int, .i=int_ast->i, .precision=needed_int->bits, .is_unsigned=needed_int->is_unsigned, .units=needed_int->units);
        return true;
    } else {
        return false;
    }
}

bool promote(env_t *env, sss_type_t *actual, gcc_rvalue_t **val, sss_type_t *needed)
{
    if (!can_promote(actual, needed))
        return false;

    // String <-> c string promotion
    if (type_eq(actual, Type(PointerType, .pointed=Type(CStringCharType))) && type_eq(needed, Type(ArrayType, .item_type=Type(CharType)))) {
        binding_t *b = get_from_namespace(env, needed, "from_pointer");
        *val = gcc_callx(env->ctx, NULL, b->func, *val);
    } else if (type_eq(actual, Type(ArrayType, .item_type=Type(CharType))) && type_eq(needed, Type(PointerType, .pointed=Type(CStringCharType)))) {
        binding_t *b = get_from_namespace(env, actual, "c_string");
        *val = gcc_callx(env->ctx, NULL, b->func, *val);
    } else if (actual->tag == StructType && base_variant(needed)->tag == StructType) { // Struct promotion
        gcc_type_t *actual_gcc_t = sss_type_to_gcc(env, actual);
        gcc_type_t *needed_gcc_t = sss_type_to_gcc(env, needed);
        auto actual_field_types = Match(actual, StructType)->field_types;
        auto needed_field_types = Match(base_variant(needed), StructType)->field_types;
        if (LENGTH(needed_field_types) == 0)
            return true;
        auto needed_fields = EMPTY_ARRAY(gcc_field_t*);
        auto field_vals = EMPTY_ARRAY(gcc_rvalue_t*);
        for (int64_t i = 0; i < LENGTH(actual_field_types); i++) {
            append(needed_fields, gcc_get_field(gcc_type_if_struct(needed_gcc_t), i));
            gcc_rvalue_t *field_val = gcc_rvalue_access_field(*val, NULL, gcc_get_field(gcc_type_if_struct(actual_gcc_t), i));
            if (!promote(env, ith(actual_field_types, i), &field_val, ith(needed_field_types, i)))
                return false;
            append(field_vals, field_val);
        }
        *val = gcc_struct_constructor(env->ctx, NULL, needed_gcc_t, LENGTH(needed_fields), needed_fields[0], field_vals[0]);
    } else if (!type_eq(actual, needed) || actual->tag == FunctionType) {
        *val = gcc_cast(env->ctx, NULL, *val, sss_type_to_gcc(env, needed));
    }
    return true;
}

void check_truthiness(env_t *env, gcc_block_t **block, ast_t *obj, gcc_block_t *if_truthy, gcc_block_t *if_falsey)
{
    sss_type_t *t = get_type(env, obj);
    gcc_rvalue_t *bool_val = compile_expr(env, block, obj); 
    if (t->tag == PointerType) {
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, bool_val, gcc_null(env->ctx, gcc_t));
    } else if (t->tag != BoolType) {
        compiler_err(env, obj, "This value can't be used for a boolean value, since it's a %T", t);
    }
    gcc_jump_condition(*block, NULL, bool_val, if_truthy, if_falsey);
    *block = NULL;
}

gcc_rvalue_t *ternary(gcc_block_t **block, gcc_rvalue_t *condition, gcc_type_t *gcc_t, gcc_rvalue_t *true_val, gcc_rvalue_t *false_val)
{
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *is_true = gcc_new_block(func, fresh("is_true")),
                *is_false = gcc_new_block(func, fresh("is_false")),
                *got_value = gcc_new_block(func, fresh("got_value"));

    gcc_lvalue_t *result = gcc_local(func, NULL, gcc_t, "_result");
    gcc_jump_condition(*block, NULL, condition, is_true, is_false);
    *block = NULL;

    gcc_assign(is_true, NULL, result, true_val);
    gcc_jump(is_true, NULL, got_value);

    gcc_assign(is_false, NULL, result, false_val);
    gcc_jump(is_false, NULL, got_value);

    *block = got_value;
    return gcc_rval(result);
}

bool can_be_lvalue(env_t *env, ast_t *ast, bool allow_slices)
{
    switch (ast->tag) {
    case Var: {
        binding_t *binding = get_binding(env, Match(ast, Var)->name);
        return binding->lval != NULL;
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        sss_type_t *fielded_t = get_type(env, access->fielded);
        if (fielded_t->tag != PointerType && !can_be_lvalue(env, access->fielded, allow_slices))
            return false;
        for (;;) {
            if (fielded_t->tag == PointerType) {
                auto ptr = Match(fielded_t, PointerType);
                if (ptr->is_optional)
                    return false;
                fielded_t = ptr->pointed;
            } else if (fielded_t->tag == VariantType) {
                fielded_t = Match(fielded_t, VariantType)->variant_of;
            } else break;
        }

        if (fielded_t->tag == StructType) {
            auto fielded_struct = Match(fielded_t, StructType);
            for (int64_t i = 0, len = LENGTH(fielded_struct->field_names); i < len; i++) {
                if (streq(ith(fielded_struct->field_names, i), access->field))
                    return true;
            }
            compiler_err(env, ast, "The struct %T doesn't have a field called '%s'", fielded_t, access->field);
        } else if (fielded_t->tag == ArrayType) { 
            // arr.x => [item.x for x in arr]
            return allow_slices;
        } else if (fielded_t->tag == TableType) {
            return streq(access->field, "default") || streq(access->field, "fallback");
        } else if (fielded_t->tag == TaggedUnionType) {
            auto tagged = Match(fielded_t, TaggedUnionType);
            for (int64_t i = 0; i < LENGTH(tagged->members); i++) {
                auto member = ith(tagged->members, i);
                if (streq(access->field, member.name))
                    return true;
            }
            return false;
        } else {
            return false;
        }
    }
    case Index: {
        auto indexing = Match(ast, Index);
        if (!allow_slices && get_type(env, indexing->index)->tag == RangeType)
            return false;
        if (!can_be_lvalue(env, indexing->indexed, allow_slices))
            return false;

        sss_type_t *indexed_t = get_type(env, indexing->indexed);
        sss_type_t *pointed_type = indexed_t;
        while (pointed_type->tag == PointerType) {
            auto ptr = Match(pointed_type, PointerType);
            if (ptr->is_optional)
                return false;
            pointed_type = ptr->pointed;
        }

        if (pointed_type->tag == ArrayType) {
            return true;
        } else if (pointed_type->tag == TableType) {
            return true;
        } else {
            return false;
        }
    }
    case StackReference:
        return false;
    case HeapAllocate:
        return false;
    default:
        return false;
    }
}

gcc_lvalue_t *get_lvalue(env_t *env, gcc_block_t **block, ast_t *ast, bool allow_slices)
{
    gcc_loc_t *loc = ast_loc(env, ast);
    switch (ast->tag) {
    case Var: {
        binding_t *binding = get_binding(env, Match(ast, Var)->name);
        if (binding) {
            if (!binding->lval)
                compiler_err(env, ast, "This variable can't be assigned to. You can try declaring a new variable with the same name, though.");
            return binding->lval;
        } else {
            const char *suggestion = spellcheck(env->bindings, Match(ast, Var)->name);
            if (suggestion)
                compiler_err(env, ast, "I don't know what this variable is referring to. Did you mean '%s'?", suggestion); 
            else
                compiler_err(env, ast, "I don't know what this variable is referring to."); 
        }
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        sss_type_t *fielded_t = get_type(env, access->fielded);

        gcc_lvalue_t *fielded_lval;
        if (fielded_t->tag == PointerType) {
            gcc_rvalue_t *ptr_rval = compile_expr(env, block, access->fielded);
            auto ptr = Match(fielded_t, PointerType);
          dereference_again:
            if (ptr->is_optional)
                compiler_err(env, ast, "Accessing a field on this value could result in trying to dereference a nil value, since the type is optional");
            if (ptr->is_readonly)
                compiler_err(env, ast, "This pointer is read-only and it can't be used to modify the data it points to");
            fielded_lval = gcc_rvalue_dereference(ptr_rval, loc);
            fielded_t = ptr->pointed;
            if (fielded_t->tag == PointerType) {
                if (Match(fielded_t, PointerType)->is_readonly)
                    compiler_err(env, ast, "This pointer is read-only and it can't be used to modify the data it points to");
                ptr_rval = gcc_rval(fielded_lval);
                ptr = Match(fielded_t, PointerType);
                goto dereference_again;
            }
        } else {
            fielded_lval = get_lvalue(env, block, access->fielded, allow_slices);
        } 

        sss_type_t *base_t = base_variant(fielded_t);
        if (base_t->tag == StructType) {
            auto fielded_struct = Match(base_t, StructType);
            for (int64_t i = 0, len = LENGTH(fielded_struct->field_names); i < len; i++) {
                if (streq(ith(fielded_struct->field_names, i), access->field)) {
                    gcc_struct_t *gcc_struct = gcc_type_if_struct(sss_type_to_gcc(env, base_t));
                    gcc_field_t *field = gcc_get_field(gcc_struct, i);
                    return gcc_lvalue_access_field(fielded_lval, loc, field);
                }
            }
            compiler_err(env, ast, "The struct %T doesn't have a field called '%s'", fielded_t, access->field);
        } else if (base_t->tag == ArrayType) { 
            // arr.x => [item.x for x in arr]
            if (!allow_slices)
                compiler_err(env, ast, "I can't use array slices as assignment targets");
            gcc_func_t *func = gcc_block_func(*block);
            gcc_rvalue_t *slice_val = array_field_slice(env, block, access->fielded, access->field, ACCESS_WRITE);
            if (!slice_val)
                compiler_err(env, ast, "This isn't a valid slice that I can assign to.");
            gcc_lvalue_t *slice = gcc_local(func, loc, sss_type_to_gcc(env, get_type(env, ast)), "_slice");
            gcc_assign(*block, loc, slice, slice_val);
            return slice;
        } else if (base_t->tag == TableType) {
            sss_type_t *table_t = base_t;
            if (streq(access->field, "default")) {
                // if (get_type(env, access->fielded)->tag != PointerType)
                //     compiler_err(env, ast, "The .default field can't be assigned to on a local value-typed variable."); 
                gcc_struct_t *gcc_struct = gcc_type_if_struct(sss_type_to_gcc(env, table_t));
                gcc_field_t *field = gcc_get_field(gcc_struct, TABLE_DEFAULT_FIELD);
                return gcc_lvalue_access_field(fielded_lval, loc, field);
            } else if (streq(access->field, "fallback")) {
                // if (get_type(env, access->fielded)->tag != PointerType)
                //     compiler_err(env, ast, "The .fallback field can't be assigned to on a local value-typed variable."); 
                gcc_struct_t *gcc_struct = gcc_type_if_struct(sss_type_to_gcc(env, table_t));
                gcc_field_t *field = gcc_get_field(gcc_struct, TABLE_FALLBACK_FIELD);
                return gcc_lvalue_access_field(fielded_lval, loc, field);
            } else {
                compiler_err(env, ast, "The only fields that can be mutated on a table are '.default' and '.fallback', not '.%s'", access->field);
            }
        } else if (base_t->tag == TaggedUnionType) {
            auto tagged = Match(base_t, TaggedUnionType);
            for (int64_t i = 0; i < LENGTH(tagged->members); i++) {
                auto member = ith(tagged->members, i);
                if (!streq(access->field, member.name)) continue;

                gcc_struct_t *tagged_struct = gcc_type_if_struct(sss_type_to_gcc(env, base_t));
                gcc_field_t *tag_field = gcc_get_field(tagged_struct, 0);
                gcc_rvalue_t *tag = gcc_rval(gcc_lvalue_access_field(fielded_lval, NULL, tag_field));
                gcc_func_t *func = gcc_block_func(*block);
                gcc_block_t *wrong_tag = gcc_new_block(func, fresh("wrong_tag")),
                            *right_tag = gcc_new_block(func, fresh("right_tag"));

                gcc_type_t *tag_gcc_t = get_tag_type(env, base_t);
                gcc_rvalue_t *correct_tag = gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value);
                gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, tag, correct_tag),
                                   wrong_tag, right_tag);
                *block = wrong_tag;
                insert_failure(env, block, ast->file, ast->start, ast->end,
                               "Error: this was expected to have the '%s' tag, but instead it's %#s", access->field,
                               fielded_t, gcc_rval(fielded_lval));
                if (*block) gcc_jump(*block, loc, *block);

                *block = right_tag;
                gcc_type_t *gcc_union_t = get_union_type(env, base_t);
                return gcc_lvalue_access_field(gcc_lvalue_access_field(fielded_lval, NULL, gcc_get_field(tagged_struct, 1)), loc,
                                               gcc_get_union_field(gcc_union_t, i));
            }
            compiler_err(env, ast, "The field '%s' is not a valid tag on the tagged union type %T", access->field, fielded_t);
        } else {
            compiler_err(env, ast, "This value is a %T, and I don't know how to assign to fields on it.", fielded_t);
        }
    }
    case Index: {
        auto indexing = Match(ast, Index);

        sss_type_t *indexed_t = get_type(env, indexing->indexed);
        if (!indexing->index && indexed_t->tag == PointerType) {
            if (Match(indexed_t, PointerType)->is_readonly)
                compiler_err(env, ast, "This pointer is read-only and it can't be used to modify the data it points to");
            gcc_rvalue_t *rval = compile_expr(env, block, indexing->indexed);
            return gcc_rvalue_dereference(rval, loc);
        }

        if (!allow_slices && get_type(env, indexing->index)->tag == RangeType)
            compiler_err(env, ast, "I can't assign to array slices");

        if (indexed_t->tag == ArrayType) {
            if (!allow_slices)
                compiler_err(env, ast, "I can't assign to an array value (which is immutable), only to array pointers.");
        } else if (indexed_t->tag == TableType) {
            if (!allow_slices)
                compiler_err(env, ast, "I can't assign to a table value (which is immutable), only to table pointers.");
        }

        sss_type_t *pointed_type = indexed_t;
        while (pointed_type->tag == PointerType) {
            auto ptr = Match(pointed_type, PointerType);
            if (ptr->is_optional)
                compiler_err(env, ast, "Accessing an index on this value could result in trying to dereference a nil value, since the type is optional");
            if (ptr->is_readonly)
                compiler_err(env, ast, "This pointer is read-only and it can't be used to modify the data it points to");
            pointed_type = ptr->pointed;
        }

        if (pointed_type->tag == ArrayType) {
            return array_index(env, block, indexing->indexed, indexing->index, indexing->unchecked, ACCESS_WRITE);
        } else if (pointed_type->tag == TableType) {
            gcc_func_t *func = gcc_block_func(*block);
            gcc_lvalue_t *table_ptr = gcc_local(func, loc, gcc_get_ptr_type(sss_type_to_gcc(env, pointed_type)), "_table");
            gcc_assign(*block, loc, table_ptr, compile_expr(env, block, indexing->indexed));
            mark_table_cow(env, block, gcc_rval(table_ptr));
            return table_lvalue(env, block, pointed_type, gcc_rval(table_ptr), indexing->index, allow_slices);
        } else {
            compiler_err(env, ast, "I only know how to index into Arrays and Tables for assigning");
        }
    }
    case StackReference:
        compiler_err(env, ast, "I can't assign to a stack reference. You don't need the '&', you can just assign to the value directly.");
    case HeapAllocate:
        compiler_err(env, ast, "I can't assign to a heap allocation. You don't need the '@', you can just assign to the value directly.");
    default:
        compiler_err(env, ast, "This is not a valid value for assignment");
    }
}

void insert_defers(env_t *env, gcc_block_t **block, defer_t *stop_at_defer)
{
    if (!*block) {
        // This can happen if the block ends due to a 'fail', in which case the defers won't run
        return;
    }
    for (; env->deferred && env->deferred != stop_at_defer; env->deferred = env->deferred->next) {
        compile_block_statement(env->deferred->environment, block, env->deferred->body);
    }
}

void insert_failure(env_t *env, gcc_block_t **block, sss_file_t *file, const char *start, const char *end, const char *user_fmt, ...)
{
    char *info = NULL, *info_nocolor = NULL;
    {
        char *buf = NULL; size_t size = 0;
        FILE *f = open_memstream(&info, &size);
        if (file && start && end)
            fprint_span(f, file, start, end, "\x1b[31;1m", 2, true);
        fflush(f);
        info = heap_strn(buf, size);
        fclose(f);
        free(buf);
    }
    {
        char *buf = NULL; size_t size = 0;
        FILE *f = open_memstream(&buf, &size);
        if (file && start && end)
            fprint_span(f, file, start, end, NULL, 2, false);
        fflush(f);
        info_nocolor = heap_strn(buf, size);
        fclose(f);
        free(buf);
    }

    gcc_func_t *fail = get_function(env, "fail");
    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *fmt_var = gcc_local(func, NULL, gcc_type(env->ctx, STRING), "_fmt");
    gcc_block_t *use_color = gcc_new_block(func, fresh("use_color")),
                *no_color = gcc_new_block(func, fresh("no_color")),
                *carry_on = gcc_new_block(func, fresh("fmt_set"));
    gcc_jump_condition(*block, NULL, get_binding(env, "USE_COLOR")->rval, use_color, no_color);

    if (file && start && end) {
        gcc_assign(no_color, NULL, fmt_var, gcc_str(
            env->ctx, heap_strf("%s:%ld.%ld: %s\n\n%s",
                file->relative_filename,
                sss_get_line_number(file, start),
                sss_get_line_column(file, start),
                user_fmt,
                info_nocolor)));
        gcc_assign(use_color, NULL, fmt_var, gcc_str(
            env->ctx, heap_strf("\x1b[31;1;7m%s:%ld.%ld: %s\x1b[m\n\n%s",
                file->relative_filename,
                sss_get_line_number(file, start),
                sss_get_line_column(file, start),
                user_fmt,
                info)));
    } else {
        gcc_assign(use_color, NULL, fmt_var, gcc_str(env->ctx, heap_strf("\x1b[31;1;7m%s\x1b[m\n\n%s", user_fmt, info)));
        gcc_assign(no_color, NULL, fmt_var, gcc_str(env->ctx, heap_strf("%s\n\n%s", user_fmt, info_nocolor)));
    }
    gcc_jump(use_color, NULL, carry_on);
    gcc_jump(no_color, NULL, carry_on);
    *block = carry_on;

    gcc_rvalue_t *fmt_val = gcc_rval(fmt_var);

    auto args = EMPTY_ARRAY(gcc_rvalue_t*);
    append(args, fmt_val);

    va_list ap;
    va_start(ap, user_fmt);

    for (const char *p = user_fmt; *p; p++) {
        if (*p != '%') continue;
        switch (*(++p)) {
        case '#': {
            assert(*(++p) == 's');
            sss_type_t *t = va_arg(ap, sss_type_t*);
            gcc_rvalue_t *rval = va_arg(ap, gcc_rvalue_t*);

            // Insert strings directly:
            if (type_eq(t, Type(ArrayType, .item_type=Type(CharType)))) {
                gcc_func_t *to_c_str = get_from_namespace(env, t, "c_string")->func;
                append(args, gcc_callx(env->ctx, NULL, to_c_str, rval));
                continue;
            }

            // Do table_t rec = {0}; def = 0; rec->default = &def; print(obj, &rec)
            sss_type_t *cycle_checker_t = Type(TableType, .key_type=Type(PointerType, .pointed=Type(MemoryType)), .value_type=Type(IntType, .bits=64));
            gcc_type_t *hashmap_gcc_t = sss_type_to_gcc(env, cycle_checker_t);
            gcc_lvalue_t *cycle_checker = gcc_local(func, NULL, hashmap_gcc_t, "_rec");
            gcc_assign(*block, NULL, cycle_checker, gcc_struct_constructor(env->ctx, NULL, hashmap_gcc_t, 0, NULL, NULL));
            gcc_lvalue_t *next_index = gcc_local(func, NULL, gcc_type(env->ctx, INT64), "_index");
            gcc_assign(*block, NULL, next_index, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
            gcc_assign(*block, NULL, gcc_lvalue_access_field(
                    cycle_checker, NULL, gcc_get_field(gcc_type_if_struct(hashmap_gcc_t), TABLE_DEFAULT_FIELD)),
                gcc_lvalue_address(next_index, NULL));

            gcc_type_t *void_star = gcc_type(env->ctx, VOID_PTR);
            gcc_func_t *cord_fn = get_cord_func(env, t);
            gcc_rvalue_t *cord_result = gcc_callx(
                env->ctx, NULL, cord_fn, rval,
                gcc_cast(env->ctx, NULL, gcc_lvalue_address(cycle_checker, NULL), void_star),
                gcc_rvalue_bool(env->ctx, false));
            gcc_lvalue_t *str_var = gcc_local(func, NULL, gcc_type(env->ctx, STRING), "_str");
            gcc_assign(*block, NULL, str_var, gcc_callx(env->ctx, NULL, get_function(env, "CORD_to_char_star"), cord_result));
            append(args, gcc_rval(str_var));
            break;
        }
        case 's': {
            const char *str = va_arg(ap, const char*);
            append(args, gcc_str(env->ctx, str));
            break;
        }
        default: compiler_err(env, NULL, "String format option not supported: %c", p[-1]);
        }
    }
    va_end(ap);

    gcc_rvalue_t *failure = gcc_jit_context_new_call(env->ctx, NULL, fail, LENGTH(args), args[0]);
    gcc_eval(*block, NULL, failure);
    gcc_jump(*block, NULL, *block);
    *block = NULL;
}

gcc_rvalue_t *pointer_offset(env_t *env, gcc_type_t *ptr_type, gcc_rvalue_t *ptr, gcc_rvalue_t *offset)
{
    // Equivalent to: (t*)&(((char*)ptr)[offset])
    return gcc_cast(
        env->ctx, NULL,
        gcc_lvalue_address(gcc_array_access(env->ctx, NULL, gcc_cast(env->ctx, NULL, ptr, gcc_type(env->ctx, STRING)), offset), NULL),
        ptr_type);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
