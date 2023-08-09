// Define per-type hash functions
#include <assert.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <limits.h>
#include <stdint.h>

#include "../ast.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../libsss/hashmap.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

gcc_func_t *get_hash_func(env_t *env, sss_type_t *t)
{
    // Return a hash function for a given type.

    while (t->tag == VariantType) t = Match(t, VariantType)->variant_of;

    // Memoize:
    binding_t *b = get_from_namespace(env, t, "__hash");
    if (b) return b->func;

    sss_type_t *void_ptr_t = Type(PointerType, .pointed=Type(MemoryType), .is_optional=true);
    if (t->tag == PointerType && !type_eq(t, void_ptr_t)) {
        gcc_func_t *func = get_hash_func(env, void_ptr_t);
        hset(get_namespace(env, t), "__hash", get_from_namespace(env, void_ptr_t, "__hash"));
        return func;
    }

    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_type_t *u32 = gcc_type(env->ctx, UINT32);

    gcc_param_t *params[] = {gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_t), fresh("obj"))};
    const char* sym_name = fresh("hash");
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, u32, sym_name, 1, params, 0);
    sss_type_t *fn_t = Type(FunctionType, .arg_types=LIST(sss_type_t*, Type(PointerType, .pointed=t)),
                           .arg_names=LIST(const char*, "obj"), .arg_defaults=NULL, .ret=Type(IntType, .bits=32, .is_unsigned=true));
    hset(get_namespace(env, t), "__hash",
         new(binding_t, .func=func, .rval=gcc_get_func_address(func, NULL), .type=fn_t));
    gcc_block_t *block = gcc_new_block(func, fresh("hash"));
    gcc_comment(block, NULL, heap_strf("Implementation of hash(%s)", type_to_string(t)));

    gcc_func_t *halfsiphash = get_function(env, "halfsiphash");

    gcc_type_t *t_void_ptr = gcc_get_type(env->ctx, GCC_T_VOID_PTR);
    gcc_rvalue_t *k = gcc_cast(env->ctx, NULL, gcc_str(env->ctx, "My secret key!!!"), t_void_ptr);

    gcc_rvalue_t *obj_ptr = gcc_param_as_rvalue(params[0]);
    gcc_lvalue_t *hashval = gcc_local(func, NULL, gcc_type(env->ctx, UINT32), "_hashval");

    switch (t->tag) {
    case StructType: {
        auto struct_type = Match(t, StructType);

        foreach (struct_type->field_types, ftype, _) {
            if ((*ftype)->tag == ArrayType || (*ftype)->tag == TableType)
                goto need_to_handle_arrays_or_tables;
        }
        goto memory_hash;

      need_to_handle_arrays_or_tables:;
        gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
        NEW_LIST(sss_type_t*, hash_members);
        NEW_LIST(gcc_rvalue_t*, values);
        for (int64_t i = 0; i < length(struct_type->field_types); i++) {
            sss_type_t *ftype = ith(struct_type->field_types, i);
            if (ftype->tag == ArrayType || ftype->tag == TableType) {
                append(hash_members, Type(IntType, .bits=32, .is_unsigned=true));
                gcc_func_t *item_hash = get_hash_func(env, ftype);
                gcc_rvalue_t *member = gcc_lvalue_address(gcc_rvalue_dereference_field(obj_ptr, NULL, gcc_get_field(struct_t, i)), NULL);
                append(values, gcc_callx(env->ctx, NULL, item_hash, member));
            } else {
                append(hash_members, ftype);
                gcc_rvalue_t *member = gcc_rval(gcc_rvalue_dereference_field(obj_ptr, NULL, gcc_get_field(struct_t, i)));
                append(values, member);
            }
        }

        sss_type_t *safe_tup_t = Type(StructType, .field_names=struct_type->field_names, .field_types=hash_members);
        gcc_type_t *safe_tup_gcc_t = sss_type_to_gcc(env, safe_tup_t);
        gcc_struct_t *safe_tup_struct = gcc_type_if_struct(safe_tup_gcc_t);
        gcc_lvalue_t *safe_tup = gcc_local(func, NULL, safe_tup_gcc_t, "_to_hash");
        gcc_assign(block, NULL, safe_tup, gcc_struct_constructor(env->ctx, NULL, safe_tup_gcc_t, 0, NULL, NULL));

        for (int64_t i = 0; i < length(values); i++)
            gcc_assign(block, NULL, gcc_lvalue_access_field(safe_tup, NULL, gcc_get_field(safe_tup_struct, i)),
                       ith(values, i));

        t = safe_tup_t;
        obj_ptr = gcc_lvalue_address(safe_tup, NULL);
        goto memory_hash;
    }
    case TaggedUnionType: {
        auto tagged = Match(t, TaggedUnionType);
        bool any_values = false;
        for (int64_t i = 0, len = length(tagged->members); i < len; i++)
            any_values = any_values || ith(tagged->members, i).type != NULL;
        if (!any_values)
            goto memory_hash;

        // Step 1: compute hash of member value (if any) using a switch statement
        // Step 2: create a struct with {tag, hash(value) or 0}
        // Step 3: return a hash of that struct
        gcc_struct_t *tagged_struct = gcc_type_if_struct(gcc_t);
        gcc_type_t *tag_gcc_t = get_tag_type(env, t);
        gcc_type_t *union_gcc_t = get_union_type(env, t);
        gcc_field_t *tag_field = gcc_get_field(tagged_struct, 0);
        gcc_field_t *data_field = gcc_get_field(tagged_struct, 1);

        gcc_field_t *info_fields[] = {
            gcc_new_field(env->ctx, NULL, tag_gcc_t, "tag"),
            gcc_new_field(env->ctx, NULL, u32, "value_hash"),
        };
        gcc_struct_t *info_struct = gcc_new_struct_type(env->ctx, NULL, "TaggedUnionHashInfo", 2, info_fields);
        gcc_type_t *info_t = gcc_struct_as_type(info_struct);
        gcc_lvalue_t *info = gcc_local(func, NULL, info_t, "hash_info");
        gcc_rvalue_t *tag = gcc_rval(gcc_rvalue_dereference_field(obj_ptr, NULL, tag_field));
        gcc_assign(block, NULL, info, gcc_struct_constructor(env->ctx, NULL, info_t, 1, &info_fields[0], &tag));

        NEW_LIST(gcc_case_t*, cases);
        gcc_block_t *done = gcc_new_block(func, fresh("done"));
        for (int64_t i = 0, len = length(tagged->members); i < len; i++) {
            auto member = ith(tagged->members, i);
            if (!member.type) continue;
            gcc_block_t *tag_block = gcc_new_block(func, fresh(member.name));
            gcc_lvalue_t *data = gcc_rvalue_dereference_field(obj_ptr, NULL, data_field);
            gcc_field_t *union_field = gcc_get_union_field(union_gcc_t, i);
            gcc_lvalue_t *member_val = gcc_lvalue_access_field(data, NULL, union_field);

            gcc_func_t *hash_func = get_hash_func(env, member.type);
            gcc_rvalue_t *value_hash = gcc_callx(env->ctx, NULL, hash_func, gcc_lvalue_address(member_val, NULL));
            gcc_assign(tag_block, NULL, gcc_lvalue_access_field(info, NULL, info_fields[1]), value_hash);
            gcc_jump(tag_block, NULL, done);

            gcc_rvalue_t *rval = gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value);
            gcc_case_t *case_ = gcc_new_case(env->ctx, rval, rval, tag_block);
            APPEND(cases, case_);
        }
        gcc_switch(block, NULL, tag, done, length(cases), cases[0]);

        block = done;

        size_t info_size = tagged->tag_bits == 64 ? 16 : 8;
        gcc_rvalue_t *inlen = gcc_rvalue_size(env->ctx, info_size);
        gcc_rvalue_t *outlen = gcc_rvalue_size(env->ctx, sizeof(uint32_t));
        gcc_rvalue_t *call = gcc_callx(
            env->ctx, NULL, halfsiphash, gcc_lvalue_address(info, NULL), inlen, k,
            gcc_cast(env->ctx, NULL, gcc_lvalue_address(hashval, NULL), t_void_ptr), outlen);
        gcc_eval(block, NULL, call);
        break;

    }
    case PointerType: case IntType: case NumType: case CharType: case CStringCharType: case BoolType:
    case RangeType: case FunctionType: case TypeType: {
      memory_hash:;
        gcc_rvalue_t *inlen = gcc_rvalue_size(env->ctx, gcc_sizeof(env, t));
        gcc_rvalue_t *outlen = gcc_rvalue_size(env->ctx, sizeof(uint32_t));
        gcc_rvalue_t *call = gcc_callx(
            env->ctx, NULL, halfsiphash, obj_ptr, inlen, k,
            gcc_cast(env->ctx, NULL, gcc_lvalue_address(hashval, NULL), t_void_ptr), outlen);
        gcc_eval(block, NULL, call);
        break;
    }
    case TableType: {
        gcc_func_t *hash_fn = get_function(env, "sss_hashmap_hash");
        sss_type_t *entry_t = table_entry_type(t);
        gcc_func_t *entry_hash = get_hash_func(env, entry_t);
        gcc_assign(block, NULL, hashval, gcc_callx(env->ctx, NULL, hash_fn, obj_ptr,
                                                   gcc_get_func_address(entry_hash, NULL),
                                                   gcc_rvalue_size(env->ctx, gcc_sizeof(env, entry_t))));
        break;
    }
    case ArrayType: {
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        gcc_rvalue_t *array = obj_ptr;
        // If necessary, flatten first:
        flatten_arrays(env, &block, t, array);
        gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
        gcc_rvalue_t *data_field = gcc_rval(gcc_rvalue_dereference_field(array, NULL, gcc_get_field(struct_t, ARRAY_DATA_FIELD)));
        gcc_rvalue_t *length_field = gcc_rval(gcc_rvalue_dereference_field(array, NULL, gcc_get_field(struct_t, ARRAY_LENGTH_FIELD)));
        sss_type_t *item_type = Match(t, ArrayType)->item_type;
        gcc_rvalue_t *inlen = gcc_rvalue_size(env->ctx, gcc_sizeof(env, item_type));
        gcc_type_t *t_size = gcc_type(env->ctx, SIZE);
        inlen = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, t_size, inlen, gcc_cast(env->ctx, NULL, length_field, t_size));
        gcc_rvalue_t *outlen = gcc_rvalue_size(env->ctx, sizeof(uint32_t));
        gcc_rvalue_t *call = gcc_callx(
            env->ctx, NULL, halfsiphash,
            gcc_cast(env->ctx, NULL, data_field, t_void_ptr), inlen, k,
            gcc_cast(env->ctx, NULL, gcc_lvalue_address(hashval, NULL), t_void_ptr), outlen);
        gcc_eval(block, NULL, call);
        break;
    }
    default:
        compiler_err(env, NULL, "Hash functions aren't yet implemented for %T", t);
    }

    gcc_return(block, NULL, gcc_rval(hashval));
    return func;
}
