// Logic for defining per-type comparison functions
#include <assert.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <gc/cord.h>
#include <limits.h>
#include <stdint.h>

#include "../ast.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../libsss/hashmap.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

// Helper function to make value comparison return an int that is one of [-1,0,1]
gcc_rvalue_t *compare_values(env_t *env, sss_type_t *t, gcc_rvalue_t *a, gcc_rvalue_t *b)
{
    // (int)((a > b) - (a < b))
    if (is_numeric(t) || t->tag == PointerType || t->tag == CharType || t->tag == CStringCharType || t->tag == BoolType || t->tag == TypeType) {
        gcc_type_t *int_t = gcc_type(env->ctx, INT);
        return gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, int_t,
                             gcc_cast(env->ctx, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, a, b), int_t),
                             gcc_cast(env->ctx, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, a, b), int_t));
    } else {
        gcc_func_t *cmp_fn = get_compare_func(env, t);
        return gcc_callx(env->ctx, NULL, cmp_fn, a, b);
    }
}

// Get a comparison function: -1 means lhs < rhs; 0 means lhs == rhs; 1 means lhs > rhs
gcc_func_t *get_compare_func(env_t *env, sss_type_t *t)
{
    while (t->tag == VariantType) t = Match(t, VariantType)->variant_of;

    // Memoize:
    binding_t *b = get_from_namespace(env, t, "__compare");
    if (b) return b->func;

    sss_type_t *void_ptr_t = Type(PointerType, .pointed=Type(MemoryType), .is_optional=true);
    if (t->tag == PointerType && !type_eq(t, void_ptr_t)) {
        gcc_func_t *func = get_compare_func(env, void_ptr_t);
        hset(get_namespace(env, t), "__compare", get_from_namespace(env, void_ptr_t, "__compare"));
        return func;
    }

    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_type_t *int_t = gcc_type(env->ctx, INT);

    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("lhs")),
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("rhs")),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, int_t, fresh("compare"), 2, params, 0);
    sss_type_t *fn_t = Type(FunctionType, .arg_types=LIST(sss_type_t*, t, t), .arg_names=LIST(const char*, "lhs", "rhs"),
                           .arg_defaults=NULL, .ret=Type(IntType, .bits=32));
    hset(get_namespace(env, t), "__compare",
         new(binding_t, .func=func, .rval=gcc_get_func_address(func, NULL), .type=fn_t));

    gcc_block_t *block = gcc_new_block(func, fresh("compare"));
    gcc_comment(block, NULL, CORD_to_char_star(CORD_cat("compare(a,b) for type: ", type_to_string(t))));
    gcc_rvalue_t *lhs = gcc_param_as_rvalue(params[0]);
    gcc_rvalue_t *rhs = gcc_param_as_rvalue(params[1]);

    switch (t->tag) {
    case StructType: {
        auto struct_ = Match(t, StructType);
        gcc_struct_t *gcc_struct = gcc_type_if_struct(sss_type_to_gcc(env, t));
        gcc_block_t *done = gcc_new_block(func, fresh("done"));
        gcc_lvalue_t *cmp = gcc_local(func, NULL, gcc_type(env->ctx, INT), "_cmp");
        gcc_rvalue_t *zero = gcc_zero(env->ctx, gcc_type(env->ctx, INT));
        gcc_assign(block, NULL, cmp, zero);
        for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
            gcc_block_t *next_field = gcc_new_block(func, fresh("next_field"));
            sss_type_t *field_t = ith(struct_->field_types, i);

            gcc_rvalue_t *lhs_field = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(gcc_struct, i));
            gcc_rvalue_t *rhs_field = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(gcc_struct, i));
            gcc_assign(block, NULL, cmp, compare_values(env, field_t, lhs_field, rhs_field));
            gcc_jump_condition(block, NULL,
                               gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, gcc_rval(cmp), zero),
                               next_field, done);

            block = next_field;
        }
        gcc_jump(block, NULL, done);
        gcc_return(done, NULL, gcc_rval(cmp));
        break;
    }
    case TaggedUnionType: {
        // Compare tags, return difference if any
        // Otherwise, switch to a comparison of the union member
        gcc_struct_t *tagged_struct = gcc_type_if_struct(gcc_t);

        auto tagged = Match(t, TaggedUnionType);
        gcc_type_t *tag_gcc_t = get_tag_type(env, t);
        gcc_type_t *union_gcc_t = get_union_type(env, t);
        gcc_field_t *data_field = gcc_get_field(tagged_struct, 1);
        NEW_LIST(gcc_case_t*, cases);
        for (int64_t i = 0, len = length(tagged->members); i < len; i++) {
            auto member = ith(tagged->members, i);
            if (!member.type) continue;
            gcc_block_t *tag_block = gcc_new_block(func, fresh(member.name));
            gcc_rvalue_t *lhs_data = gcc_rvalue_access_field(lhs, NULL, data_field);
            gcc_rvalue_t *rhs_data = gcc_rvalue_access_field(rhs, NULL, data_field);
            gcc_field_t *union_field = gcc_get_union_field(union_gcc_t, i);
            gcc_rvalue_t *lhs_field = gcc_rvalue_access_field(lhs_data, NULL, union_field);
            gcc_rvalue_t *rhs_field = gcc_rvalue_access_field(rhs_data, NULL, union_field);
            gcc_return(tag_block, NULL, compare_values(env, member.type, lhs_field, rhs_field));

            gcc_rvalue_t *rval = gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value);
            gcc_case_t *case_ = gcc_new_case(env->ctx, rval, rval, tag_block);
            APPEND(cases, case_);
        }

        gcc_field_t *tag_field = gcc_get_field(tagged_struct, 0);
        gcc_rvalue_t *lhs_tag = gcc_rvalue_access_field(lhs, NULL, tag_field);
        gcc_rvalue_t *rhs_tag = gcc_rvalue_access_field(rhs, NULL, tag_field);

        if (length(cases) == 0) {
            gcc_return(block, NULL, compare_values(env, INT_TYPE, lhs_tag, rhs_tag));
            break;
        }

        gcc_block_t *tags_equal = gcc_new_block(func, fresh("tags_equal")),
                    *tags_differ = gcc_new_block(func, fresh("tags_differ"));
        gcc_jump_condition(block, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, lhs_tag, rhs_tag),
                           tags_equal, tags_differ);

        // tags_differ:
        gcc_return(tags_differ, NULL, compare_values(env, INT_TYPE, lhs_tag, rhs_tag));

        // tags_equal:
        gcc_switch(tags_equal, NULL, lhs_tag, tags_differ, length(cases), cases[0]);
        break;
    }
    case ArrayType: {
        // Logic is roughly as follows:
        // if (lhs.items != rhs.items || lhs.stride != rhs.stride) {
        //   for (i=0; i < lhs.len && i < rhs.len; i++) {
        //     int c = cmp(lhs.items[i*lhs.stride], rhs.items[i*rhs.stride])
        //     if (c != 0) return c;
        //   }
        // }
        // return lhs.len - rhs.len

        gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
        gcc_rvalue_t *lhs_data = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, ARRAY_DATA_FIELD)),
                     *rhs_data = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, ARRAY_DATA_FIELD));
        gcc_rvalue_t *lhs_stride = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, ARRAY_STRIDE_FIELD)),
                     *rhs_stride = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, ARRAY_STRIDE_FIELD));

        gcc_block_t *loop_condition = gcc_new_block(func, fresh("loop_condition")),
                    *loop_body = gcc_new_block(func, fresh("loop_body")),
                    *loop_end = gcc_new_block(func, fresh("loop_end"));

        gcc_type_t *int32 = gcc_type(env->ctx, INT32);
        gcc_lvalue_t *index_var = gcc_local(func, NULL, int32, "_i");
        gcc_rvalue_t *index_rval = gcc_rval(index_var);
        gcc_assign(block, NULL, index_var, gcc_zero(env->ctx, int32));

        gcc_jump_condition(block, NULL,
                           gcc_binary_op(env->ctx, NULL, GCC_BINOP_LOGICAL_AND, gcc_type(env->ctx, BOOL),
                                         gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, lhs_data, rhs_data),
                                         gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, lhs_stride, rhs_stride)),
                           loop_end, loop_condition);

        // loop_condition:
        gcc_rvalue_t *lhs_len = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, ARRAY_LENGTH_FIELD)),
                     *rhs_len = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, ARRAY_LENGTH_FIELD));
        gcc_rvalue_t *lhs_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GE, index_rval, lhs_len);
        gcc_rvalue_t *rhs_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GE, index_rval, rhs_len);
        gcc_rvalue_t *either_done = gcc_binary_op(env->ctx, NULL, GCC_BINOP_LOGICAL_OR, gcc_type(env->ctx, BOOL),
                                                  lhs_done, rhs_done);

        gcc_jump_condition(loop_condition, NULL, either_done, loop_end, loop_body);

        // loop_body:
        sss_type_t *item_t = Match(t, ArrayType)->item_type;
        gcc_type_t *gcc_item_ptr_t = gcc_get_ptr_type(sss_type_to_gcc(env, item_t));

        gcc_func_t *cmp_fn = get_compare_func(env, item_t);
        assert(cmp_fn);
        gcc_rvalue_t *lhs_offset = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, int32, index_rval, gcc_cast(env->ctx, NULL, lhs_stride, int32));
        gcc_rvalue_t *rhs_offset = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, int32, index_rval, gcc_cast(env->ctx, NULL, rhs_stride, int32));
        gcc_rvalue_t *difference = gcc_callx(env->ctx, NULL, cmp_fn,
            // lhs.data[i*lhs.stride], rhs.data[i*rhs.stride]
            gcc_rval(gcc_rvalue_dereference(pointer_offset(env, gcc_item_ptr_t, lhs_data, lhs_offset), NULL)),
            gcc_rval(gcc_rvalue_dereference(pointer_offset(env, gcc_item_ptr_t, rhs_data, rhs_offset), NULL)));

        gcc_block_t *early_return = gcc_new_block(func, fresh("return_early")),
                    *keep_going = gcc_new_block(func, fresh("keep_going"));
        gcc_jump_condition(loop_body, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, difference, gcc_zero(env->ctx, int_t)),
                           early_return, keep_going);

        // early_return:
        gcc_return(early_return, NULL, difference);

        // keep_going:
        gcc_update(keep_going, NULL, index_var, GCC_BINOP_PLUS, gcc_one(env->ctx, int32));
        gcc_jump(keep_going, NULL, loop_condition);

        // loop_end:
        gcc_return(loop_end, NULL, gcc_cast(env->ctx, NULL, gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, int32, lhs_len, rhs_len), int_t));
        break;
    }
    case TableType: {
        gcc_func_t *compare_fn = get_function(env, "sss_hashmap_compare");
        sss_type_t *entry_t = table_entry_type(t);
        gcc_func_t *key_hash = get_hash_func(env, Match(t, TableType)->key_type);
        gcc_func_t *key_compare = get_indirect_compare_func(env, Match(t, TableType)->key_type);
        gcc_func_t *value_compare = get_indirect_compare_func(env, Match(t, TableType)->value_type);
        size_t key_size = gcc_sizeof(env, Match(t, TableType)->key_type);
        size_t value_align = gcc_alignof(env, Match(t, TableType)->value_type);
        size_t value_offset = key_size;
        if (value_offset % value_align != 0) value_offset = (value_offset - (value_offset % value_align) + value_align);
        gcc_return(block, NULL, gcc_callx(env->ctx, NULL, compare_fn,
                                          gcc_lvalue_address(gcc_param_as_lvalue(params[0]), NULL),
                                          gcc_lvalue_address(gcc_param_as_lvalue(params[1]), NULL),
                                          gcc_get_func_address(key_hash, NULL),
                                          gcc_get_func_address(key_compare, NULL),
                                          gcc_get_func_address(value_compare, NULL),
                                          gcc_rvalue_size(env->ctx, gcc_sizeof(env, entry_t)),
                                          gcc_rvalue_size(env->ctx, value_offset)));
        break;
    }
    default: {
        if (is_numeric(t) || t->tag == PointerType || t->tag == CharType || t->tag == CStringCharType || t->tag == BoolType || t->tag == TypeType) {
            gcc_return(block, NULL, compare_values(env, t, lhs, rhs));
            break;
        }
        fprintf(stderr, "\x1b[31;1mcompare(%s,%s) function is not implemented!\n", type_to_string(t), type_to_string(t));
        exit(1);
    }
    }
    return func;
}

// Get a comparison function for pointers to data (instead of the data itself)
// This is just a shim around the real comparison function
gcc_func_t *get_indirect_compare_func(env_t *env, sss_type_t *t)
{
    while (t->tag == VariantType) t = Match(t, VariantType)->variant_of;

    // Memoize:
    binding_t *b = get_from_namespace(env, t, "__compare_indirect");
    if (b) return b->func;

    sss_type_t *void_ptr_t = Type(PointerType, .pointed=Type(MemoryType), .is_optional=true);
    if (t->tag == PointerType && !type_eq(t, void_ptr_t)) {
        gcc_func_t *func = get_indirect_compare_func(env, void_ptr_t);
        hset(get_namespace(env, t), "__compare_indirect",
             get_from_namespace(env, void_ptr_t, "__compare_indirect"));
        return func;
    }

    gcc_type_t *gcc_t = gcc_get_ptr_type(sss_type_to_gcc(env, t));
    gcc_type_t *int_t = gcc_type(env->ctx, INT);

    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("lhs")),
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("rhs")),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, int_t, fresh("compare_indirect"), 2, params, 0);
    sss_type_t *fn_t = Type(FunctionType, .arg_types=LIST(sss_type_t*, t, t), .arg_names=LIST(const char*, "lhs", "rhs"),
                           .arg_defaults=NULL, .ret=Type(IntType, .bits=32));
    hset(get_namespace(env, t), "__compare_indirect",
         new(binding_t, .func=func, .rval=gcc_get_func_address(func, NULL), .type=fn_t));

    gcc_block_t *block = gcc_new_block(func, fresh("compare_indirect"));
    gcc_comment(block, NULL, CORD_to_char_star(CORD_cat("compare_indirect(a,b) for type: ", type_to_string(t))));
    gcc_rvalue_t *lhs = gcc_rval(gcc_rvalue_dereference(gcc_param_as_rvalue(params[0]), NULL));
    gcc_rvalue_t *rhs = gcc_rval(gcc_rvalue_dereference(gcc_param_as_rvalue(params[1]), NULL));
    gcc_func_t *compare_func = get_compare_func(env, t);
    gcc_return(block, NULL, gcc_callx(env->ctx, NULL, compare_func, lhs, rhs));
    return func;
}
