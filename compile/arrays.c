// Compilation logic for Arrays in this file
#include <assert.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>

#include "../ast.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

typedef struct {
    sss_type_t *array_type;
    gcc_rvalue_t *array_ptr;
    bool dynamic_size;
} array_insert_info_t;

static inline sss_type_t *get_item_type(sss_type_t *t) {
    for (;;) {
        if (t->tag == PointerType) t = Match(t, PointerType)->pointed; 
        else if (t->tag == VariantType) t = Match(t, VariantType)->variant_of; 
        else break;
    }
    return t->tag == ArrayType ? Match(t, ArrayType)->item_type : NULL;
}

static void add_array_item(env_t *env, gcc_block_t **block, ast_t *item, array_insert_info_t *info)
{
    sss_type_t *item_type = get_item_type(info->array_type);
    if (item_type->tag == VoidType)
        compiler_err(env, item, "Void values can't be put inside an array");

    sss_type_t *t = get_type(env, item); // item type
    if (t->tag == GeneratorType || t->tag == VoidType || t->tag == AbortType) {
        (void)compile_expr(env, block, item);
        return;
    }

    // This comes first, because the item may short-circuit
    gcc_rvalue_t *item_val = compile_expr(env, block, item);
    if (!*block) return;

    gcc_type_t *gcc_t = sss_type_to_gcc(env, info->array_type);
    gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
    gcc_lvalue_t *array = gcc_rvalue_dereference(info->array_ptr, NULL);
    gcc_lvalue_t *data_field = gcc_lvalue_access_field(array, NULL, gcc_get_field(struct_t, ARRAY_DATA_FIELD));
    gcc_lvalue_t *length_field = gcc_lvalue_access_field(array, NULL, gcc_get_field(struct_t, ARRAY_LENGTH_FIELD));

    // array.length += 1
    gcc_type_t *i32 = gcc_type(env->ctx, INT32);
    gcc_update(*block, NULL, length_field, GCC_BINOP_PLUS, gcc_one(env->ctx, i32));

    if (info->dynamic_size) {
        // array.items = GC_realloc(array.items, item_size*array.length)
        gcc_type_t *gcc_size_t = gcc_type(env->ctx, SIZE);
        gcc_rvalue_t *new_size = gcc_binary_op(
            env->ctx, NULL, GCC_BINOP_MULT, gcc_size_t, gcc_cast(env->ctx, NULL, gcc_rval(length_field), gcc_size_t),
            gcc_rvalue_from_long(env->ctx, gcc_size_t, (long)gcc_sizeof(env, item_type)));
        gcc_func_t *gc_realloc_func = get_function(env, "GC_realloc");
        gcc_rvalue_t *new_data = gcc_callx(env->ctx, NULL, gc_realloc_func, 
                                           gcc_rval(data_field), new_size);
        gcc_assign(*block, NULL, data_field,
                   gcc_cast(env->ctx, NULL, new_data, gcc_get_ptr_type(sss_type_to_gcc(env, item_type))));
    }

    // array.items[array.length-1] = item
    gcc_rvalue_t *index = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, i32, gcc_rval(length_field), gcc_one(env->ctx, i32));
    // NOTE: This assumes stride == item_size
    gcc_lvalue_t *item_home = gcc_array_access(env->ctx, NULL, gcc_rval(data_field), index);
    if (!type_eq(t, item_type))
        if (!promote(env, t, &item_val, item_type))
            compiler_err(env, item, "I can't convert this type (%T) to %T", t, item_type);

    gcc_assign(*block, NULL, item_home, item_val);
}

gcc_rvalue_t *array_contains(env_t *env, gcc_block_t **block, ast_t *array, ast_t *member)
{
    // TODO: support subsets like ("def" in "abcdefghi")
    sss_type_t *t = get_type(env, array);

    gcc_rvalue_t *array_val = compile_expr(env, block, array);
    while (t->tag == PointerType) {
        auto ptr = Match(t, PointerType);
        if (ptr->is_optional)
            compiler_err(env, array, "This is an optional pointer, which can't be safely dereferenced.");
        array_val = gcc_rval(gcc_rvalue_dereference(array_val, NULL));
        t = ptr->pointed;
    }

    sss_type_t *item_type = get_type(env, member);
    if (!type_is_a(item_type, get_item_type(t)))
        compiler_err(env, member, "This value has type %T, but you're checking an array of type %T for membership", item_type, t);

    gcc_loc_t *loc = ast_loc(env, member);
    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *member_var = gcc_local(func, loc, sss_type_to_gcc(env, item_type), "_member");
    gcc_assign(*block, loc, member_var, compile_expr(env, block, member));
    gcc_lvalue_t *contains_var = gcc_local(func, loc, gcc_type(env->ctx, BOOL), "_contains");
    gcc_assign(*block, loc, contains_var, gcc_rvalue_bool(env->ctx, 0));

    // i = 1
    gcc_lvalue_t *array_var = gcc_local(func, loc, sss_type_to_gcc(env, t), "_array");
    gcc_assign(*block, loc, array_var, array_val);
    gcc_lvalue_t *i = gcc_local(func, loc, gcc_type(env->ctx, INT64), "_i");
    gcc_assign(*block, loc, i, gcc_zero(env->ctx, gcc_type(env->ctx, INT64)));
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
    gcc_rvalue_t *items = gcc_rvalue_access_field(gcc_rval(array_var), loc, gcc_get_field(array_struct, ARRAY_DATA_FIELD));
    gcc_rvalue_t *len = gcc_rvalue_access_field(gcc_rval(array_var), loc, gcc_get_field(array_struct, ARRAY_LENGTH_FIELD));
    gcc_rvalue_t *len64 = gcc_cast(env->ctx, loc, len, gcc_type(env->ctx, INT64));
    gcc_rvalue_t *stride = gcc_rvalue_access_field(gcc_rval(array_var), loc, gcc_get_field(array_struct, ARRAY_STRIDE_FIELD));

    gcc_block_t *next = gcc_new_block(func, fresh("next_item")),
                *end = gcc_new_block(func, fresh("done"));

    // item_ptr = array.items
    gcc_type_t *gcc_item_t = sss_type_to_gcc(env, item_type);
    gcc_lvalue_t *item_ptr = gcc_local(func, loc, gcc_get_ptr_type(gcc_item_t), "_item_ptr");
    gcc_assign(*block, loc, item_ptr, items);

    // if (i < len) goto next;
    gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_LT, gcc_rval(i), len64), next, end);

    *block = next;
    // item = *item_ptr
    gcc_rvalue_t *item = gcc_rval(gcc_jit_rvalue_dereference(gcc_rval(item_ptr), loc));
    // check
    gcc_func_t *compare_func = get_compare_func(env, item_type);
    gcc_assign(next, loc, contains_var,
               gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, gcc_rvalue_int(env->ctx, 0),
                              gcc_callx(env->ctx, loc, compare_func, item, gcc_rval(member_var))));
    
    // i += 1
    assert(i);
    gcc_update(next, NULL, i, GCC_BINOP_PLUS, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
    // item_ptr = (void*)item_ptr + stride
    gcc_assign(next, NULL, item_ptr, pointer_offset(env, gcc_get_ptr_type(gcc_item_t), gcc_rval(item_ptr), stride));
    // if (i < len) goto add_comma;
    gcc_jump_condition(next, NULL, 
                       gcc_binary_op(env->ctx, loc, GCC_BINOP_LOGICAL_OR, gcc_type(env->ctx, BOOL),
                                     gcc_rval(contains_var),
                                     gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GE, gcc_rval(i), len64)),
                       end, next);

    *block = end;
    return gcc_rval(contains_var);
}

gcc_lvalue_t *array_capacity(env_t *env, gcc_rvalue_t *arr_ptr)
{
    sss_type_t *str_t = Type(ArrayType, .item_type=Type(CharType));
    gcc_type_t *str_gcc_t = sss_type_to_gcc(env, str_t);
    gcc_struct_t *array_struct = gcc_type_if_struct(str_gcc_t);
    return gcc_rvalue_dereference_field(gcc_cast(env->ctx, NULL, arr_ptr, gcc_get_ptr_type(str_gcc_t)), NULL,
                                        gcc_get_field(array_struct, ARRAY_CAPACITY_FIELD));
}

void mark_array_cow(env_t *env, gcc_block_t **block, gcc_rvalue_t *arr_ptr)
{
    gcc_lvalue_t *capacity = array_capacity(env, arr_ptr);
    gcc_assign(*block, NULL, capacity, gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, INT16), -1));
}

void check_cow(env_t *env, gcc_block_t **block, sss_type_t *arr_t, gcc_rvalue_t *arr)
{
    // Copy on write
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *needs_cow = gcc_new_block(func, "needs_cow"),
                *done = gcc_new_block(func, "done_cow");
    gcc_rvalue_t *should_cow = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(array_capacity(env, arr)), gcc_zero(env->ctx, gcc_type(env->ctx, INT16)));
    gcc_jump_condition(*block, NULL, should_cow, needs_cow, done);
    *block = needs_cow;
    // Copy array contents:
    gcc_func_t *cow_fn = get_function(env, "array_cow");
    gcc_eval(*block, NULL, gcc_callx(env->ctx, NULL, cow_fn,
                                     gcc_bitcast(env->ctx, NULL, arr, gcc_type(env->ctx, VOID_PTR)),
                                     gcc_rvalue_size(env->ctx, gcc_sizeof(env, get_item_type(arr_t))),
                                     gcc_rvalue_bool(env->ctx, has_heap_memory(arr_t))));
    gcc_jump(*block, NULL, done);
    *block = done;
}

gcc_rvalue_t *array_slice(env_t *env, gcc_block_t **block, ast_t *arr_ast, ast_t *index, access_type_e access)
{
    gcc_loc_t *loc = ast_loc(env, arr_ast);
    sss_type_t *arr_t = get_type(env, arr_ast);

    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *arr_var;
    if (access == ACCESS_WRITE) {
        arr_var = get_lvalue(env, block, arr_ast, true);
        check_cow(env, block, arr_t, gcc_lvalue_address(arr_var, loc));
    } else {
        gcc_rvalue_t *arr = compile_expr(env, block, arr_ast);
        while (arr_t->tag == PointerType) {
            auto ptr = Match(arr_t, PointerType);
            if (ptr->is_optional)
                compiler_err(env, arr_ast, "This is an optional pointer, which can't be safely dereferenced.");

            arr = gcc_rval(gcc_rvalue_dereference(arr, NULL));
            arr_t = ptr->pointed;
        }
        gcc_type_t *array_gcc_t = sss_type_to_gcc(env, arr_t);
        arr_var = gcc_local(func, loc, array_gcc_t, "_sliced");
        gcc_assign(*block, loc, arr_var, arr);
        mark_array_cow(env, block, gcc_lvalue_address(arr_var, loc));
    }
    while (arr_t->tag == VariantType)
        arr_t = Match(arr_t, VariantType)->variant_of;
    gcc_type_t *array_gcc_t = sss_type_to_gcc(env, arr_t);
    gcc_struct_t *gcc_array_struct = gcc_type_if_struct(array_gcc_t);
    gcc_rvalue_t *arr = gcc_rval(arr_var);

    // Specially optimized case for creating slices using range literals
    // This actually makes a noticeable performance difference
    if (index->tag == Range) {
        auto range = Match(index, Range);
        if (!range->step || (range->step->tag == Int && Match(range->step, Int)->i == 1)) {
            gcc_type_t *i32_t = gcc_type(env->ctx, INT32);
#define SUB(a,b) gcc_binary_op(env->ctx, loc, GCC_BINOP_MINUS, i32_t, a, b)
            gcc_rvalue_t *old_items = gcc_rvalue_access_field(arr, loc, gcc_get_field(gcc_array_struct, ARRAY_DATA_FIELD));
            gcc_rvalue_t *offset;
            if (range->first)
                offset = SUB(gcc_cast(env->ctx, loc, compile_expr(env, block, range->first), i32_t), gcc_one(env->ctx, i32_t));
            else
                offset = gcc_zero(env->ctx, i32_t);

            gcc_lvalue_t *offset_var = gcc_local(func, loc, i32_t, "_offset");
            gcc_assign(*block, loc, offset_var, offset);
            // Bit hack to branchlessly set offset to zero when it would otherwise be negative:
            // offset &= ~(offset >> 31)
            gcc_update(*block, loc, offset_var, GCC_BINOP_BITWISE_AND,
               gcc_unary_op(env->ctx, loc, GCC_UNOP_BITWISE_NEGATE, i32_t,
                   gcc_binary_op(env->ctx, loc, GCC_BINOP_RSHIFT, i32_t, offset, gcc_rvalue_int32(env->ctx, 31))));
            gcc_rvalue_t *old_stride = gcc_rvalue_access_field(arr, loc, gcc_get_field(gcc_array_struct, ARRAY_STRIDE_FIELD));
            offset = gcc_rval(offset_var);

            gcc_type_t *gcc_item_t = sss_type_to_gcc(env, get_item_type(arr_t));
            gcc_rvalue_t *items = pointer_offset(env, gcc_get_ptr_type(gcc_item_t), old_items,
                                                 gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, i32_t, offset, gcc_cast(env->ctx, loc, old_stride, i32_t)));

            gcc_lvalue_t *slice = gcc_local(func, loc, array_gcc_t, "_slice");
            // assign slice.items and slice.stride
            gcc_assign(*block, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, ARRAY_DATA_FIELD)), items);
            gcc_assign(*block, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, ARRAY_STRIDE_FIELD)), old_stride);

            // len = MIN(array_len, range.last)-first
            gcc_rvalue_t *array_len = gcc_rvalue_access_field(arr, loc, gcc_get_field(gcc_array_struct, ARRAY_LENGTH_FIELD));
            if (range->last) {
                gcc_block_t *array_shorter = gcc_new_block(func, "array_shorter"),
                            *range_shorter = gcc_new_block(func, "range_shorter"),
                            *len_assigned = gcc_new_block(func, "len_assigned");
                gcc_rvalue_t *range_len = gcc_cast(env->ctx, loc, compile_expr(env, block, range->last), i32_t);

                gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_LT, array_len, range_len), array_shorter, range_shorter);

                gcc_assign(array_shorter, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, ARRAY_LENGTH_FIELD)),
                           SUB(array_len, offset));
                gcc_jump(array_shorter, loc, len_assigned);

                gcc_assign(range_shorter, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, ARRAY_LENGTH_FIELD)),
                           SUB(range_len, offset));
                gcc_jump(range_shorter, loc, len_assigned);

                *block = len_assigned;
            } else {
                gcc_assign(*block, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, ARRAY_LENGTH_FIELD)),
                           SUB(array_len, offset));
            }
            gcc_assign(*block, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, ARRAY_CAPACITY_FIELD)),
                       gcc_rvalue_int16(env->ctx, access == ACCESS_READ ? -1 : 0));

            return gcc_rval(slice);
#undef SUB
        }
    }

    // If we're not in the optimized case, fall back to the C function:
    gcc_rvalue_t *index_val = compile_expr(env, block, index);
    gcc_type_t *str_gcc_t = sss_type_to_gcc(env, Type(ArrayType, .item_type=Type(CharType)));
    gcc_func_t *slice_fn = get_function(env, "range_slice");
    gcc_lvalue_t *slice_var = gcc_local(func, loc, array_gcc_t, "_slice");
    gcc_assign(*block, loc, slice_var, gcc_bitcast(
        env->ctx, loc,
        gcc_callx(
            env->ctx, loc, slice_fn,
            gcc_bitcast(env->ctx, loc, arr, str_gcc_t),
            index_val,
            gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), gcc_sizeof(env, get_item_type(arr_t)))),
        array_gcc_t));
    // Set COW field:
    gcc_assign(*block, loc, gcc_lvalue_access_field(slice_var, loc, gcc_get_field(gcc_array_struct, ARRAY_CAPACITY_FIELD)),
               gcc_rvalue_int16(env->ctx, access == ACCESS_READ ? -1 : 0));
    return gcc_rval(slice_var);
}

gcc_rvalue_t *array_field_slice(env_t *env, gcc_block_t **block, ast_t *ast, const char *field_name, access_type_e access)
{
    if (streq(field_name, "length"))
        return NULL;

    sss_type_t *array_t = get_type(env, ast);
    gcc_loc_t *loc = ast_loc(env, ast);
    if (array_t->tag == PointerType) {
        auto ptr = Match(array_t, PointerType);
        if (ptr->is_optional)
            compiler_err(env, ast, "This value can't be sliced, because it may or may not be null.");
        return array_field_slice(env, block, WrapAST(ast, Index, .indexed=ast), field_name, access);
    }

    sss_type_t *item_t = get_item_type(array_t);
    if (!item_t)
        return NULL;

    gcc_type_t *array_gcc_t = sss_type_to_gcc(env, array_t);
    gcc_struct_t *gcc_array_struct = gcc_type_if_struct(array_gcc_t);

    gcc_type_t *item_gcc_t = sss_type_to_gcc(env, item_t);
    gcc_struct_t *gcc_item_struct = gcc_type_if_struct(item_gcc_t);

    auto struct_type = Match(item_t, StructType);
    for (int64_t i = 0, len = length(struct_type->field_names); i < len; i++) {
        const char *struct_field_name = ith(struct_type->field_names, i);
        if (!struct_field_name) struct_field_name = heap_strf("_%ld", i+1);
        if (!streq(struct_field_name, field_name))  continue;

        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *arr_var;
        if (access == ACCESS_WRITE) {
            arr_var = get_lvalue(env, block, ast, true);
            if (access == ACCESS_WRITE)
                check_cow(env, block, array_t, gcc_lvalue_address(arr_var, loc));
        } else {
            arr_var = gcc_local(func, loc, array_gcc_t, "_sliced");
            gcc_assign(*block, loc, arr_var, compile_expr(env, block, ast));
            mark_array_cow(env, block, gcc_lvalue_address(arr_var, loc));
        }
        gcc_rvalue_t *arr_val = gcc_rval(arr_var);

        // items = &(array.items->field)
        gcc_field_t *items_field = gcc_get_field(gcc_array_struct, ARRAY_DATA_FIELD);
        gcc_rvalue_t *items = gcc_rvalue_access_field(arr_val, loc, items_field);
        gcc_field_t *struct_field = gcc_get_field(gcc_item_struct, (size_t)i);
        gcc_lvalue_t *field = gcc_rvalue_dereference_field(items, loc, struct_field);
        gcc_rvalue_t *field_addr = gcc_lvalue_address(field, loc);

        // length = array->length
        gcc_rvalue_t *len = gcc_rvalue_access_field(arr_val, loc, gcc_get_field(gcc_array_struct, ARRAY_LENGTH_FIELD));

        // stride = array->stride * sizeof(array->items[0])
        sss_type_t *field_type = ith(struct_type->field_types, i);
        gcc_rvalue_t *stride = gcc_rvalue_access_field(arr_val, loc, gcc_get_field(gcc_array_struct, ARRAY_STRIDE_FIELD));

        gcc_type_t *slice_gcc_t = sss_type_to_gcc(env, Type(ArrayType, .item_type=field_type));
        gcc_struct_t *slice_struct = gcc_type_if_struct(slice_gcc_t);
        gcc_field_t *fields[] = {
            gcc_get_field(slice_struct, ARRAY_DATA_FIELD),
            gcc_get_field(slice_struct, ARRAY_LENGTH_FIELD),
            gcc_get_field(slice_struct, ARRAY_STRIDE_FIELD),
            gcc_get_field(slice_struct, ARRAY_CAPACITY_FIELD),
        };

        gcc_rvalue_t *rvals[] = {
            field_addr,
            len,
            stride,
            gcc_rvalue_int16(env->ctx, access == ACCESS_READ ? -1 : 0),
        };

        gcc_lvalue_t *result_var = gcc_local(func, loc, slice_gcc_t, "_slice");
        gcc_assign(*block, loc, result_var, gcc_struct_constructor(env->ctx, loc, slice_gcc_t, 4, fields, rvals));
        return gcc_rval(result_var);
    }
    return NULL;
}

static gcc_lvalue_t *bounds_checked_index(env_t *env, gcc_block_t **block, gcc_loc_t *loc, span_t *span, gcc_struct_t *array_struct, gcc_type_t *gcc_item_t, gcc_rvalue_t *arr, gcc_rvalue_t *index_val)
{
    gcc_type_t *i64_t = gcc_type(env->ctx, INT64);
    gcc_rvalue_t *big_enough = gcc_comparison(env->ctx, loc, GCC_COMPARISON_GE, index_val, gcc_one(env->ctx, i64_t));
    gcc_rvalue_t *len64 = gcc_cast(env->ctx, loc, gcc_rvalue_access_field(arr, loc, gcc_get_field(array_struct, ARRAY_LENGTH_FIELD)), i64_t);
    gcc_rvalue_t *small_enough = gcc_comparison(env->ctx, loc, GCC_COMPARISON_LE, index_val, len64);
    gcc_rvalue_t *ok = gcc_binary_op(env->ctx, loc, GCC_BINOP_LOGICAL_AND, gcc_type(env->ctx, BOOL), big_enough, small_enough);

    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *bounds_safe = gcc_new_block(func, fresh("bounds_safe")),
                *bounds_unsafe = gcc_new_block(func, fresh("bounds_unsafe"));
    gcc_jump_condition(*block, loc, ok, bounds_safe, bounds_unsafe);

    // Bounds check failure:
    *block = bounds_unsafe;
    gcc_block_t *empty = gcc_new_block(func, fresh("empty")),
                *nonempty = gcc_new_block(func, fresh("nonempty"));
    gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_GT, len64, gcc_rvalue_int64(env->ctx, 0)),
                       nonempty, empty);
    *block = nonempty;
    insert_failure(env, block, span, "Error: '%#s' is not a valid index for this array (valid indices are: 1..%#s)",
                   Type(IntType, .bits=64), index_val, Type(IntType, .bits=64), len64);
    *block = empty;
    insert_failure(env, block, span, "Error: this is an empty array and it cannot be indexed into");

    // Bounds check success:
    *block = bounds_safe;
    gcc_rvalue_t *items = gcc_rvalue_access_field(arr, loc, gcc_get_field(array_struct, ARRAY_DATA_FIELD));
    gcc_rvalue_t *index0 = gcc_binary_op(env->ctx, loc, GCC_BINOP_MINUS, i64_t, index_val, gcc_one(env->ctx, i64_t));
    gcc_rvalue_t *stride64 = gcc_cast(env->ctx, loc, gcc_rvalue_access_field(arr, loc, gcc_get_field(array_struct, ARRAY_STRIDE_FIELD)), i64_t);
    index0 = gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, i64_t, index0, stride64);
    return gcc_rvalue_dereference(pointer_offset(env, gcc_get_ptr_type(gcc_item_t), items, index0), loc);
}

gcc_lvalue_t *array_index(env_t *env, gcc_block_t **block, ast_t *arr_ast, ast_t *index, bool unchecked, access_type_e access)
{
    if (!index) index = FakeAST(Range);
    sss_type_t *index_t = get_type(env, index);
    if (index_t->tag == RangeType) {
        gcc_func_t *func = gcc_block_func(*block);
        sss_type_t *slice_t = get_type(env, arr_ast);
        while (slice_t->tag == PointerType) slice_t = Match(slice_t, PointerType)->pointed;
        gcc_lvalue_t *slice = gcc_local(func, NULL, sss_type_to_gcc(env, slice_t), "_slice");
        gcc_assign(*block, NULL, slice, array_slice(env, block, arr_ast, index, access));
        return slice;
    } else if (!is_integral(index_t)) {
        compiler_err(env, index, "This array index should be an Int or a Range, not %T", index_t);
    }

    sss_type_t *arr_t = get_type(env, arr_ast);
    gcc_rvalue_t *arr = compile_expr(env, block, arr_ast);
    while (arr_t->tag == PointerType) {
        auto ptr = Match(arr_t, PointerType);
        if (ptr->is_optional)
            compiler_err(env, arr_ast, "This is an optional pointer, which can't be safely dereferenced.");

        // Copy on write
        if (ptr->pointed->tag == ArrayType && access == ACCESS_WRITE)
            check_cow(env, block, ptr->pointed, arr);

        arr = gcc_rval(gcc_rvalue_dereference(arr, NULL));
        arr_t = ptr->pointed;
    }

    sss_type_t *item_t = get_item_type(arr_t);
    if (!item_t)
        compiler_err(env, arr_ast, "Only arrays may be indexed, but this value is a %T", arr_t);

    gcc_func_t *func = gcc_block_func(*block);
    gcc_type_t *gcc_t = sss_type_to_gcc(env, arr_t);
    gcc_type_t *i64_t = gcc_type(env->ctx, INT64);
    gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
    gcc_loc_t *loc = ast_loc(env, arr_ast);
    gcc_rvalue_t *items = gcc_rvalue_access_field(arr, loc, gcc_get_field(array_struct, ARRAY_DATA_FIELD));
    gcc_lvalue_t *index_var = gcc_local(func, loc, i64_t, "_index");
    gcc_assign(*block, loc, index_var, gcc_cast(env->ctx, loc, compile_expr(env, block, index), i64_t));
    gcc_rvalue_t *index_val = gcc_rval(index_var);
    gcc_rvalue_t *stride64 = gcc_cast(env->ctx, loc, gcc_rvalue_access_field(arr, loc, gcc_get_field(array_struct, ARRAY_STRIDE_FIELD)), i64_t);

    gcc_rvalue_t *index0 = gcc_binary_op(env->ctx, loc, GCC_BINOP_MINUS, i64_t, index_val, gcc_one(env->ctx, i64_t));
    index0 = gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, i64_t, index0, stride64);

    gcc_type_t *gcc_item_t = sss_type_to_gcc(env, item_t);
    if (unchecked)
        return gcc_rvalue_dereference(pointer_offset(env, gcc_get_ptr_type(gcc_item_t), items, index0), loc);
    else
        return bounds_checked_index(env, block, loc, &index->span, array_struct, gcc_item_t, arr, index_val);
}

gcc_rvalue_t *compile_array(env_t *env, gcc_block_t **block, ast_t *ast, bool mark_cow)
{
    auto array = Match(ast, Array);
    sss_type_t *t = get_type(env, ast);
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_func_t *func = gcc_block_func(*block);

    gcc_loc_t *loc = ast_loc(env, ast);
    gcc_lvalue_t *array_var = gcc_local(func, loc, gcc_t, "_array");
    gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);

    sss_type_t *item_t = get_item_type(t);
    if (item_t->tag == VoidType)
        compiler_err(env, ast, "Arrays can't be defined with a Void item type");

    gcc_func_t *alloc_func = get_function(env, has_heap_memory(item_t) ? "GC_malloc" : "GC_malloc_atomic");
    int64_t min_length = array->items ? length(array->items) : 0;
    gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), (long)(gcc_sizeof(env, item_t) * min_length));
    gcc_type_t *gcc_item_ptr_t = sss_type_to_gcc(env, Type(PointerType, .pointed=item_t));
    gcc_rvalue_t *initial_items = min_length == 0 ? 
        gcc_null(env->ctx, gcc_item_ptr_t) : gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, size), gcc_item_ptr_t);
    gcc_assign(*block, loc, array_var, gcc_struct_constructor(
            env->ctx, loc, gcc_t, 4,
            (gcc_field_t*[]){
                gcc_get_field(gcc_struct, ARRAY_DATA_FIELD),
                gcc_get_field(gcc_struct, ARRAY_LENGTH_FIELD),
                gcc_get_field(gcc_struct, ARRAY_STRIDE_FIELD),
                gcc_get_field(gcc_struct, ARRAY_CAPACITY_FIELD),
            },
            (gcc_rvalue_t*[]){
                initial_items,
                gcc_zero(env->ctx, gcc_type(env->ctx, INT32)),
                gcc_rvalue_int16(env->ctx, gcc_sizeof(env, item_t)),
                gcc_rvalue_int16(env->ctx, mark_cow ? -1 : 0),
            }));

    if (array->items) {
        env_t env2 = *env;
        env2.comprehension_callback = (void*)add_array_item;
        array_insert_info_t info = {t, gcc_lvalue_address(array_var, loc), false};
        env2.comprehension_userdata = &info;
        env = &env2;

        gcc_block_t *array_done = gcc_new_block(func, fresh("array_done"));
        foreach (array->items, item_ast, _) {
            gcc_block_t *item_done = gcc_new_block(func, fresh("item_done"));
            env2.loop_label = &(loop_label_t){
                .enclosing = env->loop_label,
                .names = LIST(const char*, "[]"),
                .skip_label = item_done,
                .stop_label = array_done,
            };

            info.dynamic_size = info.dynamic_size || (get_type(env, *item_ast)->tag == GeneratorType);
            add_array_item(env, block, *item_ast, &info);

            if (*block)
                gcc_jump(*block, loc, item_done);
            *block = item_done;
        }
        if (*block)
            gcc_jump(*block, loc, array_done);
        *block = array_done;
    }
    return gcc_rval(array_var);
}

void compile_array_cord_func(env_t *env, gcc_block_t **block, gcc_rvalue_t *obj, gcc_rvalue_t *rec, gcc_rvalue_t *color, sss_type_t *t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_func_t *cord_cat_fn = get_function(env, "CORD_cat");
    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *cord = gcc_local(func, NULL, gcc_type(env->ctx, STRING), "cord");
    gcc_assign(*block, NULL, cord, gcc_null(env->ctx, gcc_type(env->ctx, STRING)));

#define APPEND_CORD(b, to_append) gcc_assign(b, NULL, cord, gcc_callx(env->ctx, NULL, cord_cat_fn, gcc_rval(cord), to_append))
#define APPEND_LITERAL(b, str) gcc_assign(b, NULL, cord, gcc_callx(env->ctx, NULL, cord_cat_fn, gcc_rval(cord), gcc_str(env->ctx, str)))
#define APPEND_COLOR_LITERAL(block, str) maybe_append_cord(env, block, cord, color, str)

    sss_type_t *item_type = get_item_type(t);
    bool is_string = (item_type->tag == CharType);
    gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
    gcc_rvalue_t *len = gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, ARRAY_LENGTH_FIELD));

    if (!is_string) {
        gcc_block_t *is_empty = gcc_new_block(func, fresh("is_empty")),
                    *is_not_empty = gcc_new_block(func, fresh("is_not_empty"));
        gcc_jump_condition(*block, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, len, gcc_zero(env->ctx, gcc_type(env->ctx, INT32))),
                           is_not_empty, is_empty);
        *block = is_empty;
        APPEND_COLOR_LITERAL(block, "\x1b[m");
        APPEND_LITERAL(*block, "[");
        APPEND_COLOR_LITERAL(block, "\x1b[0;2;36m");
        APPEND_LITERAL(*block, ":");
        APPEND_COLOR_LITERAL(block, "\x1b[0;36m");
        APPEND_LITERAL(*block, type_to_string_concise(item_type));
        APPEND_COLOR_LITERAL(block, "\x1b[m");
        APPEND_LITERAL(*block, "]");
        gcc_return(*block, NULL, gcc_rval(cord));

        *block = is_not_empty;
    }

    if (is_string) {
        APPEND_COLOR_LITERAL(block, "\x1b[0;35m");
        APPEND_LITERAL(*block, "\"");
    } else {
        APPEND_COLOR_LITERAL(block, "\x1b[m");
        APPEND_LITERAL(*block, "[");
    }

    // i = 1
    gcc_lvalue_t *i = gcc_local(func, NULL, gcc_type(env->ctx, INT64), "_i");
    gcc_assign(*block, NULL, i, gcc_zero(env->ctx, gcc_type(env->ctx, INT64)));
    gcc_rvalue_t *items = gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, ARRAY_DATA_FIELD));
    gcc_rvalue_t *len64 = gcc_cast(env->ctx, NULL, len, gcc_type(env->ctx, INT64));
    gcc_rvalue_t *stride = gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, ARRAY_STRIDE_FIELD));

    gcc_block_t *add_comma = gcc_new_block(func, fresh("add_comma"));
    gcc_block_t *add_next_item = gcc_new_block(func, fresh("next_item"));
    gcc_block_t *continue_next_item = add_next_item;
    gcc_block_t *end = gcc_new_block(func, fresh("done"));

    // item_ptr = array.items
    gcc_type_t *gcc_item_t = sss_type_to_gcc(env, item_type);
    gcc_lvalue_t *item_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_item_t), "_item_ptr");
    gcc_assign(*block, NULL, item_ptr, items);

    // if (i < len) goto add_next_item;
    gcc_jump_condition(*block, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(i), len64),
                  add_next_item, end);

    // add_next_item:
    // item = *item_ptr
    gcc_rvalue_t *item = gcc_rval(gcc_jit_rvalue_dereference(gcc_rval(item_ptr), NULL));
    // print(item)
    if (is_string) {
        // Custom logic for strings: inline the char printing to prevent ANSI color escapes from breaking up utf8 codepoints
        // and generally spamming the console with more characters than necessary
        // (i.e. print `\x1b[35m"Hello"\x1b[m` instead of `\x1b[35m"\x1b[35mH\x1b[m\x1b[35me\x1b[m...` )
        continue_next_item = gcc_new_block(func, fresh("carry_on"));

        char *escapes[128] = {['\a']="\\a",['\b']="\\b",['\x1b']="\\e",['\f']="\\f",['\n']="\\n",['\t']="\\t",['\r']="\\r",['\v']="\\v",['"']="\\\""};
        NEW_LIST(gcc_case_t*, cases);
#define ADD_CASE(block, lo, hi) APPEND(cases, gcc_new_case(env->ctx, gcc_rvalue_from_long(env->ctx, gcc_item_t, lo), gcc_rvalue_from_long(env->ctx, gcc_item_t, lo), block))
        for (int i = 0; i < 128; i++) {
            char *escape_str = escapes[i];
            if (!escape_str) continue;
            gcc_block_t *case_block = gcc_new_block(func, fresh("char_escape"));
            ADD_CASE(case_block, i, i);
            APPEND_COLOR_LITERAL(&case_block, "\x1b[1;34m");
            APPEND_LITERAL(case_block, escape_str);
            APPEND_COLOR_LITERAL(&case_block, "\x1b[0;35m");
            gcc_jump(case_block, NULL, continue_next_item);
        }

        // Hex escape:
        gcc_block_t *hex_block = gcc_new_block(func, fresh("char_hex_escape"));
        ADD_CASE(hex_block, '\x00', '\x06');
        ADD_CASE(hex_block, '\x0E', '\x1A');
        ADD_CASE(hex_block, '\x1C', '\x1F');
        ADD_CASE(hex_block, '\x7F', '\x7F');
        APPEND_COLOR_LITERAL(&hex_block, "\x1b[1;34m");
        gcc_func_t *cord_sprintf_fn = get_function(env, "CORD_sprintf");
        gcc_eval(hex_block, NULL, gcc_callx(env->ctx, NULL, cord_sprintf_fn, gcc_lvalue_address(cord, NULL), gcc_str(env->ctx, "%r\\x%02X"), gcc_rval(cord), item));
        APPEND_COLOR_LITERAL(&hex_block, "\x1b[0;35m");
        gcc_jump(hex_block, NULL, continue_next_item);
#undef ADD_CASE

        gcc_block_t *default_block = gcc_new_block(func, fresh("default"));
        gcc_switch(add_next_item, NULL, item, default_block, length(cases), cases[0]);

        gcc_func_t *cord_cat_char_fn = get_function(env, "CORD_cat_char");
        gcc_assign(default_block, NULL, cord, gcc_callx(env->ctx, NULL, cord_cat_char_fn, gcc_rval(cord), item));
        gcc_jump(default_block, NULL, continue_next_item);
    } else {
        gcc_func_t *item_cord_fn = get_cord_func(env, item_type);
        assert(item_cord_fn);
        APPEND_CORD(continue_next_item, gcc_callx(env->ctx, NULL, item_cord_fn, item, rec, color));
    }
    
    // i += 1
    assert(i);
    gcc_update(continue_next_item, NULL, i, GCC_BINOP_PLUS, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
    // item_ptr = (void*)item_ptr + stride
    gcc_assign(continue_next_item, NULL, item_ptr, pointer_offset(env, gcc_get_ptr_type(gcc_item_t), gcc_rval(item_ptr), stride));
    // if (i < len) goto add_comma;
    gcc_jump_condition(continue_next_item, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(i), len64),
                  add_comma, end);

    // add_comma:
    if (!is_string) {
        APPEND_COLOR_LITERAL(&add_comma, "\x1b[m");
        APPEND_LITERAL(add_comma, ", ");
    }

    // goto add_next_item;
    gcc_jump(add_comma, NULL, add_next_item);

    // end:
    if (is_string) {
        APPEND_COLOR_LITERAL(&end, "\x1b[35m");
        APPEND_LITERAL(end, "\"");
        APPEND_COLOR_LITERAL(&end, "\x1b[m");
    } else {
        APPEND_COLOR_LITERAL(&end, "\x1b[m");
        APPEND_LITERAL(end, "]");
    }

    gcc_return(end, NULL, gcc_rval(cord));
#undef APPEND_COLOR_LITERAL
#undef APPEND_LITERAL
#undef APPEND_CORD
}

#define AS_VOID_PTR(x) gcc_cast(env->ctx, NULL, x, gcc_type(env->ctx, VOID_PTR))
static void define_array_insert(env_t *env, sss_type_t *t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    sss_type_t *item_t = get_item_type(t);
    gcc_type_t *gcc_item_t = sss_type_to_gcc(env, item_t);
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_t), fresh("array")),
        gcc_new_param(env->ctx, NULL, gcc_item_t, fresh("item")),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, INT64), fresh("index")),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, gcc_type(env->ctx, VOID), fresh("insert"), 3, params, 0);
    gcc_block_t *block = gcc_new_block(func, fresh("insert"));
    gcc_func_t *c_insert_func = get_function(env, "array_insert");
    gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, c_insert_func,
                                    AS_VOID_PTR(gcc_param_as_rvalue(params[0])),
                                    AS_VOID_PTR(gcc_lvalue_address(gcc_param_as_lvalue(params[1]), NULL)),
                                    gcc_param_as_rvalue(params[2]),
                                    gcc_rvalue_size(env->ctx, gcc_sizeof(env, item_t)),
                                    gcc_rvalue_bool(env->ctx, !has_heap_memory(item_t))));
    gcc_return_void(block, NULL);

    ast_t *len_plus_one = FakeAST(Add, .lhs=FakeAST(FieldAccess, .fielded=FakeAST(Var, .name="array"), .field="length"), .rhs=FakeAST(Int, .i=1, .precision=64));
    binding_t *b = new(binding_t, .func=func,
                       .type=Type(FunctionType, .arg_names=LIST(const char*, "array", "item", "index"),
                                  .arg_types=LIST(sss_type_t*, Type(PointerType, .pointed=t, .is_stack=true), item_t, Type(IntType, .bits=64)),
                                  .arg_defaults=LIST(ast_t*, NULL, NULL, len_plus_one),
                                  .ret=Type(VoidType)));
    set_in_namespace(env, t, "insert", b);
}

static void define_array_insert_all(env_t *env, sss_type_t *t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    sss_type_t *item_t = get_item_type(t);
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_t), fresh("array")),
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("other")),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, INT64), fresh("index")),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, gcc_type(env->ctx, VOID), fresh("insert_all"), 3, params, 0);
    gcc_block_t *block = gcc_new_block(func, fresh("insert_all"));
    gcc_func_t *c_insert_all_func = get_function(env, "array_insert_all");
    gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, c_insert_all_func,
                                    AS_VOID_PTR(gcc_param_as_rvalue(params[0])),
                                    AS_VOID_PTR(gcc_lvalue_address(gcc_param_as_lvalue(params[1]), NULL)),
                                    gcc_param_as_rvalue(params[2]),
                                    gcc_rvalue_size(env->ctx, gcc_sizeof(env, item_t)),
                                    gcc_rvalue_bool(env->ctx, !has_heap_memory(item_t))));
    gcc_return_void(block, NULL);

    ast_t *len_plus_one = FakeAST(Add, .lhs=FakeAST(FieldAccess, .fielded=FakeAST(Var, .name="array"), .field="length"), .rhs=FakeAST(Int, .i=1, .precision=64));
    binding_t *b = new(binding_t, .func=func,
                       .type=Type(FunctionType, .arg_names=LIST(const char*, "array", "other", "index"),
                                  .arg_types=LIST(sss_type_t*, Type(PointerType, .pointed=t, .is_stack=true), t, Type(IntType, .bits=64)),
                                  .arg_defaults=LIST(ast_t*, NULL, NULL, len_plus_one),
                                  .ret=Type(VoidType)));
    set_in_namespace(env, t, "insert_all", b);
}

static void define_array_remove(env_t *env, sss_type_t *t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    sss_type_t *item_t = get_item_type(t);
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_t), fresh("array")),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, INT64), fresh("index")),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, INT64), fresh("count")),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, gcc_type(env->ctx, VOID), fresh("remove"), 3, params, 0);
    gcc_block_t *block = gcc_new_block(func, fresh("remove"));
    gcc_func_t *c_remove_func = get_function(env, "array_remove");
    gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, c_remove_func,
                                    AS_VOID_PTR(gcc_param_as_rvalue(params[0])),
                                    gcc_param_as_rvalue(params[1]),
                                    gcc_param_as_rvalue(params[2]),
                                    gcc_rvalue_size(env->ctx, gcc_sizeof(env, item_t)),
                                    gcc_rvalue_bool(env->ctx, !has_heap_memory(item_t))));
    gcc_return_void(block, NULL);

    ast_t *len = FakeAST(FieldAccess, .fielded=FakeAST(Var, .name="array"), .field="length");
    binding_t *b = new(binding_t, .func=func,
                       .type=Type(FunctionType, .arg_names=LIST(const char*, "array", "index", "count"),
                                  .arg_types=LIST(sss_type_t*, Type(PointerType, .pointed=t, .is_stack=true), Type(IntType, .bits=64), Type(IntType, .bits=64)),
                                  .arg_defaults=LIST(ast_t*, NULL, len, FakeAST(Int, .precision=64, .i=1)),
                                  .ret=Type(VoidType)));
    set_in_namespace(env, t, "remove", b);
}

static void define_array_pop(env_t *env, sss_type_t *t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    sss_type_t *item_t = get_item_type(t);
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_t), fresh("array")),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, INT64), fresh("index")),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, sss_type_to_gcc(env, item_t), fresh("pop"), 2, params, 0);
    gcc_block_t *block = gcc_new_block(func, fresh("pop"));

    gcc_lvalue_t *item = gcc_local(func, NULL, sss_type_to_gcc(env, item_t), "_popped");
    gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
    gcc_assign(block, NULL, item,
               gcc_rval(bounds_checked_index(env, &block, NULL, NULL, array_struct,
                                             sss_type_to_gcc(env, item_t),
                                             gcc_rval(gcc_rvalue_dereference(gcc_param_as_rvalue(params[0]), NULL)),
                                             gcc_param_as_rvalue(params[1]))));

    gcc_func_t *c_remove_func = get_function(env, "array_remove");
    gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, c_remove_func,
                                    AS_VOID_PTR(gcc_param_as_rvalue(params[0])),
                                    gcc_param_as_rvalue(params[1]),
                                    gcc_one(env->ctx, gcc_type(env->ctx, INT64)),
                                    gcc_rvalue_size(env->ctx, gcc_sizeof(env, item_t)),
                                    gcc_rvalue_bool(env->ctx, !has_heap_memory(item_t))));
    gcc_return(block, NULL, gcc_rval(item));

    ast_t *len = FakeAST(FieldAccess, .fielded=FakeAST(Var, .name="array"), .field="length");
    binding_t *b = new(binding_t, .func=func,
                       .type=Type(FunctionType, .arg_names=LIST(const char*, "array", "index"),
                                  .arg_types=LIST(sss_type_t*, Type(PointerType, .pointed=t, .is_stack=true), Type(IntType, .bits=64)),
                                  .arg_defaults=LIST(ast_t*, NULL, len),
                                  .ret=item_t));
    set_in_namespace(env, t, "pop", b);
}

static void define_array_shuffle(env_t *env, sss_type_t *t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    sss_type_t *item_t = get_item_type(t);
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_t), fresh("array")),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, gcc_type(env->ctx, VOID), fresh("shuffle"), 1, params, 0);
    gcc_block_t *block = gcc_new_block(func, fresh("shuffle"));
    gcc_func_t *c_shuffle_func = get_function(env, "array_shuffle");
    gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, c_shuffle_func,
                                    AS_VOID_PTR(gcc_param_as_rvalue(params[0])),
                                    gcc_rvalue_size(env->ctx, gcc_sizeof(env, item_t)),
                                    gcc_rvalue_bool(env->ctx, !has_heap_memory(item_t))));
    gcc_return_void(block, NULL);

    binding_t *b = new(binding_t, .func=func,
                       .type=Type(FunctionType, .arg_names=LIST(const char*, "array"),
                                  .arg_types=LIST(sss_type_t*, Type(PointerType, .pointed=t, .is_stack=true)),
                                  .ret=Type(VoidType)));
    set_in_namespace(env, t, "shuffle", b);
}

static void define_array_sort(env_t *env, sss_type_t *t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    sss_type_t *item_t = get_item_type(t);
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_t), fresh("array")),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, gcc_type(env->ctx, VOID), fresh("sort"), 1, params, 0);
    gcc_block_t *block = gcc_new_block(func, fresh("sort"));
    gcc_func_t *c_sort_func = get_function(env, "array_sort");
    sss_type_t *void_ptr_t = Type(PointerType, .pointed=Type(MemoryType));
    sss_type_t *ptr_cmp_fn_t = Type(FunctionType, .arg_types=LIST(sss_type_t*, void_ptr_t, void_ptr_t), .ret=Type(IntType, .bits=32));
    gcc_type_t *ptr_cmp_fn_gcc_t = sss_type_to_gcc(env, ptr_cmp_fn_t);
    gcc_rvalue_t *cmp = gcc_get_func_address(get_indirect_compare_func(env, item_t), NULL);
    gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, c_sort_func,
                                    AS_VOID_PTR(gcc_param_as_rvalue(params[0])),
                                    gcc_cast(env->ctx, NULL, cmp, ptr_cmp_fn_gcc_t),
                                    gcc_rvalue_size(env->ctx, gcc_sizeof(env, item_t)),
                                    gcc_rvalue_bool(env->ctx, !has_heap_memory(item_t))));
    gcc_return_void(block, NULL);

    binding_t *b = new(binding_t, .func=func,
                       .type=Type(FunctionType, .arg_names=LIST(const char*, "array"),
                                  .arg_types=LIST(sss_type_t*, Type(PointerType, .pointed=t, .is_stack=true)),
                                  .ret=Type(VoidType)));
    set_in_namespace(env, t, "sort", b);
}

static void define_array_join(env_t *env, sss_type_t *glue_t)
{
    sss_type_t *array_t = Type(ArrayType, .item_type=glue_t);
    gcc_type_t *glue_gcc_t = sss_type_to_gcc(env, glue_t);
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, glue_gcc_t, fresh("glue")),
        gcc_new_param(env->ctx, NULL, sss_type_to_gcc(env, array_t), fresh("array")),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, glue_gcc_t, fresh("join"), 2, params, 0);
    gcc_block_t *block = gcc_new_block(func, fresh("join"));
    gcc_func_t *c_join_func = get_function(env, "array_join");
    gcc_rvalue_t *retval = gcc_callx(env->ctx, NULL, c_join_func,
                                     AS_VOID_PTR(gcc_lvalue_address(gcc_param_as_lvalue(params[1]), NULL)),
                                     AS_VOID_PTR(gcc_lvalue_address(gcc_param_as_lvalue(params[0]), NULL)),
                                     gcc_rvalue_size(env->ctx, gcc_sizeof(env, glue_t)),
                                     gcc_rvalue_bool(env->ctx, !has_heap_memory(glue_t)));
    gcc_return(block, NULL, gcc_bitcast(env->ctx, NULL, retval, glue_gcc_t));
    binding_t *b = new(binding_t, .func=func,
                       .type=Type(FunctionType, .arg_names=LIST(const char*, "array", "glue"),
                                  .arg_types=LIST(sss_type_t*, glue_t, array_t),
                                  .ret=glue_t));
    set_in_namespace(env, glue_t, "join", b);
}

#undef AS_VOID_PTR

binding_t *get_array_method(env_t *env, sss_type_t *t, const char *method_name)
{
    while (t->tag == PointerType)
        t = Match(t, PointerType)->pointed;

    binding_t *b = get_from_namespace(env, t, method_name);
    if (b || !get_item_type(t)) return b;

    if (streq(method_name, "insert"))
        define_array_insert(env, t);
    else if (streq(method_name, "insert_all"))
        define_array_insert_all(env, t);
    else if (streq(method_name, "remove"))
        define_array_remove(env, t);
    else if (streq(method_name, "pop"))
        define_array_pop(env, t);
    else if (streq(method_name, "sort"))
        define_array_sort(env, t);
    else if (streq(method_name, "shuffle"))
        define_array_shuffle(env, t);
    else if (streq(method_name, "join"))
        define_array_join(env, t);
    else
        return NULL;

    return get_from_namespace(env, t, method_name);
}

void flatten_arrays(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *array_ptr)
{
    sss_type_t *item_type = get_item_type(t);
    if (!item_type) return;
    // If necessary, flatten first:
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
    gcc_func_t *func = gcc_block_func(*block);
    gcc_rvalue_t *stride_field = gcc_rval(gcc_rvalue_dereference_field(array_ptr, NULL, gcc_get_field(struct_t, ARRAY_STRIDE_FIELD)));
    gcc_block_t *needs_flattening = gcc_new_block(func, fresh("needs_flattening")),
                *already_flat = gcc_new_block(func, fresh("already_flat"));
    gcc_jump_condition(*block, NULL,
                       gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, stride_field, gcc_rvalue_int16(env->ctx, gcc_sizeof(env, item_type))),
                       needs_flattening, already_flat);
    *block = needs_flattening;
    gcc_func_t *flatten = get_function(env, "array_flatten");
    gcc_eval(*block, NULL, gcc_callx(
            env->ctx, NULL, flatten,
            gcc_cast(env->ctx, NULL, array_ptr, gcc_get_type(env->ctx, GCC_T_VOID_PTR)),
            gcc_rvalue_size(env->ctx, gcc_sizeof(env, item_type)),
            gcc_rvalue_bool(env->ctx, !has_heap_memory(item_type))));

    gcc_jump(*block, NULL, already_flat);
    *block = already_flat;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
