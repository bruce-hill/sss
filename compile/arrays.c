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
    gcc_struct_t *struct_t = gcc_type_as_struct(gcc_t);
    gcc_lvalue_t *array = gcc_rvalue_dereference(info->array_ptr, NULL);
    gcc_lvalue_t *data_field = gcc_lvalue_access_field(array, NULL, gcc_get_field(struct_t, ARRAY_DATA_FIELD));
    gcc_lvalue_t *length_field = gcc_lvalue_access_field(array, NULL, gcc_get_field(struct_t, ARRAY_LENGTH_FIELD));

    // array.length += 1
    gcc_type_t *i64 = gcc_type(env->ctx, INT64);
    gcc_update(*block, NULL, length_field, GCC_BINOP_PLUS, gcc_one(env->ctx, i64));

    if (info->dynamic_size) {
        // array.items = GC_realloc(array.items, item_size*array.length)
        gcc_type_t *gcc_size_t = gcc_type(env->ctx, SIZE);
        gcc_rvalue_t *new_size = gcc_binary_op(
            env->ctx, NULL, GCC_BINOP_MULT, gcc_size_t, gcc_cast(env->ctx, NULL, gcc_rval(length_field), gcc_size_t),
            gcc_rvalue_from_long(env->ctx, gcc_size_t, (long)gcc_sizeof(env, item_type)));
        gcc_func_t *gc_realloc_func = get_function(env, "GC_realloc");
        gcc_rvalue_t *new_data = gcc_callx(env->ctx, NULL, gc_realloc_func, 
                                           gcc_cast(env->ctx, NULL, gcc_rval(data_field), gcc_type(env->ctx, VOID_PTR)),
                                           new_size);
        gcc_assign(*block, NULL, data_field,
                   gcc_cast(env->ctx, NULL, new_data, gcc_get_ptr_type(sss_type_to_gcc(env, item_type))));
    }

    // array.items[array.length-1] = item
    gcc_rvalue_t *index = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, i64, gcc_rval(length_field), gcc_one(env->ctx, i64));
    // NOTE: This assumes stride == item_size
    gcc_lvalue_t *item_home = gcc_array_access(env->ctx, NULL, gcc_rval(data_field), index);
    if (!type_eq(t, item_type))
        if (!promote(env, t, &item_val, item_type))
            compiler_err(env, item, "I can't convert this type (%T) to %T", t, item_type);

    gcc_assign(*block, NULL, item_home, item_val);
}

gcc_lvalue_t *array_cow_field(env_t *env, gcc_rvalue_t *arr_ptr)
{
    sss_type_t *t = Type(ArrayType, .item_type=Type(CharType)); // Arbitrary array type
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_struct_t *array_struct = gcc_type_as_struct(gcc_t);
    return gcc_rvalue_dereference_field(gcc_cast(env->ctx, NULL, arr_ptr, gcc_get_ptr_type(gcc_t)), NULL,
                                        gcc_get_field(array_struct, ARRAY_COW_FIELD));
}

void mark_array_cow(env_t *env, gcc_block_t **block, gcc_rvalue_t *arr_ptr)
{
    gcc_lvalue_t *cow_field = array_cow_field(env, arr_ptr);
    gcc_assign(*block, NULL, cow_field, gcc_rvalue_bool(env->ctx, 1));
}

void check_cow(env_t *env, gcc_block_t **block, sss_type_t *arr_t, gcc_rvalue_t *arr)
{
    // Copy on write
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *needs_cow = gcc_new_block(func, "needs_cow"),
                *done = gcc_new_block(func, "done_cow");
    gcc_rvalue_t *should_cow = gcc_rval(array_cow_field(env, arr));
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
    gcc_lvalue_t *arr_var = NULL;
    if (access == ACCESS_WRITE) {
        arr_var = get_lvalue(env, block, arr_ast, true);
        check_cow(env, block, arr_t, gcc_lvalue_address(arr_var, loc));
    } else {
        gcc_rvalue_t *arr = compile_expr(env, block, arr_ast);
        while (arr_t->tag == PointerType) {
            auto ptr = Match(arr_t, PointerType);
            if (ptr->is_optional)
                compiler_err(env, arr_ast, "This is an optional pointer, which can't be safely dereferenced.");

            arr_var = gcc_rvalue_dereference(arr, NULL);
            arr = gcc_rval(arr_var);
            arr_t = ptr->pointed;
        }

        if (arr_var == NULL) {
            gcc_type_t *array_gcc_t = sss_type_to_gcc(env, arr_t);
            arr_var = gcc_local(func, loc, array_gcc_t, "_sliced");
            gcc_assign(*block, loc, arr_var, arr);
        }
        mark_array_cow(env, block, gcc_lvalue_address(arr_var, loc));
    }
    while (arr_t->tag == VariantType)
        arr_t = Match(arr_t, VariantType)->variant_of;
    gcc_type_t *array_gcc_t = sss_type_to_gcc(env, arr_t);
    gcc_struct_t *gcc_array_struct = gcc_type_as_struct(array_gcc_t);
    gcc_rvalue_t *arr = gcc_rval(arr_var);

    // Specially optimized case for creating slices using range literals
    // This actually makes a noticeable performance difference
    if (index->tag == Range) {
        auto range = Match(index, Range);
        if (!range->step || (range->step->tag == Int && Match(range->step, Int)->i == 1)) {
            gcc_type_t *i64_t = gcc_type(env->ctx, INT32);
#define SUB(a,b) gcc_binary_op(env->ctx, loc, GCC_BINOP_MINUS, i64_t, a, b)
            gcc_rvalue_t *old_items = gcc_rvalue_access_field(arr, loc, gcc_get_field(gcc_array_struct, ARRAY_DATA_FIELD));
            gcc_rvalue_t *offset;
            if (range->first)
                offset = SUB(gcc_cast(env->ctx, loc, compile_expr(env, block, range->first), i64_t), gcc_one(env->ctx, i64_t));
            else
                offset = gcc_zero(env->ctx, i64_t);

            gcc_lvalue_t *offset_var = gcc_local(func, loc, i64_t, "_offset");
            gcc_assign(*block, loc, offset_var, offset);
            // Bit hack to branchlessly set offset to zero when it would otherwise be negative:
            // offset &= ~(offset >> 31)
            gcc_update(*block, loc, offset_var, GCC_BINOP_BITWISE_AND,
               gcc_unary_op(env->ctx, loc, GCC_UNOP_BITWISE_NEGATE, i64_t,
                   gcc_binary_op(env->ctx, loc, GCC_BINOP_RSHIFT, i64_t, offset, gcc_rvalue_int32(env->ctx, 31))));
            gcc_rvalue_t *old_stride = gcc_rvalue_access_field(arr, loc, gcc_get_field(gcc_array_struct, ARRAY_STRIDE_FIELD));
            offset = gcc_rval(offset_var);

            gcc_type_t *gcc_item_t = sss_type_to_gcc(env, get_item_type(arr_t));
            gcc_rvalue_t *items = pointer_offset(env, gcc_get_ptr_type(gcc_item_t), old_items,
                                                 gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, i64_t, offset, gcc_cast(env->ctx, loc, old_stride, i64_t)));

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
                gcc_rvalue_t *range_len = gcc_cast(env->ctx, loc, compile_expr(env, block, range->last), i64_t);

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
            // Set COW flag:
            if (env->should_mark_cow) {
                gcc_assign(*block, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, ARRAY_CAPACITY_FIELD)),
                           gcc_rvalue_int16(env->ctx, access == ACCESS_READ ? -1 : 0));
            }

            return gcc_rval(slice);
#undef SUB
        }
    }

    // If we're not in the optimized case, fall back to the array.slice(range, readonly, _type) method
    binding_t *slice_binding = get_from_namespace(env, arr_t, "slice");
    assert(slice_binding);
    gcc_rvalue_t *range_val = compile_expr(env, block, index);
    return gcc_callx(
        env->ctx, loc, slice_binding->func,
        arr,
        range_val,
        gcc_rvalue_bool(env->ctx, (access == ACCESS_READ) || !env->should_mark_cow),
        get_type_pointer(env, arr_t));
}

gcc_rvalue_t *array_field_slice(env_t *env, gcc_block_t **block, ast_t *ast, const char *field_name, access_type_e access)
{
    if (streq(field_name, "length"))
        return NULL;

    sss_type_t *array_type = get_type(env, ast);
    gcc_loc_t *loc = ast_loc(env, ast);
    if (array_type->tag == PointerType) {
        auto ptr = Match(array_type, PointerType);
        if (ptr->is_optional)
            compiler_err(env, ast, "This value can't be sliced, because it may or may not be null.");
        return array_field_slice(env, block, WrapAST(ast, Index, .indexed=ast), field_name, access);
    }

    sss_type_t *item_t = get_item_type(array_type);
    if (!item_t)
        return NULL;

    gcc_type_t *array_gcc_t = sss_type_to_gcc(env, array_type);
    gcc_struct_t *gcc_array_struct = gcc_type_as_struct(array_gcc_t);

    gcc_type_t *item_gcc_t = sss_type_to_gcc(env, item_t);
    gcc_struct_t *gcc_item_struct = gcc_type_as_struct(item_gcc_t);

    auto struct_type = Match(base_variant(item_t), StructType);
    for (int64_t i = 0, len = LENGTH(struct_type->field_names); i < len; i++) {
        const char *struct_field_name = ith(struct_type->field_names, i);
        if (!struct_field_name) struct_field_name = heap_strf("_%ld", i+1);
        if (!streq(struct_field_name, field_name))  continue;

        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *arr_var;
        if (access == ACCESS_WRITE) {
            arr_var = get_lvalue(env, block, ast, true);
            if (access == ACCESS_WRITE)
                check_cow(env, block, array_type, gcc_lvalue_address(arr_var, loc));
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
        gcc_struct_t *slice_struct = gcc_type_as_struct(slice_gcc_t);
        gcc_field_t *fields[] = {
            gcc_get_field(slice_struct, ARRAY_DATA_FIELD),
            gcc_get_field(slice_struct, ARRAY_LENGTH_FIELD),
            gcc_get_field(slice_struct, ARRAY_CAPACITY_FIELD),
            gcc_get_field(slice_struct, ARRAY_COW_FIELD),
            gcc_get_field(slice_struct, ARRAY_ATOMIC_FIELD),
            gcc_get_field(slice_struct, ARRAY_STRIDE_FIELD),
        };

        gcc_rvalue_t *rvals[] = {
            field_addr,
            len,
            gcc_rvalue_uint8(env->ctx, 0), // capacity
            gcc_rvalue_bool(env->ctx, (access == ACCESS_READ)), // copy on write
            gcc_rvalue_bool(env->ctx, !has_heap_memory(item_t)), // atomic
            stride,
        };

        gcc_lvalue_t *result_var = gcc_local(func, loc, slice_gcc_t, "_slice");
        gcc_assign(*block, loc, result_var, gcc_struct_constructor(env->ctx, loc, slice_gcc_t, sizeof(fields)/sizeof(fields[0]), fields, rvals));
        return gcc_rval(result_var);
    }
    return NULL;
}

static gcc_lvalue_t *bounds_checked_index(
    env_t *env, gcc_block_t **block, gcc_loc_t *loc, sss_file_t *file, const char *start, const char *end,
    gcc_struct_t *array_struct, gcc_type_t *gcc_item_t, gcc_rvalue_t *arr, gcc_rvalue_t *index_val)
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
    insert_failure(env, block, file, start, end, "Error: '%#s' is not a valid index for this array (valid indices are: 1..%#s)",
                   Type(IntType, .bits=64), index_val, Type(IntType, .bits=64), len64);
    *block = empty;
    insert_failure(env, block, file, start, end, "Error: this is an empty array and it cannot be indexed into");

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
    gcc_struct_t *array_struct = gcc_type_as_struct(gcc_t);
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
        return bounds_checked_index(env, block, loc, index->file, index->start, index->end, array_struct, gcc_item_t, arr, index_val);
}

gcc_rvalue_t *compile_array(env_t *env, gcc_block_t **block, ast_t *ast, bool mark_cow)
{
    auto array = Match(ast, Array);
    sss_type_t *t = get_type(env, ast);
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_func_t *func = gcc_block_func(*block);

    gcc_loc_t *loc = ast_loc(env, ast);
    gcc_lvalue_t *array_var = gcc_local(func, loc, gcc_t, "_array");
    gcc_struct_t *gcc_struct = gcc_type_as_struct(gcc_t);

    sss_type_t *item_t = get_item_type(t);
    if (item_t->tag == VoidType)
        compiler_err(env, ast, "Arrays can't be defined with a Void item type");

    gcc_func_t *alloc_func = get_function(env, has_heap_memory(item_t) ? "GC_malloc" : "GC_malloc_atomic");
    int64_t min_length = array->items ? LENGTH(array->items) : 0;
    gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), (long)(gcc_sizeof(env, item_t) * min_length));
    gcc_type_t *gcc_item_ptr_t = sss_type_to_gcc(env, Type(PointerType, .pointed=item_t));
    gcc_rvalue_t *initial_items = min_length == 0 ? 
        gcc_null(env->ctx, gcc_item_ptr_t) : gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, size), gcc_item_ptr_t);
    gcc_field_t *fields[] = {
        gcc_get_field(gcc_struct, ARRAY_DATA_FIELD),
        gcc_get_field(gcc_struct, ARRAY_LENGTH_FIELD),
        gcc_get_field(gcc_struct, ARRAY_CAPACITY_FIELD),
        gcc_get_field(gcc_struct, ARRAY_COW_FIELD),
        gcc_get_field(gcc_struct, ARRAY_ATOMIC_FIELD),
        gcc_get_field(gcc_struct, ARRAY_STRIDE_FIELD),
    };
    gcc_rvalue_t *field_values[] = {
        initial_items, // data
        gcc_rvalue_int64(env->ctx, 0), // length
        gcc_rvalue_uint8(env->ctx, 0), // capacity
        gcc_rvalue_bool(env->ctx, mark_cow), // copy on write
        gcc_rvalue_bool(env->ctx, !has_heap_memory(item_t)), // atomic
        gcc_rvalue_int16(env->ctx, gcc_sizeof(env, item_t)), // stride
    };
    gcc_assign(*block, loc, array_var, gcc_struct_constructor(
            env->ctx, loc, gcc_t, sizeof(fields)/sizeof(fields[0]), fields, field_values));

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
                .names = ARRAY((const char*)"[]"),
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

void flatten_arrays(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *array_ptr)
{
    sss_type_t *item_type = get_item_type(t);
    if (!item_type) return;
    // If necessary, flatten first:
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_struct_t *struct_t = gcc_type_as_struct(gcc_t);
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
