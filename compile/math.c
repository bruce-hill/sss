// Math operations
#include <libgccjit.h>
#include <stdint.h>

#include "../ast.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

static gcc_rvalue_t *math_binop_rec(
    env_t *env, gcc_block_t **block, ast_t *ast,
    sss_type_t *lhs_t, gcc_rvalue_t *lhs,
    gcc_binary_op_e op,
    sss_type_t *rhs_t, gcc_rvalue_t *rhs)
{
    gcc_loc_t *loc = ast_loc(env, ast);

    // Automatically dereference non-nil pointers:
    if (lhs_t->tag == PointerType) {
        auto ptr = Match(lhs_t, PointerType);
        if (ptr->is_optional)
            compiler_err(env, ast, "The left hand side of this operation contains a potentially nil pointer that can't be safely dereferenced.");
        return math_binop_rec(env, block, ast, ptr->pointed, gcc_rval(gcc_rvalue_dereference(lhs, ast_loc(env, ast))), op, rhs_t, rhs);
    }

    // Automatically dereference non-nil pointers:
    if (rhs_t->tag == PointerType) {
        auto ptr = Match(rhs_t, PointerType);
        if (ptr->is_optional)
            compiler_err(env, ast, "The right hand side of this operation contains a potentially nil pointer that can't be safely dereferenced.");
        return math_binop_rec(env, block, ast, lhs_t, lhs, op, ptr->pointed, gcc_rval(gcc_rvalue_dereference(rhs, ast_loc(env, ast))));
    }

    if (lhs_t->tag == VariantType)
        return math_binop_rec(env, block, ast, Match(lhs_t, VariantType)->variant_of, lhs, op, rhs_t, rhs);
    else if (rhs_t->tag == VariantType)
        return math_binop_rec(env, block, ast, lhs_t, lhs, op, Match(rhs_t, VariantType)->variant_of, rhs);

    gcc_func_t *func = gcc_block_func(*block);

    gcc_type_t *i32 = gcc_type(env->ctx, INT32);
    gcc_type_t *i16 = gcc_type(env->ctx, INT16);

    if (lhs_t->tag == ArrayType && rhs_t->tag == ArrayType) {
        // Use the minimum common length:
        // [1,2,3] += [10,20] ==> [11,22]
        // [1,2,3] += [10,20,30,40] ==> [11,22,33]

        // Pseudocode:
        // len = MIN(lhs->len, rhs->len)
        // result = alloc(len)
        // for (i = 0; i < len; i++)
        //     result->data[i] = lhs->data[i] {OP} &rhs->data[i]

        sss_type_t *result_t = get_math_type(env, ast, lhs_t, ast->tag, rhs_t);
        gcc_type_t *result_gcc_t = sss_type_to_gcc(env, result_t);
        gcc_struct_t *result_array_struct = gcc_type_if_struct(result_gcc_t);
        gcc_lvalue_t *result = gcc_local(func, loc, result_gcc_t, "_result");

        gcc_type_t *lhs_gcc_t = sss_type_to_gcc(env, lhs_t);
        gcc_struct_t *lhs_array_struct = gcc_type_if_struct(lhs_gcc_t);
        gcc_rvalue_t *lhs_len32 = gcc_rvalue_access_field(lhs, loc, gcc_get_field(lhs_array_struct, ARRAY_LENGTH_FIELD));
        gcc_rvalue_t *lhs_stride16 = gcc_rvalue_access_field(lhs, loc, gcc_get_field(lhs_array_struct, ARRAY_STRIDE_FIELD));

        gcc_type_t *rhs_gcc_t = sss_type_to_gcc(env, rhs_t);
        gcc_struct_t *rhs_array_struct = gcc_type_if_struct(rhs_gcc_t);
        gcc_rvalue_t *rhs_len32 = gcc_rvalue_access_field(rhs, loc, gcc_get_field(rhs_array_struct, ARRAY_LENGTH_FIELD));
        gcc_rvalue_t *rhs_stride16 = gcc_rvalue_access_field(rhs, loc, gcc_get_field(rhs_array_struct, ARRAY_STRIDE_FIELD));

        gcc_rvalue_t *len = ternary(block,
                                    gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LE, lhs_len32, rhs_len32),
                                    i32, lhs_len32, rhs_len32);

        sss_type_t *item_t = Match(result_t, ArrayType)->item_type;
        gcc_func_t *alloc_func = hget(&env->global->funcs, has_heap_memory(item_t) ? "GC_malloc" : "GC_malloc_atomic", gcc_func_t*);
        gcc_type_t *gcc_item_ptr_t = sss_type_to_gcc(env, Type(PointerType, .pointed=item_t));
        gcc_type_t *gcc_size = gcc_type(env->ctx, SIZE);
        gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_size, (long)(gcc_sizeof(env, item_t)));
        size = gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, gcc_size, size, gcc_cast(env->ctx, loc, len, gcc_size));
        gcc_rvalue_t *initial_items = gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, size), gcc_item_ptr_t);
        gcc_assign(*block, loc, result, gcc_struct_constructor(
                env->ctx, loc, result_gcc_t, 4,
                (gcc_field_t*[]){
                    gcc_get_field(result_array_struct, ARRAY_DATA_FIELD),
                    gcc_get_field(result_array_struct, ARRAY_LENGTH_FIELD),
                    gcc_get_field(result_array_struct, ARRAY_STRIDE_FIELD),
                    gcc_get_field(result_array_struct, ARRAY_CAPACITY_FIELD),
                },
                (gcc_rvalue_t*[]){
                    initial_items, len, gcc_rvalue_int16(env->ctx, gcc_sizeof(env, item_t)), gcc_zero(env->ctx, i16),
                }));

        gcc_block_t *loop_condition = gcc_new_block(func, fresh("loop_condition")),
                    *loop_body = gcc_new_block(func, fresh("loop_body")),
                    *loop_end = gcc_new_block(func, fresh("loop_end"));

        gcc_lvalue_t *offset = gcc_local(func, NULL, i32, "_offset");
        gcc_assign(*block, NULL, offset, gcc_zero(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);

        gcc_jump_condition(loop_condition, loc, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(offset), len),
                           loop_body, loop_end);

        *block = loop_body;
#define ITEM(t, item_ptr, stride) gcc_rvalue_dereference(pointer_offset(env, sss_type_to_gcc(env, Type(PointerType, .pointed=Match(t, ArrayType)->item_type)), item_ptr, gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, i32, gcc_rval(offset), gcc_cast(env->ctx, NULL, stride, i32))), NULL)
        gcc_rvalue_t *lhs_item_ptr = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(lhs_array_struct, ARRAY_DATA_FIELD));
        gcc_rvalue_t *lhs_item = gcc_rval(ITEM(lhs_t, lhs_item_ptr, lhs_stride16));
        gcc_rvalue_t *rhs_item_ptr = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(rhs_array_struct, ARRAY_DATA_FIELD));
        gcc_rvalue_t *rhs_item = gcc_rval(ITEM(rhs_t, rhs_item_ptr, rhs_stride16));

        gcc_rvalue_t *result_item_ptr = gcc_rvalue_access_field(gcc_rval(result), NULL, gcc_get_field(result_array_struct, ARRAY_DATA_FIELD));
        gcc_lvalue_t *result_item = ITEM(result_t, result_item_ptr, gcc_rvalue_int16(env->ctx, gcc_sizeof(env, item_t)));

        gcc_rvalue_t *item = math_binop_rec(env, block, ast, Match(lhs_t, ArrayType)->item_type, lhs_item, op, 
                                            Match(rhs_t, ArrayType)->item_type, rhs_item);
        gcc_assign(*block, NULL, result_item, item);
        gcc_update(*block, NULL, offset, GCC_BINOP_PLUS, gcc_one(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);
        *block = loop_end;
        return gcc_rval(result);
    } else if (lhs_t->tag == ArrayType || rhs_t->tag == ArrayType) {
        // Pseudocode:
        // result = alloc(arr->len)
        // for (i = 0; i < arr->len; i++)
        //     result->data[i] = arr->data[i] {OP} scalar

        gcc_rvalue_t *array, *scalar;
        sss_type_t *array_t, *scalar_t;
        bool scalar_left = (lhs_t->tag != ArrayType);
        if (scalar_left) {
            array = rhs, scalar = lhs;
            array_t = rhs_t, scalar_t = lhs_t;
        } else {
            array = lhs, scalar = rhs;
            array_t = lhs_t, scalar_t = rhs_t;
        }
        sss_type_t *result_t = scalar_left ? get_math_type(env, ast, scalar_t, ast->tag, array_t) : get_math_type(env, ast, array_t, ast->tag, scalar_t);
        gcc_type_t *result_gcc_t = sss_type_to_gcc(env, result_t);
        gcc_struct_t *result_array_struct = gcc_type_if_struct(result_gcc_t);
        gcc_lvalue_t *result = gcc_local(func, loc, result_gcc_t, "_result");

        gcc_type_t *array_gcc_t = sss_type_to_gcc(env, array_t);
        gcc_struct_t *array_struct = gcc_type_if_struct(array_gcc_t);
        gcc_rvalue_t *len = gcc_rvalue_access_field(array, loc, gcc_get_field(array_struct, ARRAY_LENGTH_FIELD));
        gcc_rvalue_t *stride16 = gcc_rvalue_access_field(array, loc, gcc_get_field(array_struct, ARRAY_STRIDE_FIELD));

        gcc_block_t *loop_condition = gcc_new_block(func, fresh("loop_condition")),
                    *loop_body = gcc_new_block(func, fresh("loop_body")),
                    *loop_end = gcc_new_block(func, fresh("loop_end"));

        sss_type_t *item_t = Match(result_t, ArrayType)->item_type;
        gcc_func_t *alloc_func = hget(&env->global->funcs, has_heap_memory(item_t) ? "GC_malloc" : "GC_malloc_atomic", gcc_func_t*);
        gcc_type_t *gcc_item_ptr_t = sss_type_to_gcc(env, Type(PointerType, .pointed=item_t));
        gcc_type_t *gcc_size = gcc_type(env->ctx, SIZE);
        gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_size, (long)(gcc_sizeof(env, item_t)));
        size = gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, gcc_size, size, gcc_cast(env->ctx, loc, len, gcc_size));
        gcc_rvalue_t *initial_items = gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, size), gcc_item_ptr_t);
        gcc_assign(*block, loc, result, gcc_struct_constructor(
                env->ctx, loc, result_gcc_t, 4, (gcc_field_t*[]){
                    gcc_get_field(result_array_struct, ARRAY_DATA_FIELD),
                    gcc_get_field(result_array_struct, ARRAY_LENGTH_FIELD),
                    gcc_get_field(result_array_struct, ARRAY_STRIDE_FIELD),
                    gcc_get_field(result_array_struct, ARRAY_CAPACITY_FIELD),
                },
                (gcc_rvalue_t*[]){
                    initial_items, len, gcc_rvalue_int16(env->ctx, gcc_sizeof(env, item_t)), gcc_rvalue_int16(env->ctx, 0),
                }));

        gcc_lvalue_t *offset = gcc_local(func, NULL, i32, "_offset");
        gcc_assign(*block, NULL, offset, gcc_zero(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);

        gcc_jump_condition(loop_condition, loc, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(offset), len),
                           loop_body, loop_end);

        *block = loop_body;
        gcc_rvalue_t *array_item_ptr = gcc_rvalue_access_field(array, NULL, gcc_get_field(array_struct, ARRAY_DATA_FIELD));
        gcc_rvalue_t *array_item = gcc_rval(ITEM(array_t, array_item_ptr, stride16));

        gcc_rvalue_t *result_item_ptr = gcc_rvalue_access_field(gcc_rval(result), NULL, gcc_get_field(result_array_struct, ARRAY_DATA_FIELD));
        gcc_lvalue_t *result_item = ITEM(result_t, result_item_ptr, gcc_rvalue_int16(env->ctx, gcc_sizeof(env, item_t)));

        sss_type_t *array_item_t = Match(array_t, ArrayType)->item_type;
        gcc_rvalue_t *item;
        if (scalar_left) 
            item = math_binop_rec(env, block, ast, scalar_t, scalar, op, array_item_t, array_item);
        else
            item = math_binop_rec(env, block, ast, array_item_t, array_item, op, scalar_t, scalar);

        gcc_assign(*block, NULL, result_item, item);
        gcc_update(*block, NULL, offset, GCC_BINOP_PLUS, gcc_one(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);
        *block = loop_end;
        return gcc_rval(result);
    }
#undef ITEM

    sss_type_t *struct_t = NULL;
    if (lhs_t->tag == StructType && rhs_t->tag == StructType) {
        if (!type_eq(with_units(lhs_t, NULL), with_units(rhs_t, NULL)))
            compiler_err(env, ast, "I don't know how to do math operations between %T and %T", lhs_t, rhs_t);
        struct_t = lhs_t;
    } else if (lhs_t->tag == StructType) {
        struct_t = lhs_t;
    } else if (rhs_t->tag == StructType) {
        struct_t = rhs_t;
    } else if (lhs_t->tag == TaggedUnionType && type_eq(lhs_t, rhs_t)) {
        // For tagged unions, Foo.One + Foo.Two --> bitwise or of the tags
        // TODO: confirm no values
        gcc_type_t *gcc_tagged_t = sss_type_to_gcc(env, lhs_t);
        gcc_struct_t *gcc_tagged_s = gcc_type_if_struct(gcc_tagged_t);
        gcc_field_t *tag_field = gcc_get_field(gcc_tagged_s, 0);
        gcc_type_t *tag_gcc_t = get_tag_type(env, lhs_t);
        auto members = Match(lhs_t, TaggedUnionType)->members;
        for (int64_t i = 0; i < length(members); i++) {
            if (ith(members, i).type)
                compiler_err(env, ast, "%T tagged union values can't be combined because some tags have data attached to them.", lhs_t);
        }
        gcc_rvalue_t *result_tag;
        if (op == GCC_BINOP_PLUS || op == GCC_BINOP_BITWISE_OR) {
            result_tag = gcc_binary_op(env->ctx, loc, GCC_BINOP_BITWISE_OR, tag_gcc_t,
                                       gcc_rvalue_access_field(lhs, loc, tag_field),
                                       gcc_rvalue_access_field(rhs, loc, tag_field));
        } else if (op == GCC_BINOP_MINUS) {
            result_tag = gcc_binary_op(env->ctx, loc, GCC_BINOP_BITWISE_AND, tag_gcc_t,
                                       gcc_rvalue_access_field(lhs, loc, tag_field),
                                       gcc_unary_op(env->ctx, loc, GCC_UNOP_BITWISE_NEGATE, tag_gcc_t, gcc_rvalue_access_field(rhs, loc, tag_field)));
        } else if (op == GCC_BINOP_BITWISE_XOR) {
            result_tag = gcc_binary_op(env->ctx, loc, GCC_BINOP_BITWISE_XOR, tag_gcc_t,
                                       gcc_rvalue_access_field(lhs, loc, tag_field),
                                       gcc_rvalue_access_field(rhs, loc, tag_field));
        } else if (op == GCC_BINOP_BITWISE_AND) {
            result_tag = gcc_binary_op(env->ctx, loc, GCC_BINOP_BITWISE_AND, tag_gcc_t,
                                       gcc_rvalue_access_field(lhs, loc, tag_field),
                                       gcc_rvalue_access_field(rhs, loc, tag_field));
        } else {
            compiler_err(env, ast, "This math operation is not supported on tagged union types");
        }
        return gcc_struct_constructor(env->ctx, NULL, gcc_tagged_t, 1, (gcc_field_t*[]){tag_field}, &result_tag);
    } else if (lhs_t->tag == BoolType && rhs_t->tag == BoolType) {
        switch (op) {
        case GCC_BINOP_BITWISE_AND: return gcc_binary_op(env->ctx, loc, GCC_BINOP_LOGICAL_AND, sss_type_to_gcc(env, lhs_t), lhs, rhs);
        case GCC_BINOP_BITWISE_OR: return gcc_binary_op(env->ctx, loc, GCC_BINOP_LOGICAL_OR, sss_type_to_gcc(env, lhs_t), lhs, rhs);
        case GCC_BINOP_BITWISE_XOR: return gcc_binary_op(env->ctx, loc, GCC_BINOP_BITWISE_XOR, sss_type_to_gcc(env, lhs_t), lhs, rhs);
        default: compiler_err(env, ast, "I don't know how to do math operations on boolean values.");
        }
    } else {
        if (!is_numeric(lhs_t) || !is_numeric(rhs_t))
            compiler_err(env, ast, "I don't know how to do math operations between %T and %T", lhs_t, rhs_t);

        sss_type_t *result_t;
        if (promote(env, with_units(lhs_t, NULL), &lhs, with_units(rhs_t, NULL)))
            result_t = rhs_t;
        else if (promote(env, with_units(rhs_t, NULL), &rhs, with_units(lhs_t, NULL)))
            result_t = lhs_t;
        else
            compiler_err(env, ast, "The result of a math operation between %T and %T can't always fit in either type.", lhs_t, rhs_t);

        return gcc_binary_op(env->ctx, loc, op, sss_type_to_gcc(env, result_t), lhs, rhs);
    }

    auto struct_ = Match(struct_t, StructType);
    gcc_type_t *gcc_t = sss_type_to_gcc(env, struct_t);
    gcc_struct_t *struct_gcc_t = gcc_type_if_struct(gcc_t);
    NEW_LIST(gcc_field_t*, fields);
    NEW_LIST(gcc_rvalue_t*, members);
    for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
        gcc_field_t *field = gcc_get_field(struct_gcc_t, i);
        APPEND(fields, field);
        sss_type_t *field_t = ith(struct_->field_types, i);

        sss_type_t *lhs_field_t, *rhs_field_t;
        gcc_rvalue_t *lhs_field_val, *rhs_field_val;
        if (lhs_t->tag == StructType) {
            lhs_field_val = gcc_rvalue_access_field(lhs, loc, field);
            lhs_field_t = field_t;
        } else {
            lhs_field_val = lhs;
            lhs_field_t = lhs_t;
        }

        if (rhs_t->tag == StructType) {
            rhs_field_val = gcc_rvalue_access_field(rhs, loc, field);
            rhs_field_t = field_t;
        } else {
            rhs_field_val = rhs;
            rhs_field_t = rhs_t;
        }

        gcc_rvalue_t *member = math_binop_rec(env, block, ast, lhs_field_t, lhs_field_val, op, rhs_field_t, rhs_field_val);
        // Just in case things get messed up with promotions:
        if (is_numeric(field_t))
            member = gcc_cast(env->ctx, loc, member, sss_type_to_gcc(env, field_t));
        APPEND(members, member);
    }
    return gcc_struct_constructor(env->ctx, loc, gcc_t, length(fields), fields[0], members[0]);
}

gcc_rvalue_t *math_binop(env_t *env, gcc_block_t **block, ast_t *ast)
{
    gcc_binary_op_e op;
    switch (ast->tag) {
    case Add: op = GCC_BINOP_PLUS; break;
    case Subtract: op = GCC_BINOP_MINUS; break;
    case Multiply: op = GCC_BINOP_MULT; break;
    case Divide: op = GCC_BINOP_DIVIDE; break;
    case And: op = GCC_BINOP_BITWISE_AND; break;
    case Or: op = GCC_BINOP_BITWISE_OR; break;
    case Xor: op = GCC_BINOP_BITWISE_XOR; break;
    default: compiler_err(env, ast, "Unsupported math operation");
    }

    // Unsafe! This assumes each of these types has the same tagged union struct layout. It saves some duplicated code.
    ast_t *lhs = ast->__data.Add.lhs, *rhs = ast->__data.Add.rhs;
    // End unsafe

    gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
    gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
    return math_binop_rec(env, block, ast, get_type(env, lhs), lhs_val, op, get_type(env, rhs), rhs_val);
}

void math_update_rec(
    env_t *env, gcc_block_t **block, ast_t *ast, sss_type_t *lhs_t, gcc_lvalue_t *lhs,
    gcc_binary_op_e op, sss_type_t *rhs_t, gcc_rvalue_t *rhs)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, lhs_t);
    gcc_loc_t *loc = ast_loc(env, ast);

    // Automatically dereference non-nil pointers:
    if (lhs_t->tag == PointerType) {
        auto ptr = Match(lhs_t, PointerType);
        if (ptr->is_optional)
            compiler_err(env, ast, "The left hand side of this operation contains a potentially nil pointer that can't be safely dereferenced.");

        return math_update_rec(env, block, ast, ptr->pointed, gcc_rvalue_dereference(gcc_rval(lhs), ast_loc(env, ast)), op, rhs_t, rhs);
    }

    // Automatically dereference non-nil pointers:
    if (rhs_t->tag == PointerType) {
        auto ptr = Match(rhs_t, PointerType);
        if (ptr->is_optional)
            compiler_err(env, ast, "The right hand side of this operation contains a potentially nil pointer that can't be safely dereferenced.");
        return math_update_rec(env, block, ast, lhs_t, lhs, op, ptr->pointed, gcc_rval(gcc_rvalue_dereference(rhs, ast_loc(env, ast))));
    }

    if (lhs_t->tag == VariantType)
        return math_update_rec(env, block, ast, Match(lhs_t, VariantType)->variant_of, lhs, op, rhs_t, rhs);
    else if (rhs_t->tag == VariantType)
        return math_update_rec(env, block, ast, lhs_t, lhs, op, Match(rhs_t, VariantType)->variant_of, rhs);

    if (type_units(rhs_t) && (op == GCC_BINOP_MULT || op == GCC_BINOP_DIVIDE))
        compiler_err(env, ast, "I can't do this math operation because it would change the left hand side's units");
    else if (!streq(type_units(lhs_t), type_units(rhs_t)) && (op == GCC_BINOP_PLUS || op == GCC_BINOP_MINUS))
        compiler_err(env, ast, "I can't do this math operation because it requires math operations between incompatible units: %T and %T", lhs_t, rhs_t);

    gcc_type_t *i32 = gcc_type(env->ctx, INT32);
    if (lhs_t->tag == ArrayType && rhs_t->tag == ArrayType) {
        check_cow(env, block, lhs_t, gcc_lvalue_address(lhs, loc));

        // Use the minimum common length:
        // [1,2,3] += [10,20] ==> [11,22,3]
        // [1,2,3] += [10,20,30,40] ==> [11,22,33]

        // Pseudocode:
        // len = MIN(lhs->len, rhs->len)
        // for (i = 0; i < len; i++)
        //     update(&lhs->data[i], &rhs->data[i]);

        gcc_type_t *lhs_gcc_t = sss_type_to_gcc(env, lhs_t);
        gcc_struct_t *lhs_array_struct = gcc_type_if_struct(lhs_gcc_t);
        gcc_rvalue_t *lhs_len32 = gcc_rvalue_access_field(gcc_rval(lhs), loc, gcc_get_field(lhs_array_struct, ARRAY_LENGTH_FIELD));
        gcc_rvalue_t *lhs_stride16 = gcc_rvalue_access_field(gcc_rval(lhs), loc, gcc_get_field(lhs_array_struct, ARRAY_STRIDE_FIELD));

        gcc_type_t *rhs_gcc_t = sss_type_to_gcc(env, rhs_t);
        gcc_struct_t *rhs_array_struct = gcc_type_if_struct(rhs_gcc_t);
        gcc_rvalue_t *rhs_len32 = gcc_rvalue_access_field(rhs, loc, gcc_get_field(rhs_array_struct, ARRAY_LENGTH_FIELD));
        gcc_rvalue_t *rhs_stride16 = gcc_rvalue_access_field(rhs, loc, gcc_get_field(rhs_array_struct, ARRAY_STRIDE_FIELD));

        gcc_rvalue_t *len = ternary(block,
                                    gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LE, lhs_len32, rhs_len32),
                                    i32, lhs_len32, rhs_len32);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_block_t *loop_condition = gcc_new_block(func, fresh("loop_condition")),
                    *loop_body = gcc_new_block(func, fresh("loop_body")),
                    *loop_end = gcc_new_block(func, fresh("loop_end"));

        gcc_lvalue_t *offset = gcc_local(func, NULL, i32, "_offset");
        gcc_assign(*block, NULL, offset, gcc_zero(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);

        gcc_jump_condition(loop_condition, loc, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(offset), len),
                           loop_body, loop_end);

        *block = loop_body;
#define ITEM(t, item_ptr, stride) gcc_rvalue_dereference(pointer_offset(env, sss_type_to_gcc(env, Type(PointerType, .pointed=Match(t, ArrayType)->item_type)), item_ptr, gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, i32, gcc_rval(offset), gcc_cast(env->ctx, NULL, stride, i32))), NULL)
        gcc_rvalue_t *lhs_item_ptr = gcc_rvalue_access_field(gcc_rval(lhs), NULL, gcc_get_field(lhs_array_struct, ARRAY_DATA_FIELD));
        gcc_lvalue_t *lhs_item = ITEM(lhs_t, lhs_item_ptr, lhs_stride16);
        gcc_rvalue_t *rhs_item_ptr = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(rhs_array_struct, ARRAY_DATA_FIELD));
        gcc_rvalue_t *rhs_item = gcc_rval(ITEM(rhs_t, rhs_item_ptr, rhs_stride16));

        math_update_rec(env, block, ast, Match(lhs_t, ArrayType)->item_type, lhs_item,
                        op, Match(rhs_t, ArrayType)->item_type, rhs_item);
        gcc_update(*block, NULL, offset, GCC_BINOP_PLUS, gcc_one(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);
        *block = loop_end;
    } else if (lhs_t->tag == ArrayType) {
        check_cow(env, block, lhs_t, gcc_lvalue_address(lhs, loc));

        // Pseudocode:
        // for (i = 0; i < lhs->len; i++)
        //     update(&lhs->data[i], rhs)
        gcc_type_t *lhs_gcc_t = sss_type_to_gcc(env, lhs_t);
        gcc_struct_t *lhs_array_struct = gcc_type_if_struct(lhs_gcc_t);
        gcc_rvalue_t *len = gcc_rvalue_access_field(gcc_rval(lhs), loc, gcc_get_field(lhs_array_struct, ARRAY_LENGTH_FIELD));
        gcc_rvalue_t *stride = gcc_rvalue_access_field(gcc_rval(lhs), loc, gcc_get_field(lhs_array_struct, ARRAY_STRIDE_FIELD));

        gcc_func_t *func = gcc_block_func(*block);
        gcc_block_t *loop_condition = gcc_new_block(func, fresh("loop_condition")),
                    *loop_body = gcc_new_block(func, fresh("loop_body")),
                    *loop_end = gcc_new_block(func, fresh("loop_end"));

        gcc_lvalue_t *offset = gcc_local(func, NULL, i32, "_offset");
        gcc_assign(*block, NULL, offset, gcc_zero(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);

        gcc_jump_condition(loop_condition, loc, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(offset), len),
                           loop_body, loop_end);

        *block = loop_body;
        gcc_rvalue_t *lhs_item_ptr = gcc_rvalue_access_field(gcc_rval(lhs), NULL, gcc_get_field(lhs_array_struct, ARRAY_DATA_FIELD));
        gcc_lvalue_t *lhs_item = ITEM(lhs_t, lhs_item_ptr, stride);
#undef ITEM

        math_update_rec(env, block, ast, Match(lhs_t, ArrayType)->item_type, lhs_item, op, rhs_t, rhs);
        gcc_update(*block, NULL, offset, GCC_BINOP_PLUS, gcc_one(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);
        *block = loop_end;
    } else if (lhs_t->tag == StructType && rhs_t->tag == StructType) {
        if (!type_eq(with_units(lhs_t, NULL), with_units(rhs_t, NULL)))
            compiler_err(env, ast, "I can't do this math operation because it requires math operations between incompatible types: %T and %T", lhs_t, rhs_t);

        auto struct_ = Match(lhs_t, StructType);
        gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
        for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
            gcc_field_t *field = gcc_get_field(struct_t, i);
            sss_type_t *field_t = ith(struct_->field_types, i);
            math_update_rec(
                env, block, ast, field_t, gcc_lvalue_access_field(lhs, loc, field),
                op, field_t, gcc_rvalue_access_field(rhs, loc, field));
        }
    } else if (lhs_t->tag == StructType) {
        auto struct_ = Match(lhs_t, StructType);
        gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
        for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
            gcc_field_t *field = gcc_get_field(struct_t, i);
            sss_type_t *field_t = ith(struct_->field_types, i);
            gcc_rvalue_t *rhs_field = rhs_t->tag == StructType ? gcc_rvalue_access_field(rhs, loc, field) : rhs;
            sss_type_t *rhs_field_t = rhs_t->tag == StructType ? field_t : rhs_t;
            math_update_rec(
                env, block, ast, with_units(field_t, type_units(lhs_t)), gcc_lvalue_access_field(lhs, loc, field),
                op, rhs_field_t, rhs_field);
        }
    } else if (is_numeric(lhs_t) && is_numeric(rhs_t)) {
        if (!promote(env, with_units(rhs_t, NULL), &rhs, with_units(lhs_t, NULL)))
            compiler_err(env, ast, "I can't automatically convert from %T to %T without losing precision", rhs_t, lhs_t);
        if ((op == GCC_BINOP_BITWISE_AND || op == GCC_BINOP_BITWISE_OR || op == GCC_BINOP_BITWISE_XOR) && lhs_t->tag == NumType)
            compiler_err(env, ast, "Bitwise operations cannot be performed in floating point numbers.");
        return gcc_update(*block, loc, lhs, op, rhs);
    } else if (lhs_t->tag == BoolType && rhs_t->tag == BoolType) {
        switch (op) {
        case GCC_BINOP_BITWISE_AND: return gcc_update(*block, loc, lhs, GCC_BINOP_LOGICAL_AND, rhs);
        case GCC_BINOP_BITWISE_OR: return gcc_update(*block, loc, lhs, GCC_BINOP_LOGICAL_OR, rhs);
        case GCC_BINOP_BITWISE_XOR: return gcc_update(*block, loc, lhs, GCC_BINOP_BITWISE_XOR, rhs);
        default: compiler_err(env, ast, "I don't know how to do math operations on boolean values.");
        }
    } else {
        compiler_err(env, ast, "I don't know how to do math update operations between %T and %T", lhs_t, rhs_t);
    }
}

gcc_rvalue_t *math_update(env_t *env, gcc_block_t **block, ast_t *ast)
{
    // Unsafe! This assumes each of these types has the same tagged union struct layout. It saves some duplicated code.
    ast_t *lhs = ast->__data.AddUpdate.lhs, *rhs = ast->__data.AddUpdate.rhs;
    // End unsafe

    gcc_lvalue_t *lhs_val = get_lvalue(env, block, lhs, true);
    gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);

    gcc_binary_op_e op;
    switch (ast->tag) {
    case AddUpdate: op = GCC_BINOP_PLUS; break;
    case SubtractUpdate: op = GCC_BINOP_MINUS; break;
    case MultiplyUpdate: op = GCC_BINOP_MULT; break;
    case DivideUpdate: op = GCC_BINOP_DIVIDE; break;
    case AndUpdate: op = GCC_BINOP_BITWISE_AND; break;
    case OrUpdate: op = GCC_BINOP_BITWISE_OR; break;
    case XorUpdate: op = GCC_BINOP_BITWISE_XOR; break;
    default: compiler_err(env, ast, "Unsupported math operation");
    }

    math_update_rec(env, block, ast, get_type(env, lhs), lhs_val, op, get_type(env, rhs), rhs_val);
    return gcc_rval(lhs_val);
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
