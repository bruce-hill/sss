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
    bl_type_t *lhs_t, gcc_rvalue_t *lhs,
    gcc_binary_op_e op,
    bl_type_t *rhs_t, gcc_rvalue_t *rhs)
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
    gcc_func_t *func = gcc_block_func(*block);

    if (lhs_t->tag == ArrayType && rhs_t->tag == ArrayType) {
        // Use the minimum common length:
        // [1,2,3] += [10,20] ==> [11,22]
        // [1,2,3] += [10,20,30,40] ==> [11,22,33]

        // Pseudocode:
        // len = MIN(lhs->len, rhs->len)
        // result = alloc(len)
        // for (i = 0; i < len; i++)
        //     result->data[i] = lhs->data[i] {OP} &rhs->data[i]

        bl_type_t *result_t = get_math_type(env, ast, lhs_t, ast->tag, rhs_t);
        gcc_type_t *result_gcc_t = bl_type_to_gcc(env, result_t);
        gcc_struct_t *result_array_struct = gcc_type_if_struct(result_gcc_t);
        gcc_lvalue_t *result = gcc_local(func, loc, result_gcc_t, "_result");

        gcc_type_t *lhs_gcc_t = bl_type_to_gcc(env, lhs_t);
        gcc_struct_t *lhs_array_struct = gcc_type_if_struct(lhs_gcc_t);
        gcc_rvalue_t *lhs_len32 = gcc_rvalue_access_field(lhs, loc, gcc_get_field(lhs_array_struct, 1));
        gcc_rvalue_t *lhs_stride32 = gcc_rvalue_access_field(lhs, loc, gcc_get_field(lhs_array_struct, 2));

        gcc_type_t *rhs_gcc_t = bl_type_to_gcc(env, rhs_t);
        gcc_struct_t *rhs_array_struct = gcc_type_if_struct(rhs_gcc_t);
        gcc_rvalue_t *rhs_len32 = gcc_rvalue_access_field(rhs, loc, gcc_get_field(rhs_array_struct, 1));
        gcc_rvalue_t *rhs_stride32 = gcc_rvalue_access_field(rhs, loc, gcc_get_field(rhs_array_struct, 2));

        gcc_type_t *i32 = gcc_type(env->ctx, INT32);
        gcc_rvalue_t *len = ternary(block,
                                    gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LE, lhs_len32, rhs_len32),
                                    i32, lhs_len32, rhs_len32);

        bl_type_t *item_t = Match(result_t, ArrayType)->item_type;
        gcc_func_t *alloc_func = hget(env->global_funcs, has_heap_memory(item_t) ? "GC_malloc" : "GC_malloc_atomic", gcc_func_t*);
        gcc_type_t *gcc_item_ptr_t = bl_type_to_gcc(env, Type(PointerType, .pointed=item_t));
        gcc_type_t *gcc_size = gcc_type(env->ctx, SIZE);
        gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_size, (long)(gcc_sizeof(env, item_t)));
        size = gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, gcc_size, size, gcc_cast(env->ctx, loc, len, gcc_size));
        gcc_rvalue_t *initial_items = gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, size), gcc_item_ptr_t);
        gcc_assign(*block, loc, result, gcc_struct_constructor(
                env->ctx, loc, result_gcc_t, 3,
                (gcc_field_t*[]){gcc_get_field(result_array_struct, 0), gcc_get_field(result_array_struct, 1), gcc_get_field(result_array_struct, 2)},
                (gcc_rvalue_t*[]){initial_items, len, gcc_one(env->ctx, gcc_type(env->ctx, INT32))}));

        gcc_block_t *loop_condition = gcc_new_block(func, fresh("loop_condition")),
                    *loop_body = gcc_new_block(func, fresh("loop_body")),
                    *loop_end = gcc_new_block(func, fresh("loop_end"));

        gcc_lvalue_t *offset = gcc_local(func, NULL, i32, "_offset");
        gcc_assign(*block, NULL, offset, gcc_zero(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);

        gcc_jump_condition(loop_condition, loc, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(offset), len),
                           loop_body, loop_end);

        *block = loop_body;
#define ITEM(item_ptr, stride) gcc_array_access(env->ctx, NULL, item_ptr, gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, i32, gcc_rval(offset), stride))
        gcc_rvalue_t *lhs_item_ptr = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(lhs_array_struct, 0));
        gcc_rvalue_t *lhs_item = gcc_rval(ITEM(lhs_item_ptr, lhs_stride32));
        gcc_rvalue_t *rhs_item_ptr = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(rhs_array_struct, 0));
        gcc_rvalue_t *rhs_item = gcc_rval(ITEM(rhs_item_ptr, rhs_stride32));

        gcc_rvalue_t *result_item_ptr = gcc_rvalue_access_field(gcc_rval(result), NULL, gcc_get_field(result_array_struct, 0));
        gcc_lvalue_t *result_item = ITEM(result_item_ptr, gcc_one(env->ctx, i32));

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
        bl_type_t *array_t, *scalar_t;
        bool scalar_left = (lhs_t->tag != ArrayType);
        if (scalar_left) {
            array = rhs, scalar = lhs;
            array_t = rhs_t, scalar_t = lhs_t;
        } else {
            array = lhs, scalar = rhs;
            array_t = lhs_t, scalar_t = rhs_t;
        }
        bl_type_t *result_t = scalar_left ? get_math_type(env, ast, scalar_t, ast->tag, array_t) : get_math_type(env, ast, array_t, ast->tag, scalar_t);
        gcc_type_t *result_gcc_t = bl_type_to_gcc(env, result_t);
        gcc_struct_t *result_array_struct = gcc_type_if_struct(result_gcc_t);
        gcc_lvalue_t *result = gcc_local(func, loc, result_gcc_t, "_result");

        gcc_type_t *array_gcc_t = bl_type_to_gcc(env, array_t);
        gcc_struct_t *array_struct = gcc_type_if_struct(array_gcc_t);
        gcc_rvalue_t *len = gcc_rvalue_access_field(array, loc, gcc_get_field(array_struct, 1));
        gcc_rvalue_t *stride = gcc_rvalue_access_field(array, loc, gcc_get_field(array_struct, 2));

        gcc_type_t *i32 = gcc_type(env->ctx, INT32);
        gcc_block_t *loop_condition = gcc_new_block(func, fresh("loop_condition")),
                    *loop_body = gcc_new_block(func, fresh("loop_body")),
                    *loop_end = gcc_new_block(func, fresh("loop_end"));

        bl_type_t *item_t = Match(result_t, ArrayType)->item_type;
        gcc_func_t *alloc_func = hget(env->global_funcs, has_heap_memory(item_t) ? "GC_malloc" : "GC_malloc_atomic", gcc_func_t*);
        gcc_type_t *gcc_item_ptr_t = bl_type_to_gcc(env, Type(PointerType, .pointed=item_t));
        gcc_type_t *gcc_size = gcc_type(env->ctx, SIZE);
        gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_size, (long)(gcc_sizeof(env, item_t)));
        size = gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, gcc_size, size, gcc_cast(env->ctx, loc, len, gcc_size));
        gcc_rvalue_t *initial_items = gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, size), gcc_item_ptr_t);
        gcc_assign(*block, loc, result, gcc_struct_constructor(
                env->ctx, loc, result_gcc_t, 3,
                (gcc_field_t*[]){gcc_get_field(result_array_struct, 0), gcc_get_field(result_array_struct, 1), gcc_get_field(result_array_struct, 2)},
                (gcc_rvalue_t*[]){initial_items, len, gcc_one(env->ctx, gcc_type(env->ctx, INT32))}));

        gcc_lvalue_t *offset = gcc_local(func, NULL, i32, "_offset");
        gcc_assign(*block, NULL, offset, gcc_zero(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);

        gcc_jump_condition(loop_condition, loc, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(offset), len),
                           loop_body, loop_end);

        *block = loop_body;
        gcc_rvalue_t *array_item_ptr = gcc_rvalue_access_field(array, NULL, gcc_get_field(array_struct, 0));
        gcc_rvalue_t *array_item = gcc_rval(ITEM(array_item_ptr, stride));

        gcc_rvalue_t *result_item_ptr = gcc_rvalue_access_field(gcc_rval(result), NULL, gcc_get_field(result_array_struct, 0));
        gcc_lvalue_t *result_item = ITEM(result_item_ptr, gcc_one(env->ctx, i32));

        bl_type_t *array_item_t = Match(array_t, ArrayType)->item_type;
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

    bl_type_t *struct_t = NULL;
    if (lhs_t->tag == StructType && rhs_t->tag == StructType) {
        if (!type_eq(lhs_t, rhs_t)) compiler_err(env, ast, "I don't know how to do math operations between %s and %s", type_to_string(lhs_t), type_to_string(rhs_t));
        struct_t = lhs_t;
    } else if (lhs_t->tag == StructType) {
        struct_t = lhs_t;
    } else if (rhs_t->tag == StructType) {
        struct_t = rhs_t;
    } else {
        if (!is_numeric(lhs_t) || !is_numeric(rhs_t))
            compiler_err(env, ast, "I don't know how to do math operations between %s and %s",
                        type_to_string(lhs_t), type_to_string(rhs_t));

        if (numtype_priority(lhs_t) < numtype_priority(rhs_t)) {
            lhs = gcc_cast(env->ctx, NULL, lhs, bl_type_to_gcc(env, rhs_t));
            lhs_t = rhs_t;
        } else if (numtype_priority(lhs_t) > numtype_priority(rhs_t)) {
            rhs = gcc_cast(env->ctx, NULL, rhs, bl_type_to_gcc(env, lhs_t));
        }
        return gcc_binary_op(env->ctx, loc, op, bl_type_to_gcc(env, lhs_t), lhs, rhs);
    }

    auto struct_ = Match(struct_t, StructType);
    gcc_type_t *gcc_t = bl_type_to_gcc(env, struct_t);
    gcc_struct_t *struct_gcc_t = gcc_type_if_struct(gcc_t);
    NEW_LIST(gcc_field_t*, fields);
    NEW_LIST(gcc_rvalue_t*, members);
    for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
        gcc_field_t *field = gcc_get_field(struct_gcc_t, i);
        APPEND(fields, field);
        bl_type_t *field_t = ith(struct_->field_types, i);

        bl_type_t *lhs_field_t, *rhs_field_t;
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
            member = gcc_cast(env->ctx, loc, member, bl_type_to_gcc(env, field_t));
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
    env_t *env, gcc_block_t **block, ast_t *ast, bl_type_t *lhs_t, gcc_lvalue_t *lhs,
    gcc_binary_op_e op, bl_type_t *rhs_t, gcc_rvalue_t *rhs)
{
    gcc_type_t *gcc_t = bl_type_to_gcc(env, lhs_t);
    gcc_loc_t *loc = ast_loc(env, ast);

    // Automatically dereference non-nil pointers:
    if (lhs_t->tag == PointerType) {
        auto ptr = Match(lhs_t, PointerType);
        if (ptr->is_optional)
            compiler_err(env, ast, "The left hand side of this operation contains a potentially nil pointer that can't be safely dereferenced.");

        if (Match(lhs_t, PointerType)->pointed->tag == ArrayType)
            check_cow(env, block, Match(lhs_t, PointerType)->pointed, gcc_rval(lhs));

        return math_update_rec(env, block, ast, ptr->pointed, gcc_rvalue_dereference(gcc_rval(lhs), ast_loc(env, ast)), op, rhs_t, rhs);
    }

    // Automatically dereference non-nil pointers:
    if (rhs_t->tag == PointerType) {
        auto ptr = Match(rhs_t, PointerType);
        if (ptr->is_optional)
            compiler_err(env, ast, "The right hand side of this operation contains a potentially nil pointer that can't be safely dereferenced.");
        return math_update_rec(env, block, ast, lhs_t, lhs, op, ptr->pointed, gcc_rval(gcc_rvalue_dereference(rhs, ast_loc(env, ast))));
    }

    if (type_units(rhs_t) && (op == GCC_BINOP_MULT || op == GCC_BINOP_DIVIDE))
        compiler_err(env, ast, "I can't do this math operation because it would change the left hand side's units");
    else if (!streq(type_units(lhs_t), type_units(rhs_t)) && (op == GCC_BINOP_PLUS || op == GCC_BINOP_MINUS))
        compiler_err(env, ast, "I can't do this math operation because it requires math operations between incompatible units: %s and %s",
                    type_to_string(lhs_t), type_to_string(rhs_t));

    if (lhs_t->tag == ArrayType && rhs_t->tag == ArrayType) {
        // Use the minimum common length:
        // [1,2,3] += [10,20] ==> [11,22,3]
        // [1,2,3] += [10,20,30,40] ==> [11,22,33]

        // Pseudocode:
        // len = MIN(lhs->len, rhs->len)
        // for (i = 0; i < len; i++)
        //     update(&lhs->data[i], &rhs->data[i]);

        gcc_type_t *lhs_gcc_t = bl_type_to_gcc(env, lhs_t);
        gcc_struct_t *lhs_array_struct = gcc_type_if_struct(lhs_gcc_t);
        gcc_rvalue_t *lhs_len32 = gcc_rvalue_access_field(gcc_rval(lhs), loc, gcc_get_field(lhs_array_struct, 1));
        gcc_rvalue_t *lhs_stride32 = gcc_rvalue_access_field(gcc_rval(lhs), loc, gcc_get_field(lhs_array_struct, 2));

        gcc_type_t *rhs_gcc_t = bl_type_to_gcc(env, rhs_t);
        gcc_struct_t *rhs_array_struct = gcc_type_if_struct(rhs_gcc_t);
        gcc_rvalue_t *rhs_len32 = gcc_rvalue_access_field(rhs, loc, gcc_get_field(rhs_array_struct, 1));
        gcc_rvalue_t *rhs_stride32 = gcc_rvalue_access_field(rhs, loc, gcc_get_field(rhs_array_struct, 2));

        gcc_type_t *i32 = gcc_type(env->ctx, INT32);
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
#define ITEM(item_ptr, stride) gcc_array_access(env->ctx, NULL, item_ptr, gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, i32, gcc_rval(offset), stride))
        gcc_rvalue_t *lhs_item_ptr = gcc_rvalue_access_field(gcc_rval(lhs), NULL, gcc_get_field(lhs_array_struct, 0));
        gcc_lvalue_t *lhs_item = ITEM(lhs_item_ptr, lhs_stride32);
        gcc_rvalue_t *rhs_item_ptr = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(rhs_array_struct, 0));
        gcc_rvalue_t *rhs_item = gcc_rval(ITEM(rhs_item_ptr, rhs_stride32));

        math_update_rec(env, block, ast, Match(lhs_t, ArrayType)->item_type, lhs_item,
                        op, Match(rhs_t, ArrayType)->item_type, rhs_item);
        gcc_update(*block, NULL, offset, GCC_BINOP_PLUS, gcc_one(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);
        *block = loop_end;
    } else if (lhs_t->tag == ArrayType) {
        // Pseudocode:
        // for (i = 0; i < lhs->len; i++)
        //     update(&lhs->data[i], rhs)
        gcc_type_t *lhs_gcc_t = bl_type_to_gcc(env, lhs_t);
        gcc_struct_t *lhs_array_struct = gcc_type_if_struct(lhs_gcc_t);
        gcc_rvalue_t *len = gcc_rvalue_access_field(gcc_rval(lhs), loc, gcc_get_field(lhs_array_struct, 1));
        gcc_rvalue_t *stride = gcc_rvalue_access_field(gcc_rval(lhs), loc, gcc_get_field(lhs_array_struct, 2));

        gcc_type_t *i32 = gcc_type(env->ctx, INT32);
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
        gcc_rvalue_t *lhs_item_ptr = gcc_rvalue_access_field(gcc_rval(lhs), NULL, gcc_get_field(lhs_array_struct, 0));
        gcc_lvalue_t *lhs_item = ITEM(lhs_item_ptr, stride);
#undef ITEM

        math_update_rec(env, block, ast, Match(lhs_t, ArrayType)->item_type, lhs_item, op, rhs_t, rhs);
        gcc_update(*block, NULL, offset, GCC_BINOP_PLUS, gcc_one(env->ctx, i32));
        gcc_jump(*block, NULL, loop_condition);
        *block = loop_end;
    } else if (lhs_t->tag == StructType && rhs_t->tag == StructType) {
        if (!type_eq(with_units(lhs_t, NULL), with_units(rhs_t, NULL)))
            compiler_err(env, ast, "I can't do this math operation because it requires math operations between incompatible types: %s and %s",
                        type_to_string(lhs_t), type_to_string(rhs_t));

        auto struct_ = Match(lhs_t, StructType);
        gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
        for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
            gcc_field_t *field = gcc_get_field(struct_t, i);
            bl_type_t *field_t = ith(struct_->field_types, i);
            math_update_rec(
                env, block, ast, field_t, gcc_lvalue_access_field(lhs, loc, field),
                op, field_t, gcc_rvalue_access_field(rhs, loc, field));
        }
    } else if (lhs_t->tag == StructType) {
        auto struct_ = Match(lhs_t, StructType);
        gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
        for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
            gcc_field_t *field = gcc_get_field(struct_t, i);
            bl_type_t *field_t = ith(struct_->field_types, i);
            gcc_rvalue_t *rhs_field = rhs_t->tag == StructType ? gcc_rvalue_access_field(rhs, loc, field) : rhs;
            bl_type_t *rhs_field_t = rhs_t->tag == StructType ? field_t : rhs_t;
            math_update_rec(
                env, block, ast, with_units(field_t, type_units(lhs_t)), gcc_lvalue_access_field(lhs, loc, field),
                op, rhs_field_t, rhs_field);
        }
    } else if (is_numeric(lhs_t) && is_numeric(rhs_t)) {
        if (numtype_priority(lhs_t) > numtype_priority(rhs_t))
            rhs = gcc_cast(env->ctx, NULL, rhs, bl_type_to_gcc(env, lhs_t));
        else if (numtype_priority(lhs_t) < numtype_priority(rhs_t))
            compiler_err(env, ast, "I can't automatically convert from %s to %s without losing precision",
                        type_to_string(rhs_t), type_to_string(lhs_t));
        return gcc_update(*block, loc, lhs, op, rhs);
    } else {
        compiler_err(env, ast, "I don't know how to do math update operations between %s and %s",
                    type_to_string(lhs_t), type_to_string(rhs_t));
    }
}

gcc_rvalue_t *math_update(env_t *env, gcc_block_t **block, ast_t *ast)
{
    // Unsafe! This assumes each of these types has the same tagged union struct layout. It saves some duplicated code.
    ast_t *lhs = ast->__data.AddUpdate.lhs, *rhs = ast->__data.AddUpdate.rhs;
    // End unsafe

    // switch (get_type(env, lhs)->tag) {
    // case ArrayType: case TableType:
    //     compiler_err(env, lhs, "This is an immutable value and it can't be updated");
    // default: break;
    // }
    gcc_lvalue_t *lhs_val = get_lvalue(env, block, lhs, true);
    gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);

    gcc_binary_op_e op;
    switch (ast->tag) {
    case AddUpdate: op = GCC_BINOP_PLUS; break;
    case SubtractUpdate: op = GCC_BINOP_MINUS; break;
    case MultiplyUpdate: op = GCC_BINOP_MULT; break;
    case DivideUpdate: op = GCC_BINOP_DIVIDE; break;
    default: compiler_err(env, ast, "Unsupported math operation");
    }

    math_update_rec(env, block, ast, get_type(env, lhs), lhs_val, op, get_type(env, rhs), rhs_val);
    return gcc_rval(lhs_val);
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
