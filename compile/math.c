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

    bl_type_t *struct_t = NULL;
    if (lhs_t->tag == StructType && rhs_t->tag == StructType) {
        if (lhs_t != rhs_t) compile_err(env, ast, "I don't know how to do math operations between %s and %s", type_to_string(lhs_t), type_to_string(rhs_t));
        struct_t = lhs_t;
    } else if (lhs_t->tag == StructType) {
        struct_t = lhs_t;
    } else if (rhs_t->tag == StructType) {
        struct_t = rhs_t;
    } else {
        if (!is_numeric(lhs_t) || !is_numeric(rhs_t))
            compile_err(env, ast, "I don't know how to do math operations between %s and %s",
                        type_to_string(lhs_t), type_to_string(rhs_t));

        if (promote(env, rhs_t, &rhs, lhs_t))
            return gcc_binary_op(env->ctx, loc, op, bl_type_to_gcc(env, lhs_t), lhs, rhs);
        else if (promote(env, lhs_t, &lhs, rhs_t))
            return gcc_binary_op(env->ctx, loc, op, bl_type_to_gcc(env, rhs_t), lhs, rhs);
        else
            compile_err(env, ast, "This math operation is between %s and %s, and I can't figure out how to do that without losing precision.",
                        type_to_string(lhs_t), type_to_string(rhs_t));
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
    default: compile_err(env, ast, "Unsupported math operation");
    }

    // Unsafe! This assumes each of these types has the same tagged union struct layout. It saves some duplicated code.
    ast_t *lhs = ast->__data.Add.lhs, *rhs = ast->__data.Add.rhs;
    // End unsafe

    gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
    gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
    return math_binop_rec(env, block, ast, get_type(env, lhs), lhs_val, op, get_type(env, rhs), rhs_val);
}

static void math_update_rec(
    env_t *env, gcc_block_t **block, ast_t *ast, bl_type_t *lhs_t, gcc_lvalue_t *lhs,
    gcc_binary_op_e op, bl_type_t *rhs_t, gcc_rvalue_t *rhs)
{
    gcc_type_t *gcc_t = bl_type_to_gcc(env, lhs_t);
    gcc_loc_t *loc = ast_loc(env, ast);

    if (lhs_t->tag == StructType && rhs_t->tag == StructType && lhs_t != rhs_t)
        compile_err(env, ast, "I don't know how to do math operations between %s and %s",
                    type_to_string(lhs_t), type_to_string(rhs_t));

    if (lhs_t->tag == StructType) {
        auto struct_ = Match(lhs_t, StructType);
        gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
        for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
            gcc_field_t *field = gcc_get_field(struct_t, i);
            bl_type_t *field_t = ith(struct_->field_types, i);
            gcc_rvalue_t *rhs_field = rhs_t->tag == StructType ? gcc_rvalue_access_field(rhs, loc, field) : rhs;
            bl_type_t *rhs_field_t = rhs_t->tag == StructType ? field_t : rhs_t;
            math_update_rec(
                env, block, ast, field_t, gcc_lvalue_access_field(lhs, loc, field),
                op, rhs_field_t, rhs_field);
        }
    } else {
        if (!is_numeric(lhs_t) || !is_numeric(rhs_t))
            compile_err(env, ast, "I don't know how to do math operations between %s and %s",
                        type_to_string(lhs_t), type_to_string(rhs_t));
        if (!promote(env, rhs_t, &rhs, lhs_t))
            compile_err(env, ast, "I can't automatically convert from %s to %s without losing precision",
                        type_to_string(rhs_t), type_to_string(lhs_t));
        return gcc_update(*block, loc, lhs, op, rhs);
    }
}

void math_update(env_t *env, gcc_block_t **block, ast_t *ast)
{
    // Unsafe! This assumes each of these types has the same tagged union struct layout. It saves some duplicated code.
    ast_t *lhs = ast->__data.AddUpdate.lhs, *rhs = ast->__data.AddUpdate.rhs;
    // End unsafe

    gcc_lvalue_t *lhs_val = get_lvalue(env, block, lhs);
    gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);

    gcc_binary_op_e op;
    switch (ast->tag) {
    case AddUpdate: op = GCC_BINOP_PLUS; break;
    case SubtractUpdate: op = GCC_BINOP_MINUS; break;
    case MultiplyUpdate: op = GCC_BINOP_MULT; break;
    case DivideUpdate: op = GCC_BINOP_DIVIDE; break;
    default: compile_err(env, ast, "Unsupported math operation");
    }

    math_update_rec(env, block, ast, get_type(env, lhs), lhs_val, op, get_type(env, rhs), rhs_val);
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
