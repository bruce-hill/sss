// Math operations
#include <libgccjit.h>
#include <stdint.h>

#include "../ast.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

static gcc_rvalue_t *math_binop_rec(env_t *env, gcc_block_t **block, ast_t *ast, bl_type_t *t, gcc_rvalue_t *lhs, gcc_binary_op_e op, gcc_rvalue_t *rhs)
{
    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
    gcc_loc_t *loc = ast_loc(env, ast);

    switch (t->tag) {
    case StructType: {
        auto struct_ = Match(t, StructType);
        gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
        NEW_LIST(gcc_field_t*, fields);
        NEW_LIST(gcc_rvalue_t*, members);
        for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
            gcc_field_t *field = gcc_get_field(struct_t, i);
            APPEND(fields, field);
            gcc_rvalue_t *member = math_binop_rec(
                env, block, ast, ith(struct_->field_types, i),
                gcc_rvalue_access_field(lhs, loc, field),
                op,
                gcc_rvalue_access_field(rhs, loc, field));
            APPEND(members, member);
        }
        return gcc_struct_constructor(env->ctx, loc, gcc_t, length(fields), fields[0], members[0]);
    }
    case PointerType: {
        compile_err(env, ast, "Pointers don't support math operations, you have to explicitly use values");
    }
    case ArrayType: {
        compile_err(env, ast, "Arrays don't support math operations (yet). For now, you have to use an array comprehension");
    }
    default: {
        if (!is_numeric(t))
            compile_err(env, ast, "I don't know how to do math operations on %s", type_to_string(t));
        return gcc_binary_op(env->ctx, loc, op, gcc_t, lhs, rhs);
    }
    }
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

    bl_type_t *lhs_t = get_type(env, lhs);
    bl_type_t *rhs_t = get_type(env, rhs);

    gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
    gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);

    // Numeric promotion:
    bl_type_t *t = NULL;
    if (promote(env, lhs_t, &lhs_val, rhs_t))
        t = rhs_t;
    else if (promote(env, rhs_t, &rhs_val, lhs_t))
        t = lhs_t;
    else
        compile_err(env, ast, "The left hand side of this operation has type %s, but the right hand side has type %s and I can't figure out how to combine them.",
                    type_to_string(lhs_t), type_to_string(rhs_t));

    return math_binop_rec(env, block, ast, t, lhs_val, op, rhs_val);
}

static void math_update_rec(env_t *env, gcc_block_t **block, ast_t *ast, bl_type_t *t, gcc_lvalue_t *lhs, gcc_binary_op_e op, gcc_rvalue_t *rhs)
{
    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
    gcc_loc_t *loc = ast_loc(env, ast);

    switch (t->tag) {
    case StructType: {
        auto struct_ = Match(t, StructType);
        gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
        for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
            gcc_field_t *field = gcc_get_field(struct_t, i);
            math_update_rec(
                env, block, ast, ith(struct_->field_types, i),
                gcc_lvalue_access_field(lhs, loc, field),
                op,
                gcc_rvalue_access_field(rhs, loc, field));
        }
        break;
    }
    case PointerType: {
        compile_err(env, ast, "Pointers don't support math operations, you have to explicitly use values");
    }
    case ArrayType: {
        compile_err(env, ast, "Arrays don't support math operations (yet). For now, you have to use a loop");
    }
    default: {
        if (!is_numeric(t))
            compile_err(env, ast, "I don't know how to do math operations on %s", type_to_string(t));
        return gcc_update(*block, loc, lhs, op, rhs);
    }
    }
}

void math_update(env_t *env, gcc_block_t **block, ast_t *ast)
{
    // Unsafe! This assumes each of these types has the same tagged union struct layout. It saves some duplicated code.
    ast_t *lhs = ast->__data.AddUpdate.lhs, *rhs = ast->__data.AddUpdate.rhs;
    // End unsafe

    bl_type_t *lhs_t = get_type(env, lhs);
    bl_type_t *rhs_t = get_type(env, rhs);

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

    if (!promote(env, rhs_t, &rhs_val, lhs_t))
        compile_err(env, ast, "The left hand side of this assignment has type %s, but the right hand side has type %s and I can't figure out how to combine them without losing precision.",
                    type_to_string(lhs_t), type_to_string(rhs_t));
    math_update_rec(env, block, ast, lhs_t, lhs_val, op, rhs_val);
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
