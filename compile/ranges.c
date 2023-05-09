// Logic for compiling SSS ranges (`1..10`)
#include <assert.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <gc/cord.h>
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

gcc_rvalue_t *compile_range(env_t *env, gcc_block_t **block, ast_t *ast)
{
    gcc_loc_t *loc = ast_loc(env, ast);
    gcc_type_t *range_t = sss_type_to_gcc(env, Type(RangeType));
    gcc_struct_t *range_struct = gcc_type_if_struct(range_t);
    assert(range_struct);
    auto range = Match(ast, Range);
    gcc_rvalue_t *values[] = {
        range->first ? compile_expr(env, block, range->first) : gcc_int64(env->ctx, INT64_MIN),
        range->step ? compile_expr(env, block, range->step) : gcc_int64(env->ctx, 1),
        range->last ? compile_expr(env, block, range->last) : gcc_int64(env->ctx, INT64_MAX),
    };
    gcc_rvalue_t *range_val = gcc_struct_constructor(env->ctx, loc, range_t, 3, NULL, values);
    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *range_var = gcc_local(func, loc, range_t, "_range");
    gcc_assign(*block, loc, range_var, range_val);
    gcc_block_t *if_zero = gcc_new_block(func, fresh("zero_step")),
                *done = gcc_new_block(func, fresh("done"));
    gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ,
                                                   gcc_rvalue_access_field(gcc_rval(range_var), loc, gcc_get_field(range_struct, 1)),
                                                   gcc_zero(env->ctx, gcc_type(env->ctx, INT64))),
                       if_zero, done);

    insert_failure(env, &if_zero, &ast->span, "This range was created with a step of zero, which is not allowed");
    
    *block = done;
    return gcc_rval(range_var);
}

gcc_rvalue_t *range_len(env_t *env, gcc_type_t *gcc_t, gcc_rvalue_t *range)
{
    gcc_struct_t *range_struct = gcc_type_if_struct(gcc_t);
    assert(range_struct);
    gcc_rvalue_t *first = gcc_rvalue_access_field(range, NULL, gcc_get_field(range_struct, 0)),
                 *step = gcc_rvalue_access_field(range, NULL, gcc_get_field(range_struct, 1)),
                 *last = gcc_rvalue_access_field(range, NULL, gcc_get_field(range_struct, 2));
    gcc_type_t *i64_t = gcc_type(env->ctx, INT64);
#define BINOP(a,op,b) gcc_binary_op(env->ctx, NULL, GCC_BINOP_ ## op, i64_t, a, b)
#define UNOP(op,a) gcc_unary_op(env->ctx, NULL, GCC_UNOP_ ## op, i64_t, a)
    // (last - first)//step + 1
    gcc_rvalue_t *len = BINOP(BINOP(BINOP(last, MINUS, first), DIVIDE, step), PLUS, gcc_one(env->ctx, i64_t));
    // If less than zero, set to zero (without a conditional branch)
    // len = len & ~(len >> 63)
    return BINOP(len, BITWISE_AND, UNOP(BITWISE_NEGATE, BINOP(len, RSHIFT, gcc_int64(env->ctx, 63))));
#undef BINOP
#undef UNOP
}

gcc_rvalue_t *range_contains(env_t *env, gcc_block_t **block, ast_t *range, ast_t *member)
{
    gcc_loc_t *loc = ast_loc(env, range);
    gcc_func_t *func = gcc_block_func(*block);

    sss_type_t *member_t = get_type(env, member);
    gcc_lvalue_t *member_var = gcc_local(func, loc, gcc_type(env->ctx, INT64), "_member");
    gcc_rvalue_t *member_val = compile_expr(env, block, member);
    if (!promote(env, member_t, &member_val, Type(IntType, .bits=64)))
        compiler_err(env, member, "The only thing that can be in a range is an Int, not %s", type_to_string(member_t));
    gcc_assign(*block, loc, member_var, member_val);
    member_val = gcc_rval(member_var);

    gcc_type_t *gcc_range_t = sss_type_to_gcc(env, get_type(env, range));
    gcc_struct_t *range_struct = gcc_type_if_struct(gcc_range_t);
    gcc_lvalue_t *range_var = gcc_local(func, loc, gcc_range_t, "_range");
    gcc_assign(*block, loc, range_var, compile_range(env, block, range));

    gcc_lvalue_t *contained_var = gcc_local(func, loc, gcc_type(env->ctx, BOOL), "_is_contained");
    gcc_rvalue_t *first = gcc_rvalue_access_field(gcc_rval(range_var), NULL, gcc_get_field(range_struct, 0)),
                 *step = gcc_rvalue_access_field(gcc_rval(range_var), NULL, gcc_get_field(range_struct, 1)),
                 *last = gcc_rvalue_access_field(gcc_rval(range_var), NULL, gcc_get_field(range_struct, 2));
    gcc_block_t *positive = gcc_new_block(func, fresh("positive_step")),
                *negative = gcc_new_block(func, fresh("negative_step")),
                *done = gcc_new_block(func, fresh("done"));
    gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_GE, step, gcc_zero(env->ctx, gcc_type(env->ctx, INT64))),
                       positive, negative);
#define AND(a,b) gcc_binary_op(env->ctx, loc, GCC_BINOP_LOGICAL_AND, gcc_type(env->ctx, BOOL), a, b)
#define CMP(a,op,b) gcc_comparison(env->ctx, loc, GCC_COMPARISON_##op, a, b)
#define MOD(x,divisor) gcc_binary_op(env->ctx, loc, GCC_BINOP_MODULO, gcc_type(env->ctx, INT64), x, divisor)
    *block = positive;
    gcc_assign(*block, loc, contained_var,
               AND(CMP(member_val, GE, first),
                   AND(CMP(member_val, LE, last),
                       CMP(MOD(member_val, step), EQ, MOD(first, step)))));
    gcc_jump(*block, loc, done);

    *block = negative;
    gcc_assign(*block, loc, contained_var,
               AND(CMP(member_val, LE, first),
                   AND(CMP(member_val, GE, last),
                       CMP(MOD(member_val, step), EQ, MOD(first, step)))));
    gcc_jump(*block, loc, done);
#undef AND
#undef CMP
#undef MOD

    *block = done;
    return gcc_rval(contained_var);
}
