// Logic for compiling Blang ranges (`1..10`)
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
    gcc_type_t *range_t = bl_type_to_gcc(env, Type(RangeType));
    gcc_struct_t *range_struct = gcc_type_if_struct(range_t);
    assert(range_struct);
    auto range = Match(ast, Range);
    gcc_rvalue_t *values[] = {
        range->first && range->first->tag != Ellipsis ? compile_expr(env, block, range->first) : gcc_int64(env->ctx, INT64_MIN),
        range->step ? compile_expr(env, block, range->step) : gcc_int64(env->ctx, 1),
        range->last && range->last->tag != Ellipsis ? compile_expr(env, block, range->last) : gcc_int64(env->ctx, INT64_MAX),
    };
    return gcc_struct_constructor(env->ctx, NULL, range_t, 3, NULL, values);
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
