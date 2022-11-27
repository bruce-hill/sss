// Logic for compiling Blang ranges (`1..10`)
#include <assert.h>
#include <bhash.h>
#include <libgccjit.h>
#include <bp/files.h>
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
    gcc_rvalue_t *values[] = {
        ast->range.first ? compile_expr(env, block, ast->range.first) : gcc_int64(env->ctx, INT64_MIN),
        ast->range.step ? compile_expr(env, block, ast->range.step) : gcc_int64(env->ctx, 1),
        ast->range.last ? compile_expr(env, block, ast->range.last) : gcc_int64(env->ctx, INT64_MAX),
    };
    return gcc_struct_constructor(env->ctx, NULL, range_t, 3, NULL, values);
}

void compile_range_iteration(
    env_t *env, gcc_block_t **block, ast_t *range,
    loop_handler_t body_compiler, loop_handler_t between_compiler, void *userdata)
{
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *loop_body = gcc_new_block(func, fresh("loop_body")),
                *loop_between = between_compiler ? gcc_new_block(func, fresh("loop_between")) : NULL,
                *loop_next = gcc_new_block(func, fresh("loop_next")),
                *loop_end = gcc_new_block(func, fresh("loop_end"));

    env_t loop_env = *env;
    loop_env.bindings = hashmap_new();
    loop_env.bindings->fallback = env->bindings;
    loop_env.loop_label = &(loop_label_t){
        .enclosing = env->loop_label,
        .name = intern_str("for"),
        .skip_label = loop_next,
        .stop_label = loop_end,
    };
    env = &loop_env;

    // Preamble:
    bl_type_t *range_t = get_type(env->file, env->bindings, range);
    assert(range_t->kind == RangeType);
    gcc_rvalue_t *range_val = compile_expr(env, block, range);
    gcc_type_t *gcc_range_t = bl_type_to_gcc(env, range_t);
    gcc_type_t *i64_t = gcc_type(env->ctx, INT64);

    // iter = range.first
    gcc_struct_t *range_struct = gcc_type_if_struct(gcc_range_t);
    assert(range_struct);
    gcc_lvalue_t *iter = gcc_local(func, NULL, i64_t, fresh("iter"));
    gcc_assign(*block, NULL, iter,
               gcc_rvalue_access_field(range_val, NULL, gcc_get_field(range_struct, 0)));

    // step = range.step
    gcc_lvalue_t *step = gcc_local(func, NULL, i64_t, fresh("step"));
    gcc_assign(*block, NULL, step,
               gcc_rvalue_access_field(range_val, NULL, gcc_get_field(range_struct, 1)));

    // sign = step / abs(step)
    gcc_lvalue_t *sign = gcc_local(func, NULL, i64_t, fresh("sign"));
    gcc_assign(*block, NULL, sign,
               gcc_binary_op(env->ctx, NULL, GCC_BINOP_DIVIDE, i64_t,
                             gcc_lvalue_as_rvalue(step),
                             gcc_unary_op(env->ctx, NULL, GCC_UNOP_ABS, i64_t, gcc_lvalue_as_rvalue(step))));

    // last = range.last
    gcc_lvalue_t *last = gcc_local(func, NULL, i64_t, fresh("last"));
    gcc_assign(*block, NULL, last,
               gcc_rvalue_access_field(range_val, NULL, gcc_get_field(range_struct, 2)));

    // index = 1
    gcc_rvalue_t *one64 = gcc_one(env->ctx, gcc_type(env->ctx, INT64));
    gcc_lvalue_t *index_var = gcc_local(func, NULL, i64_t, fresh("index"));
    gcc_assign(*block, NULL, index_var, one64);

    // goto ((last - iter)*sign < 0) ? body : end
    gcc_rvalue_t *is_done = gcc_comparison(
        env->ctx, NULL, GCC_COMPARISON_LT,
        gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, i64_t,
                      gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, i64_t,
                                    gcc_lvalue_as_rvalue(last),
                                    gcc_lvalue_as_rvalue(iter)),
                      gcc_lvalue_as_rvalue(sign)),
        gcc_zero(env->ctx, i64_t));
    gcc_jump_condition(*block, NULL, is_done, loop_end, loop_body);
    *block = NULL;

    // body:
    gcc_block_t *loop_body_end = loop_body;

    iterator_info_t info = {
        .key_type = Type(IntType),
        .key_rval = gcc_lvalue_as_rvalue(index_var),
        .value_type = Type(IntType),
        .value_rval = gcc_lvalue_as_rvalue(iter),
    };

    // body block
    if (body_compiler)
        body_compiler(env, &loop_body_end, &info, userdata);

    if (loop_body_end)
        gcc_jump(loop_body_end, NULL, loop_next);

    // next:
    // index++, iter+=step
    if (index_var)
        gcc_update(loop_next, NULL, index_var, GCC_BINOP_PLUS, one64);
    gcc_update(loop_next, NULL, iter, GCC_BINOP_PLUS, gcc_lvalue_as_rvalue(step));

    // goto is_done ? end : between
    gcc_jump_condition(loop_next, NULL, is_done, loop_end, loop_between);
    // between:
    if (between_compiler)
        between_compiler(env, &loop_body_end, &info, userdata);

    if (loop_between)
        gcc_jump(loop_between, NULL, loop_body); // goto body

    *block = loop_end;
}
