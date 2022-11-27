// Compilation logic for while/repeat/loop loops
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

void compile_loop_iteration(
    env_t *env, gcc_block_t **block, const char *loop_name, ast_t *condition,
    loop_handler_t body_compiler, loop_handler_t between_compiler, void *userdata)
{
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *loop_top = gcc_new_block(func, fresh("loop_top")),
                *loop_body = gcc_new_block(func, fresh("loop_body")),
                *loop_between = gcc_new_block(func, fresh("loop_between")),
                *loop_end = gcc_new_block(func, fresh("loop_end"));

    env_t loop_env = *env;
    loop_env.bindings = hashmap_new();
    loop_env.bindings->fallback = env->bindings;
    loop_env.loop_label = &(loop_label_t){
        .enclosing = env->loop_label,
        .name = intern_str(loop_name),
        .skip_label = loop_top,
        .stop_label = loop_end,
    };
    env = &loop_env;

    // gcc_jump(*block, NULL, loop_top);
    // Stupid bullshit hack to make GCC happy, otherwise it thinks loop_between may be unreachable:
    gcc_rvalue_t *yes = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, BOOL), 1);
    gcc_jump_condition(*block, NULL, yes, loop_top, loop_between);

    *block = loop_top;

    gcc_comment(*block, NULL, "Loop");
    if (condition) {
        check_truthiness(env, block, condition, loop_body, loop_end);
        env->loop_label->stop_reachable = true;
    } else {
        // GCC isn't happy if `loop_end` is unreachable
        // gcc_jump(*block, NULL, loop_body);
        gcc_rvalue_t *yes = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, BOOL), 1);
        gcc_jump_condition(*block, NULL, yes, loop_body, loop_end);
    }
    *block = loop_body;

    if (body_compiler)
        body_compiler(env, block, NULL, userdata);

    if (*block) {
        if (condition) {
            check_truthiness(env, block, condition, loop_between, loop_end);
            env->loop_label->stop_reachable = true;
        } else {
            // gcc_jump(*block, NULL, loop_between);
            gcc_rvalue_t *yes = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, BOOL), 1);
            gcc_jump_condition(*block, NULL, yes, loop_between, loop_end);
        }

        *block = loop_between;
        between_compiler(env, block, NULL, userdata);
        if (*block) {
            gcc_jump(*block, NULL, loop_body);
            *block = NULL;
        }
    } else {
        gcc_jump(loop_between, NULL, loop_body);
    }

    *block = loop_end;
}

void compile_iteration(env_t *env, gcc_block_t **block, ast_t *ast, loop_handler_t body_compiler, loop_handler_t between_compiler, void *userdata)
{
    switch (ast->kind) {
    case For: {
        bl_type_t *iter_t = get_type(env->file, env->bindings, ast->for_loop.iter);
        if (iter_t->kind == OptionalType) iter_t = iter_t->nonnil;
        switch (iter_t->kind) {
        case ListType: {
            compile_list_iteration(env, block, ast->for_loop.iter, body_compiler, between_compiler, userdata);
            return;
        }
        case RangeType: {
            compile_range_iteration(env, block, ast->for_loop.iter, body_compiler, between_compiler, userdata);
            return;
        }
        default: ERROR(env, ast->for_loop.iter, "Iteration is not supported for %s", type_to_string(iter_t));
        }
    }
    case Repeat: case While: {
        compile_loop_iteration(env, block, ast->kind == While ? "while" : "repeat",
                               ast->loop.condition, body_compiler, between_compiler, userdata);
        return;
    }
    default: ERROR(env, ast, "This is not an interation");
    }
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
