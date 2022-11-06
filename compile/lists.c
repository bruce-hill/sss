// Compilation logic for Lists in this file
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

gcc_rvalue_t *compile_list(env_t *env, gcc_block_t **block, ast_t *ast)
{
    gcc_ctx_t *ctx = env->ctx;
    bl_type_t *t = get_type(env->file, env->bindings, ast);
    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
    gcc_type_t *item_gcc_type = bl_type_to_gcc(env, t->item_type);
    gcc_lvalue_t *list = gcc_local(gcc_block_func(*block), NULL, bl_type_to_gcc(env, t), fresh("list"));

#define PARAM(_t, _name) gcc_new_param(ctx, NULL, gcc_type(ctx, _t), _name)
    gcc_param_t *list_params[] = {PARAM(SIZE, "item_size"), PARAM(SIZE, "min_items")};
    gcc_func_t *new_list_func = gcc_new_func(
        env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_t, "list_new", 2, list_params, 0);

    gcc_param_t *list_insert_params[] = {
        gcc_new_param(ctx, NULL, gcc_t, "list"), PARAM(SIZE,"item_size"), PARAM(INT64,"index"),
        gcc_new_param(ctx, NULL, gcc_get_ptr_type(item_gcc_type), "item"), PARAM(STRING,"err_msg"),
    };
    gcc_func_t *list_insert_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, VOID), "list_insert", 5, list_insert_params, 0);
#undef PARAM

    ssize_t item_size = gcc_type_is_integral(item_gcc_type) ? gcc_type_size(item_gcc_type) : 8;
    gcc_rvalue_t *new_list_args[] = {
        gcc_rvalue_from_long(ctx, gcc_type(ctx, SIZE), (long)item_size),
        gcc_rvalue_from_long(ctx, gcc_type(ctx, SIZE), ast->list.items ? length(ast->list.items): 0),
    };
    gcc_assign(*block, ast_loc(env, ast), list, gcc_call(ctx, NULL, new_list_func, 2, new_list_args));

    if (ast->list.items) {
        gcc_lvalue_t *item = gcc_local(gcc_block_func(*block), NULL, item_gcc_type, fresh("item"));
        gcc_rvalue_t *item_addr = gcc_lvalue_address(item, NULL);
        foreach (ast->list.items, item_ast, _) {
            switch ((*item_ast)->kind) {
            case For: case While: case Repeat: case If: {
                errx(1, "Comprehensions not yet implemented");
            }
            default: {
                gcc_rvalue_t *val = compile_expr(env, block, (*item_ast));
                gcc_assign(*block, NULL, item, val);

                gcc_rvalue_t *insert_args[] = {
                    gcc_lvalue_as_rvalue(list),
                    gcc_rvalue_from_long(ctx, gcc_type(ctx, SIZE), (long)item_size),
                    gcc_int64(ctx, INT_NIL),
                    item_addr,
                    gcc_null(ctx, gcc_type(ctx, STRING)),
                };
                gcc_eval(*block, NULL, gcc_call(ctx, NULL, list_insert_func, 5, insert_args));
            }
            }
        }
    }
    return gcc_lvalue_as_rvalue(list);
}

void compile_list_iteration(env_t *env, gcc_block_t **block, ast_t *ast, block_compiler_t body_compiler, void *userdata)
{

    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *loop_body = gcc_new_block(func, NULL),
                *loop_between = ast->for_loop.between ? gcc_new_block(func, NULL) : NULL,
                *loop_next = gcc_new_block(func, NULL),
                *loop_end = gcc_new_block(func, NULL);

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
    gcc_rvalue_t *list = compile_expr(env, block, ast->for_loop.iter);
    bl_type_t *list_t = get_type(env->file, env->bindings, ast->for_loop.iter);
    assert(list_t->kind == ListType);
    gcc_type_t *gcc_list_t = bl_type_to_gcc(env, list_t);
    bl_type_t *item_t = list_t->item_type;
    gcc_type_t *gcc_item_t = bl_type_to_gcc(env, item_t);

    // item_ptr = list->items
    gcc_struct_t *list_struct = gcc_type_if_struct(gcc_type_if_pointer(gcc_list_t));
    assert(list_struct);
    gcc_lvalue_t *item_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_item_t), fresh("item_ptr"));
    gcc_assign(*block, NULL, item_ptr,
               gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(list, NULL, gcc_get_field(list_struct, 0))));
    // len = list->len
    gcc_lvalue_t *len = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("len"));
    gcc_assign(*block, NULL, len,
               gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(list, NULL, gcc_get_field(list_struct, 1))));

    gcc_lvalue_t *item_var = NULL;
    if (ast->for_loop.value) {
        if (ast->for_loop.value->kind != Var)
            ERROR(env, ast->for_loop.value, "This needs to be a variable");
        item_var = gcc_local(func, ast_loc(env, ast->for_loop.value), gcc_item_t, fresh(ast->for_loop.value->str));
        hashmap_set(env->bindings, ast->for_loop.value->str, new(binding_t, .rval=gcc_lvalue_as_rvalue(item_var), .type=item_t));
    }

    // index = 1
    gcc_lvalue_t *index_var;
    if (ast->for_loop.key) {
        index_var = gcc_local(func, ast_loc(env, ast->for_loop.key), gcc_type(env->ctx, INT64), fresh(ast->for_loop.key->str));
        if (ast->for_loop.key->kind != Var)
            ERROR(env, ast->for_loop.key, "This needs to be a variable");
        hashmap_set(env->bindings, ast->for_loop.key->str, new(binding_t, .rval=gcc_lvalue_as_rvalue(index_var), .type=Type(IntType)));
    } else {
        index_var = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("i"));
    }
    gcc_rvalue_t *one64 = gcc_one(env->ctx, gcc_type(env->ctx, INT64));
    gcc_assign(*block, NULL, index_var, one64);

    // goto (index > len) ? end : body
    gcc_rvalue_t *is_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, gcc_lvalue_as_rvalue(index_var), gcc_lvalue_as_rvalue(len));
    gcc_jump_condition(*block, NULL, is_done, loop_end, loop_body);
    *block = NULL;

    // body:
    gcc_block_t *loop_body_end = loop_body;
    // item = *item_ptr
    if (item_var)
        gcc_assign(loop_body_end, ast_loc(env, ast->for_loop.value), item_var,
                   gcc_lvalue_as_rvalue(gcc_jit_rvalue_dereference(gcc_lvalue_as_rvalue(item_ptr), NULL)));

    // body block
    if (body_compiler)
        body_compiler(env, &loop_body_end, ast->for_loop.body, userdata);
    else
        (void)compile_block(env, &loop_body_end, ast->for_loop.body, false);

    if (loop_body_end)
        gcc_jump(loop_body_end, NULL, loop_next);

    // next: index++, item_ptr++
    gcc_update(loop_next, NULL, index_var, GCC_BINOP_PLUS, one64);
    gcc_assign(loop_next, NULL, item_ptr,
               gcc_lvalue_address(gcc_array_access(env->ctx, NULL, gcc_lvalue_as_rvalue(item_ptr), one64), NULL));

    if (loop_between) {
        // goto is_done ? end : between
        gcc_jump_condition(loop_next, NULL, is_done, loop_end, loop_between);
        // between:
        if (item_var)
            gcc_assign(loop_between, ast_loc(env, ast->for_loop.value), item_var,
                       gcc_lvalue_as_rvalue(gcc_jit_rvalue_dereference(gcc_lvalue_as_rvalue(item_ptr), NULL)));
        (void)compile_block(env, &loop_between, ast->for_loop.between, false);
        if (loop_between)
            gcc_jump(loop_between, NULL, loop_body); // goto body
    } else {
        // goto is_done ? end : body
        gcc_jump_condition(loop_next, NULL, is_done, loop_end, loop_body);
    }

    *block = loop_end;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
