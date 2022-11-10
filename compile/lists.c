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
    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *list = gcc_local(func, NULL, bl_type_to_gcc(env, t), fresh("list"));

#define PARAM(_t, _name) gcc_new_param(ctx, NULL, gcc_type(ctx, _t), _name)
    gcc_param_t *list_params[] = {PARAM(SIZE, "item_size"), PARAM(SIZE, "min_items")};
    gcc_func_t *new_list_func = gcc_new_func(
        env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_t, "list_new", 2, list_params, 0);

    gcc_param_t *append_params[] = {
        gcc_new_param(ctx, NULL, gcc_t, "list"), PARAM(SIZE,"item_size"),
        gcc_new_param(ctx, NULL, gcc_get_ptr_type(item_gcc_type), "item"),
    };
    gcc_func_t *append_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, VOID), "list_append", 3, append_params, 0);
#undef PARAM

    ssize_t item_size = gcc_type_is_integral(item_gcc_type) ? gcc_type_size(item_gcc_type) : 8;
    gcc_rvalue_t *new_list_args[] = {
        gcc_rvalue_from_long(ctx, gcc_type(ctx, SIZE), (long)item_size),
        gcc_rvalue_from_long(ctx, gcc_type(ctx, SIZE), ast->list.items ? length(ast->list.items): 0),
    };
    gcc_assign(*block, ast_loc(env, ast), list, gcc_call(ctx, NULL, new_list_func, 2, new_list_args));

    if (ast->list.items) {
        gcc_lvalue_t *item_var = gcc_local(func, NULL, item_gcc_type, fresh("item"));
        gcc_rvalue_t *item_addr = gcc_lvalue_address(item_var, NULL);

        foreach (ast->list.items, item_ast, _) {
            switch ((*item_ast)->kind) {
            case For: {
                ast_t *key_ast = (*item_ast)->for_loop.key,
                      *value_ast = (*item_ast)->for_loop.value,
                      *body = (*item_ast)->for_loop.body,
                      *between = (*item_ast)->for_loop.between;
                void add_list_item(env_t *env, gcc_block_t **block, iterator_info_t *info) {
                    if (key_ast)
                        hashmap_set(env->bindings, key_ast->str, new(binding_t, .type=info->key_type, .rval=info->key_rval));
                    if (value_ast)
                        hashmap_set(env->bindings, value_ast->str, new(binding_t, .type=info->value_type, .rval=info->value_rval));

                    gcc_assign(*block, NULL, item_var, compile_expr(env, block, body));
                    gcc_rvalue_t *append_args[] = {
                        gcc_lvalue_as_rvalue(list),
                        gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), (long)item_size),
                        item_addr,
                    };
                    gcc_eval(*block, NULL, gcc_call(env->ctx, NULL, append_func, 3, append_args));
                }
                void compile_between(env_t *env, gcc_block_t **block, iterator_info_t *info) {
                    (void)info;
                    if (between)
                        compile_block_statement(env, block, between);
                }

                compile_iteration(env, block, *item_ast, add_list_item, compile_between);
                break;
            }
            case While: case Repeat: case If: {
                ERROR(env, *item_ast, "Not yet implemented");
            }
            default: {
                gcc_rvalue_t *val = compile_expr(env, block, (*item_ast));
                gcc_assign(*block, NULL, item_var, val);

                gcc_rvalue_t *append_args[] = {
                    gcc_lvalue_as_rvalue(list),
                    gcc_rvalue_from_long(ctx, gcc_type(ctx, SIZE), (long)item_size),
                    item_addr,
                };
                gcc_eval(*block, NULL, gcc_call(ctx, NULL, append_func, 3, append_args));
            }
            }
        }
    }
    return gcc_lvalue_as_rvalue(list);
}

void compile_list_iteration(
    env_t *env, gcc_block_t **block, ast_t *list_ast,
    loop_handler_t body_compiler, loop_handler_t between_compiler)
{
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *loop_preamble = gcc_new_block(func, NULL),
                *loop_body = gcc_new_block(func, NULL),
                *loop_between = gcc_new_block(func, NULL),
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
    gcc_rvalue_t *list = compile_expr(env, block, list_ast);
    bl_type_t *list_t = get_type(env->file, env->bindings, list_ast);
    gcc_type_t *gcc_list_t = bl_type_to_gcc(env, list_t);
    if (list_t->kind == OptionalType) {
        gcc_rvalue_t *is_nil;
        if (gcc_type_if_pointer(gcc_list_t))
            is_nil = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, list, gcc_null(env->ctx, gcc_list_t));
        else
            is_nil = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, list, gcc_zero(env->ctx, gcc_list_t));
        gcc_jump_condition(*block, NULL, is_nil, loop_end, loop_preamble);
        list_t = list_t->nonnil;
    } else {
        gcc_jump(*block, NULL, loop_preamble);
    }
    *block = NULL;
    assert(list_t->kind == ListType);
    bl_type_t *item_t = list_t->item_type;
    gcc_type_t *gcc_item_t = bl_type_to_gcc(env, item_t);

    // item_ptr = list->items
    gcc_struct_t *list_struct = gcc_type_if_struct(gcc_type_if_pointer(gcc_list_t));
    assert(list_struct);
    gcc_lvalue_t *item_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_item_t), fresh("item_ptr"));
    gcc_assign(loop_preamble, NULL, item_ptr,
               gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(list, NULL, gcc_get_field(list_struct, 0))));
    // len = list->len
    gcc_lvalue_t *len = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("len"));
    gcc_assign(loop_preamble, NULL, len,
               gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(list, NULL, gcc_get_field(list_struct, 1))));

    gcc_lvalue_t *item_var = gcc_local(func, NULL, gcc_item_t, fresh("item"));

    // index = 1
    gcc_type_t *i64 = gcc_type(env->ctx, INT64);
    gcc_lvalue_t *index_var = gcc_local(func, NULL, i64, fresh("i"));
    gcc_rvalue_t *one64 = gcc_one(env->ctx, gcc_type(env->ctx, INT64));
    gcc_assign(loop_preamble, NULL, index_var, one64);

    // goto (index > len) ? end : body
    gcc_rvalue_t *is_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, gcc_lvalue_as_rvalue(index_var), gcc_lvalue_as_rvalue(len));
    gcc_jump_condition(loop_preamble, NULL, is_done, loop_end, loop_body);
    loop_preamble = NULL;

    // body:
    gcc_block_t *loop_body_end = loop_body;
    // item = *item_ptr
    if (item_var)
        gcc_assign(loop_body_end, NULL, item_var,
                   gcc_lvalue_as_rvalue(gcc_jit_rvalue_dereference(gcc_lvalue_as_rvalue(item_ptr), NULL)));

    iterator_info_t info = {
        .key_type = Type(IntType),
        .key_rval = gcc_lvalue_as_rvalue(index_var),
        .value_type = item_t,
        .value_rval = gcc_lvalue_as_rvalue(item_var),
    };

    // body block
    if (body_compiler)
        body_compiler(env, &loop_body_end, &info);

    if (loop_body_end)
        gcc_jump(loop_body_end, NULL, loop_next);

    // next: index++, item_ptr++
    assert(index_var);
    gcc_update(loop_next, NULL, index_var, GCC_BINOP_PLUS, one64);
    gcc_assign(loop_next, NULL, item_ptr,
               gcc_lvalue_address(gcc_array_access(env->ctx, NULL, gcc_lvalue_as_rvalue(item_ptr), one64), NULL));

    // goto is_done ? end : between
    gcc_jump_condition(loop_next, NULL, is_done, loop_end, loop_between);
    // between:
    gcc_assign(loop_between, NULL, item_var,
               gcc_lvalue_as_rvalue(gcc_jit_rvalue_dereference(gcc_lvalue_as_rvalue(item_ptr), NULL)));
    if (between_compiler)
        between_compiler(env, &loop_between, &info);

    if (loop_between)
        gcc_jump(loop_between, NULL, loop_body); // goto body

    *block = loop_end;
}

void compile_list_tostring_func(env_t *env, gcc_block_t **block, gcc_rvalue_t *obj, bl_type_t *t)
{
    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
    gcc_comment(*block, NULL, CORD_to_char_star(CORD_cat("tostring() for type: ", type_to_string(t))));

    gcc_func_t *CORD_cat_func = hashmap_gets(env->global_funcs, "CORD_cat");
    gcc_func_t *CORD_to_char_star_func = hashmap_gets(env->global_funcs, "CORD_to_char_star");
#define LITERAL(str) gcc_new_string(env->ctx, str)
#define CORD_str(cord) gcc_call(env->ctx, NULL, CORD_to_char_star_func, 1, (gcc_rvalue_t*[]){cord})

    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *str = gcc_local(func, NULL, gcc_type(env->ctx, STRING), fresh("str"));
    gcc_assign(*block, NULL, str,
               gcc_call(env->ctx, NULL, CORD_cat_func, 2, (gcc_rvalue_t*[]){
                        gcc_null(env->ctx, gcc_type(env->ctx, STRING)),
                        gcc_new_string(env->ctx, "["),
               }));
    gcc_lvalue_t *i = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("i"));
    gcc_assign(*block, NULL, i, gcc_zero(env->ctx, gcc_type(env->ctx, INT64)));
    gcc_struct_t *list_struct = gcc_type_if_struct(gcc_type_if_pointer(gcc_t));
    gcc_rvalue_t *items = gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(obj, NULL, gcc_get_field(list_struct, 0)));
    gcc_rvalue_t *len = gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(obj, NULL, gcc_get_field(list_struct, 1)));

    gcc_block_t *add_comma = gcc_new_block(func, NULL);
    gcc_block_t *add_next_item = gcc_new_block(func, NULL);
    gcc_block_t *end = gcc_new_block(func, NULL);

    // if (i < len) goto add_next_item;
    gcc_jump_condition(*block, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_lvalue_as_rvalue(i), len),
                  add_next_item, end);

    // add_next_item:
    gcc_rvalue_t *item = gcc_lvalue_as_rvalue(gcc_array_access(env->ctx, NULL, items, gcc_lvalue_as_rvalue(i)));
    gcc_rvalue_t *item_str;
    gcc_func_t *item_tostring = get_tostring_func(env, t->item_type);
    gcc_rvalue_t *args[] = {
        item,
        gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR)),
    };
    item_str = item_tostring ? gcc_call(env->ctx, NULL, item_tostring, 2, args) : item;
    gcc_assign(add_next_item, NULL, str,
               gcc_call(env->ctx, NULL, CORD_cat_func, 2, (gcc_rvalue_t*[]){
                        gcc_lvalue_as_rvalue(str),
                        item_str,
               }));
    
    // i += 1
    assert(i);
    gcc_update(add_next_item, NULL, i, GCC_BINOP_PLUS, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
    // if (i < len) goto add_comma;
    gcc_jump_condition(add_next_item, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_lvalue_as_rvalue(i), len),
                  add_comma, end);

    // add_comma:
    gcc_assign(add_comma, NULL, str,
               gcc_call(env->ctx, NULL, CORD_cat_func, 2, (gcc_rvalue_t*[]){
                        gcc_lvalue_as_rvalue(str),
                        gcc_new_string(env->ctx, ", "),
               }));
    // goto add_next_item;
    gcc_jump(add_comma, NULL, add_next_item);

    // end:
    gcc_rvalue_t *ret = gcc_call(env->ctx, NULL, CORD_cat_func, 2, (gcc_rvalue_t*[]){
                                 gcc_lvalue_as_rvalue(str),
                                 gcc_new_string(env->ctx, "]"),
                        });
    gcc_return(end, NULL, CORD_str(ret));
#undef CORD_str 
#undef LITERAL
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
