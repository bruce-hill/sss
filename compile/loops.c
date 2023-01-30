// Compilation logic for while/repeat/loop loops
#include <assert.h>
#include <bhash.h>
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

void setup_iteration(env_t *env, ast_t *iter, gcc_block_t **block, iter_blocks_t iter_blocks,
                     gcc_lvalue_t **index_var, bl_type_t **item_type, gcc_lvalue_t **item_var)
{
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *first_block = iter_blocks.first && *iter_blocks.first ? *iter_blocks.first : *iter_blocks.body;
    gcc_block_t *empty_block = iter_blocks.empty && *iter_blocks.empty ? *iter_blocks.empty : *iter_blocks.end;
    gcc_block_t *between_dest = iter_blocks.between && *iter_blocks.between ? *iter_blocks.between : *iter_blocks.body;

    bl_type_t *iter_t = get_type(env, iter);
    gcc_rvalue_t *iter_rval = compile_expr(env, block, iter);
    gcc_type_t *gcc_iter_t = bl_type_to_gcc(env, iter_t);
    while (iter_t->tag == PointerType) {
        auto ptr = Match(iter_t, PointerType);
        if (ptr->is_optional) {
            gcc_rvalue_t *is_nil = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, iter_rval, gcc_null(env->ctx, gcc_iter_t));
            gcc_block_t *continued = gcc_new_block(func, fresh("nonnil"));
            gcc_jump_condition(*block, NULL, is_nil, empty_block, continued);
            *block = continued;
        }

        iter_rval = gcc_rval(gcc_rvalue_dereference(iter_rval, NULL));
        iter_t = Match(iter_t, PointerType)->pointed;
        gcc_iter_t = bl_type_to_gcc(env, iter_t);
    }

    // Index tracking is always the same:
    gcc_type_t *i64 = gcc_type(env->ctx, INT64);
    *index_var = gcc_local(func, NULL, i64, fresh("i"));
    gcc_assign(*block, NULL, *index_var, gcc_one(env->ctx, i64));
    gcc_lvalue_t *index_shadow = gcc_local(func, NULL, i64, fresh("i"));
    gcc_assign(*iter_blocks.body, NULL, index_shadow, gcc_rval(*index_var));
    if (iter_blocks.first && *iter_blocks.first)
        gcc_assign(*iter_blocks.first, NULL, index_shadow, gcc_rval(*index_var));
    gcc_update(*iter_blocks.next, NULL, *index_var, GCC_BINOP_PLUS, gcc_one(env->ctx, i64));

    switch (iter_t->tag) {
    case ArrayType: {
        // item_ptr = array->items
        gcc_struct_t *array_struct = gcc_type_if_struct(gcc_iter_t);
        bl_type_t *item_t = Match(iter_t, ArrayType)->item_type;
        gcc_type_t *gcc_item_t = bl_type_to_gcc(env, item_t);
        gcc_lvalue_t *item_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_item_t), fresh("item_ptr"));
        gcc_assign(*block, NULL, item_ptr,
                   gcc_rvalue_access_field(iter_rval, NULL, gcc_get_field(array_struct, 0)));

        // len = array->len
        gcc_lvalue_t *len = gcc_local(func, NULL, gcc_type(env->ctx, INT32), fresh("len"));
        gcc_assign(*block, NULL, len,
                   gcc_rvalue_access_field(iter_rval, NULL, gcc_get_field(array_struct, 1)));

        *item_type = item_t;
        *item_var = gcc_local(func, NULL, gcc_item_t, fresh("item"));
        gcc_rvalue_t *stride = gcc_rvalue_access_field(iter_rval, NULL, gcc_get_field(array_struct, 2));

        // goto (index > len) ? end : body
        gcc_rvalue_t *is_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, gcc_rval(*index_var),
                                               gcc_cast(env->ctx, NULL, gcc_rval(len), gcc_type(env->ctx, INT64)));
        gcc_jump_condition(*block, NULL, is_done, empty_block, first_block);
        *block = NULL;

        // Now populate top of loop body (with variable bindings)
        // item = *item_ptr (or item = item_ptr)
        gcc_rvalue_t *item_rval = gcc_rval(gcc_jit_rvalue_dereference(gcc_rval(item_ptr), NULL));
        gcc_assign(*iter_blocks.body, NULL, *item_var, item_rval);
        if (iter_blocks.first && *iter_blocks.first)
            gcc_assign(*iter_blocks.first, NULL, *item_var, item_rval);

        // Now populate .next block
        gcc_assign(*iter_blocks.next, NULL, item_ptr,
                   gcc_lvalue_address(gcc_array_access(env->ctx, NULL, gcc_rval(item_ptr), stride), NULL));

        // goto is_done ? end : between
        gcc_jump_condition(*iter_blocks.next, NULL, is_done, *iter_blocks.end, between_dest);

        // between:
        if (iter_blocks.between)
            gcc_assign(*iter_blocks.between, NULL, *item_var, item_rval);

        break;
    }
    default: compile_err(env, iter, "Iteration not supported yet");
    }
    *block = *iter_blocks.body;
}

void finalize_iteration(env_t *env, gcc_block_t **block, iter_blocks_t iter_blocks)
{
    (void)env;
    assert(iter_blocks.body);
    assert(iter_blocks.next);
    gcc_jump(*iter_blocks.body, NULL, *iter_blocks.next);
    if (iter_blocks.first && *iter_blocks.first)
        gcc_jump(*iter_blocks.first, NULL, *iter_blocks.next);
    if (iter_blocks.between && *iter_blocks.between)
        gcc_jump(*iter_blocks.between, NULL, *iter_blocks.body);
    if (iter_blocks.empty && *iter_blocks.empty)
        gcc_jump(*iter_blocks.empty, NULL, *iter_blocks.end);
    *block = *iter_blocks.end;
}

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
        .names = LIST(istr_t, intern_str(loop_name)),
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

// For objects that have a .next pointer:
void compile_linked_iteration(
    env_t *env, gcc_block_t **block, ast_t *ast,
    loop_handler_t body_compiler, loop_handler_t between_compiler, void *userdata, int64_t field_index)
{
    // for (auto iter = obj; iter; iter = iter->next)
    auto for_loop = Match(ast, For);

    bool deref_key = for_loop->key && for_loop->key->tag == Dereference;
    if (deref_key)
        compile_err(env, for_loop->key, "I don't support dereferenced loop indexes for structs");
    bool deref_value = for_loop->value && for_loop->value->tag == Dereference;
    if (deref_value)
        compile_err(env, for_loop->value, "I don't support dereferenced loop values for structs");

    ast_t *obj = for_loop->iter;
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *loop_body = gcc_new_block(func, fresh("body")),
                *loop_between = between_compiler ? gcc_new_block(func, fresh("between")) : NULL,
                *loop_next = gcc_new_block(func, fresh("next")),
                *loop_end = gcc_new_block(func, fresh("end"));

    env_t loop_env = *env;
    loop_env.bindings = hashmap_new();
    loop_env.bindings->fallback = env->bindings;
    NEW_LIST(istr_t, label_names);
    append(label_names, intern_str("for"));
    if (for_loop->key)
        append(label_names, Match(for_loop->key, Var)->name);
    if (for_loop->value)
        append(label_names, Match(for_loop->value, Var)->name);
    loop_env.loop_label = &(loop_label_t){
        .enclosing = env->loop_label,
        .names = label_names,
        .skip_label = loop_next,
        .stop_label = loop_end,
    };
    env = &loop_env;

    // Preamble:
    bl_type_t *iter_t = get_type(env, obj);
    assert(iter_t->tag == PointerType && Match(iter_t, PointerType)->pointed->tag == StructType);
    gcc_rvalue_t *obj_val = compile_expr(env, block, obj);
    gcc_type_t *gcc_iter_t = bl_type_to_gcc(env, iter_t);
    gcc_type_t *i64_t = gcc_type(env->ctx, INT64);

    // iter = obj
    gcc_lvalue_t *iter_var = gcc_local(func, NULL, gcc_iter_t, fresh("_iter"));
    gcc_assign(*block, NULL, iter_var, obj_val);

    // index = 1
    gcc_rvalue_t *one64 = gcc_one(env->ctx, gcc_type(env->ctx, INT64));
    gcc_lvalue_t *index_var = gcc_local(func, NULL, i64_t, fresh("_index"));
    gcc_assign(*block, NULL, index_var, one64);

    // goto (iter == NULL) ? end : body
    gcc_rvalue_t *is_done = gcc_comparison(
        env->ctx, NULL, GCC_COMPARISON_EQ, gcc_rval(iter_var), gcc_null(env->ctx, gcc_iter_t));
    gcc_jump_condition(*block, NULL, is_done, loop_end, loop_body);
    *block = NULL;

    // body:
    gcc_block_t *loop_body_end = loop_body;

    // Shadow loop variables so they can be mutated without breaking the loop's functionality
    gcc_lvalue_t *index_shadow = gcc_local(func, NULL, i64_t, fresh("index")),
                 *iter_shadow = gcc_local(func, NULL, gcc_iter_t, fresh("iter"));
    gcc_assign(loop_body, NULL, index_shadow, gcc_rval(index_var));
    gcc_assign(loop_body, NULL, iter_shadow, gcc_rval(iter_var));
    iterator_info_t info = {
        .key_type = INT_TYPE,
        .key_lval = index_shadow,
        .value_type = Type(PointerType, .pointed=Match(iter_t, PointerType)->pointed, .is_optional=false),
        .value_lval = iter_shadow,
    };

    // body block
    if (body_compiler)
        body_compiler(env, &loop_body_end, &info, userdata);

    if (loop_body_end)
        gcc_jump(loop_body_end, NULL, loop_next);

    // next:
    // index++, iter = iter->next
    gcc_update(loop_next, NULL, index_var, GCC_BINOP_PLUS, one64);

    gcc_struct_t *iter_struct = gcc_type_if_struct(bl_type_to_gcc(env, Match(iter_t, PointerType)->pointed));
    assert(iter_struct);
    gcc_assign(loop_next, NULL, iter_var,
               gcc_rval(
                   gcc_rvalue_dereference_field(
                       gcc_rval(iter_var), NULL, gcc_get_field(iter_struct, field_index))));

    // goto is_done ? end : between
    gcc_jump_condition(loop_next, NULL, is_done, loop_end, loop_between);
    // between:
    if (between_compiler)
        between_compiler(env, &loop_between, &info, userdata);

    if (loop_between)
        gcc_jump(loop_between, NULL, loop_body); // goto body

    *block = loop_end;
}

void compile_iteration(env_t *env, gcc_block_t **block, ast_t *ast, loop_handler_t body_compiler, loop_handler_t between_compiler, void *userdata)
{
    switch (ast->tag) {
    case For: {
        auto for_loop = Match(ast, For);
        bl_type_t *iter_t = get_type(env, for_loop->iter);
        if (iter_t->tag == PointerType) iter_t = Match(iter_t, PointerType)->pointed;
        switch (iter_t->tag) {
        case ArrayType: {
            compile_array_iteration(env, block, ast, body_compiler, between_compiler, userdata);
            return;
        }
        case RangeType: {
            compile_range_iteration(env, block, ast, body_compiler, between_compiler, userdata);
            return;
        }
        case StructType: {
            auto struct_ = Match(iter_t, StructType);
            for (int64_t i = 0, len = length(struct_->field_names); i < len; i++) {
                if (ith(struct_->field_names, i) == intern_str("next")
                    && ith(struct_->field_types, i) == Type(PointerType, .pointed=iter_t, .is_optional=true)) {
                    // Bingo: found a obj->next : @?Obj
                    compile_linked_iteration(env, block, ast, body_compiler, between_compiler, userdata, i);
                    return;
                }
            }
            break;
        }
        default: break;
        }
        compile_err(env, for_loop->iter, "I don't know how to iterate over a %s value like this.", type_to_string(iter_t));
    }
    case Repeat: {
        compile_loop_iteration(env, block, "repeat", NULL, body_compiler, between_compiler, userdata);
        return;
    }
    case While: {
        auto loop = Match(ast, While);
        compile_loop_iteration(env, block, "while", loop->condition, body_compiler, between_compiler, userdata);
        return;
    }
    default: compile_err(env, ast, "This is not an interation");
    }
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
