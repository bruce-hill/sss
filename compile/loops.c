// Compilation logic for while/repeat/loop loops
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

static gcc_rvalue_t *rvalue_in_var(gcc_block_t **block, const char *name, gcc_type_t *gcc_t, gcc_rvalue_t *rval)
{
    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *var = gcc_local(func, NULL, gcc_t, name);
    gcc_assign(*block, NULL, var, rval);
    return gcc_rval(var);
}

void compile_for_loop(env_t *env, gcc_block_t **block, ast_t *ast)
{
    auto for_ = Match(ast, For);

    ast_t *iter = for_->iter;
    sss_type_t *iter_t = get_type(env, iter);

    if (iter_t->tag == GeneratorType) {
        switch (for_->iter->tag) {
        case For: {
            auto inner = Match(for_->iter, For);
            if (inner->between)
                compiler_err(env, inner->between, "The language doesn't support iterating over inner loops with a 'between' block");

            // (outer_body for x in (inner_body for y in 1..10)) --> ((do x := inner_body; outer_body) for y in 1..10)
            // (outer_body for x1,x2 in (inner_body for y1,y2 in 1..10)) --> ((do x1 := y1; x2 := inner_body; outer_body) for y1,y2 in 1..10)
            ast_t *index = inner->index ? inner->index : for_->index;
            assert(for_->value);
            ast_t *value = inner->value ? inner->value : for_->value;
            ast_t *first, *body, *between, *empty;
            if (for_->first) {
                NEW_LIST(ast_t*, stmts);
                if (inner->index && for_->index)
                    append(stmts, WrapAST(ast, Declare, for_->index, inner->index));
                append(stmts, WrapAST(ast, Declare, for_->value, inner->first ? inner->first : inner->body));
                append(stmts, for_->first);
                first = WrapAST(ast, Block, stmts);
            } else {
                first = inner->first;
            }

            if (for_->body) {
                NEW_LIST(ast_t*, stmts);
                if (inner->index && for_->index)
                    append(stmts, WrapAST(ast, Declare, for_->index, inner->index));
                if (!inner->body) compiler_err(env, ast, "I can't iterate over a loop that doesn't have a body");
                append(stmts, WrapAST(ast, Declare, for_->value, inner->body));
                append(stmts, for_->body);
                body = WrapAST(ast, Block, stmts);
            } else {
                body = NULL;
            }

            if (for_->between) {
                NEW_LIST(ast_t*, stmts);
                if (inner->index && for_->index)
                    append(stmts, WrapAST(ast, Declare, for_->index, inner->index));
                append(stmts, WrapAST(ast, Declare, for_->value, inner->between ? inner->between : inner->body));
                append(stmts, for_->between);
                between = WrapAST(ast, Block, stmts);
            } else {
                between = inner->between;
            }

            if (for_->empty && inner->empty) {
                empty = WrapAST(for_->empty, Block, LIST(ast_t*, inner->empty, for_->empty));
            } else {
                empty = inner->empty ? inner->empty : for_->empty;
            }

            ast_t *rebuilt = WrapAST(ast, For, .index=index, .value=value, .iter=inner->iter, .first=first, .body=body, .between=between, .empty=empty);
            return compile_for_loop(env, block, rebuilt);
        }
        default:
            compiler_err(env, ast, "This type of iteration is not currently supported by the compiler");
        }
    }

    while (iter_t->tag == VariantType)
        iter_t = Match(iter_t, VariantType)->variant_of;

    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *for_first = for_->first ? gcc_new_block(func, fresh("for_first")) : NULL,
                *for_body = gcc_new_block(func, fresh("for_body")),
                *for_between = for_->between ? gcc_new_block(func, fresh("for_between")) : NULL,
                *for_empty = for_->empty ? gcc_new_block(func, fresh("for_empty")) : NULL,
                *for_next = gcc_new_block(func, fresh("for_next")),
                *for_end = gcc_new_block(func, fresh("for_end"));

    gcc_comment(*block, NULL, "For Loop");
    gcc_rvalue_t *iter_rval = compile_expr(env, block, iter);
    gcc_type_t *gcc_iter_t = sss_type_to_gcc(env, iter_t);
    gcc_rvalue_t *original_pointer = NULL;
    if (iter_t->tag == PointerType) {
        auto ptr = Match(iter_t, PointerType);
        if (base_variant(ptr->pointed)->tag == StructType) {
            if (ptr->is_optional) {
                gcc_rvalue_t *is_nil = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, iter_rval, gcc_null(env->ctx, gcc_iter_t));
                gcc_block_t *continued = gcc_new_block(func, fresh("nonnil"));
                gcc_jump_condition(*block, NULL, is_nil, for_empty ? for_empty : for_end, continued);
                *block = continued;
            }

            iter_rval = gcc_rval(gcc_rvalue_dereference(iter_rval, NULL));
            iter_t = ptr->pointed;
            gcc_iter_t = sss_type_to_gcc(env, iter_t);
        } else {
            compiler_err(env, iter, "This value is a %T pointer. You must dereference the pointer with %#W[] to access the underlying value to iterate over it.",
                         iter_t, iter);
        }
    }

    // Index tracking is always the same:
    gcc_type_t *i64 = gcc_type(env->ctx, INT64);
    gcc_lvalue_t *index_var = gcc_local(func, NULL, i64, "_i");
    gcc_assign(*block, NULL, index_var, gcc_one(env->ctx, i64));
    gcc_lvalue_t *index_shadow = gcc_local(func, NULL, i64, "_i_shadow");
    gcc_assign(for_body, NULL, index_shadow, gcc_rval(index_var));
    if (for_first)
        gcc_assign(for_first, NULL, index_shadow, gcc_rval(index_var));
    gcc_update(for_next, NULL, index_var, GCC_BINOP_PLUS, gcc_one(env->ctx, i64));

    gcc_lvalue_t *item_shadow;
    sss_type_t *item_t;
    switch (base_variant(iter_t)->tag) {
    case ArrayType: {
        // item_ptr = array->items
        gcc_struct_t *array_struct = gcc_type_if_struct(gcc_iter_t);
        item_t = Match(base_variant(iter_t), ArrayType)->item_type;
        gcc_type_t *gcc_item_t = sss_type_to_gcc(env, item_t);
        gcc_lvalue_t *item_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_item_t), "_item_ptr");
        gcc_assign(*block, NULL, item_ptr,
                   gcc_rvalue_access_field(iter_rval, NULL, gcc_get_field(array_struct, ARRAY_DATA_FIELD)));

        // len = array->len
        gcc_lvalue_t *len = gcc_local(func, NULL, gcc_type(env->ctx, INT32), "_len");
        gcc_assign(*block, NULL, len,
                   gcc_rvalue_access_field(iter_rval, NULL, gcc_get_field(array_struct, ARRAY_LENGTH_FIELD)));

        item_shadow = gcc_local(func, NULL, gcc_item_t, "_item");
        gcc_rvalue_t *stride = gcc_rvalue_access_field(iter_rval, NULL, gcc_get_field(array_struct, ARRAY_STRIDE_FIELD));

        // goto (index > len) ? end : body
        gcc_rvalue_t *is_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, gcc_rval(index_var),
                                               gcc_cast(env->ctx, NULL, gcc_rval(len), gcc_type(env->ctx, INT64)));
        gcc_jump_condition(*block, NULL, is_done, for_empty ? for_empty : for_end,
                           for_first ? for_first : for_body);
        *block = NULL;

        // Now populate top of loop body (with variable bindings)
        // item = *item_ptr (or item = item_ptr)
        gcc_rvalue_t *item_rval = gcc_rval(gcc_jit_rvalue_dereference(gcc_rval(item_ptr), NULL));
        gcc_assign(for_body, NULL, item_shadow, item_rval);
        if (for_first)
            gcc_assign(for_first, NULL, item_shadow, item_rval);

        // Now populate .next block
        gcc_assign(for_next, NULL, item_ptr, pointer_offset(env, gcc_get_ptr_type(gcc_item_t), gcc_rval(item_ptr), stride));

        // goto is_done ? end : between
        gcc_jump_condition(for_next, NULL, is_done, for_end,
                           for_between ? for_between : for_body);

        // between:
        if (for_between)
            gcc_assign(for_between, NULL, item_shadow, item_rval);

        break;
    }
    case TableType: {
        // entry_ptr = table->entries
        gcc_struct_t *array_struct = gcc_type_if_struct(gcc_iter_t);
        item_t = table_entry_type(iter_t);
        gcc_type_t *gcc_item_t = sss_type_to_gcc(env, item_t);
        gcc_lvalue_t *entry_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_item_t), "_entry_ptr");
        gcc_assign(*block, NULL, entry_ptr,
                   gcc_cast(env->ctx, NULL, gcc_rvalue_access_field(iter_rval, NULL, gcc_get_field(array_struct, TABLE_ENTRIES_FIELD)),
                            gcc_get_ptr_type(gcc_item_t)));

        // len = table->count
        gcc_lvalue_t *len = gcc_local(func, NULL, gcc_type(env->ctx, INT32), "_len");
        gcc_assign(*block, NULL, len,
                   gcc_cast(env->ctx, NULL, gcc_rvalue_access_field(iter_rval, NULL, gcc_get_field(array_struct, TABLE_COUNT_FIELD)), gcc_type(env->ctx, INT32)));

        item_shadow = gcc_local(func, NULL, gcc_item_t, "_item");

        // goto (index > len) ? end : body
        gcc_rvalue_t *is_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, gcc_rval(index_var),
                                               gcc_cast(env->ctx, NULL, gcc_rval(len), gcc_type(env->ctx, INT64)));
        gcc_jump_condition(*block, NULL, is_done, for_empty ? for_empty : for_end,
                           for_first ? for_first : for_body);
        *block = NULL;

        // Now populate top of loop body (with variable bindings)
        // item = *entry_ptr (or item = entry_ptr)
        gcc_rvalue_t *item_rval = gcc_rval(gcc_jit_rvalue_dereference(gcc_rval(entry_ptr), NULL));
        gcc_assign(for_body, NULL, item_shadow, item_rval);
        if (for_first)
            gcc_assign(for_first, NULL, item_shadow, item_rval);

        // Now populate .next block
        gcc_assign(for_next, NULL, entry_ptr, pointer_offset(env, gcc_get_ptr_type(gcc_item_t), gcc_rval(entry_ptr),
                                                             gcc_rvalue_int32(env->ctx, gcc_sizeof(env, item_t))));

        // goto is_done ? end : between
        gcc_jump_condition(for_next, NULL, is_done, for_end,
                           for_between ? for_between : for_body);

        // between:
        if (for_between)
            gcc_assign(for_between, NULL, item_shadow, item_rval);

        break;
    }
    case RangeType: {
        gcc_lvalue_t *iter_var = gcc_local(func, NULL, gcc_iter_t, "_iter");
        gcc_assign(*block, NULL, iter_var, iter_rval);
        iter_rval = gcc_rval(iter_var);
        // x = range.first
        gcc_struct_t *range_struct = gcc_type_if_struct(gcc_iter_t);
        assert(range_struct);
        gcc_lvalue_t *x_var = gcc_local(func, NULL, i64, "_x");
        gcc_assign(*block, NULL, x_var,
                   gcc_rvalue_access_field(iter_rval, NULL, gcc_get_field(range_struct, 0)));
        gcc_rvalue_t *x = gcc_rval(x_var);

        gcc_rvalue_t *step = gcc_rvalue_access_field(iter_rval, NULL, gcc_get_field(range_struct, 1));
        step = rvalue_in_var(block, "step", i64, step);
        gcc_rvalue_t *last = gcc_rvalue_access_field(iter_rval, NULL, gcc_get_field(range_struct, 2));
        last = rvalue_in_var(block, "last", i64, last);

        gcc_type_t *b = gcc_type(env->ctx, BOOL);
        gcc_rvalue_t *zero64 = gcc_zero(env->ctx, i64);
        gcc_rvalue_t *i64_max = gcc_rvalue_from_long(env->ctx, i64, INT64_MAX);
        gcc_rvalue_t *i64_min = gcc_rvalue_from_long(env->ctx, i64, INT64_MIN);

        //////////////////////////////////////////////////////////////////////////////////////////////
        ///// The logic around this is *very* sensitive to overflow errors, so be careful!!! /////////
        //////////////////////////////////////////////////////////////////////////////////////////////
#define BINOP(t,a,op,b) gcc_binary_op(env->ctx, NULL, GCC_BINOP_ ## op, t, a, b)
#define CMP(a,op,b) gcc_comparison(env->ctx, NULL, GCC_COMPARISON_ ## op, a, b)
        // Check if the range is empty:
        // is_empty = ((step > 0) && (x > last)) || ((step < 0) && (x < last))
        gcc_rvalue_t *is_empty = BINOP(
            b, BINOP(b, CMP(step, GT, zero64), LOGICAL_AND, CMP(x, GT, last)),
            LOGICAL_OR,
            BINOP(b, CMP(step, LT, zero64), LOGICAL_AND, CMP(x, LT, last)));

        // We can only take the next step if no over/underflow will occur and we don't go past `last`:
        // can_continue = ((step > 0) && x <= (INT64_MAX - step) && (x + step <= last))
        //             || ((step < 0) && x >= (INT64_MIN - step) && (x + step >= last))
        gcc_rvalue_t *can_continue = BINOP(
            b, BINOP(b, CMP(step, GT, zero64), LOGICAL_AND,
                     BINOP(b, CMP(x, LE, BINOP(i64, i64_max, MINUS, step)), LOGICAL_AND, CMP(BINOP(i64, x, PLUS, step), LE, last))),
            LOGICAL_OR,
            BINOP(b, CMP(step, LT, zero64), LOGICAL_AND,
                  BINOP(b, CMP(x, GE, BINOP(i64, i64_min, MINUS, step)), LOGICAL_AND, CMP(BINOP(i64, x, PLUS, step), GE, last))));
#undef CMP
#undef BINOP
        //////////////////////////////////////////////////////////////////////////////////////////////
        /////////////////////////// Overflow-sensitive code ends here ////////////////////////////////
        //////////////////////////////////////////////////////////////////////////////////////////////

        gcc_jump_condition(*block, NULL, is_empty, for_empty ? for_empty : for_end,
                           for_first ? for_first : for_body);
        *block = NULL;

        // Shadow loop variables so they can be mutated without breaking the loop's functionality
        item_t = Type(IntType, .bits=64);
        item_shadow = gcc_local(func, NULL, i64, "_x");
        gcc_assign(for_body, NULL, item_shadow, x);
        if (for_first)
            gcc_assign(for_first, NULL, item_shadow, x);

        // goto can_continue ? update : end
        gcc_block_t *for_update = gcc_new_block(func, "for_update");
        gcc_jump_condition(for_next, NULL, can_continue, for_update, for_end);

        // x+=step
        gcc_update(for_update, NULL, x_var, GCC_BINOP_PLUS, step);
        gcc_jump(for_update, NULL, for_between ? for_between : for_body);

        if (for_between)
            gcc_assign(for_between, NULL, item_shadow, x);

        break;
    }
    case StructType: {
        auto struct_ = Match(base_variant(iter_t), StructType);
        int64_t field_index;
        for (field_index = 0; field_index < length(struct_->field_names); field_index++) {
            if (streq(ith(struct_->field_names, field_index), "next")
                && type_eq(ith(struct_->field_types, field_index), Type(PointerType, .pointed=iter_t, .is_optional=true))) {
                // Bingo: found a obj->next : ?Obj
                goto found_next_field;
            }
        }

        compiler_err(env, iter, "This value doesn't have an optional .next pointer field, so it can't be used for iteration.");

      found_next_field:

        sss_type_t *iter_var_t = Type(PointerType, .is_optional=false, .pointed=iter_t);
        item_t = iter_t;
        gcc_type_t *gcc_iter_var_t = sss_type_to_gcc(env, iter_var_t);

        // iter = obj
        gcc_lvalue_t *tmp = NULL;
        if (!original_pointer) {
            tmp = gcc_local(func, NULL, gcc_iter_t, "_tmp");
            gcc_assign(*block, NULL, tmp, iter_rval);
        }
        gcc_lvalue_t *iter_var = gcc_local(func, NULL, gcc_iter_var_t, "_iter");
        gcc_assign(*block, NULL, iter_var, original_pointer ? original_pointer : gcc_lvalue_address(tmp, NULL));

        // goto (iter == NULL) ? end : body
        gcc_rvalue_t *is_done = gcc_comparison(
            env->ctx, NULL, GCC_COMPARISON_EQ, gcc_rval(iter_var), gcc_null(env->ctx, gcc_iter_var_t));
        gcc_jump_condition(*block, NULL, is_done, for_empty ? for_empty : for_end,
                           for_first ? for_first : for_body);
        *block = NULL;

        // Shadow loop variables so they can be mutated without breaking the loop's functionality
        item_shadow = gcc_local(func, NULL, sss_type_to_gcc(env, item_t), "_item");
        gcc_rvalue_t *to_assign = type_eq(item_t, iter_var_t) ? gcc_rval(iter_var)
            : gcc_rval(gcc_rvalue_dereference(gcc_rval(iter_var), NULL));
        gcc_assign(for_body, NULL, item_shadow, to_assign);
        if (for_first)
            gcc_assign(for_first, NULL, item_shadow, to_assign);

        // next:
        // iter = iter->next
        gcc_struct_t *iter_struct = gcc_type_if_struct(gcc_iter_t);
        assert(iter_struct);
        gcc_assign(for_next, NULL, iter_var,
                   gcc_rval(
                       gcc_rvalue_dereference_field(
                           gcc_rval(iter_var), NULL, gcc_get_field(iter_struct, field_index))));

        // goto is_done ? end : between
        gcc_jump_condition(for_next, NULL, is_done, for_end,
                           for_between ? for_between : for_body);

        // between:
        if (for_between)
            gcc_assign(for_between, NULL, item_shadow, gcc_rval(iter_var));

        break;
    }
    default: compiler_err(env, iter, "Iteration isn't supported for %T", iter_t);
    }

    env_t *loop_env = fresh_scope(env);

    auto label_names = LIST(const char*, "for");
    if (for_->index) {
        append(label_names, Match(for_->index, Var)->name);
        hset(loop_env->bindings, Match(for_->index, Var)->name,
             new(binding_t, .rval=gcc_rval(index_shadow), .lval=index_shadow, .type=INT_TYPE));
    }
    if (for_->value) {
        append(label_names, Match(for_->value, Var)->name);
        hset(loop_env->bindings, Match(for_->value, Var)->name,
             new(binding_t, .rval=gcc_rval(item_shadow), .lval=item_shadow, .type=item_t));
    }
    loop_env->loop_label = &(loop_label_t){
        .enclosing = env->loop_label,
        .names = label_names,
        .skip_label = for_next,
        .stop_label = for_end,
        .deferred = env->deferred,
    };

    if (for_->first) {
        *block = for_first;
        if (loop_env->comprehension_callback)
            loop_env->comprehension_callback(loop_env, block, for_->first, loop_env->comprehension_userdata);
        else
            compile_block_statement(loop_env, block, for_->first);

        if (*block)
            gcc_jump(*block, NULL, for_next);
    }

    *block = for_body;
    if (for_->body) {
        if (loop_env->comprehension_callback)
            loop_env->comprehension_callback(loop_env, block, for_->body, loop_env->comprehension_userdata);
        else
            compile_block_statement(loop_env, block, for_->body);
    }
    if (*block)
        gcc_jump(*block, NULL, for_next);

    if (for_->between) {
        *block = for_between;
        if (loop_env->comprehension_callback)
            loop_env->comprehension_callback(loop_env, block, for_->between, loop_env->comprehension_userdata);
        else
            compile_block_statement(loop_env, block, for_->between);

        if (*block)
            gcc_jump(*block, NULL, for_body);
    }

    if (for_->empty) {
        *block = for_empty;
        if (loop_env->comprehension_callback)
            loop_env->comprehension_callback(loop_env, block, for_->empty, loop_env->comprehension_userdata);
        else
            compile_block_statement(loop_env, block, for_->empty);

        if (*block)
            gcc_jump(*block, NULL, for_end);
    }

    *block = for_end;
}

void compile_while_loop(env_t *env, gcc_block_t **block, const char* loop_name, ast_t *condition, ast_t *body, ast_t *between)
{
    gcc_func_t *func = gcc_block_func(*block);

    gcc_comment(*block, NULL, "While Loop");
    gcc_block_t *loop_top = gcc_new_block(func, fresh("loop_top"));
    assert(*block);
    gcc_jump(*block, NULL, loop_top);
    *block = loop_top;

    gcc_block_t *loop_body = gcc_new_block(func, fresh("loop_body"));
    gcc_block_t *loop_end = gcc_new_block(func, fresh("loop_end"));

    env_t loop_env = *env;
    loop_env.loop_label = &(loop_label_t){
        .enclosing = env->loop_label,
        .names = LIST(const char*, loop_name),
        .skip_label = loop_top,
        .stop_label = loop_end,
        .deferred = env->deferred,
    };

    if (condition) {
        check_truthiness(env, block, condition, loop_body, loop_end);
    } else {
        gcc_jump(*block, NULL, loop_body);
    }

    *block = loop_body;
    if (body) {
        if (loop_env.comprehension_callback)
            loop_env.comprehension_callback(&loop_env, block, body, loop_env.comprehension_userdata);
        else
            compile_block_statement(&loop_env, block, body);
    }

    if (between) {
        gcc_block_t *between_block = gcc_new_block(func, fresh("loop_between"));
        if (*block) {
            if (condition)
                check_truthiness(env, block, condition, between_block, loop_end);
            else
                gcc_jump(*block, NULL, between_block);
        }
        *block = between_block;
        if (loop_env.comprehension_callback)
            loop_env.comprehension_callback(&loop_env, block, between, loop_env.comprehension_userdata);
        else
            compile_block_statement(&loop_env, block, between);
        if (*block) gcc_jump(*block, NULL, loop_body);
    } else {
        if (*block) gcc_jump(*block, NULL, loop_top);
    }

    *block = loop_end;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
