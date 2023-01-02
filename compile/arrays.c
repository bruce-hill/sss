// Compilation logic for Arrays in this file
#include <assert.h>
#include <bhash.h>
#include <libgccjit.h>
#include <bp/files.h>
#include <ctype.h>
#include <err.h>
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

typedef struct {
    bl_type_t *array_type;
    gcc_rvalue_t *array_ptr;
} array_insert_info_t;

static void add_array_item(env_t *env, gcc_block_t **block, ast_t *item, array_insert_info_t *info)
{
    bl_type_t *t = get_type(env, item); // item type
    if (t->tag == GeneratorType) {
        gcc_rvalue_t *val = compile_expr(env, block, item);
        assert(!val);
        return;
    }

    bl_type_t *item_type = Match(info->array_type, ArrayType)->item_type;

    // This comes first, because the item may short-circuit
    gcc_rvalue_t *item_val = compile_expr(env, block, item);
    if (!*block) return;

    gcc_type_t *gcc_t = bl_type_to_gcc(env, info->array_type);
    gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
    gcc_lvalue_t *array = gcc_rvalue_dereference(info->array_ptr, NULL);
    gcc_lvalue_t *data_field = gcc_lvalue_access_field(array, NULL, gcc_get_field(struct_t, 0));
    gcc_lvalue_t *length_field = gcc_lvalue_access_field(array, NULL, gcc_get_field(struct_t, 1));

    // array.length += 1
    gcc_type_t *i32 = gcc_type(env->ctx, INT32);
    gcc_rvalue_t *one32 = gcc_one(env->ctx, i32);
    gcc_update(*block, NULL, length_field, GCC_BINOP_PLUS, one32);

    // array.items = GC_realloc(array.items, item_size*array.length)
    gcc_type_t *gcc_size_t = gcc_type(env->ctx, SIZE);
    gcc_rvalue_t *new_size = gcc_binary_op(
        env->ctx, NULL, GCC_BINOP_MULT, gcc_size_t, gcc_cast(env->ctx, NULL, gcc_lvalue_as_rvalue(length_field), gcc_size_t),
        gcc_rvalue_from_long(env->ctx, gcc_size_t, (long)gcc_sizeof(env, item_type)));
    gcc_func_t *gc_realloc_func = hashmap_gets(env->global_funcs, "GC_realloc");
    gcc_rvalue_t *new_data = gcc_callx(env->ctx, NULL, gc_realloc_func, 
                                       gcc_lvalue_as_rvalue(data_field),
                                       new_size);
    gcc_assign(*block, NULL, data_field,
               gcc_cast(env->ctx, NULL, new_data, gcc_get_ptr_type(bl_type_to_gcc(env, item_type))));

    // array.items[array.length-1] = item
    gcc_rvalue_t *index = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, i32, gcc_lvalue_as_rvalue(length_field), one32);
    gcc_lvalue_t *item_home = gcc_array_access(env->ctx, NULL, gcc_lvalue_as_rvalue(data_field), index);
    if (t != item_type)
        if (!promote(env, t, &item_val, item_type))
            compile_err(env, item, "I can't convert this type to %s", type_to_string(item_type));

    gcc_assign(*block, NULL, item_home, item_val);
}

gcc_rvalue_t *compile_array(env_t *env, gcc_block_t **block, ast_t *ast)
{
    auto array = Match(ast, Array);
    bl_type_t *t = get_type(env, ast);
    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
    gcc_func_t *func = gcc_block_func(*block);

    gcc_lvalue_t *array_var = gcc_local(func, NULL, gcc_t, fresh("array"));
    gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
    gcc_assign(*block, NULL, array_var,
               gcc_struct_constructor(env->ctx, NULL, gcc_t, 1,
                                      (gcc_field_t*[]){gcc_get_field(gcc_struct, 2)}, // stride = 1
                                      (gcc_rvalue_t*[]){gcc_one(env->ctx, gcc_type(env->ctx, INT32))}));

    env_t env2 = *env;
    env2.comprehension_callback = (void*)add_array_item;
    array_insert_info_t info = {t, gcc_lvalue_address(array_var, NULL)};
    env2.comprehension_userdata = &info;
    env = &env2;

    if (array->items) {
        gcc_block_t *array_done = gcc_new_block(func, fresh("array_done"));
        foreach (array->items, item_ast, _) {
            gcc_block_t *item_done = gcc_new_block(func, fresh("item_done"));
            env2.loop_label = &(loop_label_t){
                .enclosing = env->loop_label,
                    .names = LIST(istr_t, intern_str("[]")),
                    .skip_label = item_done,
                    .stop_label = array_done,
            };

            add_array_item(env, block, *item_ast, &info);

            if (*block)
                gcc_jump(*block, NULL, item_done);
            *block = item_done;
        }
        if (*block)
            gcc_jump(*block, NULL, array_done);
        *block = array_done;
    }
    return gcc_lvalue_as_rvalue(array_var);
}

void compile_array_iteration(
    env_t *env, gcc_block_t **block, ast_t *ast,
    loop_handler_t body_compiler, loop_handler_t between_compiler, void *data)
{
    assert(ast->tag == For);
    auto for_loop = Match(ast, For);
    ast_t *array_ast = for_loop->iter;
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *loop_preamble = gcc_new_block(func, fresh("loop_preamble")),
                *loop_body = gcc_new_block(func, fresh("loop_body")),
                *loop_between = gcc_new_block(func, fresh("loop_between")),
                *loop_next = gcc_new_block(func, fresh("loop_next")),
                *loop_end = gcc_new_block(func, fresh("loop_end"));

    env_t loop_env = *env;
    loop_env.bindings = hashmap_new();
    loop_env.bindings->fallback = env->bindings;
    NEW_LIST(istr_t, label_names);
    append(label_names, intern_str("for"));
    if (for_loop->key)
        append(label_names, for_loop->key);
    if (for_loop->value)
        append(label_names, for_loop->value);
    loop_env.loop_label = &(loop_label_t){
        .enclosing = env->loop_label,
        .names = label_names,
        .skip_label = loop_next,
        .stop_label = loop_end,
    };
    env = &loop_env;

    // Preamble:
    gcc_rvalue_t *array = compile_expr(env, block, array_ast);
    bl_type_t *array_t = get_type(env, array_ast);
    gcc_type_t *gcc_array_t = bl_type_to_gcc(env, array_t);
    if (array_t->tag == PointerType) {
        auto array_ptr = Match(array_t, PointerType);
        if (array_ptr->is_optional) {
            gcc_rvalue_t *is_nil = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, array, gcc_null(env->ctx, gcc_array_t));
            gcc_jump_condition(*block, NULL, is_nil, loop_end, loop_preamble);
        }
        array = gcc_lvalue_as_rvalue(gcc_rvalue_dereference(array, NULL));
        array_t = array_ptr->pointed;
        gcc_array_t = bl_type_to_gcc(env, array_t);
    } else {
        gcc_jump(*block, NULL, loop_preamble);
    }
    *block = NULL;
    bl_type_t *item_t = Match(array_t, ArrayType)->item_type;
    gcc_type_t *gcc_item_t = bl_type_to_gcc(env, item_t);

    // item_ptr = array->items
    gcc_struct_t *array_struct = gcc_type_if_struct(gcc_array_t);
    assert(array_struct);
    gcc_lvalue_t *item_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_item_t), fresh("item_ptr"));
    gcc_assign(loop_preamble, NULL, item_ptr,
               gcc_rvalue_access_field(array, NULL, gcc_get_field(array_struct, 0)));
    // len = array->len
    gcc_lvalue_t *len = gcc_local(func, NULL, gcc_type(env->ctx, INT32), fresh("len"));
    gcc_assign(loop_preamble, NULL, len,
               gcc_rvalue_access_field(array, NULL, gcc_get_field(array_struct, 1)));

    gcc_lvalue_t *item_var = gcc_local(func, NULL, gcc_item_t, fresh("item"));

    // index = 1
    gcc_lvalue_t *index_var = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("i"));
    gcc_rvalue_t *stride = gcc_rvalue_access_field(array, NULL, gcc_get_field(array_struct, 2));
    gcc_assign(loop_preamble, NULL, index_var, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));

    // goto (index > len) ? end : body
    gcc_rvalue_t *is_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, gcc_lvalue_as_rvalue(index_var),
                                           gcc_cast(env->ctx, NULL, gcc_lvalue_as_rvalue(len), gcc_type(env->ctx, INT64)));
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
        body_compiler(env, &loop_body_end, &info, data);

    if (loop_body_end)
        gcc_jump(loop_body_end, NULL, loop_next);

    // next: index++, item_ptr = &item_ptr[stride]
    assert(index_var);
    gcc_update(loop_next, NULL, index_var, GCC_BINOP_PLUS, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
    gcc_assign(loop_next, NULL, item_ptr,
               gcc_lvalue_address(gcc_array_access(env->ctx, NULL, gcc_lvalue_as_rvalue(item_ptr), stride), NULL));

    // goto is_done ? end : between
    gcc_jump_condition(loop_next, NULL, is_done, loop_end, loop_between);
    // between:
    gcc_assign(loop_between, NULL, item_var,
               gcc_lvalue_as_rvalue(gcc_jit_rvalue_dereference(gcc_lvalue_as_rvalue(item_ptr), NULL)));
    if (between_compiler)
        between_compiler(env, &loop_between, &info, data);

    if (loop_between)
        gcc_jump(loop_between, NULL, loop_body); // goto body

    *block = loop_end;
}

void compile_array_print_func(env_t *env, gcc_block_t **block, gcc_rvalue_t *obj, gcc_rvalue_t *file, bl_type_t *t)
{
    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);

    gcc_func_t *fputs_fn = hashmap_gets(env->global_funcs, "fputs");

#define WRITE_LITERAL(str) gcc_callx(env->ctx, NULL, fputs_fn, gcc_str(env->ctx, str), file)
#define ADD_WRITE(b, w) gcc_update(b, NULL, written_var, GCC_BINOP_PLUS, w)

    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *written_var = gcc_local(func, NULL, gcc_type(env->ctx, INT), fresh("written"));
    gcc_assign(*block, NULL, written_var, gcc_zero(env->ctx, gcc_type(env->ctx, INT)));

    bool is_string = (Match(t, ArrayType)->item_type == Type(CharType));
    if (!is_string)
        ADD_WRITE(*block, WRITE_LITERAL("["));

    // i = 1
    gcc_lvalue_t *i = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("i"));
    gcc_assign(*block, NULL, i, gcc_zero(env->ctx, gcc_type(env->ctx, INT64)));
    gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
    gcc_rvalue_t *items = gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, 0));
    gcc_rvalue_t *len = gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, 1));
    gcc_rvalue_t *len64 = gcc_cast(env->ctx, NULL, len, gcc_type(env->ctx, INT64));
    gcc_rvalue_t *stride = gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, 2));

    gcc_block_t *add_comma = gcc_new_block(func, fresh("add_comma"));
    gcc_block_t *add_next_item = gcc_new_block(func, fresh("next_item"));
    gcc_block_t *end = gcc_new_block(func, fresh("done"));

    // item_ptr = array.items
    gcc_type_t *gcc_item_t = bl_type_to_gcc(env, Match(t, ArrayType)->item_type);
    gcc_lvalue_t *item_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_item_t), fresh("item_ptr"));
    gcc_assign(*block, NULL, item_ptr, items);

    // if (i < len) goto add_next_item;
    gcc_jump_condition(*block, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_lvalue_as_rvalue(i), len64),
                  add_next_item, end);

    // add_next_item:
    // item = *item_ptr
    gcc_rvalue_t *item = gcc_lvalue_as_rvalue(gcc_jit_rvalue_dereference(gcc_lvalue_as_rvalue(item_ptr), NULL));
    // item_str = tocord(item)
    gcc_func_t *item_print = get_print_func(env, Match(t, ArrayType)->item_type);
    assert(item_print);
    ADD_WRITE(add_next_item,
              gcc_callx(env->ctx, NULL, item_print,
                        item, file,
                        gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR))));
    
    // i += 1
    assert(i);
    gcc_update(add_next_item, NULL, i, GCC_BINOP_PLUS, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
    // item_ptr = &item_ptr[stride]
    gcc_assign(add_next_item, NULL, item_ptr,
               gcc_lvalue_address(gcc_array_access(env->ctx, NULL, gcc_lvalue_as_rvalue(item_ptr), stride), NULL));
    // if (i < len) goto add_comma;
    gcc_jump_condition(add_next_item, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_lvalue_as_rvalue(i), len64),
                  add_comma, end);

    // add_comma:
    if (!is_string)
        ADD_WRITE(add_comma, WRITE_LITERAL(", "));

    // goto add_next_item;
    gcc_jump(add_comma, NULL, add_next_item);

    // end:
    if (!is_string)
        ADD_WRITE(end, WRITE_LITERAL("]"));

    gcc_return(end, NULL, gcc_lvalue_as_rvalue(written_var));
#undef WRITE_LITERAL 
#undef ADD_INT
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
