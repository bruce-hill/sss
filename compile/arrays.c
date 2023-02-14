// Compilation logic for Arrays in this file
#include <assert.h>
#include <bhash.h>
#include <libgccjit.h>
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
        env->ctx, NULL, GCC_BINOP_MULT, gcc_size_t, gcc_cast(env->ctx, NULL, gcc_rval(length_field), gcc_size_t),
        gcc_rvalue_from_long(env->ctx, gcc_size_t, (long)gcc_sizeof(env, item_type)));
    gcc_func_t *gc_realloc_func = hashmap_gets(env->global_funcs, "GC_realloc");
    gcc_rvalue_t *new_data = gcc_callx(env->ctx, NULL, gc_realloc_func, 
                                       gcc_rval(data_field), new_size);
    gcc_assign(*block, NULL, data_field,
               gcc_cast(env->ctx, NULL, new_data, gcc_get_ptr_type(bl_type_to_gcc(env, item_type))));

    // array.items[array.length-1] = item
    gcc_rvalue_t *index = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, i32, gcc_rval(length_field), one32);
    gcc_lvalue_t *item_home = gcc_array_access(env->ctx, NULL, gcc_rval(data_field), index);
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

    gcc_loc_t *loc = ast_loc(env, ast);
    gcc_lvalue_t *array_var = gcc_local(func, loc, gcc_t, fresh("array"));
    gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);

    bl_type_t *item_t = Match(t, ArrayType)->item_type;
    gcc_func_t *alloc_func = hashmap_gets(env->global_funcs, has_heap_memory(item_t) ? "GC_malloc" : "GC_malloc_atomic");
    gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), (long)(gcc_sizeof(env, item_t) * length(array->items)));
    gcc_type_t *gcc_item_ptr_t = bl_type_to_gcc(env, Type(PointerType, .pointed=item_t));
    gcc_rvalue_t *initial_items = length(array->items) == 0 ? 
        gcc_null(env->ctx, gcc_item_ptr_t) : gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, size), gcc_item_ptr_t);
    gcc_assign(*block, loc, array_var, gcc_struct_constructor(
            env->ctx, loc, gcc_t, 2,
            (gcc_field_t*[]){gcc_get_field(gcc_struct, 0), gcc_get_field(gcc_struct, 2)}, // stride = 1
            (gcc_rvalue_t*[]){initial_items, gcc_one(env->ctx, gcc_type(env->ctx, INT32))}));

    env_t env2 = *env;
    env2.comprehension_callback = (void*)add_array_item;
    array_insert_info_t info = {t, gcc_lvalue_address(array_var, loc)};
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
                gcc_jump(*block, loc, item_done);
            *block = item_done;
        }
        if (*block)
            gcc_jump(*block, loc, array_done);
        *block = array_done;
    }
    return gcc_rval(array_var);
}

void compile_array_print_func(env_t *env, gcc_block_t **block, gcc_rvalue_t *obj, gcc_rvalue_t *rec, gcc_rvalue_t *file, bl_type_t *t)
{
    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);

    gcc_func_t *fputs_fn = hashmap_gets(env->global_funcs, "fputs");

#define WRITE_LITERAL(str) gcc_callx(env->ctx, NULL, fputs_fn, gcc_str(env->ctx, str), file)
#define ADD_WRITE(b, w) gcc_update(b, NULL, written_var, GCC_BINOP_PLUS, w)

    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *written_var = gcc_local(func, NULL, gcc_type(env->ctx, INT), fresh("written"));
    gcc_assign(*block, NULL, written_var, gcc_zero(env->ctx, gcc_type(env->ctx, INT)));

    bl_type_t *item_type = Match(t, ArrayType)->item_type;
    bool is_string = (item_type == Type(CharType));
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
    gcc_type_t *gcc_item_t = bl_type_to_gcc(env, item_type);
    gcc_lvalue_t *item_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_item_t), fresh("item_ptr"));
    gcc_assign(*block, NULL, item_ptr, items);

    // if (i < len) goto add_next_item;
    gcc_jump_condition(*block, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(i), len64),
                  add_next_item, end);

    // add_next_item:
    // item = *item_ptr
    gcc_rvalue_t *item = gcc_rval(gcc_jit_rvalue_dereference(gcc_rval(item_ptr), NULL));
    // item_str = tocord(item)
    gcc_func_t *item_print = get_print_func(env, item_type);
    assert(item_print);
    ADD_WRITE(add_next_item, gcc_callx(env->ctx, NULL, item_print, quote_string(env, item_type, item), file, rec));
    
    // i += 1
    assert(i);
    gcc_update(add_next_item, NULL, i, GCC_BINOP_PLUS, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
    // item_ptr = &item_ptr[stride]
    gcc_assign(add_next_item, NULL, item_ptr,
               gcc_lvalue_address(gcc_array_access(env->ctx, NULL, gcc_rval(item_ptr), stride), NULL));
    // if (i < len) goto add_comma;
    gcc_jump_condition(add_next_item, NULL, 
                  gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_rval(i), len64),
                  add_comma, end);

    // add_comma:
    if (!is_string)
        ADD_WRITE(add_comma, WRITE_LITERAL(", "));

    // goto add_next_item;
    gcc_jump(add_comma, NULL, add_next_item);

    // end:
    if (!is_string)
        ADD_WRITE(end, WRITE_LITERAL("]"));

    gcc_return(end, NULL, gcc_rval(written_var));
#undef WRITE_LITERAL 
#undef ADD_INT
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
