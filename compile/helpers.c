// Helper functions used for compiling
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

istr_t fresh(istr_t name)
{
    static int id = 0;
    CORD ret;
    CORD_sprintf(&ret, "%s__%d", name, id++);
    return intern_str(CORD_to_char_star(ret));
}

// This must be memoized because GCC JIT doesn't do structural equality
gcc_type_t *bl_type_to_gcc(env_t *env, bl_type_t *t)
{
    gcc_type_t *gcc_t = hashmap_get(env->gcc_types, t);
    if (gcc_t) return gcc_t;

    switch (t->kind) {
    case IntType: gcc_t = gcc_type(env->ctx, INT64); break;
    case Int32Type: gcc_t = gcc_type(env->ctx, INT32); break;
    case Int16Type: gcc_t = gcc_type(env->ctx, INT16); break;
    case Int8Type: gcc_t = gcc_type(env->ctx, INT8); break;
    case BoolType: gcc_t = gcc_type(env->ctx, BOOL); break;
    case NumType: gcc_t = gcc_type(env->ctx, DOUBLE); break;
    case Num32Type: gcc_t = gcc_type(env->ctx, FLOAT); break;
    case VoidType: gcc_t = gcc_type(env->ctx, VOID); break;
    case StringType: gcc_t = gcc_type(env->ctx, STRING); break;
    case OptionalType: gcc_t = bl_type_to_gcc(env, t->nonnil); break;
    case RangeType: {
        gcc_type_t *i64 = gcc_type(env->ctx, INT64);
        gcc_field_t *fields[3] = {
            gcc_new_field(env->ctx, NULL, i64, "first"),
            gcc_new_field(env->ctx, NULL, i64, "step"),
            gcc_new_field(env->ctx, NULL, i64, "last"),
        };
        gcc_struct_t *range = gcc_new_struct_type(env->ctx, NULL, "Range", 3, fields);
        gcc_t = gcc_struct_as_type(range);
        break;
    }
    case ListType: {
        gcc_field_t *fields[3] = {
            gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(bl_type_to_gcc(env, t->item_type)), "items"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT64), "len"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT64), "slack"),
        };
        gcc_struct_t *list = gcc_new_struct_type(env->ctx, NULL, "List", 3, fields);
        gcc_t = gcc_get_ptr_type(gcc_struct_as_type(list));
        break;
    }
    case FunctionType: {
        NEW_LIST(gcc_type_t*, arg_types);
        foreach (t->args, arg_t, _)
            append(arg_types, bl_type_to_gcc(env, *arg_t));
        gcc_type_t *ret_type = bl_type_to_gcc(env, t->ret);
        gcc_t = gcc_new_func_type(env->ctx, NULL, ret_type, length(arg_types), arg_types[0], 0);
        break;
    }
    case TypeType: {
        gcc_t = gcc_type(env->ctx, STRING);
        break;
    }
    default: {
        errx(1, "The following BL type doesn't have a GCC type: %s", type_to_string(t));
    }
    }

    hashmap_set(env->gcc_types, t, gcc_t);
    return gcc_t;
}

hashmap_t *global_bindings(hashmap_t *bindings)
{
    hashmap_t *globals = hashmap_new();
    for (hashmap_t *h = bindings; h; h = h->fallback) {
        for (istr_t key = NULL; (key = hashmap_next(h, key)); ) {
            binding_t *val = hashmap_get_raw(h, key);
            assert(val);
            if (val->is_global)
                hashmap_set(globals, key, val);
        }
    }
    return globals;
}

void check_truthiness(env_t *env, gcc_block_t **block, ast_t *obj, gcc_block_t *if_truthy, gcc_block_t *if_falsey)
{
    bl_type_t *t = get_type(env->file, env->bindings, obj);
    gcc_rvalue_t *bool_val = compile_expr(env, block, obj); 
    if (t->kind != BoolType) {
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        if (gcc_type_if_pointer(gcc_t))
            bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, bool_val, gcc_null(env->ctx, gcc_t));
        else
            bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, bool_val, gcc_zero(env->ctx, gcc_t));
    }
    gcc_jump_condition(*block, NULL, bool_val, if_truthy, if_falsey);
    *block = NULL;
}

gcc_func_t *get_tostring_func(env_t *env, bl_type_t *t)
{
    if (t->kind == StringType || t->kind == TypeType)
        return NULL;

    gcc_func_t *func = hashmap_get(env->tostring_funcs, t);
    if (func) return func;

    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);

    gcc_param_t *params[2] = {
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("obj")),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, VOID_PTR), fresh("stack")),
    };
    func = gcc_new_func(
        env->ctx, NULL, GCC_FUNCTION_INTERNAL, gcc_type(env->ctx, STRING),
        fresh("tostring"), 2, params, 0);
    hashmap_set(env->tostring_funcs, t, func);

    gcc_block_t *block = gcc_new_block(func, NULL);
    gcc_comment(block, NULL, CORD_to_char_star(CORD_cat("tostring() for type: ", type_to_string(t))));
    gcc_rvalue_t *obj = gcc_param_as_rvalue(params[0]);

    gcc_func_t *CORD_to_char_star_func = hashmap_gets(env->global_funcs, "CORD_to_char_star");
#define LITERAL(str) gcc_new_string(env->ctx, str)
#define CORD_str(cord) gcc_call(env->ctx, NULL, CORD_to_char_star_func, 1, (gcc_rvalue_t*[]){cord})
    
    switch (t->kind) {
    case BoolType: {
        gcc_block_t *yes_block = gcc_new_block(func, NULL);
        gcc_block_t *no_block = gcc_new_block(func, NULL);
        gcc_jump_condition(block, NULL, obj, yes_block, no_block);
        gcc_return(yes_block, NULL, LITERAL("yes"));
        gcc_return(no_block, NULL, LITERAL("no"));
        break;
    }
    case IntType: case Int32Type: case Int16Type: case Int8Type: case NumType: case Num32Type: {
        const char *fmt;
        switch (t->kind) {
        case Int32Type: case Int16Type: case Int8Type: fmt = "%d"; break;
        case NumType: case Num32Type: fmt = "%g"; break;
        default: fmt = "%ld"; break;
        }

        gcc_lvalue_t *str = gcc_local(func, NULL, gcc_type(env->ctx, STRING), fresh("str"));
        gcc_rvalue_t *args[] = {
            gcc_lvalue_address(str, NULL),
            gcc_new_string(env->ctx, fmt),
            obj,
        };
        gcc_func_t *cord_sprintf = hashmap_gets(env->global_funcs, "CORD_sprintf");
        gcc_eval(block, NULL, gcc_call(env->ctx, NULL, cord_sprintf, 3, args));
        gcc_return(block, NULL, CORD_str(gcc_lvalue_as_rvalue(str)));
        break;
    }
    case RangeType: {
        errx(1, "This should be handled by an externally defined function.");
    }
    case OptionalType: {
        gcc_block_t *nil_block = gcc_new_block(func, NULL);
        gcc_block_t *nonnil_block = gcc_new_block(func, NULL);

        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_rvalue_t *is_nil;
        if (gcc_type_if_pointer(gcc_t))
            is_nil = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, obj, gcc_null(env->ctx, gcc_t));
        else
            is_nil = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, obj, gcc_zero(env->ctx, gcc_t));

        gcc_jump_condition(block, NULL, is_nil, nil_block, nonnil_block);
        block = NULL;

        gcc_return(nil_block, NULL, LITERAL("(nil)"));

        gcc_rvalue_t *args[] = {
            obj,
            gcc_param_as_rvalue(params[1]),
        };
        gcc_func_t *tostring = get_tostring_func(env, t->nonnil);
        gcc_rvalue_t *ret = tostring ? gcc_call(env->ctx, NULL, tostring, 2, args) : obj;
        gcc_return(nonnil_block, NULL, ret);
        break;
    }
    case ListType: {
        compile_list_tostring_func(env, &block, obj, t);
        break;
    }

    default: {
        fprintf(stderr, "\x1b[31;1mtostring(%s) function is not yet implemented!\n", type_to_string(t));
        exit(1);
    }
    }
    return func;
#undef CORD_str 
#undef LITERAL
}

void coerce_numbers(env_t *env, bl_type_t *lhs_type, gcc_rvalue_t **lhs, bl_type_t *rhs_type, gcc_rvalue_t **rhs)
{
    if (numtype_priority(lhs_type) < numtype_priority(rhs_type))
        *lhs = gcc_cast(env->ctx, NULL, *lhs, bl_type_to_gcc(env, rhs_type));
    else if (numtype_priority(lhs_type) > numtype_priority(rhs_type))
        *rhs = gcc_cast(env->ctx, NULL, *rhs, bl_type_to_gcc(env, lhs_type));
}

gcc_lvalue_t *get_lvalue(env_t *env, gcc_block_t **block, ast_t *ast)
{
    (void)block;
    switch (ast->kind) {
    case Var: {
        binding_t *binding = hashmap_get(env->bindings, ast->str);
        if (binding) {
            return binding->lval;
        } else {
            ERROR(env, ast, "Error: variable is not defined"); 
        }
    }
    default:
        ERROR(env, ast, "This is not a valid Lvalue");
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
