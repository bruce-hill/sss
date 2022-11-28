// Compilation logic for expressions
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

static gcc_rvalue_t *move_to_heap(env_t *env, gcc_block_t **block, bl_type_t *t, gcc_rvalue_t *val)
{
    gcc_func_t *gc_malloc_func = hashmap_gets(env->global_funcs, "GC_malloc");
    gcc_func_t *func = gcc_block_func(*block);
    gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), gcc_sizeof(env, t));
    gcc_type_t *gcc_t = gcc_get_ptr_type(bl_type_to_gcc(env, t));
    gcc_lvalue_t *tmp = gcc_local(func, NULL, gcc_t, fresh("tmp"));
    // TODO: use gc_malloc_atomic() when possible
    gcc_assign(*block, NULL, tmp, gcc_cast(env->ctx, NULL, gcc_call(env->ctx, NULL, gc_malloc_func, 1, &size), gcc_t));
    gcc_assign(*block, NULL, gcc_rvalue_dereference(gcc_lvalue_as_rvalue(tmp), NULL), val);
    return gcc_lvalue_as_rvalue(tmp);
}

static void compile_for_body(env_t *env, gcc_block_t **block, iterator_info_t *info, void *data) {
    ast_t *ast = data;
    if (ast->for_loop.key)
        hashmap_set(env->bindings, ast->for_loop.key->str, new(binding_t, .rval=info->key_rval, .type=info->key_type));

    if (ast->for_loop.value)
        hashmap_set(env->bindings, ast->for_loop.value->str, new(binding_t, .rval=info->value_rval, .type=info->value_type));

    if (ast->for_loop.body)
        compile_block_statement(env, block, ast->for_loop.body);
}

static void compile_for_between(env_t *env, gcc_block_t **block, iterator_info_t *info, void *data) {
    (void)info;
    ast_t *ast = data;
    if (ast->for_loop.between)
        compile_block_statement(env, block, ast->for_loop.between);
}

static void compile_while_body(env_t *env, gcc_block_t **block, iterator_info_t *info, void *data) {
    (void)info;
    ast_t *ast = data;
    if (ast->loop.body)
        compile_block_statement(env, block, ast->loop.body);
}

static void compile_while_between(env_t *env, gcc_block_t **block, iterator_info_t *info, void *data) {
    (void)info;
    ast_t *ast = data;
    if (ast->loop.between)
        compile_block_statement(env, block, ast->loop.between);
}

gcc_rvalue_t *compile_expr(env_t *env, gcc_block_t **block, ast_t *ast)
{
    switch (ast->kind) {
    case Var: {
        binding_t *binding = hashmap_get(env->bindings, ast->str);
        if (binding) {
            if (binding->rval)
                return binding->rval;
            if (binding->lval)
                return gcc_lvalue_as_rvalue(binding->lval);
            if (binding->func)
                return gcc_get_func_address(binding->func, NULL);
        }
        ERROR(env, ast, "I can't find a definition for this variable"); 
    }
    case Declare: {
        gcc_rvalue_t *rval = compile_expr(env, block, ast->rhs);
        bl_type_t *t = get_type(env->file, env->bindings, ast->rhs);
        assert(t);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *lval = gcc_local(func, ast_loc(env, ast->lhs), gcc_t, fresh(ast->lhs->str));
        hashmap_set(env->bindings, ast->lhs->str,
                    new(binding_t, .lval=lval, .rval=gcc_lvalue_as_rvalue(lval), .type=t));
        assert(rval);
        gcc_assign(*block, ast_loc(env, ast), lval, rval);
        return gcc_lvalue_as_rvalue(lval);
    }

    case Extern: {
        bl_type_t *t = get_type(env->file, env->bindings, ast->type);
        assert(t->kind == TypeType);
        t = t->type;
        if (t->kind == FunctionType) {
            gcc_type_t *gcc_ret_t = bl_type_to_gcc(env, t->ret);
            NEW_LIST(gcc_param_t*, params);
            foreach (t->args, arg, _) {
                gcc_type_t *arg_t = bl_type_to_gcc(env, *arg);
                APPEND(params, gcc_new_param(env->ctx, NULL, arg_t, fresh("arg")));
            }
            assert(ast->expr->kind == Var);
            gcc_func_t *func = gcc_new_func(
                env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_ret_t, ast->expr->str, LIST_LEN(params), params[0], 0);
            hashmap_set(env->bindings, ast->expr->str, new(binding_t, .func=func, .type=t, .is_global=true));
        } else {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
            assert(ast->expr->kind == Var);
            gcc_rvalue_t *glob = gcc_lvalue_as_rvalue(gcc_global(env->ctx, ast_loc(env, ast), GCC_GLOBAL_IMPORTED, gcc_t, ast->expr->str));
            hashmap_set(env->bindings, ast->expr->str,
                        new(binding_t, .rval=glob, .type=t, .is_global=true));
        }
        return NULL;
    }

    case Assign: {
        int64_t len = length(ast->multiassign.lhs);
        NEW_LIST(gcc_lvalue_t*, lvals);
        foreach (ast->multiassign.lhs, lhs, _) {
            append(lvals, get_lvalue(env, block, *lhs));
        }
        gcc_func_t *func = gcc_block_func(*block);
        NEW_LIST(gcc_rvalue_t*, rvals);
        for (int64_t i = 0; i < len; i++) {
            bl_type_t *t_lhs = get_type(env->file, env->bindings, ith(ast->multiassign.lhs, i));
            bl_type_t *t_rhs = get_type(env->file, env->bindings, ith(ast->multiassign.rhs, i));
            gcc_rvalue_t *rval = compile_expr(env, block, ith(ast->multiassign.rhs, i));

            if (is_numeric(t_lhs) && is_numeric(t_rhs) && numtype_priority(t_lhs) >= numtype_priority(t_rhs)) {
                rval = gcc_cast(env->ctx, NULL, rval, bl_type_to_gcc(env, t_lhs));
            } else if (type_is_a(t_rhs, t_lhs) && t_lhs->kind == OptionalType && t_rhs->kind != OptionalType) {
                rval = move_to_heap(env, block, t_rhs, rval);
            } else if (!type_is_a(t_rhs, t_lhs)) {
                ERROR(env, ith(ast->multiassign.rhs, i), "This value is a %s, but it needs to be a %s",
                      type_to_string(t_rhs), type_to_string(t_lhs));
            }

            if (len > 1) {
                gcc_lvalue_t *tmp = gcc_local(func, NULL, bl_type_to_gcc(env, t_rhs), fresh("tmp"));
                assert(rval);
                gcc_assign(*block, NULL, tmp, rval);
                append(rvals, gcc_lvalue_as_rvalue(tmp));
            } else {
                append(rvals, rval);
            }
        }
        for (int64_t i = 0; i < len; i++)
            gcc_assign(*block, ast_loc(env, ast), ith(lvals, i), ith(rvals, i));
        return ith(rvals, length(rvals)-1);
    }
    case Do: {
        // TODO: support do/else?
        if (length(ast->children) > 1)
            ERROR(env, ith(ast->children, 1), "`do` statments with else clauses are not currently supported");
        return compile_expr(env, block, ith(ast->children, 0));
    }
    case Block: {
        // Create scope:
        env_t block_env = *env;
        block_env.bindings = hashmap_new();
        block_env.bindings->fallback = env->bindings;
        return compile_block_expr(&block_env, block, ast);
    }
    case FunctionDef: case Lambda: {
        binding_t *binding = hashmap_get(env->bindings, ast->fn.name);
        if (binding && binding->func) {
            return binding->rval;
        } else {
            // At this point, either this is a lambda or a function def used as a value
            // instead of a statement
            gcc_func_t *func = get_function_def(env, ast, false);
            compile_function(env, func, ast);
            return gcc_get_func_address(func, NULL);
        }
    }
    case Return: {
        assert(env->return_type);
        if (env->return_type->kind == VoidType) {
            if (ast->child)
                ERROR(env, ast, "I was expecting a plain `return` with no expression here, because the function this is inside has no declared return type. If you want to return a value, please change the function's definition to include a return type.");
            gcc_return_void(*block, NULL);
        } else {
            if (!ast->child)
                ERROR(env, ast, "I was expecting this `return` to have a value of type %s because of the function's type signature, but no value is being returned here.",
                      type_to_string(env->return_type));
            bl_type_t *t = get_type(env->file, env->bindings, ast->child);
            gcc_rvalue_t *val = compile_expr(env, block, ast->child);
            if (is_numeric(t) && is_numeric(env->return_type) && numtype_priority(t) < numtype_priority(env->return_type))
                val = gcc_cast(env->ctx, NULL, val, bl_type_to_gcc(env, env->return_type));
            else if (env->return_type->kind == OptionalType && t->kind != OptionalType
                && !gcc_type_if_pointer(bl_type_to_gcc(env, t)))
                val = move_to_heap(env, block, t, val);
            else if (!type_is_a(t, env->return_type))
                ERROR(env, ast, "I was expecting this `return` to have value of type %s because of the function's type signature, but this value has type %s",
                      type_to_string(env->return_type), type_to_string(t));

            gcc_return(*block, NULL, val);
        }
        *block = NULL;
        return NULL;
    }
    case Int: {
        return gcc_int64(env->ctx, ast->i);
    }
    case Num: {
        return gcc_rvalue_from_double(env->ctx, gcc_type(env->ctx, DOUBLE), ast->n);
    }
    case StringLiteral: {
        return gcc_new_string(env->ctx, ast->str);
    }
    case StringJoin: {
        gcc_func_t *intern_str_func = hashmap_gets(env->global_funcs, "intern_str");
        // Optimize to avoid using cords in the cases of 0 or 1 string chunks/interpolations
        if (length(ast->children) == 0) {
            gcc_rvalue_t *empty = gcc_new_string(env->ctx, "");
            return gcc_call(env->ctx, NULL, intern_str_func, 1, &empty);
        } else if (length(ast->children) == 1) {
            ast_t *child = ith(ast->children, 0);
            gcc_rvalue_t *str = compile_expr(env, block, child);
            bl_type_t *t = get_type(env->file, env->bindings, child);
            gcc_func_t *tostring = get_tostring_func(env, t);
            gcc_rvalue_t *args[] = {str, gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR))};
            str = tostring ? gcc_call(env->ctx, ast_loc(env, child), tostring, 2, args) : args[0];
            return gcc_call(env->ctx, NULL, intern_str_func, 1, &str);
        }
        gcc_rvalue_t *str = gcc_null(env->ctx, gcc_type(env->ctx, STRING));
        gcc_func_t *CORD_cat_func = hashmap_gets(env->global_funcs, "CORD_cat");
        gcc_func_t *CORD_to_char_star_func = hashmap_gets(env->global_funcs, "CORD_to_char_star");

        foreach (ast->children, chunk, _) {
            gcc_rvalue_t *val;
            bl_type_t *t = get_type(env->file, env->bindings, *chunk);
            if (t->kind == StringType) {
                val = compile_expr(env, block, *chunk);
            } else {
                gcc_func_t *tostring = get_tostring_func(env, t);
                gcc_rvalue_t *args[] = {
                    compile_expr(env, block, *chunk),
                    gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR)),
                };
                val = tostring ? gcc_call(env->ctx, ast_loc(env, *chunk), tostring, 2, args) : args[0];
            }
            str = gcc_call(env->ctx, ast_loc(env, *chunk), CORD_cat_func, 2, (gcc_rvalue_t*[]){str, val});
        }
        str = gcc_call(env->ctx, ast_loc(env, ast), CORD_to_char_star_func, 1, &str);
        str = gcc_call(env->ctx, ast_loc(env, ast), intern_str_func, 1, &str);
        return str;
    }
    case List: {
        return compile_list(env, block, ast);
    }
    case StructDef: {
        foreach (ast->struct_.members, member, _) {
            if ((*member)->kind == FunctionDef) {
                istr_t name = intern_strf("%s.%s", ast->struct_.name, (*member)->fn.name);
                binding_t *binding = hashmap_get(env->bindings, name);
                assert(binding);
                compile_function(env, binding->func, *member);
            } else if ((*member)->kind == Declare) {
                ast_t *decl = *member;
                gcc_rvalue_t *rval = compile_expr(env, block, decl->rhs);
                bl_type_t *t = get_type(env->file, env->bindings, decl->rhs);
                assert(t);
                gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
                gcc_func_t *func = gcc_block_func(*block);
                gcc_lvalue_t *lval = gcc_local(func, ast_loc(env, decl->lhs), gcc_t, fresh(decl->lhs->str));
                istr_t name = intern_strf("%s.%s", ast->struct_.name, decl->lhs->str);
                hashmap_set(env->bindings, name,
                            new(binding_t, .lval=lval, .rval=gcc_lvalue_as_rvalue(lval), .type=t));
                assert(rval);
                gcc_assign(*block, ast_loc(env, decl), lval, rval);
            }
        }

        return NULL;
    }
    case Struct: {
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        if (length(ast->struct_.members) > length(t->struct_.field_names))
            ERROR(env, ast, "I expected this %s literal to only have %ld fields, but you provided %ld fields.",
                  type_to_string(t),
                  length(t->struct_.field_names), length(ast->struct_.members));

        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
        size_t num_fields = gcc_field_count(gcc_struct);
        assert(num_fields == (size_t)length(t->struct_.field_names));
        gcc_field_t *unused_fields[num_fields];
        for (size_t i = 0, count = gcc_field_count(gcc_struct); i < count; i++)
            unused_fields[i] = gcc_get_field(gcc_struct, i);

        size_t num_values = length(ast->struct_.members);

        struct {
            size_t field_num;
            ast_t *ast;
            gcc_rvalue_t *value;
            gcc_field_t *field;
        } entries[num_values];

        for (size_t i = 0; i < num_values; i++) {
            ast_t *member = ith(ast->struct_.members, i);
            assert(member->kind == StructField);
            entries[i].ast = member->named.value;
            entries[i].value = compile_expr(env, block, member->named.value);
        }

        // Put in named fields first:
        for (size_t value_index = 0; value_index < num_values; value_index++) {
            ast_t *member = ith(ast->struct_.members, value_index);
            if (!member->named.name) continue;
            for (size_t field_index = 0; field_index < num_fields; field_index++) {
                if (ith(t->struct_.field_names, field_index) != member->named.name)
                    continue;

                gcc_field_t *field = unused_fields[field_index];
                if (!field)
                    ERROR(env, member, "You already provided a value for this field earlier in this struct.");

                // Found the field:
                entries[value_index].field = field;
                entries[value_index].field_num = field_index;
                unused_fields[field_index] = NULL;
                goto found_name;
            }

            ERROR(env, member, "There is no struct field with this name");

          found_name: continue;
        }

        // Now put in unnamed fields:
        for (size_t value_index = 0; value_index < num_values; value_index++) {
            ast_t *member = ith(ast->struct_.members, value_index);
            if (member->named.name) continue;
            for (size_t field_index = 0; field_index < num_fields; field_index++) {
                if (!unused_fields[field_index])
                    continue;

                // Found the field:
                entries[value_index].field = unused_fields[field_index];
                entries[value_index].field_num = field_index;
                unused_fields[field_index] = NULL;
                goto found_index;
            }

            // Unreachable, this should be handled earlier
            ERROR(env, member, "This field is beyond the number of fields in this struct.");

          found_index: continue;
        }

        for (size_t field_index = 0; field_index < num_fields; field_index++) {
            bl_type_t *ft = ith(t->struct_.field_types, field_index);
            if (ft->kind == OptionalType) continue;
            if (unused_fields[field_index])
                ERROR(env, ast, "%s structs are supposed to have a non-optional field '%s' (%s), but you didn't provide a value for it.",
                      type_to_string(t),
                      ith(t->struct_.field_names, field_index), type_to_string(ft));
        }

        // GCC is dumb and requires sorting the fields:
        qsort_r(entries, num_values, sizeof(entries[0]), (int(*)(const void*,const void*,void*))(void*)memcmp, (void*)sizeof(size_t));
        gcc_field_t *populated_fields[num_values];
        gcc_rvalue_t *rvalues[num_values];
        for (size_t i = 0; i < num_values; i++) {
            // Check type:
            bl_type_t *expected = ith(t->struct_.field_types, entries[i].field_num);
            bl_type_t *actual = get_type(env->file, env->bindings, entries[i].ast);

            if (expected == actual)
                rvalues[i] = entries[i].value;
            else if (expected->kind == OptionalType && actual->kind != OptionalType
                && !gcc_type_if_pointer(bl_type_to_gcc(env, actual)))
                rvalues[i] = move_to_heap(env, block, actual, entries[i].value);
            else if (is_numeric(expected) && is_numeric(actual)
                     && numtype_priority(expected) >= numtype_priority(actual))
                rvalues[i] = gcc_cast(env->ctx, NULL, entries[i].value, bl_type_to_gcc(env, expected));
            else
                ERROR(env, entries[i].ast, "I was expecting a value of type %s for the %s.%s field, but this value is a %s.", 
                      type_to_string(expected), type_to_string(t), ith(t->struct_.field_names, entries[i].field_num),
                      type_to_string(actual));

            populated_fields[i] = entries[i].field;
        }

        gcc_rvalue_t *rval = gcc_struct_constructor(env->ctx, NULL, gcc_t, num_values, populated_fields, rvalues);
        assert(rval);
        return rval;
    }
    case FunctionCall: {
        gcc_rvalue_t *fn_ptr = NULL;
        gcc_func_t *fn = NULL;
        bl_type_t *fn_t = get_type(env->file, env->bindings, ast->call.fn);
        NEW_LIST(gcc_rvalue_t*, arg_vals);
        int64_t num_selfs = 0;
        // Method calls:
        if (ast->call.fn->kind == Index) {
            ast_t *self = ast->call.fn->indexed;
            bl_type_t *self_t = get_type(env->file, env->bindings, self);
            switch (self_t->kind) {
            case TypeType: {
                if (ast->call.fn->index->kind != FieldName)
                    ERROR(env, ast->call.fn, "I was expecting this to be a field index like Foo.baz()");
                istr_t method_name = intern_strf("%s.%s", type_to_string(self_t->type), ast->call.fn->index->str);
                binding_t *binding = hashmap_get(env->bindings, method_name);
                if (!binding)
                    ERROR(env, ast->call.fn, "I couldn't find any method called %s for %s.", ast->call.fn->index->str, type_to_string(self_t->type));
                fn = binding->func;
                fn_ptr = binding->rval;
                break;
            }
            default: {
                if (ast->call.fn->index->kind != FieldName)
                    ERROR(env, ast->call.fn, "I was expecting this to be a field index like foo.baz()");
                istr_t method_name = intern_strf("%s.%s", type_to_string(self_t), ast->call.fn->index->str);
                binding_t *binding = hashmap_get(env->bindings, method_name);
                if (!binding)
                    ERROR(env, ast->call.fn, "I couldn't find a method with this name defined for a %s.", type_to_string(self_t));
                gcc_rvalue_t *self_val = compile_expr(env, block, self);
                append(arg_vals, self_val);
                num_selfs += 1;
                fn = binding->func;
                fn_ptr = binding->rval;
                break;
            }
            }
        } else {
            fn_ptr = compile_expr(env, block, ast->call.fn);
            fn = NULL;
        }
        // TODO: keyword args
        foreach (ast->call.args, arg, _) {
            gcc_rvalue_t *val = compile_expr(env, block, *arg);
            bl_type_t *actual = get_type(env->file, env->bindings, *arg);
            bl_type_t *expected = ith(fn_t->args, num_selfs + (int64_t)(arg - *ast->call.args));
            // Numeric promotion:
            if (is_numeric(actual) && is_numeric(expected) && numtype_priority(actual) < numtype_priority(expected))
                val = gcc_cast(env->ctx, NULL, val, bl_type_to_gcc(env, expected));
            append(arg_vals, val);
        }

        // Optional values get passed as nil:
        for (int64_t i = length(arg_vals); i < length(fn_t->args); i++) {
            gcc_rvalue_t *nil = gcc_null(env->ctx, bl_type_to_gcc(env, ith(fn_t->args, i)));
            append(arg_vals, nil);
        }

        if (fn)
            return gcc_call(env->ctx, ast_loc(env, ast), fn, length(arg_vals), arg_vals[0]);
        else if (fn_ptr)
            return gcc_call_ptr(env->ctx, ast_loc(env, ast), fn_ptr, length(arg_vals), arg_vals[0]);
        else
            assert(false);
    }
    case KeywordArg: {
        return compile_expr(env, block, ast->named.value);
    }
    case Bool: {
        return gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, BOOL), ast->b ? 1 : 0);
    }
    case Maybe: {
        gcc_rvalue_t *rval = compile_expr(env, block, ast->child);
        bl_type_t *nonnil_t = get_type(env->file, env->bindings, ast->child);
        gcc_type_t *gcc_nonnil_t = bl_type_to_gcc(env, nonnil_t);
        if (!gcc_type_if_pointer(gcc_nonnil_t))
            rval = move_to_heap(env, block, nonnil_t, rval);
        return rval;
    }
    case Len: {
        bl_type_t *t = get_type(env->file, env->bindings, ast->child);
        gcc_rvalue_t *obj = compile_expr(env, block, ast->child);
        switch (t->kind) {
        case ListType: {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
            gcc_struct_t *list_struct = gcc_type_if_struct(gcc_type_if_pointer(gcc_t));
            return gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(obj, NULL, gcc_get_field(list_struct, 1)));
        }
        case StringType: case DSLType: case TypeType: {
            gcc_func_t *len_func = hashmap_gets(env->global_funcs, "intern_len");
            gcc_rvalue_t *len = gcc_call(env->ctx, ast_loc(env, ast), len_func, 1, &obj);
            return gcc_cast(env->ctx, ast_loc(env, ast), len, gcc_type(env->ctx, INT64));
        }
        case RangeType: {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
            gcc_struct_t *range_struct = gcc_type_if_struct(gcc_t);
            assert(range_struct);
            gcc_rvalue_t *first = gcc_rvalue_access_field(obj, NULL, gcc_get_field(range_struct, 0)),
                         *step = gcc_rvalue_access_field(obj, NULL, gcc_get_field(range_struct, 1)),
                         *last = gcc_rvalue_access_field(obj, NULL, gcc_get_field(range_struct, 2));
            gcc_type_t *i64_t = gcc_type(env->ctx, INT64);
#define BINOP(a,op,b) gcc_binary_op(env->ctx, NULL, GCC_BINOP_ ## op, i64_t, a, b)
#define UNOP(op,a) gcc_unary_op(env->ctx, NULL, GCC_UNOP_ ## op, i64_t, a)
            // (last - first)//step + 1
            gcc_func_t *func = gcc_block_func(*block);
            gcc_lvalue_t *len_var = gcc_local(func, NULL, i64_t, fresh("len"));
            gcc_assign(*block, NULL, len_var, BINOP(BINOP(BINOP(last, MINUS, first), DIVIDE, step), PLUS, gcc_one(env->ctx, i64_t)));
            // If less than zero, set to zero (without a conditional branch)
            // len = len & ~(len >> 63)
            gcc_rvalue_t *len = gcc_lvalue_as_rvalue(len_var);
            return BINOP(len, BITWISE_AND, UNOP(BITWISE_NEGATE, BINOP(len, RSHIFT, gcc_int64(env->ctx, 63))));
#undef BINOP
            return len;
        }
        default: ERROR(env, ast, "I don't know how to get the length of a %s", type_to_string(t));
        }
    }
    case Index: {
        (void)get_type(env->file, env->bindings, ast); // typecheck
        bl_type_t *indexed_t = get_type(env->file, env->bindings, ast->indexed);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, indexed_t);
        gcc_rvalue_t *obj = compile_expr(env, block, ast->indexed);
        if (ast->index->kind == FieldName) {
            if (indexed_t->kind == StructType) {
                assert(ast->index->kind == FieldName);
                for (int64_t i = 0, len = LIST_LEN(indexed_t->struct_.field_names); i < len; i++) {
                    if (LIST_ITEM(indexed_t->struct_.field_names, i) == ast->index->str) {
                        gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
                        gcc_field_t *field = gcc_get_field(gcc_struct, (size_t)i);
                        return gcc_rvalue_access_field(obj, NULL, field);
                    }
                }
            }
            bl_type_t *namespace = indexed_t->kind == TypeType ? indexed_t->type : indexed_t;
            istr_t name = intern_strf("%s.%s", type_to_string(namespace), ast->index->str);
            binding_t *binding = hashmap_get(env->bindings, name);
            if (binding)
                return binding->rval;
            else
                ERROR(env, ast, "I can't find any field or method called \"%s\" on a %s.", ast->index->str, type_to_string(indexed_t));
        } else {
            if (indexed_t->kind != ListType)
                ERROR(env, ast, "I only know how to index into lists, but this is a %s", type_to_string(indexed_t));

            gcc_struct_t *list_struct = gcc_type_if_struct(gcc_type_if_pointer(gcc_t));
            gcc_rvalue_t *items = gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(obj, NULL, gcc_get_field(list_struct, 0)));
            gcc_rvalue_t *index = compile_expr(env, block, ast->index);
            // Bounds check:
            gcc_type_t *i64_t = gcc_type(env->ctx, INT64);
            gcc_rvalue_t *big_enough = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GE, index, gcc_one(env->ctx, i64_t));
            gcc_rvalue_t *len = gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(obj, NULL, gcc_get_field(list_struct, 1)));
            gcc_rvalue_t *small_enough = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LE, index, len);
            gcc_rvalue_t *ok = gcc_binary_op(env->ctx, NULL, GCC_BINOP_LOGICAL_AND, gcc_type(env->ctx, BOOL), big_enough, small_enough);

            gcc_func_t *func = gcc_block_func(*block);
            gcc_block_t *bounds_safe = gcc_new_block(func, fresh("bounds_safe")),
                        *bounds_unsafe = gcc_new_block(func, fresh("bounds_unsafe"));
            gcc_jump_condition(*block, NULL, ok, bounds_safe, bounds_unsafe);

            // Bounds check failure:
            gcc_rvalue_t *fmt = gcc_new_string(env->ctx, "\x1b[31;7mError: invalid list index: %ld (list is size %ld)\x1b[m\n\n%s");
            char *info = NULL;
            size_t size = 0;
            FILE *f = open_memstream(&info, &size);
            highlight_match(f, env->file, ast->match, 2);
            fputc('\0', f);
            fflush(f);
            gcc_rvalue_t *callstack = gcc_new_string(env->ctx, info);
            gcc_func_t *fail = hashmap_gets(env->global_funcs, "fail");
            gcc_eval(bounds_unsafe, NULL, gcc_call(env->ctx, NULL, fail, 4, (gcc_rvalue_t*[]){fmt, index, len, callstack}));
            fclose(f);
            gcc_jump(bounds_unsafe, NULL, bounds_unsafe);

            // Bounds check success:
            *block = bounds_safe;
            gcc_rvalue_t *index0 = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, i64_t, index, gcc_one(env->ctx, i64_t));
            return gcc_lvalue_as_rvalue(gcc_array_access(env->ctx, NULL, items, index0));
        }
    }
    case TypeOf: {
        gcc_func_t *intern_str_func = hashmap_gets(env->global_funcs, "intern_str");
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        return gcc_call(env->ctx, NULL, intern_str_func, 1,
                        (gcc_rvalue_t*[]){gcc_new_string(env->ctx, type_to_string(t->type))});
    }
    case SizeOf: {
        bl_type_t *t = get_type(env->file, env->bindings, ast->child);
        ssize_t size = gcc_sizeof(env, t);
        return gcc_int64(env->ctx, size);
    }
    case Cast: {
        gcc_rvalue_t *val = compile_expr(env, block, ast->expr);
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        return gcc_bitcast(env->ctx, NULL, val, bl_type_to_gcc(env, t));
    }
    case As: {
        gcc_rvalue_t *val = compile_expr(env, block, ast->expr);
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        return gcc_cast(env->ctx, NULL, val, bl_type_to_gcc(env, t));
    }
    case Nil: {
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        if (gcc_type_if_pointer(gcc_t))
            return gcc_null(env->ctx, bl_type_to_gcc(env, t));
        else
            return gcc_zero(env->ctx, bl_type_to_gcc(env, t));
    }
    case Not: {
        bl_type_t *t = get_type(env->file, env->bindings, ast->child);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_rvalue_t *val = compile_expr(env, block, ast->child);
        if (t->kind == BoolType)
            return gcc_unary_op(env->ctx, ast_loc(env, ast), GCC_UNOP_LOGICAL_NEGATE, gcc_t, val);
        else if (is_integral(t))
            return gcc_unary_op(env->ctx, ast_loc(env, ast), GCC_UNOP_BITWISE_NEGATE, gcc_t, val);
        else
            ERROR(env, ast, "The 'not' operator isn't supported for values with type %s.", type_to_string(t));
    }
    case Equal: case NotEqual: {
        (void)get_type(env->file, env->bindings, ast); // Check type
        gcc_rvalue_t *lhs_val = compile_expr(env, block, ast->lhs);
        gcc_rvalue_t *rhs_val = compile_expr(env, block, ast->rhs);
        coerce_numbers(
            env, get_type(env->file, env->bindings, ast->lhs), &lhs_val,
            get_type(env->file, env->bindings, ast->rhs), &rhs_val);
        return gcc_comparison(env->ctx, NULL, ast->kind == Equal ? GCC_COMPARISON_EQ : GCC_COMPARISON_NE, lhs_val, rhs_val);
    }
    case Less: case LessEqual: case Greater: case GreaterEqual: {
        bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
        bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, ast->lhs);
        gcc_rvalue_t *rhs_val = compile_expr(env, block, ast->rhs);
        coerce_numbers(env, lhs_t, &lhs_val, rhs_t, &rhs_val);
        gcc_comparison_e cmp;
        switch (ast->kind) {
        case Less: cmp = GCC_COMPARISON_LT; break;
        case LessEqual: cmp = GCC_COMPARISON_LE; break;
        case Greater: cmp = GCC_COMPARISON_GT; break;
        case GreaterEqual: cmp = GCC_COMPARISON_GE; break;
        default: assert(false);
        }
        if (lhs_t == rhs_t && (lhs_t->kind == StringType || lhs_t->kind == DSLType)) {
            gcc_param_t *cmp_params[] = {
                gcc_new_param(env->ctx, NULL, bl_type_to_gcc(env, lhs_t), "str1"),
                gcc_new_param(env->ctx, NULL, bl_type_to_gcc(env, rhs_t), "str2"),
            };
            gcc_func_t *cmp_func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(env->ctx, INT), "strcmp", 2, cmp_params, 0);
            return gcc_comparison(env->ctx, NULL, cmp,
                                  gcc_call(env->ctx, NULL, cmp_func, 2, (gcc_rvalue_t*[]){lhs_val, rhs_val}),
                                  gcc_zero(env->ctx, gcc_type(env->ctx, INT)));
        } else if (lhs_t == rhs_t && is_numeric(lhs_t)) {
            return gcc_comparison(env->ctx, NULL, cmp, lhs_val, rhs_val);
        }
        ERROR(env, ast, "I don't know how to do ordered comparison between a %s and a %s.", type_to_string(lhs_t), type_to_string(rhs_t));
    }
    case Negative: {
        bl_type_t *t = get_type(env->file, env->bindings, ast->child);
        if (!is_numeric(t))
            ERROR(env, ast, "I only know how to negate numbers, not %s", type_to_string(t));
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_rvalue_t *rval = compile_expr(env, block, ast->child);
        return gcc_unary_op(env->ctx, NULL, GCC_UNOP_MINUS, gcc_t, rval);
    }
    case And: {
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
        bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, ast->lhs);
        if (t->kind == BoolType && rhs_t->kind == BoolType) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, ast->rhs);
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_LOGICAL_AND, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        } else if (is_integral(lhs_t) && is_integral(rhs_t)) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, ast->rhs);
            // Numeric promotion:
            if (numtype_priority(rhs_t) < numtype_priority(t))
                rhs_val = gcc_cast(env->ctx, NULL, rhs_val, bl_type_to_gcc(env, t));
            if (numtype_priority(lhs_t) < numtype_priority(t))
                lhs_val = gcc_cast(env->ctx, NULL, lhs_val, bl_type_to_gcc(env, t));
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_BITWISE_AND, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        }
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *result = gcc_local(func, NULL, bl_type_to_gcc(env, t), fresh("and_result"));
        gcc_block_t *if_truthy = gcc_new_block(func, fresh("and_truthy"));
        gcc_block_t *if_falsey = gcc_new_block(func, fresh("and_falsey"));
        gcc_block_t *done = gcc_new_block(func, fresh("and_done"));

        gcc_type_t *lhs_gcc_t = bl_type_to_gcc(env, lhs_t);
        gcc_rvalue_t *bool_val = lhs_val;
        if (t->kind != BoolType) {
            gcc_rvalue_t *zero = gcc_type_if_pointer(lhs_gcc_t) ? gcc_null(env->ctx, lhs_gcc_t) : gcc_zero(env->ctx, lhs_gcc_t);
            bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, lhs_val, zero);
        }
        gcc_jump_condition(*block, NULL, bool_val, if_truthy, if_falsey);

        if (rhs_t->kind == AbortType) {
            compile_statement(env, &if_truthy, ast->rhs);
        } else {
            gcc_rvalue_t *rhs_val = compile_expr(env, &if_truthy, ast->rhs);
            if (if_truthy)
                gcc_assign(if_truthy, NULL, result, rhs_val);
        }
        if (if_truthy)
            gcc_jump(if_truthy, NULL, done);

        gcc_assign(if_falsey, NULL, result, lhs_val);
        gcc_jump(if_falsey, NULL, done);

        *block = done;
        return gcc_lvalue_as_rvalue(result);
    }
    case Or: {
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
        bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, ast->lhs);
        if (t->kind == BoolType && rhs_t->kind == BoolType) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, ast->rhs);
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_LOGICAL_OR, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        } else if (is_integral(lhs_t) && is_integral(rhs_t)) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, ast->rhs);
            // Numeric promotion:
            if (numtype_priority(rhs_t) < numtype_priority(t))
                rhs_val = gcc_cast(env->ctx, NULL, rhs_val, bl_type_to_gcc(env, t));
            if (numtype_priority(lhs_t) < numtype_priority(t))
                lhs_val = gcc_cast(env->ctx, NULL, lhs_val, bl_type_to_gcc(env, t));
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_BITWISE_OR, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        }
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *result = gcc_local(func, NULL, bl_type_to_gcc(env, t), fresh("and_result"));
        gcc_block_t *if_truthy = gcc_new_block(func, fresh("or_truthy"));
        gcc_block_t *if_falsey = gcc_new_block(func, fresh("or_falsey"));
        gcc_block_t *done = gcc_new_block(func, fresh("or_done"));

        gcc_type_t *lhs_gcc_t = bl_type_to_gcc(env, lhs_t);
        gcc_rvalue_t *bool_val = lhs_val;
        if (t->kind != BoolType) {
            gcc_rvalue_t *zero = gcc_type_if_pointer(lhs_gcc_t) ? gcc_null(env->ctx, lhs_gcc_t) : gcc_zero(env->ctx, lhs_gcc_t);
            bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, lhs_val, zero);

            if (lhs_t->kind == OptionalType && t->kind != OptionalType
                && !gcc_type_if_pointer(bl_type_to_gcc(env, t)))
                lhs_val = gcc_lvalue_as_rvalue(gcc_rvalue_dereference(lhs_val, NULL));
        }
        gcc_jump_condition(*block, NULL, bool_val, if_truthy, if_falsey);

        gcc_assign(if_truthy, NULL, result, lhs_val);
        gcc_jump(if_truthy, NULL, done);

        if (rhs_t->kind == AbortType) {
            compile_statement(env, &if_falsey, ast->rhs);
        } else {
            gcc_rvalue_t *rhs_val = compile_expr(env, &if_falsey, ast->rhs);
            if (rhs_t->kind == OptionalType && t->kind != OptionalType
                && !gcc_type_if_pointer(bl_type_to_gcc(env, t)))
                rhs_val = gcc_lvalue_as_rvalue(gcc_rvalue_dereference(rhs_val, NULL));
            if (if_falsey)
                gcc_assign(if_falsey, NULL, result, rhs_val);
        }
        if (if_falsey)
            gcc_jump(if_falsey, NULL, done);

        *block = done;
        return gcc_lvalue_as_rvalue(result);
    }
    case Xor: {
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, ast->lhs);
        gcc_rvalue_t *rhs_val = compile_expr(env, block, ast->rhs);
        return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_BITWISE_XOR, bl_type_to_gcc(env, t), lhs_val, rhs_val);
    }
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate:
    case Add: case Subtract: case Divide: case Multiply: case Modulus: {
        bl_type_t *t = get_type(env->file, env->bindings, ast);

        gcc_binary_op_e op;
        switch (ast->kind) {
        case Add: case AddUpdate: op = GCC_BINOP_PLUS; break;
        case Subtract: case SubtractUpdate: op = GCC_BINOP_MINUS; break;
        case Multiply: case MultiplyUpdate: op = GCC_BINOP_MULT; break;
        case Modulus: op = GCC_BINOP_MODULO; break;
        default: ERROR(env, ast, "Unsupported math operation");
        }

        bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
        bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
        if (AddUpdate <= ast->kind && ast->kind <= DivideUpdate) {
            gcc_lvalue_t *lhs_val = get_lvalue(env, block, ast->lhs);
            gcc_rvalue_t *rhs_val = compile_expr(env, block, ast->rhs);

            // Numeric promotion:
            if (numtype_priority(lhs_t) > numtype_priority(rhs_t))
                rhs_val = gcc_cast(env->ctx, NULL, rhs_val, bl_type_to_gcc(env, lhs_t));
            gcc_update(*block, ast_loc(env, ast), lhs_val, op, rhs_val);
            return gcc_lvalue_as_rvalue(lhs_val);
        } else {
            gcc_rvalue_t *lhs_val = compile_expr(env, block, ast->lhs);
            gcc_rvalue_t *rhs_val = compile_expr(env, block, ast->rhs);
            // Numeric promotion:
            if (numtype_priority(rhs_t) < numtype_priority(t))
                rhs_val = gcc_cast(env->ctx, NULL, rhs_val, bl_type_to_gcc(env, t));
            if (numtype_priority(lhs_t) < numtype_priority(t))
                lhs_val = gcc_cast(env->ctx, NULL, lhs_val, bl_type_to_gcc(env, t));
            return gcc_binary_op(env->ctx, ast_loc(env, ast), op, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        }
    }
    case If: {
        bl_type_t *if_t = get_type(env->file, env->bindings, ast);
        bl_type_t *nonnil_t = if_t->kind == OptionalType ? if_t->nonnil : if_t;
        bool has_value = !(nonnil_t->kind == AbortType || nonnil_t->kind == VoidType);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *if_ret = has_value ? gcc_local(func, NULL, bl_type_to_gcc(env, if_t), fresh("if_value")) : NULL;

        gcc_block_t *end_if = if_t->kind == AbortType ? NULL : gcc_new_block(func, fresh("endif"));

        foreach (ast->clauses, clause, last_clause) {
            gcc_block_t *if_truthy = gcc_new_block(func, fresh("if_true"));
            gcc_block_t *if_falsey = (clause < last_clause || ast->else_body) ? gcc_new_block(func, fresh("elseif")) : end_if;

            ast_t *condition = clause->condition, *body = clause->body;
            env_t branch_env = *env;
            branch_env.bindings = hashmap_new();
            branch_env.bindings->fallback = env->bindings;
            check_truthiness(&branch_env, block, condition, if_truthy, if_falsey);
            gcc_rvalue_t *branch_val = compile_expr(&branch_env, &if_truthy, body);

            if (if_truthy) {
                if (branch_val) {
                    if (if_ret)
                        gcc_assign(if_truthy, NULL, if_ret, branch_val);
                    else
                        gcc_eval(if_truthy, NULL, branch_val);
                }
                gcc_jump(if_truthy, NULL, end_if);
            }
            *block = if_falsey;
        }
        if (ast->else_body) {
            gcc_rvalue_t *branch_val = compile_expr(env, block, ast->else_body);
            if (branch_val) {
                if (if_ret)
                    gcc_assign(*block, NULL, if_ret, branch_val);
                else
                    gcc_eval(*block, NULL, branch_val);
            }
            if (*block)
                gcc_jump(*block, NULL, end_if);
        }
        *block = end_if;
        return if_ret ? gcc_lvalue_as_rvalue(if_ret) : NULL;
    }
    case Range: {
        return compile_range(env, block, ast);
    }
    case For: {
        compile_iteration(env, block, ast, compile_for_body, compile_for_between, (void*)ast);
        return NULL;
    }
    case While: case Repeat: {
        compile_iteration(env, block, ast, compile_while_body, compile_while_between, (void*)ast);
        return NULL;
    }
    case Skip: case Stop: {
        gcc_block_t *jump_dest = NULL;
        if (ast->str) {
            for (loop_label_t *lbl = env->loop_label; lbl; lbl = lbl->enclosing) {
                if (lbl->name == ast->str) {
                    if (ast->kind == Skip) {
                        jump_dest = lbl->skip_label;
                        lbl->skip_reachable = true;
                    } else {
                        jump_dest = lbl->stop_label;
                        lbl->stop_reachable = true;
                    }
                    break;
                }
            }
        } else {
            if (env->loop_label) {
                if (ast->kind == Skip) {
                    jump_dest = env->loop_label->skip_label;
                    env->loop_label->skip_reachable = true;
                } else {
                    jump_dest = env->loop_label->stop_label;
                    env->loop_label->stop_reachable = true;
                }
            }
        }
        if (!jump_dest) ERROR(env, ast, "I'm not sure what %s is referring to", ast->str);
        gcc_jump(*block, NULL, jump_dest);
        *block = NULL;
        return NULL;
    }
    case Fail: {
        gcc_rvalue_t *msg;
        if (ast->child) {
            msg = compile_expr(env, block, ast->child);
        } else {
            msg = gcc_new_string(env->ctx, "A failure occurred");
        }

        gcc_rvalue_t *fmt = gcc_new_string(env->ctx, "\x1b[31;7mError: %s\x1b[m\n\n%s");

        char *info = NULL;
        size_t size = 0;
        FILE *f = open_memstream(&info, &size);
        highlight_match(f, env->file, ast->match, 2);
        fputc('\0', f);
        fflush(f);
        gcc_rvalue_t *callstack = gcc_new_string(env->ctx, info);
        gcc_func_t *fail = hashmap_gets(env->global_funcs, "fail");
        gcc_rvalue_t *ret = gcc_call(env->ctx, NULL, fail, 3, (gcc_rvalue_t*[]){fmt, msg, callstack});
        fclose(f);
        return ret;
    }
    default: break;
    }
    ERROR(env, ast, "I haven't yet implemented compiling for %s", get_ast_kind_name(ast->kind)); 
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
