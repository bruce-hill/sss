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
        ERROR(env, ast, "Error: variable is not defined"); 
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
            hashmap_set(env->bindings, ast->expr->str, new(binding_t, .func=func, .type=t));
        } else {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
            assert(ast->expr->kind == Var);
            gcc_rvalue_t *glob = gcc_lvalue_as_rvalue(gcc_global(env->ctx, ast_loc(env, ast), GCC_GLOBAL_IMPORTED, gcc_t, ast->expr->str));
            hashmap_set(env->bindings, ast->expr->str,
                        new(binding_t, .rval=glob, .type=t));
        }
        return NULL;
    }

    case Assign: {
        int64_t len = length(ast->multiassign.lhs);
        NEW_LIST(gcc_lvalue_t*, lvals);
        foreach (ast->multiassign.lhs, lhs, _) {
            append(lvals, get_lvalue(env, block, *lhs));
        }
        NEW_LIST(gcc_rvalue_t*, rvals);
        for (int64_t i = 0; i < len; i++) {
            bl_type_t *t_lhs = get_type(env->file, env->bindings, ith(ast->multiassign.lhs, i));
            bl_type_t *t_rhs = get_type(env->file, env->bindings, ith(ast->multiassign.rhs, i));
            gcc_rvalue_t *rval = compile_expr(env, block, ith(ast->multiassign.rhs, i));

            if (is_numeric(t_lhs) && is_numeric(t_rhs) && numtype_priority(t_lhs) >= numtype_priority(t_rhs)) {
                rval = gcc_cast(env->ctx, NULL, rval, bl_type_to_gcc(env, t_lhs));
            } else if (!type_is_a(t_rhs, t_lhs)) {
                ERROR(env, ith(ast->multiassign.rhs, i), "This value is a %s, but it needs to be a %s",
                      type_to_string(t_rhs), type_to_string(t_lhs));
            }

            if (len > 1) {
                gcc_func_t *func = gcc_block_func(*block);
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
        if (ast->child) {
            gcc_rvalue_t *val = compile_expr(env, block, ast->child);
            gcc_return(*block, NULL, val);
        } else {
            gcc_return_void(*block, NULL);
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
        return NULL;
    }
    case Struct: {
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        if (length(ast->struct_.members) > length(t->struct_.field_names))
            ERROR(env, ast, "This struct literal has too many fields (expected only %ld)", length(t->struct_.field_names));

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
            gcc_rvalue_t *value;
            gcc_field_t *field;
        } entries[num_values];

        for (size_t i = 0; i < num_values; i++) {
            ast_t *member = ith(ast->struct_.members, i);
            assert(member->kind == StructField);
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
                    ERROR(env, member, "This field is a duplicate of an earlier field")

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

            ERROR(env, member, "This field is beyond the number of fields in this struct");

          found_index: continue;
        }

        // GCC is dumb and requires sorting the fields:
        qsort_r(entries, num_values, sizeof(entries[0]), (int(*)(const void*,const void*,void*))(void*)memcmp, (void*)sizeof(size_t));
        gcc_field_t *populated_fields[num_values];
        gcc_rvalue_t *rvalues[num_values];
        for (size_t i = 0; i < num_values; i++) {
            populated_fields[i] = entries[i].field;
            rvalues[i] = entries[i].value;
        }

        gcc_rvalue_t *rval = gcc_struct_constructor(env->ctx, NULL, gcc_t, num_values, populated_fields, rvalues);
        assert(rval);
        return rval;
    }
    case FunctionCall: {
        gcc_rvalue_t *fn = compile_expr(env, block, ast->call.fn);
        NEW_LIST(gcc_rvalue_t*, arg_vals);
        // TODO: keyword args
        foreach (ast->call.args, arg, _) {
            // TODO: coerce numeric args? sqrt(5) -> sqrt(5.0)
            gcc_rvalue_t *val = compile_expr(env, block, *arg);
            append(arg_vals, val);
        }
        gcc_rvalue_t *call = gcc_call_ptr(env->ctx, ast_loc(env, ast), fn, length(arg_vals), arg_vals[0]);
        return call;
    }
    case KeywordArg: {
        return compile_expr(env, block, ast->named.value);
    }
    case Bool: {
        return gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, BOOL), ast->b ? 1 : 0);
    }
    case Maybe: {
        return compile_expr(env, block, ast->child);
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
        default: ERROR(env, ast, "Length is not implemented for %s", type_to_string(t));
        }
    }
    case Index: {
        (void)get_type(env->file, env->bindings, ast); // typecheck
        bl_type_t *indexed_t = get_type(env->file, env->bindings, ast->indexed);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, indexed_t);
        gcc_rvalue_t *obj = compile_expr(env, block, ast->indexed);
        switch (indexed_t->kind) {
        case StructType: {
            assert(ast->index->kind == FieldName);
            for (int64_t i = 0, len = LIST_LEN(indexed_t->struct_.field_names); i < len; i++) {
                if (LIST_ITEM(indexed_t->struct_.field_names, i) == ast->index->str) {
                    gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
                    gcc_field_t *field = gcc_get_field(gcc_struct, (size_t)i);
                    return gcc_rvalue_access_field(obj, NULL, field);
                }
            }
            ERROR(env, ast->index, "Not a valid field on type %s", type_to_string(indexed_t));
        }
        case ListType: {
            gcc_struct_t *list_struct = gcc_type_if_struct(gcc_type_if_pointer(gcc_t));
            gcc_rvalue_t *items = gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(obj, NULL, gcc_get_field(list_struct, 0)));
            gcc_rvalue_t *index = compile_expr(env, block, ast->index);
            // TODO: bounds check
            return gcc_lvalue_as_rvalue(gcc_array_access(env->ctx, NULL, items, index));
        }
        default: {
            ERROR(env, ast, "Indexing is not supported for %s", type_to_string(indexed_t));
        }
        }
    }
    case TypeOf: {
        gcc_func_t *intern_str_func = hashmap_gets(env->global_funcs, "intern_str");
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        return gcc_call(env->ctx, NULL, intern_str_func, 1,
                        (gcc_rvalue_t*[]){gcc_new_string(env->ctx, type_to_string(t->type))});
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
        switch (t->kind) {
        case BoolType:
            return gcc_unary_op(env->ctx, ast_loc(env, ast), GCC_UNOP_LOGICAL_NEGATE, gcc_t, val);
        case OptionalType: {
            if (gcc_type_if_pointer(gcc_t))
                return gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, val, gcc_null(env->ctx, gcc_t));
            else
                return gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, val, gcc_zero(env->ctx, gcc_t));
        }
        default: {
            ERROR(env, ast, "Logical negation is not supported for %s", type_to_string(t));
        }
        }
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
        ERROR(env, ast, "Ordered comparison is not supported for %s and %s", type_to_string(lhs_t), type_to_string(rhs_t));
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

        if (AddUpdate <= ast->kind && ast->kind <= DivideUpdate) {
            gcc_lvalue_t *lval = get_lvalue(env, block, ast->lhs);
            gcc_rvalue_t *rval = compile_expr(env, block, ast->rhs);

            bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
            bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
            if (numtype_priority(lhs_t) > numtype_priority(rhs_t))
                rval = gcc_cast(env->ctx, NULL, rval, bl_type_to_gcc(env, lhs_t));
            gcc_update(*block, ast_loc(env, ast), lval, op, rval);
            return gcc_lvalue_as_rvalue(lval);
        } else {
            gcc_rvalue_t *lhs_val = compile_expr(env, block, ast->lhs);
            gcc_rvalue_t *rhs_val = compile_expr(env, block, ast->rhs);
            return gcc_binary_op(env->ctx, ast_loc(env, ast), op, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        }
    }
    case If: {
        bl_type_t *if_t = get_type(env->file, env->bindings, ast);
        bl_type_t *nonnil_t = if_t->kind == OptionalType ? if_t->nonnil : if_t;
        bool has_value = !(nonnil_t->kind == AbortType || nonnil_t->kind == VoidType);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *if_ret = has_value ? gcc_local(func, NULL, bl_type_to_gcc(env, if_t), fresh("if_value")) : NULL;

        gcc_block_t *end_if = gcc_new_block(func, NULL);

        foreach (ast->clauses, clause, last_clause) {
            gcc_block_t *if_truthy = gcc_new_block(func, NULL);
            gcc_block_t *if_falsey = (clause < last_clause || ast->else_body) ? gcc_new_block(func, NULL) : end_if;

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
            gcc_jump(*block, NULL, end_if);
        }
        *block = end_if;
        return if_ret ? gcc_lvalue_as_rvalue(if_ret) : NULL;
    }
    case Range: {
        return compile_range(env, block, ast);
    }
    case For: {
        void compile_body(env_t *env, gcc_block_t **block, iterator_info_t *info) {
            if (ast->for_loop.key)
                hashmap_set(env->bindings, ast->for_loop.key->str, new(binding_t, .rval=info->key_rval, .type=info->key_type));

            if (ast->for_loop.value)
                hashmap_set(env->bindings, ast->for_loop.value->str, new(binding_t, .rval=info->value_rval, .type=info->value_type));

            if (ast->for_loop.body)
                compile_block_statement(env, block, ast->for_loop.body);
        }
        void compile_between(env_t *env, gcc_block_t **block, iterator_info_t *info) {
            (void)info;
            if (ast->for_loop.between)
                compile_block_statement(env, block, ast->for_loop.between);
        }
        compile_iteration(env, block, ast, compile_body, compile_between);
        return NULL;
    }
    case While: case Repeat: {
        void compile_body(env_t *env, gcc_block_t **block, iterator_info_t *info) {
            (void)info;
            if (ast->loop.body)
                compile_block_statement(env, block, ast->loop.body);
        }
        void compile_between(env_t *env, gcc_block_t **block, iterator_info_t *info) {
            (void)info;
            if (ast->loop.between)
                compile_block_statement(env, block, ast->loop.between);
        }
        compile_iteration(env, block, ast, compile_body, compile_between);
        return NULL;
    }
    case Skip: case Stop: {
        gcc_block_t *jump_dest = NULL;
        if (ast->str) {
            for (loop_label_t *lbl = env->loop_label; lbl; lbl = lbl->enclosing) {
                if (lbl->name == ast->str) {
                    jump_dest = ast->kind == Skip ? lbl->skip_label : lbl->stop_label;
                    break;
                }
            }
        } else {
            if (env->loop_label)
                jump_dest = ast->kind == Skip ? env->loop_label->skip_label : env->loop_label->stop_label;
        }
        if (!jump_dest) ERROR(env, ast, "I'm not sure what %s is referring to", ast->str);
        gcc_jump(*block, NULL, jump_dest);
        *block = NULL;
        return NULL;
    }
    case Fail: {
        gcc_rvalue_t *msg;
        gcc_func_t *cat = hashmap_gets(env->global_funcs, "CORD_cat");
        if (ast->child) {
            msg = gcc_call(env->ctx, NULL, cat, 2, (gcc_rvalue_t*[]){
                           gcc_new_string(env->ctx, "\x1b[41;30m Error: \x1b[0;31;1m "),
                           compile_expr(env, block, ast->child),
                           });
            msg = gcc_call(env->ctx, NULL, cat, 2, (gcc_rvalue_t*[]){
                           msg,
                           gcc_new_string(env->ctx, "\x1b[m\n\n"),
                           });
        } else {
            msg = gcc_new_string(env->ctx, "\x1b[31;7mError: A failure occurred\x1b[m\n\n");
        }

        char *info = NULL;
        size_t size = 0;
        FILE *f = open_memstream(&info, &size);
        highlight_match(f, env->file, ast->match, 2);
        fputc('\0', f);
        fflush(f);
        msg = gcc_call(env->ctx, NULL, cat, 2, (gcc_rvalue_t*[]){msg, gcc_new_string(env->ctx, info)});
        gcc_func_t *to_char_star = hashmap_gets(env->global_funcs, "CORD_to_char_star");
        msg = gcc_call(env->ctx, NULL, to_char_star, 1, (gcc_rvalue_t*[]){msg});
        gcc_func_t *intern = hashmap_gets(env->global_funcs, "intern_str");
        msg = gcc_call(env->ctx, NULL, intern, 1, (gcc_rvalue_t*[]){msg});
        gcc_func_t *fail = hashmap_gets(env->global_funcs, "fail");
        msg = gcc_call(env->ctx, NULL, fail, 1, (gcc_rvalue_t*[]){msg});
        fclose(f);
        return msg;
    }
    default: break;
    }
    ERROR(env, ast, "Error: compiling is not yet implemented for %s", get_ast_kind_name(ast->kind)); 
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
