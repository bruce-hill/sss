// Compilation logic for expressions
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
#include "../parse.h"
#include "../span.h"
#include "../typecheck.h"
#include "../types.h"
#include "../units.h"
#include "../util.h"
#include "../SipHash/halfsiphash.h"
#include "compile.h"
#include "libgccjit_abbrev.h"

gcc_rvalue_t *compile_constant(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case Var: {
        auto var = Match(ast, Var);
        binding_t *binding = get_binding(env, var->name);
        if (!binding) {
            compiler_err(env, ast, "I can't find a definition for this variable"); 
        } else if (!binding->is_constant) {
            compiler_err(env, ast, "This variable is not a constant, but I need a constant value here that is known at compile-time and can't change."); 
        } else if (binding->rval) {
            return binding->rval;
        } else if (binding->lval) {
            return gcc_rval(binding->lval);
        } else if (binding->func) {
            return gcc_get_func_address(binding->func, NULL);
        }
        break;
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        sss_type_t *fielded_t = get_type(env, access->fielded);
        binding_t *binding = get_ast_binding(env, ast);
        if (!binding) {
            compiler_err(env, ast, "I can't find any constant-value field or method called \"%s\" on a %T.", access->field, fielded_t);
        } else if (!binding->is_constant) {
            compiler_err(env, ast, "This variable is not a constant, but I need a constant value here that is known at compile-time and can't change."); 
        } else if (binding->rval) {
            return binding->rval;
        } else if (binding->lval) {
            return gcc_rval(binding->lval);
        } else if (binding->func) {
            return gcc_get_func_address(binding->func, NULL);
        }

        return binding->rval;
    }
    case Int: {
        gcc_type_t *gcc_t = sss_type_to_gcc(env, get_type(env, ast));

        auto intval = Match(ast, Int);
        int64_t i = intval->i;
        double n = (double)i;
        const char* units = intval->units;
        if (units) {
            units = unit_derive(units, &n, env->derived_units);
            if ((double)((int64_t)n) == n) // no floating point rounding
                i = (int64_t)n;
            else
                compiler_err(env, ast, "This is an integer with a derived unit of measure (%s) which is %g<%s> in base units, "
                            "but that can't be represented as an integer without rounding errors",
                            intval->units, n, units);
        }

        int64_t min = INT64_MIN, max = INT64_MAX;
        switch (intval->precision) {
        case 32:
            min = intval->is_unsigned ? 0 : INT32_MIN;
            max = intval->is_unsigned ? UINT32_MAX : INT32_MAX;
            break;
        case 16:
            min = intval->is_unsigned ? 0 : INT16_MIN;
            max = intval->is_unsigned ? UINT16_MAX : INT16_MAX;
            break;
        case 8:
            min = intval->is_unsigned ? 0 : INT8_MIN;
            max = intval->is_unsigned ? UINT8_MAX : INT8_MAX;
            break;
        default: break;
        }

        if (i < min)
            compiler_err(env, ast, "This integer literal is too small to fit in a %d bit %s integer: %ld",
                         intval->precision, intval->is_unsigned ? "unsigned" : "signed", i);
        else if (i > max)
            compiler_err(env, ast, "This integer literal is too big to fit in a %d bit %s integer: %ld",
                         intval->precision, intval->is_unsigned ? "unsigned" : "signed", i);

        return gcc_rvalue_from_long(env->ctx, gcc_t, i);
    }
    case Char: {
        return gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, CHAR), Match(ast, Char)->c);
    }
    case Num: {
        gcc_type_t *gcc_t = sss_type_to_gcc(env, get_type(env, ast));
        double n = Match(ast, Num)->n;
        const char* units = Match(ast, Num)->units;
        units = unit_derive(units, &n, env->derived_units);
        return gcc_rvalue_from_double(env->ctx, gcc_t, n);
    }
    default: break;
    }
    compiler_err(env, ast, "I can't evaluate this value at compile-time. It needs to be a constant value.");
}

gcc_rvalue_t *compile_len(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *obj)
{
    gcc_func_t *func = gcc_block_func(*block);
    switch (t->tag) {
    case ArrayType: {
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
        return gcc_cast(env->ctx, NULL, gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, ARRAY_LENGTH_FIELD)), gcc_type(env->ctx, INT64));
    }
    case TableType: {
        gcc_struct_t *table_struct = gcc_type_if_struct(sss_type_to_gcc(env, t));
        return gcc_cast(env->ctx, NULL, gcc_rvalue_access_field(obj, NULL, gcc_get_field(table_struct, TABLE_COUNT_FIELD)), gcc_type(env->ctx, INT64));
    }
    case VariantType: {
        return compile_len(env, block, Match(t, VariantType)->variant_of, obj);
    }
    case PointerType: {
        auto ptr = Match(t, PointerType);
        if (ptr->is_optional) {
            gcc_lvalue_t *len_var = gcc_local(func, NULL, gcc_type(env->ctx, INT64), "len");
            gcc_block_t *if_nil = gcc_new_block(func, fresh("if_nil")),
                        *if_nonnil = gcc_new_block(func, fresh("if_nonnil")),
                        *done = gcc_new_block(func, fresh("if_nonnil"));
            gcc_jump_condition(*block, NULL,
                               gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, obj, gcc_null(env->ctx, sss_type_to_gcc(env, t))),
                               if_nil, if_nonnil);
            *block = if_nil;
            gcc_assign(*block, NULL, len_var, gcc_zero(env->ctx, gcc_type(env->ctx, INT64)));
            gcc_jump(*block, NULL, done);

            *block = if_nonnil;
            gcc_assign(*block, NULL, len_var, compile_len(env, block, ptr->pointed, gcc_rval(gcc_rvalue_dereference(obj, NULL))));
            gcc_jump(*block, NULL, done);

            *block = done;
            return gcc_rval(len_var);
        }

        if (ptr->pointed->tag == CharType) {
            // String length
            gcc_func_t *len_func = hget(&env->global->funcs, "strlen", gcc_func_t*);
            gcc_rvalue_t *len = gcc_callx(env->ctx, NULL, len_func, obj);
            return gcc_cast(env->ctx, NULL, len, gcc_type(env->ctx, INT64));
        } else {
            return compile_len(env, block, ptr->pointed, gcc_rval(gcc_rvalue_dereference(obj, NULL)));
        }
    }
    case TypeType: {
        size_t len = strlen(type_to_string(Match(t, TypeType)->type));
        return gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, INT64), len);
    }
    case RangeType: {
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        return range_len(env, gcc_t, obj);
    }
    default: break;
    }
    return NULL;
}

static gcc_rvalue_t *set_pointer_level(env_t *env, gcc_block_t **block, ast_t *ast, unsigned int desired_level)
{
    sss_type_t *t = get_type(env, ast);
    unsigned int level = 0;
    for (sss_type_t *tmp = t; tmp->tag == PointerType; tmp = Match(tmp, PointerType)->pointed)
        ++level;

    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *lval = gcc_local(func, NULL, sss_type_to_gcc(env, t), "val");
    gcc_assign(*block, NULL, lval, compile_expr(env, block, ast));
    gcc_rvalue_t *rval = gcc_rval(lval);
    while (level > desired_level) {
        auto ptr = Match(t, PointerType);
        if (ptr->is_optional)
            compiler_err(env, ast, "This value cannot be safely dereferenced, since it may be nil");
        t = ptr->pointed;
        rval = gcc_rval(gcc_rvalue_dereference(rval, NULL));
        --level;
    }

    while (level < desired_level) {
        gcc_lvalue_t *lval = gcc_local(func, NULL, sss_type_to_gcc(env, t), "val");
        gcc_assign(*block, NULL, lval, rval);
        rval = gcc_lvalue_address(lval, NULL);
        t = Type(PointerType, .pointed=t, .is_optional=false);
        ++level;
    }
    return rval;
}

static gcc_rvalue_t *make_cycle_checker(env_t *env, gcc_loc_t *loc, gcc_block_t *block)
{
    sss_type_t *cycle_checker_t = Type(TableType, .key_type=Type(PointerType, .pointed=Type(VoidType)), .value_type=Type(IntType, .bits=64));
    gcc_type_t *hashmap_gcc_t = sss_type_to_gcc(env, cycle_checker_t);
    gcc_func_t *func = gcc_block_func(block);
    gcc_lvalue_t *cycle_checker = gcc_local(func, loc, hashmap_gcc_t, "_rec");
    gcc_assign(block, loc, cycle_checker, gcc_struct_constructor(env->ctx, loc, hashmap_gcc_t, 0, NULL, NULL));
    gcc_lvalue_t *next_index = gcc_local(func, loc, gcc_type(env->ctx, INT64), "_index");
    gcc_assign(block, loc, next_index, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
    gcc_assign(block, loc, gcc_lvalue_access_field(
            cycle_checker, loc, gcc_get_field(gcc_type_if_struct(hashmap_gcc_t), TABLE_DEFAULT_FIELD)),
        gcc_lvalue_address(next_index, loc));
    return gcc_cast(env->ctx, loc, gcc_lvalue_address(cycle_checker, loc), gcc_type(env->ctx, VOID_PTR));
}

static void print_doctest_value(env_t *env, gcc_block_t **block, gcc_loc_t *loc, const char *info, sss_type_t *t, gcc_rvalue_t *rval)
{
    gcc_rvalue_t *stderr_val = gcc_rval(gcc_global(env->ctx, NULL, GCC_GLOBAL_IMPORTED, gcc_type(env->ctx, FILE_PTR), "stderr"));
    gcc_func_t *fputs_fn = hget(&env->global->funcs, "fputs", gcc_func_t*);
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *use_dim = gcc_new_block(func, fresh("use_dim")),
                *no_dim = gcc_new_block(func, fresh("no_dim")),
                *done_with_dim = gcc_new_block(func, "done_with_dim");
    gcc_jump_condition(*block, loc, get_binding(env, "USE_COLOR")->rval, use_dim, no_dim);

    gcc_eval(use_dim, loc, gcc_callx(env->ctx, loc, fputs_fn, gcc_str(env->ctx, heap_strf("\x1b[0;2m%s\x1b[m", info)), stderr_val)); 
    gcc_jump(use_dim, loc, done_with_dim);

    gcc_eval(no_dim, loc, gcc_callx(env->ctx, loc, fputs_fn, gcc_str(env->ctx, info), stderr_val)); 
    gcc_jump(no_dim, loc, done_with_dim);

    *block = done_with_dim;
    gcc_func_t *print_fn = get_print_func(env, t);
    gcc_rvalue_t *cycle_checker = make_cycle_checker(env, loc, *block);
    gcc_eval(*block, loc, gcc_callx(env->ctx, loc, print_fn, rval, stderr_val, cycle_checker, get_binding(env, "USE_COLOR")->rval));

    gcc_block_t *use_dim_type = gcc_new_block(func, fresh("use_dim_type")),
                *no_dim_type = gcc_new_block(func, fresh("no_dim_type")),
                *done_with_type = gcc_new_block(func, fresh("done_with_type"));
    gcc_jump_condition(*block, loc, get_binding(env, "USE_COLOR")->rval, use_dim_type, no_dim_type);
    gcc_eval(use_dim_type, loc, gcc_callx(env->ctx, loc, fputs_fn, gcc_str(env->ctx, heap_strf(" \x1b[0;2m: %s\x1b[m\n", type_to_string_concise(t))), stderr_val)); 
    gcc_jump(use_dim_type, loc, done_with_type);
    gcc_eval(no_dim_type, loc, gcc_callx(env->ctx, loc, fputs_fn, gcc_str(env->ctx, heap_strf(" : %s\n", type_to_string_concise(t))), stderr_val)); 
    gcc_jump(no_dim_type, loc, done_with_type);
    *block = done_with_type;
}

gcc_rvalue_t *compile_expr(env_t *env, gcc_block_t **block, ast_t *ast)
{
    gcc_loc_t *loc = ast_loc(env, ast);
    sss_type_t *variant_t = NULL;
    switch (ast->tag) {
    case Var: {
        auto var = Match(ast, Var);
        binding_t *binding = get_binding(env, var->name);
        if (binding) {
            if (binding->rval)
                return binding->rval;
            if (binding->lval)
                return gcc_rval(binding->lval);
            if (binding->func)
                return gcc_get_func_address(binding->func, loc);
        }
        compiler_err(env, ast, "I can't find a definition for this variable"); 
    }
    case Declare: {
        auto decl = Match(ast, Declare);
        sss_type_t *t = get_type(env, decl->value);
        assert(t);
        if (t->tag == GeneratorType)
            compiler_err(env, decl->value, "This expression isn't guaranteed to have a single value, so you can't use it to initialize a variable."); 
        else if (t->tag == VoidType)
            compiler_err(env, decl->value, "This expression doesn't have a value (it has a Void type), so you can't store it in a variable."); 
        gcc_rvalue_t *rval = compile_expr(env, block, decl->value);
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        gcc_lvalue_t *lval;
        const char* name = Match(decl->var, Var)->name;
        const char* sym_name = decl->is_global ? fresh(name) : name;
        if (decl->is_global) {
            lval = gcc_global(env->ctx, ast_loc(env, ast), GCC_GLOBAL_EXPORTED, gcc_t, sym_name);
        } else {
            gcc_func_t *func = gcc_block_func(*block);
            lval = gcc_local(func, ast_loc(env, ast), gcc_t, sym_name);
        }
        binding_t *clobbered = get_local_binding(env, name);
        if (clobbered && clobbered->type->tag == TypeType && clobbered->rval)
            compiler_err(env, ast, "This name is already being used for the name of a type (struct or enum) in the same block, "
                  "and I get confused if you try to redeclare the name of a namespace.");
        hset(env->bindings, name,
             new(binding_t, .lval=lval, .rval=gcc_rval(lval), .type=t, .sym_name=decl->is_global ? sym_name : NULL,
                 .visible_in_closures=decl->is_global));
        assert(rval);
        gcc_assign(*block, ast_loc(env, ast), lval, rval);
        return gcc_rval(lval);
    }

    case Extern: {
        auto ext = Match(ast, Extern);
        sss_type_t *t = parse_type_ast(env, ext->type);
        if (t->tag == FunctionType) {
            auto fn = Match(t, FunctionType); 
            gcc_type_t *gcc_ret_t = sss_type_to_gcc(env, fn->ret);
            NEW_LIST(gcc_param_t*, params);
            for (int64_t i = 0, len = length(fn->arg_types); i < len; i++) {
                gcc_type_t *arg_t = sss_type_to_gcc(env, ith(fn->arg_types, i));
                const char* arg_name = fn->arg_names ? ith(fn->arg_names, i) : NULL;
                if (!arg_name) arg_name = fresh("arg");
                APPEND(params, gcc_new_param(env->ctx, loc, arg_t, arg_name));
            }
            gcc_func_t *func = gcc_new_func(
                env->ctx, loc, GCC_FUNCTION_IMPORTED, gcc_ret_t, ext->name, length(params), params[0], 0);
            return gcc_get_func_address(func, loc);
        } else {
            gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
            gcc_lvalue_t *lval = gcc_global(env->ctx, ast_loc(env, ast), GCC_GLOBAL_IMPORTED, gcc_t, ext->name);
            return ext->address ? gcc_lvalue_address(lval, loc) : gcc_rval(lval);
        }
    }

    case Assign: {
        auto assignment = Match(ast, Assign);
        List(ast_t*) targets = assignment->targets;
        List(ast_t*) values = assignment->values;
        int64_t len = length(targets);
        int64_t num_values = length(values);
        if (num_values != len)
            compiler_err(env, ast, "There's a mismatch in this assignment. The left side has %ld values, but the right side has %ld values.", len, num_values);

        gcc_func_t *func = gcc_block_func(*block);
        typedef struct {
            sss_type_t *type;
            bool is_table;
            union {
                gcc_lvalue_t *lval;
                struct {
                    gcc_rvalue_t *table;
                    gcc_lvalue_t *key;
                    sss_type_t *table_type;
                };
            };
        } assign_target_t;
        NEW_LIST(assign_target_t, lvals);
        foreach (targets, lhs, _) {
            sss_type_t *t_lhs = get_type(env, *lhs);
            assign_target_t target = {.type=t_lhs};
            if ((*lhs)->tag == Index && value_type(get_type(env, Match(*lhs, Index)->indexed))->tag == TableType) {
                target.is_table = true;
                sss_type_t *table_t = get_type(env, Match(*lhs, Index)->indexed);
                if (table_t->tag != PointerType)
                    compiler_err(env, *lhs, "This is an immutable table value, it can't be modified");

                target.table = compile_expr(env, block, Match(*lhs, Index)->indexed);
              carry_on:
                if (Match(table_t, PointerType)->is_optional)
                    compiler_err(env, *lhs, "This table pointer might be nil, so it's not safe to dereference it");

                // Check for indirect references like @@@t[x]
                if (Match(table_t, PointerType)->pointed->tag == PointerType) {
                    target.table = gcc_rval(gcc_rvalue_dereference(target.table, loc));
                    table_t = Match(table_t, PointerType)->pointed;
                    goto carry_on;
                }
                target.table_type = value_type(table_t);
                target.key = gcc_local(func, loc, sss_type_to_gcc(env, Match(target.table_type, TableType)->key_type), "key");
                gcc_assign(*block, loc, target.key, compile_expr(env, block, Match(*lhs, Index)->index));
            } else {
                target.is_table = false;
                target.lval = get_lvalue(env, block, *lhs, false);
            }
            APPEND_STRUCT(lvals, target);
        }
        NEW_LIST(gcc_rvalue_t*, rvals);
        for (int64_t i = 0; i < len; i++) {
            auto target = ith(lvals, i);
            sss_type_t *t_lhs = target.type;
            ast_t *rhs = ith(values, i);
            sss_type_t *t_rhs = get_type(env, rhs);
            // TODO: maybe allow generators to assign the *last* value, if any
            if (t_rhs->tag == GeneratorType)
                compiler_err(env, rhs, "This expression isn't guaranteed to have a single value, so you can't assign it to a variable."); 
            gcc_rvalue_t *rval = compile_expr(env, block, ith(values, i));

            if (!promote(env, t_rhs, &rval, t_lhs))
                compiler_err(env, rhs, "You're assigning this %T value to a variable with type %T and I can't figure out how to make that work.",
                    t_rhs, t_lhs);

            gcc_lvalue_t *tmp = gcc_local(func, loc, sss_type_to_gcc(env, t_lhs), "to_assign");
            assert(rval);
            gcc_assign(*block, loc, tmp, rval);
            append(rvals, gcc_rval(tmp));
        }
        NEW_LIST(sss_type_t*, target_types);
        NEW_LIST(const char*, field_names);
        for (int64_t i = 0; i < len; i++) {
            auto target = ith(lvals, i);
            if (target.is_table) {
                sss_type_t *key_t = Match(target.table_type, TableType)->key_type;
                sss_type_t *value_t = Match(target.table_type, TableType)->value_type;

                size_t key_size = gcc_sizeof(env, key_t);
                size_t value_align = gcc_alignof(env, value_t);
                size_t value_offset = key_size;
                if (value_align > 0 && value_offset % value_align != 0) value_offset = (value_offset - (value_offset % value_align) + value_align);
                gcc_rvalue_t *entry_offset = gcc_rvalue_size(env->ctx, value_offset);

                gcc_func_t *hashmap_set_fn = get_function(env, "sss_hashmap_set");
                gcc_func_t *key_hash = get_hash_func(env, key_t);
                gcc_func_t *key_cmp = get_indirect_compare_func(env, key_t);
                gcc_lvalue_t *val_lval = gcc_local(func, loc, sss_type_to_gcc(env, value_t), "value");
                gcc_assign(*block, loc, val_lval, ith(rvals, i));
                gcc_rvalue_t *call = gcc_callx(
                    env->ctx, loc, hashmap_set_fn,
                    gcc_cast(env->ctx, loc, target.table, gcc_type(env->ctx, VOID_PTR)),
                    gcc_cast(env->ctx, loc, gcc_get_func_address(key_hash, loc), gcc_type(env->ctx, VOID_PTR)),
                    gcc_cast(env->ctx, loc, gcc_get_func_address(key_cmp, loc), gcc_type(env->ctx, VOID_PTR)),
                    gcc_rvalue_size(env->ctx, gcc_sizeof(env, table_entry_type(target.table_type))),
                    gcc_cast(env->ctx, loc, gcc_lvalue_address(target.key, loc), gcc_type(env->ctx, VOID_PTR)),
                    entry_offset,
                    gcc_cast(env->ctx, loc, gcc_lvalue_address(val_lval, loc), gcc_type(env->ctx, VOID_PTR)));
                gcc_eval(*block, loc, call);
            } else {
                gcc_assign(*block, ast_loc(env, ast), target.lval, ith(rvals, i));
            }

            APPEND(target_types, target.type);
            APPEND(field_names, heap_strf("_%ld", i+1));
        }
        sss_type_t *values_tuple = Type(StructType, .field_types=target_types, .field_names=field_names);
        gcc_type_t *values_gcc_t = sss_type_to_gcc(env, values_tuple);
        gcc_field_t *fields[len];
        for (int64_t i = 0; i < len; i++)
            fields[i] = gcc_get_field(gcc_type_if_struct(values_gcc_t), i);
        return gcc_struct_constructor(env->ctx, loc, values_gcc_t, len, fields, rvals[0]);
    }
    case Delete: {
        ast_t *to_delete = Match(ast, Delete)->value;
        switch (to_delete->tag) {
        case Index: {
            auto index = Match(to_delete, Index);
            sss_type_t *t = get_type(env, index->indexed);
            if (t->tag != PointerType)
                compiler_err(env, ast, "Only mutable containers can be used with 'del', not %T", t);
            if (Match(t, PointerType)->is_optional)
                compiler_err(env, index->indexed, "This value is optional and can't be safely dereferenced");
            sss_type_t *container_t = Match(t, PointerType)->pointed;
            if (container_t->tag == TableType) {
                sss_type_t *key_t = get_type(env, index->index);
                if (!type_is_a(key_t, Match(container_t, TableType)->key_type))
                    compiler_err(env, index->index, "This key has type %T, but this table has a different key type: %T",
                                key_t, Match(container_t, TableType)->key_type);

                table_remove(env, block, container_t, compile_expr(env, block, index->indexed), compile_expr(env, block, index->index));
            } else if (container_t->tag == ArrayType) {
                binding_t *b = get_array_method(env, container_t, "remove");
                gcc_func_t *func = gcc_block_func(*block);
                gcc_lvalue_t *arr_var = gcc_local(func, loc, sss_type_to_gcc(env, t), "_array");
                gcc_assign(*block, loc, arr_var, compile_expr(env, block, index->indexed));
                gcc_rvalue_t *index_val = compile_expr(env, block, index->index);
                gcc_eval(*block, loc, gcc_callx(env->ctx, loc, b->func, gcc_rval(arr_var), index_val, gcc_rvalue_int64(env->ctx, 1)));
            } else {
                compiler_err(env, ast, "Only mutable tables can be used with 'del', not %T", t);
            }
            return NULL;
        }
        case FieldAccess: {
            auto access = Match(to_delete, FieldAccess);
            sss_type_t *t = get_type(env, access->fielded);
            if (t->tag != PointerType)
                compiler_err(env, ast, "Only mutable tables can be used with 'del', not %T", t);
            if (Match(t, PointerType)->is_optional)
                compiler_err(env, access->fielded, "This value is optional and can't be safely dereferenced");
            sss_type_t *table_t = Match(t, PointerType)->pointed;
            if (table_t->tag != TableType)
                compiler_err(env, ast, "Only mutable tables can be used with 'del', not %T", t);

            if (streq(access->field, "default")) {
                gcc_rvalue_t *table = compile_expr(env, block, access->fielded);
                gcc_field_t *field = gcc_get_field(gcc_type_if_struct(sss_type_to_gcc(env, table_t)), TABLE_DEFAULT_FIELD);
                gcc_assign(*block, loc, gcc_rvalue_dereference_field(table, loc, field),
                           gcc_null(env->ctx, gcc_get_ptr_type(sss_type_to_gcc(env, Match(table_t, TableType)->value_type))));
            } else if (streq(access->field, "fallback")) {
                gcc_rvalue_t *table = compile_expr(env, block, access->fielded);
                gcc_field_t *field = gcc_get_field(gcc_type_if_struct(sss_type_to_gcc(env, table_t)), TABLE_FALLBACK_FIELD);
                gcc_assign(*block, loc, gcc_rvalue_dereference_field(table, loc, field),
                           gcc_null(env->ctx, gcc_get_ptr_type(sss_type_to_gcc(env, table_t))));
            } else {
                compiler_err(env, ast, "The only table fields that can be deleted are .default and .fallback, not .%s", access->field);
            }
            return NULL;
        }
        default: {
            sss_type_t *t = get_type(env, to_delete);
            if (t->tag == PointerType) {
                if (Match(t, PointerType)->is_optional)
                    compiler_err(env, to_delete, "This value is optional and can't be safely dereferenced");
                sss_type_t *value_t = Match(t, PointerType)->pointed;
                if (value_t->tag == TableType) {
                    table_remove(env, block, value_t, compile_expr(env, block, to_delete), NULL);
                } else if (value_t->tag == ArrayType) {
                    binding_t *b = get_array_method(env, value_t, "remove");
                    gcc_func_t *func = gcc_block_func(*block);
                    gcc_lvalue_t *arr_var = gcc_local(func, loc, sss_type_to_gcc(env, t), "_array");
                    gcc_assign(*block, loc, arr_var, compile_expr(env, block, to_delete));
                    gcc_rvalue_t *len = compile_len(env, block, t, gcc_rval(arr_var));
                    gcc_eval(*block, loc, gcc_callx(env->ctx, loc, b->func, gcc_rval(arr_var), len, gcc_rvalue_int64(env->ctx, 1)));
                } else {
                    compiler_err(env, ast, "Only mutable tables can be used with 'del', not %T", t);
                }

                return NULL;
            }
            compiler_err(env, ast, "The 'del' statement is only supported for indexed values like val[x]");
        }
        }
    }
    case Do: {
        auto do_ = Match(ast, Do);
        gcc_func_t *func = gcc_block_func(*block);
        sss_type_t *t = get_type(env, ast);
        gcc_lvalue_t *result = (t->tag != VoidType && t->tag != AbortType)? gcc_local(func, loc, sss_type_to_gcc(env, t), "_do_result") : NULL;

        env_t *do_env = fresh_scope(env);
        gcc_block_t *do_else = gcc_new_block(func, fresh("do_else")),
                    *do_end = gcc_new_block(func, fresh("do_end"));
        
        do_env->loop_label = &(loop_label_t){
            .enclosing = env->loop_label,
            .names = LIST(const char*, do_->label),
            .skip_label = do_else,
            .stop_label = do_else,
            .deferred = do_env->deferred,
        };

        gcc_rvalue_t *do_rval = compile_block_expr(do_env, block, do_->body);
        if (*block) {
            if (do_rval && result)
                gcc_assign(*block, loc, result, do_rval);
            else if (do_rval)
                gcc_eval(*block, loc, do_rval);
            gcc_jump(*block, loc, do_end);
        }

        if (do_->else_body) {
            do_env->loop_label->skip_label = NULL;
            *block = do_else;
            gcc_rvalue_t *else_rval = compile_block_expr(do_env, block, do_->else_body);
            if (*block) {
                if (do_rval && result)
                    gcc_assign(*block, loc, result, else_rval);
                else if (else_rval)
                    gcc_eval(*block, loc, else_rval);
                gcc_jump(*block, loc, do_end);
                *block = NULL;
            }
        } else {
            gcc_jump(do_else, loc, do_end);
        }
        *block = do_end;
        return result ? gcc_rval(result) : NULL;
    }
    case Defer: {
        // For all variables in the local scope, copy their current values,
        // so that when the deferred code runs, it uses the value when the `defer`
        // was written, rather than any new values that have been taken on.
        //
        // This is important for a use case like this:
        //
        //     f := open(file1)
        //     defer f.close() // close file1, even if we reassign `f`
        //     f = open(file2)
        //     defer f.close() // close file2
        //
        // TODO: we don't technically need to copy *all* local variables, since
        // some aren't referenced inside the defer block, but this is a pretty
        // small amount of extra work that should hopefully be elided by GCC
        env_t *defer_env = fresh_scope(env);
        defer_env->bindings->fallback = env->bindings->fallback;
        gcc_func_t *func = gcc_block_func(*block);
        for (uint32_t i = 1; i <= env->bindings->count; i++) {
            auto entry = hnth(env->bindings, i , const char*, binding_t*);
            gcc_lvalue_t *cached = gcc_local(func, loc, sss_type_to_gcc(env, entry->value->type), entry->key);
            if (!entry->value->rval) continue;
            gcc_assign(*block, loc, cached, entry->value->rval);
            binding_t *cached_binding = new(binding_t);
            *cached_binding = *entry->value;
            cached_binding->lval = cached;
            cached_binding->rval = gcc_rval(cached);
            hset(defer_env->bindings, entry->key, cached_binding);
        }
        defer_env->is_deferred = true;
        env->deferred = new(defer_t, .next=env->deferred, .body=Match(ast, Defer)->body, .environment=defer_env);
        return NULL;
    }
    case With: { // with var := expr, cleanup(var) ...
        auto with = Match(ast, With);
        NEW_LIST(ast_t*, statements);
        ast_t *var = with->var ? with->var : WrapAST(ast, Var, .name="with");
        APPEND(statements, WrapAST(ast, Declare, .var=var, .value=with->expr));
        ast_t *cleanup = with->cleanup;
        if (!cleanup)
            cleanup = WrapAST(ast, FunctionCall, .fn=WrapAST(ast, FieldAccess, .fielded=var, .field="close"), .args=LIST(ast_t*));
        APPEND(statements, WrapAST(ast, Defer, .body=cleanup));
        APPEND(statements, with->body);
        return compile_expr(env, block, WrapAST(ast, Block, .statements=statements));
    }
    case Block: {
        // Create scope:
        return compile_block_expr(fresh_scope(env), block, ast);
    }
    case FunctionDef: {
        auto fn = Match(ast, FunctionDef);
        binding_t *binding = get_binding(env, fn->name);
        assert(binding && binding->func);
        // compile_function(env, binding->func, ast);
        return binding->rval;
    }
    case Lambda: {
        gcc_func_t *func = get_function_def(env, ast, fresh("lambda"));
        // compile_function(env, func, ast);
        return gcc_get_func_address(func, loc);
    }
    case Return: {
        if (env->is_deferred)
            compiler_err(env, ast, "This 'return' is inside a 'defer' block, which is not allowed");

        auto ret = Match(ast, Return);
        assert(env->return_type);

        if (!ret->value) {
            if (env->return_type->tag != VoidType)
                compiler_err(env, ast, "I was expecting this `return` to have a value of type %T because of the function's type signature, but no value is being returned here.",
                             env->return_type);

            insert_defers(env, block, NULL);
            gcc_return_void(*block, loc);
            *block = NULL;
            return NULL;
        }

        // Validate and compile the return value:
        sss_type_t *value_t = get_type(env, ret->value);
        if (env->return_type->tag == VoidType && value_t->tag != VoidType)
            compiler_err(env, ast, "I was expecting a plain `return` with no expression here or a Void-type function call, because the function this is inside has no declared return type. If you want to return a value, please change the function's definition to include a return type.");

        gcc_rvalue_t *val = compile_expr(env, block, ret->value);
        if (!promote(env, value_t, &val, env->return_type))
            compiler_err(env, ast, "I was expecting this `return` to have value of type %T because of the function's type signature, but this value has type %T",
                  env->return_type, value_t);

        // Tail call optimization under the right conditions:
        if (ret->value->tag == FunctionCall && env->tail_calls && !env->deferred && type_eq(value_t, env->return_type))
            gcc_rvalue_require_tail_call(val, 1);

        // Evaluate return expression before returning void or defers (if any):
        if (env->return_type->tag == VoidType && val) {
            gcc_eval(*block, loc, val);
            val = NULL;
        } else if (env->deferred) {
            // Cache the return value in a variable before running defers:
            gcc_func_t *func = gcc_block_func(*block);
            gcc_lvalue_t *return_var = gcc_local(func, loc, sss_type_to_gcc(env, env->return_type), "_return_val");
            gcc_assign(*block, loc, return_var, val);
            val = gcc_rval(return_var);
        }

        insert_defers(env, block, NULL);

        if (env->return_type->tag == VoidType)
            gcc_return_void(*block, loc);
        else
            gcc_return(*block, loc, val);

        *block = NULL;
        return NULL;
    }
    case Int: case Num: case Char: {
        return compile_constant(env, ast);
    }
    case Interp: {
        return compile_expr(env, block, WrapAST(ast, StringJoin, .children=LIST(ast_t*, ast)));
    }
#define STRING_STRUCT(env, gcc_t, str_rval, len_rval, stride_rval) \
        gcc_struct_constructor(env->ctx, loc, gcc_t, 4, (gcc_field_t*[]){ \
                                          gcc_get_field(gcc_type_if_struct(gcc_t), ARRAY_DATA_FIELD), \
                                          gcc_get_field(gcc_type_if_struct(gcc_t), ARRAY_LENGTH_FIELD), \
                                          gcc_get_field(gcc_type_if_struct(gcc_t), ARRAY_STRIDE_FIELD), \
                                          gcc_get_field(gcc_type_if_struct(gcc_t), ARRAY_CAPACITY_FIELD), \
                                      }, (gcc_rvalue_t*[]){ \
                                          str_rval, \
                                          gcc_cast(env->ctx, loc, len_rval, gcc_type(env->ctx, INT32)), \
                                          gcc_cast(env->ctx, loc, stride_rval, gcc_type(env->ctx, INT16)), \
                                          gcc_rvalue_int16(env->ctx, -1), \
                                      })
    case StringLiteral: {
        const char* str = Match(ast, StringLiteral)->str;
        gcc_rvalue_t *str_rval = gcc_str(env->ctx, str);
        gcc_rvalue_t *len_rval = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, INT32), strlen(str));
        gcc_rvalue_t *stride_rval = gcc_one(env->ctx, gcc_type(env->ctx, INT16));
        gcc_type_t *gcc_t = sss_type_to_gcc(env, Type(ArrayType, .item_type=Type(CharType)));
        return STRING_STRUCT(env, gcc_t, str_rval, len_rval, stride_rval);
    }
    case Variant: {
        auto variant = Match(ast, Variant);
        variant_t = parse_type_ast(env, variant->type);
        if (variant_t->tag != VariantType)
            compiler_err(env, variant->type, "This is not a variant type, it's %T", variant_t);

        sss_type_t *variant_of_t = Match(variant_t, VariantType)->variant_of;
        if (variant_of_t->tag == ArrayType && variant->value->tag == StringJoin) {
            ast = variant->value;
        } else {
            sss_type_t *val_t = get_type(env, variant->value);
            if (!type_eq(variant_of_t, val_t))
                compiler_err(env, variant->value, "The %s variant expects a value of type %T, but this value has type %T",
                             Match(variant_t, VariantType)->name, variant_of_t, val_t);
            return compile_expr(env, block, variant->value);
        }
    }
    // Fall through
    case StringJoin: {
        auto string_join = Match(ast, StringJoin);
        auto chunks = string_join->children;
        foreach (chunks, chunk, _) {
            sss_type_t *t = get_type(env, *chunk);
            if (t->tag == VoidType)
                compiler_err(env, *chunk, "This expression doesn't have a value (it has a Void type), so you can't use it in a string."); 
        }

        sss_type_t *string_t = variant_t ? variant_t : Type(ArrayType, .item_type=Type(CharType));
        gcc_type_t *gcc_t = sss_type_to_gcc(env, string_t);
        gcc_type_t *i16_t = gcc_type(env->ctx, INT16);
        gcc_type_t *i32_t = gcc_type(env->ctx, INT32);

        // Optimize the case of empty strings
        if (length(chunks) == 0) {
            return STRING_STRUCT(env, gcc_t, gcc_null(env->ctx, gcc_type(env->ctx, STRING)), gcc_zero(env->ctx, i32_t), gcc_one(env->ctx, i16_t));
        } else if (length(chunks) == 1 && LIST_ITEM(chunks, 0)->tag == StringLiteral) {
            // Optimize the case of a single string literal
            return compile_expr(env, block, LIST_ITEM(chunks, 0));
        }

        gcc_func_t *open_memstream_fn = hget(&env->global->funcs, "open_memstream", gcc_func_t*);
        gcc_func_t *free_fn = hget(&env->global->funcs, "free", gcc_func_t*);
        gcc_func_t *fputs_fn = hget(&env->global->funcs, "fputs", gcc_func_t*);
        gcc_func_t *fflush_fn = hget(&env->global->funcs, "fflush", gcc_func_t*);
        gcc_func_t *fclose_fn = hget(&env->global->funcs, "fclose", gcc_func_t*);

        // char *buf; size_t size;
        // FILE *f = open_memstream(&buf, &size);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *buf_var = gcc_local(func, loc, gcc_type(env->ctx, STRING), "buf");
        gcc_lvalue_t *size_var = gcc_local(func, loc, gcc_type(env->ctx, SIZE), "size");
        gcc_lvalue_t *file_var = gcc_local(func, loc, gcc_type(env->ctx, FILE_PTR), "file");
        gcc_assign(*block, loc, file_var,
                   gcc_callx(env->ctx, loc, open_memstream_fn, gcc_lvalue_address(buf_var, loc), gcc_lvalue_address(size_var, loc)));
        gcc_rvalue_t *file = gcc_rval(file_var);

        foreach (chunks, chunk, _) {
            gcc_loc_t *chunk_loc = ast_loc(env, *chunk);
            if ((*chunk)->tag == StringLiteral) {
                const char* str = Match(*chunk, StringLiteral)->str;
                gcc_eval(*block, chunk_loc,
                         gcc_callx(env->ctx, chunk_loc, fputs_fn, gcc_str(env->ctx, str), file));
                continue;
            }
            assert((*chunk)->tag == Interp);
            auto interp = Match(*chunk, Interp);
            if (interp->labelled) {
                char *buf; size_t size;
                FILE *f = open_memstream(&buf, &size);
                span_t span = interp->value->span;
                fwrite(span.start, 1, (size_t)(span.end - span.start), f);
                fprintf(f, ": ");
                fflush(f);
                gcc_eval(*block, chunk_loc, gcc_callx(env->ctx, chunk_loc, fputs_fn, gcc_str(env->ctx, buf), file));
                fclose(f);
            }

            ast_t *interp_value = interp->value;
            sss_type_t *t = get_type(env, interp_value);

            if (variant_t && !type_eq(t, variant_t) && !get_from_namespace(env, variant_t, heap_strf("#convert-from:%s", type_to_string(t)))
                && get_from_namespace(env, variant_t, heap_strf("#convert-from:%s", type_to_string(Type(ArrayType, .item_type=Type(CharType)))))) {
                interp_value = WrapAST(interp_value, Interp, interp_value);
                t = Type(ArrayType, .item_type=Type(CharType));
            }

            gcc_lvalue_t *interp_var = gcc_local(func, loc, sss_type_to_gcc(env, t), "_interp_str");
            gcc_assign(*block, loc, interp_var, compile_expr(env, block, interp_value));
            gcc_rvalue_t *obj = gcc_rval(interp_var);

            if (!type_eq(t, string_t)) {
                binding_t *convert_b = get_from_namespace(env, string_t, heap_strf("#convert-from:%s", type_to_string(t)));
                if (convert_b) {
                    gcc_lvalue_t *converted_var = gcc_local(func, loc, sss_type_to_gcc(env, string_t), "_converted");
                    gcc_assign(*block, loc, converted_var, gcc_callx(env->ctx, loc, convert_b->func, obj));
                    obj = gcc_rval(converted_var);
                    t = string_t;
                } else if (variant_t) {
                    compiler_err(env, interp->value, "I don't know how to interpolate %T values in a %T string", t, variant_t);
                }
            }

            if (!interp->quote_string && type_eq(t, string_t)) {
                // i = 1
                gcc_type_t *i32 = gcc_type(env->ctx, INT32);
                gcc_lvalue_t *i = gcc_local(func, loc, i32, "_i");
                gcc_assign(*block, loc, i, gcc_zero(env->ctx, i32));
                gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
                gcc_rvalue_t *items = gcc_rvalue_access_field(obj, loc, gcc_get_field(array_struct, ARRAY_DATA_FIELD));
                gcc_rvalue_t *len = gcc_rvalue_access_field(obj, loc, gcc_get_field(array_struct, ARRAY_LENGTH_FIELD));
                gcc_rvalue_t *stride = gcc_cast(env->ctx, loc, gcc_rvalue_access_field(obj, loc, gcc_get_field(array_struct, ARRAY_STRIDE_FIELD)), i32_t);

                gcc_block_t *add_next_item = gcc_new_block(func, fresh("next_item")),
                            *done = gcc_new_block(func, fresh("done"));

                // if (i < len) goto add_next_item;
                gcc_jump_condition(*block, loc, 
                              gcc_comparison(env->ctx, loc, GCC_COMPARISON_LT, gcc_rval(i), len),
                              add_next_item, done);

                // add_next_item:
                // fputc(items[i*stride], file)
                gcc_func_t *fputc_fn = get_function(env, "fputc");
                gcc_eval(add_next_item, loc, gcc_callx(
                        env->ctx, loc, fputc_fn,
                        gcc_rval(gcc_array_access(env->ctx, loc, items,
                                                  gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, i32_t, gcc_rval(i), gcc_cast(env->ctx, loc, stride, i32_t)))),
                        gcc_rval(file_var)));
                
                // i += 1
                gcc_update(add_next_item, loc, i, GCC_BINOP_PLUS, gcc_one(env->ctx, i32_t));
                // if (i < len) goto add_next_item;
                gcc_jump_condition(add_next_item, loc, 
                              gcc_comparison(env->ctx, loc, GCC_COMPARISON_LT, gcc_rval(i), len),
                              add_next_item, done);

                *block = done;
                continue;
            }
            gcc_func_t *print_fn = get_print_func(env, t);
            assert(print_fn);
            
            gcc_rvalue_t *cycle_checker = make_cycle_checker(env, loc, *block);
            gcc_rvalue_t *print_call = gcc_callx(
                env->ctx, chunk_loc, print_fn, obj, file, cycle_checker,
                interp->colorize ? get_binding(env, "USE_COLOR")->rval : gcc_rvalue_bool(env->ctx, false));
            assert(print_call);
            gcc_eval(*block, chunk_loc, print_call);
        }
        gcc_eval(*block, loc, gcc_callx(env->ctx, loc, fflush_fn, file));
        gcc_lvalue_t *str_struct_var = gcc_local(func, loc, gcc_t, "_str_final");
        gcc_rvalue_t *len32 = gcc_cast(env->ctx, loc, gcc_rval(size_var), i32_t);

        gcc_func_t *alloc_fn = hget(&env->global->funcs, "GC_malloc_atomic", gcc_func_t*);
        gcc_rvalue_t *size = gcc_rval(size_var);
        gcc_rvalue_t *str = gcc_callx(
            env->ctx, loc, alloc_fn,
            gcc_binary_op(env->ctx, loc, GCC_BINOP_PLUS, gcc_type(env->ctx, SIZE),
                          size, gcc_one(env->ctx, gcc_type(env->ctx, SIZE))));
        gcc_func_t *memcpy_fn = hget(&env->global->funcs, "memcpy", gcc_func_t*);
        str = gcc_callx(env->ctx, loc, memcpy_fn, str, gcc_rval(buf_var), size);
        str = gcc_cast(env->ctx, loc, str, gcc_type(env->ctx, STRING));
        gcc_assign(*block, loc, str_struct_var, STRING_STRUCT(env, gcc_t, str, len32, gcc_one(env->ctx, i16_t)));
        gcc_eval(*block, loc, gcc_callx(env->ctx, loc, fclose_fn, file));
        gcc_eval(*block, loc, gcc_callx(env->ctx, loc, free_fn, gcc_rval(buf_var)));
        return gcc_rval(str_struct_var);
    }
    case Array: {
        return compile_array(env, block, ast, true);
    }
    case Table: {
        return compile_table(env, block, ast, true);
    }
    case TableEntry: {
        env->comprehension_callback(env, block, ast, env->comprehension_userdata);
        return NULL;
    }
    case UnitDef: {
        return NULL;
    }
    case ConvertDef: {
        auto convert = Match(ast, ConvertDef);
        sss_type_t *src_t = parse_type_ast(env, convert->source_type);
        sss_type_t *target_t = parse_type_ast(env, convert->target_type);

        ast_t *def = NewAST(
            env->file, ast->span.start, ast->span.end,
            FunctionDef,
            .arg_names=LIST(const char*, convert->var),
            .arg_types=LIST(ast_t*, convert->source_type),
            .ret_type=convert->target_type,
            .body=convert->body);
        gcc_func_t *func = get_function_def(env, def, fresh("convert"));
        // compile_function(env, func, def);
        const char *name = heap_strf("#convert-from:%s", type_to_string(src_t));
        sss_hashmap_t *ns = get_namespace(env, target_t);
        hset(ns, name, new(binding_t, .type=Type(FunctionType, .arg_types=LIST(sss_type_t*, src_t), .ret=target_t),
                           .func=func, .rval=gcc_get_func_address(func, NULL), .visible_in_closures=true));
        return NULL;
    }
    case StructDef: case TaggedUnionDef: case VariantDef: {
        sss_type_t *t;
        List(ast_t*) members;
        if (ast->tag == StructDef) {
            auto struct_def = Match(ast, StructDef);
            members = struct_def->definitions;
            binding_t *b = get_binding(env, struct_def->name);
            if (!b) compiler_err(env, ast, "I couldn't find the type for the struct named '%s'", struct_def->name);
            t = Match(b->type, TypeType)->type;
        } else if (ast->tag == TaggedUnionDef) {
            auto tu_def = Match(ast, TaggedUnionDef);
            members = tu_def->definitions;
            binding_t *b = get_binding(env, tu_def->name);
            if (!b) compiler_err(env, ast, "I couldn't find the type for the tagged union named '%s'", tu_def->name);
            t = Match(b->type, TypeType)->type;
        } else if (ast->tag == VariantDef) {
            auto def = Match(ast, VariantDef);
            members = def->body ? Match(def->body, Block)->statements : LIST(ast_t*);
            t = Type(VariantType, .filename=ast->span.file->filename,
                     .name=def->name, .variant_of=parse_type_ast(env, def->variant_of));
        } else {
            errx(1, "Unreachable");
        }
        env = get_type_env(env, t);
        foreach (members, member, _) {
            if ((*member)->tag == Declare) {
                auto decl = Match((*member), Declare);
                gcc_rvalue_t *rval = compile_expr(env, block, decl->value);
                sss_type_t *t = get_type(env, decl->value);
                assert(t);
                gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
                const char* name = Match(decl->var, Var)->name;
                const char* sym_name = fresh(name);
                gcc_lvalue_t *lval = gcc_global(env->ctx, ast_loc(env, (*member)), GCC_GLOBAL_INTERNAL, gcc_t, sym_name);
                hset(env->bindings, name,
                     new(binding_t, .lval=lval, .rval=gcc_rval(lval), .type=t, .sym_name=sym_name, .visible_in_closures=true));
                assert(rval);
                gcc_assign(*block, ast_loc(env, (*member)), lval, rval);
            } else {
                compile_statement(env, block, *member);
            }
        }
        return NULL;
    }
    case Struct: {
        auto struct_ = Match(ast, Struct);
        sss_type_t *t;
        double unit_scaling = 1.0;
        if (struct_->units)
            unit_derive(struct_->units, &unit_scaling, env->derived_units);
        binding_t *binding;
        if (struct_->type) {
            binding = get_ast_binding(env, struct_->type);
            if (!binding || binding->type->tag != TypeType)
                compiler_err(env, struct_->type, "This isn't a struct type that I recognize");
            t = Match(binding->type, TypeType)->type;
        } else {
            binding = NULL;
            t = get_type(env, ast);
        }

        while (t->tag == VariantType)
            t = Match(t, VariantType)->variant_of;

        if (t->tag != StructType)
            compiler_err(env, ast, "This is not a valid struct type");

        auto struct_type = Match(t, StructType);
        size_t num_values = length(struct_->members);
        if (length(struct_type->field_names) == 0) {
            // GCC doesn't allow empty constructors for empty structs, but for
            // some reason, it's perfectly fine to just declare an empty struct
            // variable and use that as an rvalue.
            gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
            gcc_lvalue_t *lval = gcc_local(gcc_block_func(*block), loc, gcc_t, "_empty_singleton");
            return gcc_rval(lval);
        }
        if (length(struct_->members) > length(struct_type->field_names))
            compiler_err(env, ast, "I expected this %T literal to only have %ld fields, but you provided %ld fields.",
                  t, length(struct_type->field_names), length(struct_->members));

        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
        size_t num_fields = gcc_field_count(gcc_struct);
        if (!(num_fields == (size_t)length(struct_type->field_names)))
            compiler_err(env, ast, "Something went wrong with this struct!");
        gcc_field_t *unused_fields[num_fields];
        for (size_t i = 0, count = gcc_field_count(gcc_struct); i < count; i++)
            unused_fields[i] = gcc_get_field(gcc_struct, i);

        struct {
            size_t field_num;
            ast_t *ast;
            gcc_rvalue_t *value;
            gcc_field_t *field;
        } entries[num_fields];

        for (size_t i = 0; i < num_values; i++) {
            auto member = Match(ith(struct_->members, i), StructField);
            entries[i].ast = member->value;
            entries[i].value = compile_expr(env, block, member->value);
        }

        // Put in named fields first:
        for (size_t value_index = 0; value_index < num_values; value_index++) {
            ast_t *member_ast = ith(struct_->members, value_index);
            auto member = Match(member_ast, StructField);
            if (!member->name) continue;
            for (size_t field_index = 0; field_index < num_fields; field_index++) {
                if (!streq(ith(struct_type->field_names, field_index), member->name))
                    continue;

                gcc_field_t *field = unused_fields[field_index];
                if (!field)
                    compiler_err(env, member_ast, "You already provided a value for this field earlier in this struct.");

                // Found the field:
                entries[value_index].field = field;
                entries[value_index].field_num = field_index;
                unused_fields[field_index] = NULL;
                goto found_name;
            }

            compiler_err(env, member_ast, "There is no struct field with this name");

          found_name: continue;
        }

        // Now put in unnamed fields:
        for (size_t value_index = 0; value_index < num_values; value_index++) {
            ast_t *member_ast = ith(struct_->members, value_index);
            auto member = Match(member_ast, StructField);
            if (member->name) continue;
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
            compiler_err(env, member_ast, "This field is beyond the number of fields in this struct.");

          found_index: continue;
        }

        // Populate default struct field values (or nil for optional types)
        for (size_t field_index = 0; num_values < num_fields && field_index < num_fields; field_index++) {
            sss_type_t *ft = ith(struct_type->field_types, field_index);
            ast_t *def = struct_type->field_defaults ? ith(struct_type->field_defaults, field_index) : NULL;
            if (def != NULL) {
                entries[num_values].field = unused_fields[field_index];
                entries[num_values].field_num = field_index;
                entries[num_values].ast = def;
                gcc_rvalue_t *def_val = compile_expr(env, block, def);
                if (!promote(env, get_type(env, def), &def_val, ft))
                    compiler_err(env, def, "I couldn't make this default value work as a %T", ft);
                entries[num_values].value = def_val;
                unused_fields[field_index] = NULL;
                ++num_values;
                continue;
            }

            if (!can_leave_uninitialized(ft) && unused_fields[field_index]) {
                compiler_err(env, ast, "%T structs are supposed to have a non-optional field '%s' (%T), but you didn't provide a value for it.",
                      t, ith(struct_type->field_names, field_index), ft);
            }
        }

        // GCC is dumb and requires sorting the fields:
        qsort_r(entries, num_values, sizeof(entries[0]), (int(*)(const void*,const void*,void*))(void*)memcmp, (void*)sizeof(size_t));
        gcc_field_t *populated_fields[num_values];
        gcc_rvalue_t *rvalues[num_values];
        for (size_t i = 0; i < num_values; i++) {
            // Check type:
            sss_type_t *expected = ith(struct_type->field_types, entries[i].field_num);
            if (demote_int_literals(&entries[i].ast, expected))
                entries[i].value = compile_expr(env, block, entries[i].ast);
            sss_type_t *actual = get_type(env, entries[i].ast);
            rvalues[i] = entries[i].value;
            if (!promote(env, actual, &rvalues[i], expected)) {
                compiler_err(env, entries[i].ast, "I was expecting a value of type %T for the %T.%s field, but this value is a %T.", 
                      expected, t, ith(struct_type->field_names, entries[i].field_num), actual);
            }

            if (unit_scaling != 1.0 && is_numeric(expected)) {
                gcc_type_t *expected_gcc_t = sss_type_to_gcc(env, expected);
                rvalues[i] = gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, expected_gcc_t,
                                           rvalues[i], gcc_rvalue_from_double(env->ctx, expected_gcc_t, unit_scaling));
            }

            populated_fields[i] = entries[i].field;
        }

        gcc_rvalue_t *rval = gcc_struct_constructor(env->ctx, loc, gcc_t, num_values, populated_fields, rvalues);
        assert(rval);
        return rval;
    }
    case FunctionCall: {
        auto call = Match(ast, FunctionCall);
        gcc_rvalue_t *fn_ptr = NULL;
        gcc_func_t *fn = NULL;
        sss_type_t *fn_bl_t = get_type(env, call->fn);
        if (fn_bl_t->tag != FunctionType)
            compiler_err(env, call->fn, "This is not a callable function (it's a %T)", fn_bl_t);
        auto fn_t = Match(fn_bl_t, FunctionType);

        int64_t num_args = length(fn_t->arg_types);
        gcc_rvalue_t **arg_vals = GC_MALLOC(sizeof(gcc_rvalue_t*)*num_args);
        // method calls:
        if (call->fn->tag == FieldAccess) { // method call (foo.method())
            auto access = Match(call->fn, FieldAccess);
            ast_t *self = access->fielded;
            sss_type_t *self_t = get_type(env, self);
            if (streq(access->field, "__hash"))
                (void)get_hash_func(env, self_t); 
            else if (streq(access->field, "__compare"))
                (void)get_compare_func(env, self_t); 
            else if (streq(access->field, "__print"))
                (void)get_print_func(env, self_t); 
            sss_type_t *value_type = self_t;
            while (value_type->tag == PointerType)
                value_type = Match(value_type, PointerType)->pointed;
            switch (value_type->tag) {
            case ModuleType: goto non_method_fncall;
            case TypeType: {
                sss_type_t *fielded_type = Match(value_type, TypeType)->type;
                binding_t *binding = get_from_namespace(env, fielded_type, access->field);
                if (!binding)
                    compiler_err(env, call->fn, "I couldn't find any method called %s for %T.", access->field, fielded_type);
                fn = binding->func;
                fn_ptr = binding->rval;
                break;
            }
            default: {
                binding_t *binding = (value_type->tag == ArrayType) ?
                    get_array_method(env, value_type, access->field)
                    : get_from_namespace(env, self_t, access->field);
                if (!binding)
                    binding = get_from_namespace(env, value_type, access->field);
                if (!binding)
                    goto non_method_fncall;
                if (binding->type->tag != FunctionType)
                    compiler_err(env, call->fn, "This value isn't a function, it's a %T", binding->type);
                auto fn_info = Match(binding->type, FunctionType);
                if (length(fn_info->arg_types) < 1)
                    compiler_err(env, call->fn, "This function doesn't take any arguments. If you want to call it anyways, use the class name like %T.%s()",
                          value_type, access->field);

                gcc_rvalue_t *self_val = compile_expr(env, block, self);
                sss_type_t *expected_self = ith(fn_info->arg_types, 0);
                if (!type_eq(self_t, expected_self) && !promote(env, self_t, &self_val, expected_self))
                    compiler_err(env, ast, "The method %T.%s(...) is being called on a %T, but it wants a %T.",
                          self_t, access->field, self_t, expected_self);
                arg_vals[0] = self_val;
                fn = binding->func;
                fn_ptr = binding->rval;
                break;
            }
            }
        } else {
          non_method_fncall:
            fn_ptr = compile_expr(env, block, call->fn);
            fn = NULL;
        }

        const int64_t MASK = -1; // mask is because the hashmap api returns 0 for missing keys
        sss_hashmap_t arg_positions = {0};
        if (fn_t->arg_names) {
            for (int64_t i = 0; i < num_args; i++) {
                if (!ith(fn_t->arg_names, i)) continue;
                int64_t masked = i ^ MASK;
                hset(&arg_positions, ith(fn_t->arg_names, i), masked);
            }
        }

        // First: keyword args
        foreach (call->args, arg, _) {
            if ((*arg)->tag != KeywordArg) continue;
            auto kwarg = Match((*arg), KeywordArg);
            int64_t arg_index = hget(&arg_positions, kwarg->name, int64_t);
            if (arg_index == 0) // missing key
                compiler_err(env, *arg, "\"%s\" is not the name of any argument for this function that I'm aware of",
                      kwarg->name);
            arg_index ^= MASK;
            if (arg_vals[arg_index])
                compiler_err(env, *arg, "This argument was already passed in earlier to this function call");
            sss_type_t *expected = ith(fn_t->arg_types, arg_index);
            ast_t *arg_val = kwarg->arg;
            demote_int_literals(&arg_val, expected);
            gcc_rvalue_t *val = compile_expr(env, block, arg_val);
            sss_type_t *actual = get_type(env, arg_val);
            if (!promote(env, actual, &val, expected))
                compiler_err(env, *arg, "This function expected this argument to have type %T, but this value is a %T", expected, actual);
            arg_vals[arg_index] = val;
        }

        // Then positional args
        int64_t pos = 0;
        for (int64_t call_index = 0, len = length(call->args); call_index < len; call_index++) {
            ast_t *arg = ith(call->args, call_index);
            if (arg->tag == KeywordArg) continue;
            // Find the next unspecified arg:
            if (pos >= num_args)
                compiler_err(env, arg, "This is one argument too many for this function call");
            while (arg_vals[pos]) {
                ++pos;
                if (pos >= num_args)
                    compiler_err(env, arg, "This is one argument too many for this function call");
                assert(pos < num_args);
            }
            sss_type_t *expected = ith(fn_t->arg_types, pos);
            demote_int_literals(&arg, expected);
            gcc_rvalue_t *val = compile_expr(env, block, arg);
            sss_type_t *actual = get_type(env, arg);
            if (!promote(env, actual, &val, expected))
                compiler_err(env, arg, "This function expected this argument to have type %T, but this value is a %T", expected, actual);
            arg_vals[pos] = val;
        }

        env_t default_arg_env = fn_t->env ? *(env_t*)fn_t->env : *file_scope(env);
        default_arg_env.bindings = new(sss_hashmap_t, .fallback=default_arg_env.bindings);
        gcc_func_t *func = gcc_block_func(*block);

        if (fn_t->arg_names && fn_t->arg_defaults) {
            // Stash args in local variables so we don't evaluate them multiple times (e.g. def foo(x:Int,y=x)... foo(Int.random()))
            for (int64_t i = 0; i < num_args; i++) {
                if (arg_vals[i]) {
                    const char *arg_name = ith(fn_t->arg_names, i);
                    gcc_lvalue_t *tmp = gcc_local(func, loc, sss_type_to_gcc(env, ith(fn_t->arg_types, i)), arg_name ? arg_name : "_arg");
                    gcc_assign(*block, loc, tmp, arg_vals[i]);
                    arg_vals[i] = gcc_rval(tmp);
                }
            }
            for (int64_t i = 0; i < num_args; i++) {
                if (arg_vals[i] && ith(fn_t->arg_names, i))
                    hset(default_arg_env.bindings, ith(fn_t->arg_names, i), new(binding_t, .type=ith(fn_t->arg_types, i), .rval=arg_vals[i]));
            }
        }

        // Optional values get passed as nil or default values are used:
        for (int64_t len = num_args; pos < len; pos++) {
            if (arg_vals[pos]) continue;
            if (fn_t->arg_defaults) {
                ast_t *default_val = ith(fn_t->arg_defaults, pos);
                if (default_val) {
                    const char *arg_name = ith(fn_t->arg_names, pos);
                    gcc_lvalue_t *tmp = gcc_local(func, loc, sss_type_to_gcc(env, ith(fn_t->arg_types, pos)), arg_name ? arg_name : "_arg");
                    gcc_assign(*block, loc, tmp, compile_expr(&default_arg_env, block, default_val));
                    arg_vals[pos] = gcc_rval(tmp);
                    hset(default_arg_env.bindings, ith(fn_t->arg_names, pos), new(binding_t, .type=ith(fn_t->arg_types, pos), .rval=arg_vals[pos]));
                    continue;
                }
            }
            sss_type_t *arg_t = ith(fn_t->arg_types, pos);
            if (arg_t->tag != PointerType || !Match(arg_t, PointerType)->is_optional) {
                const char *arg_name = fn_t->arg_names ? ith(fn_t->arg_names, pos) : NULL;
                compiler_err(env, ast, "The non-optional argument %s was not provided",
                      arg_name ? arg_name : heap_strf("%ld", pos));
            }
            arg_vals[pos] = gcc_null(env->ctx, sss_type_to_gcc(env, arg_t));
            hset(default_arg_env.bindings, ith(fn_t->arg_names, pos), new(binding_t, .type=ith(fn_t->arg_types, pos), .rval=arg_vals[pos]));
        }

        if (fn)
            return gcc_call(env->ctx, ast_loc(env, ast), fn, num_args, arg_vals);
        else if (fn_ptr)
            return gcc_call_ptr(env->ctx, ast_loc(env, ast), fn_ptr, num_args, arg_vals);
        else
            assert(false);
    }
    case KeywordArg: {
        return compile_expr(env, block, Match(ast, KeywordArg)->arg);
    }
    case Bool: {
        return gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, BOOL), Match(ast, Bool)->b ? 1 : 0);
    }
    case Maybe: case HeapAllocate: {
        ast_t *value = ast->tag == Maybe ? Match(ast, Maybe)->value : Match(ast, HeapAllocate)->value;
        gcc_rvalue_t *rval;
        // Don't mark these as COW as they would otherwise be:
        if (value->tag == Array)
            rval = compile_array(env, block, value, false);
        else if (value->tag == Table)
            rval = compile_table(env, block, value, false);
        else
            rval = compile_expr(env, block, value);
        sss_type_t *t = get_type(env, value);
        if (t->tag == VoidType)
            compiler_err(env, value, "This expression is a Void type, which can't be heap allocated");
        gcc_func_t *func = gcc_block_func(*block);
        ssize_t gcc_size = gcc_sizeof(env, t);
        if (t->tag == ArrayType)
            gcc_size += 4; // Hidden "capacity" field
        gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), gcc_size);
        gcc_type_t *gcc_t = gcc_get_ptr_type(sss_type_to_gcc(env, t));
        gcc_lvalue_t *tmp = gcc_local(func, loc, gcc_t, heap_strf("_heap_%s", type_to_string(t)));
        gcc_func_t *alloc_func = get_function(env, has_heap_memory(t) ? "GC_malloc" : "GC_malloc_atomic");
        gcc_assign(*block, loc, tmp, gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, size), gcc_t));
        gcc_assign(*block, loc, gcc_rvalue_dereference(gcc_rval(tmp), loc), rval);
        if (t->tag == TableType && value->tag != Table)
            mark_table_cow(env, block, gcc_rval(tmp));
        else if (t->tag == ArrayType && value->tag != Array)
            mark_array_cow(env, block, gcc_rval(tmp));
        return gcc_rval(tmp);
    }
    case StackReference: {
        ast_t *value = Match(ast, StackReference)->value;
        sss_type_t *t = get_type(env, value);
        gcc_func_t *func = gcc_block_func(*block);
        switch (value->tag) {
        case Var: case FieldAccess: case Index: case Dereference: {
            gcc_lvalue_t *lval = get_lvalue(env, block, value, false);
            // With &x, &x.y, &x[i], &*x, we need to set the COW flag for arrays/tables:
            if (t->tag == TableType)
                mark_table_cow(env, block, gcc_lvalue_address(lval, loc));
            else if (t->tag == ArrayType)
                mark_array_cow(env, block, gcc_lvalue_address(lval, loc));
            return gcc_lvalue_address(lval, loc);
        }
        case Array: {
            // Don't mark &[...] literals as COW, since there are no other possible aliases yet
            gcc_lvalue_t *var = gcc_local(func, loc, sss_type_to_gcc(env, t), "_array");
            gcc_assign(*block, loc, var, compile_array(env, block, value, false));
            return gcc_lvalue_address(var, loc);
        }
        case Table: {
            // Don't mark &{..=>..} literals as COW, since there are no other possible aliases yet
            gcc_lvalue_t *var = gcc_local(func, loc, sss_type_to_gcc(env, t), "_table");
            gcc_assign(*block, loc, var, compile_table(env, block, value, false));
            return gcc_lvalue_address(var, loc);
        }
        default: {
            gcc_lvalue_t *var = gcc_local(func, loc, sss_type_to_gcc(env, t), "_value");
            gcc_assign(*block, loc, var, compile_expr(env, block, value));
            return gcc_lvalue_address(var, loc);
        }
        }
    }
    case Dereference: {
        sss_type_t *t = get_type(env, ast); // Check this is a pointer type
        ast_t *value = Match(ast, Dereference)->value;
        gcc_rvalue_t *obj = compile_expr(env, block, value);
        if (t->tag == ArrayType)
            mark_array_cow(env, block, obj);
        else if (t->tag == TableType)
            mark_table_cow(env, block, obj);
        return gcc_rval(gcc_rvalue_dereference(obj, loc));
    }
    case AssertNonNull: {
        sss_type_t *t = get_type(env, ast);
        ast_t *value = Match(ast, AssertNonNull)->value;
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *value_var = gcc_local(func, loc, gcc_t, "_ptr");
        gcc_assign(*block, loc, value_var, compile_expr(env, block, value));
        gcc_block_t *if_nil = gcc_new_block(func, fresh("if_nil")),
                    *done = gcc_new_block(func, fresh("done"));
        gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, gcc_rval(value_var), gcc_null(env->ctx, gcc_t)),
                           if_nil, done);
        *block = if_nil;
        insert_failure(env, block, &ast->span, "Error: this %s value was null when it was expected to be non-null",
                       type_to_string(get_type(env, value)));
        *block = done;
        return gcc_rval(value_var);
    }
    case Len: {
        ast_t *value = Match(ast, Len)->value;
        sss_type_t *t = get_type(env, value);
        gcc_rvalue_t *obj = compile_expr(env, block, value);
        gcc_rvalue_t *len = compile_len(env, block, t, obj);
        if (!len)
            compiler_err(env, ast, "I don't know how to get the length of a %T", t);
        return len;
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        gcc_rvalue_t *slice = array_field_slice(env, block, access->fielded, access->field, ACCESS_READ);
        if (slice) return slice;

        (void)get_type(env, ast); // typecheck
        sss_type_t *fielded_t = get_type(env, access->fielded);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *obj_lval = gcc_local(func, loc, sss_type_to_gcc(env, fielded_t), "_fielded");
        gcc_assign(*block, loc, obj_lval, compile_expr(env, block, access->fielded));
        gcc_rvalue_t *obj = gcc_rval(obj_lval);

      get_field:
        switch (fielded_t->tag) {
        case PointerType: {
            auto ptr = Match(fielded_t, PointerType);
            if (ptr->is_optional)
                compiler_err(env, ast, "This field access is unsafe because the value may be nil");

            if (ptr->pointed->tag == ArrayType)
                mark_array_cow(env, block, obj);
            else if (ptr->pointed->tag == TableType)
                mark_table_cow(env, block, obj);

            obj = gcc_rval(gcc_rvalue_dereference(obj, loc));
            fielded_t = ptr->pointed;
            goto get_field;
        }
        case VariantType: {
            fielded_t = Match(fielded_t, VariantType)->variant_of;
            goto get_field;
        }
        case StructType: {
            gcc_type_t *gcc_t = sss_type_to_gcc(env, fielded_t);
            gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
            auto struct_type = Match(fielded_t, StructType);
            for (int64_t i = 0, len = length(struct_type->field_names); i < len; i++) {
                if (streq(ith(struct_type->field_names, i), access->field)) {
                    gcc_field_t *field = gcc_get_field(gcc_struct, (size_t)i);
                    return gcc_rvalue_access_field(obj, loc, field);
                }
            }
            break;
        }
        case TableType: {
            if (streq(access->field, "has_default")) {
                gcc_type_t *ptr_t = gcc_get_ptr_type(sss_type_to_gcc(env, Match(fielded_t, TableType)->value_type));
                gcc_lvalue_t *default_ptr = gcc_local(func, loc, ptr_t, "_default_ptr");
                gcc_field_t *field = gcc_get_field(gcc_type_if_struct(sss_type_to_gcc(env, fielded_t)), TABLE_DEFAULT_FIELD);
                gcc_assign(*block, loc, default_ptr, gcc_rvalue_access_field(obj, loc, field));
                return gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, gcc_rval(default_ptr), gcc_null(env->ctx, ptr_t));
            } else if (streq(access->field, "has_fallback")) {
                gcc_type_t *ptr_t = gcc_get_ptr_type(sss_type_to_gcc(env, fielded_t));
                gcc_lvalue_t *fallback_ptr = gcc_local(func, loc, ptr_t, "_fallback_ptr");
                gcc_field_t *field = gcc_get_field(gcc_type_if_struct(sss_type_to_gcc(env, fielded_t)), TABLE_FALLBACK_FIELD);
                gcc_assign(*block, loc, fallback_ptr, gcc_rvalue_access_field(obj, loc, field));
                return gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, gcc_rval(fallback_ptr), gcc_null(env->ctx, ptr_t));
            } else if (streq(access->field, "default")) {
                gcc_type_t *ptr_t = gcc_get_ptr_type(sss_type_to_gcc(env, Match(fielded_t, TableType)->value_type));
                gcc_lvalue_t *default_ptr = gcc_local(func, loc, ptr_t, "_default_ptr");
                gcc_field_t *field = gcc_get_field(gcc_type_if_struct(sss_type_to_gcc(env, fielded_t)), TABLE_DEFAULT_FIELD);
                gcc_assign(*block, loc, default_ptr, gcc_rvalue_access_field(obj, loc, field));

                gcc_block_t *if_nil = gcc_new_block(func, fresh("if_nil")),
                            *if_nonnil = gcc_new_block(func, fresh("if_nonnil"));
                gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, gcc_rval(default_ptr), gcc_null(env->ctx, ptr_t)),
                                   if_nil, if_nonnil);
                *block = if_nil;
                insert_failure(env, block, &ast->span, "This table has no default value");
                *block = if_nonnil;
                return gcc_rval(gcc_rvalue_dereference(gcc_rval(default_ptr), loc));
            } else if (streq(access->field, "fallback")) {
                gcc_type_t *ptr_t = gcc_get_ptr_type(sss_type_to_gcc(env, fielded_t));
                gcc_lvalue_t *fallback_ptr = gcc_local(func, loc, ptr_t, "_fallback_ptr");
                gcc_type_t *hashmap_gcc_t = sss_type_to_gcc(env, fielded_t);
                gcc_assign(*block, loc, fallback_ptr, gcc_rvalue_access_field(obj, loc, gcc_get_field(gcc_type_if_struct(hashmap_gcc_t), TABLE_FALLBACK_FIELD)));

                gcc_block_t *if_nil = gcc_new_block(func, fresh("if_nil")),
                            *if_nonnil = gcc_new_block(func, fresh("if_nonnil"));
                gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, gcc_rval(fallback_ptr), gcc_null(env->ctx, ptr_t)),
                                   if_nil, if_nonnil);
                *block = if_nil;
                insert_failure(env, block, &ast->span, "This table has no fallback value");
                *block = if_nonnil;
                return gcc_rval(gcc_rvalue_dereference(gcc_rval(fallback_ptr), loc));
            } else if (streq(access->field, "keys")) {
                sss_type_t *item_t = Match(fielded_t, TableType)->key_type;
                gcc_type_t *gcc_t = sss_type_to_gcc(env, Type(ArrayType, .item_type=item_t));
                gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
                gcc_struct_t *table_struct = gcc_type_if_struct(sss_type_to_gcc(env, fielded_t));
                size_t entry_size = gcc_sizeof(env, table_entry_type(fielded_t));
                return gcc_struct_constructor(
                    env->ctx, loc, gcc_t, 4,
                    (gcc_field_t*[]){
                        gcc_get_field(gcc_struct, ARRAY_DATA_FIELD),
                        gcc_get_field(gcc_struct, ARRAY_LENGTH_FIELD),
                        gcc_get_field(gcc_struct, ARRAY_STRIDE_FIELD),
                        gcc_get_field(gcc_struct, ARRAY_CAPACITY_FIELD),
                    },
                    (gcc_rvalue_t*[]){
                        gcc_cast(env->ctx, loc, gcc_rvalue_access_field(obj, loc, gcc_get_field(table_struct, TABLE_ENTRIES_FIELD)),
                                 gcc_get_ptr_type(sss_type_to_gcc(env, item_t))), // items
                        gcc_cast(env->ctx, loc, gcc_rvalue_access_field(obj, loc, gcc_get_field(table_struct, TABLE_COUNT_FIELD)),
                                 gcc_type(env->ctx, INT32)), // len
                        gcc_rvalue_int16(env->ctx, entry_size), // stride
                        gcc_rvalue_int16(env->ctx, -1), // capacity
                    });
            } else if (streq(access->field, "values")) {
                sss_type_t *key_t = Match(fielded_t, TableType)->key_type;
                sss_type_t *value_t = Match(fielded_t, TableType)->value_type;
                gcc_type_t *gcc_t = sss_type_to_gcc(env, Type(ArrayType, .item_type=value_t));
                gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
                gcc_struct_t *table_struct = gcc_type_if_struct(sss_type_to_gcc(env, fielded_t));

                gcc_rvalue_t *items_ptr = gcc_cast(
                    env->ctx, loc, gcc_rvalue_access_field(obj, loc, gcc_get_field(table_struct, TABLE_ENTRIES_FIELD)),
                    gcc_type(env->ctx, STRING));
                items_ptr = pointer_offset(env, gcc_get_ptr_type(sss_type_to_gcc(env, value_t)), items_ptr, gcc_rvalue_size(env->ctx, gcc_sizeof(env, key_t)));
                size_t entry_size = gcc_sizeof(env, table_entry_type(fielded_t));
                return gcc_struct_constructor(
                    env->ctx, loc, gcc_t, 4,
                    (gcc_field_t*[]){
                        gcc_get_field(gcc_struct, ARRAY_DATA_FIELD),
                        gcc_get_field(gcc_struct, ARRAY_LENGTH_FIELD),
                        gcc_get_field(gcc_struct, ARRAY_STRIDE_FIELD),
                        gcc_get_field(gcc_struct, ARRAY_CAPACITY_FIELD),
                    },
                    (gcc_rvalue_t*[]){
                        items_ptr, // items
                        gcc_cast(env->ctx, loc, gcc_rvalue_access_field(obj, loc, gcc_get_field(table_struct, TABLE_COUNT_FIELD)),
                                 gcc_type(env->ctx, INT32)), // len
                        gcc_rvalue_int16(env->ctx, entry_size), // stride
                        gcc_rvalue_int16(env->ctx, -1), // capacity
                    });
            }
            break;
        }
        case TaggedUnionType: {
            auto tagged = Match(fielded_t, TaggedUnionType);
            for (int64_t i = 0; i < length(tagged->members); i++) {
                auto member = ith(tagged->members, i);
                if (!streq(access->field, member.name)) continue;

                gcc_struct_t *tagged_struct = gcc_type_if_struct(sss_type_to_gcc(env, fielded_t));
                gcc_field_t *tag_field = gcc_get_field(tagged_struct, 0);
                gcc_rvalue_t *tag = gcc_rvalue_access_field(obj, NULL, tag_field);
                gcc_block_t *wrong_tag = gcc_new_block(func, fresh("wrong_tag")),
                            *right_tag = gcc_new_block(func, fresh("right_tag"));

                gcc_type_t *tag_gcc_t = get_tag_type(env, fielded_t);
                gcc_rvalue_t *correct_tag = gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value);
                gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, tag, correct_tag),
                                   wrong_tag, right_tag);
                *block = wrong_tag;
                insert_failure(env, block, &ast->span, "Error: this was expected to have the '%s' tag, but instead it's %#s", access->field,
                               fielded_t, obj);
                if (*block) gcc_jump(*block, loc, *block);

                *block = right_tag;
                gcc_type_t *gcc_union_t = get_union_type(env, fielded_t);
                return gcc_rvalue_access_field(gcc_rvalue_access_field(obj, NULL, gcc_get_field(tagged_struct, 1)), loc,
                                               gcc_get_union_field(gcc_union_t, i));
            }
            goto class_lookup;
        }
        case TypeType: {
            sss_type_t *t = Match(fielded_t, TypeType)->type;
            binding_t *val_binding = get_from_namespace(env, t, access->field);
            if (val_binding)
                return val_binding->rval;
            else
                compiler_err(env, ast, "I can't find any field or method called \"%s\" on the type %T.", access->field, t);
            break;
        }
        default: break;
        }
        // Class lookup:
      class_lookup:
        binding_t *binding = get_from_namespace(env, fielded_t, access->field);
        if (binding)
            return binding->rval;
        else {
            compiler_err(env, ast, "I can't find any field or method called \"%s\" on the type %T.", access->field, fielded_t);
        }
    }
    case Index: {
        auto indexing = Match(ast, Index);

        sss_type_t *t = get_type(env, indexing->indexed);
        for (;;) {
            if (t->tag == PointerType)
                t = Match(t, PointerType)->pointed;
            else if (t->tag == VariantType)
                t = Match(t, VariantType)->variant_of;
            else break;
        }

        if (t->tag == ArrayType) {
            return gcc_rval(array_index(env, block, indexing->indexed, indexing->index, indexing->unchecked, ACCESS_READ));
        } else if (t->tag == TableType) {
            gcc_rvalue_t *key_rval;
            gcc_rvalue_t *val_opt = table_lookup_optional(env, block, indexing->indexed, indexing->index, &key_rval, false);
            if (indexing->unchecked)
                return gcc_rval(gcc_rvalue_dereference(val_opt, loc));

            gcc_func_t *func = gcc_block_func(*block);
            gcc_type_t *gcc_value_t = sss_type_to_gcc(env, Match(t, TableType)->value_type);
            gcc_lvalue_t *value_var = gcc_local(func, loc, gcc_value_t, "_value");
            gcc_block_t *if_nil = gcc_new_block(func, fresh("if_nil")),
                        *if_nonnil = gcc_new_block(func, fresh("if_nonnil")),
                        *done = gcc_new_block(func, fresh("done"));
            gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, val_opt, gcc_null(env->ctx, gcc_get_ptr_type(gcc_value_t))),
                               if_nil, if_nonnil);
            *block = if_nil;
            insert_failure(env, block, &ast->span, "Error: this table does not have the given key: %#s",
                           Match(t, TableType)->key_type, key_rval);

            *block = if_nonnil;
            gcc_assign(*block, loc, value_var, gcc_rval(gcc_rvalue_dereference(val_opt, loc)));
            gcc_jump(*block, loc, done);

            *block = done;
            return gcc_rval(value_var);
        } else {
            compiler_err(env, ast, "I only know how to index Arrays and Tables, not %T", t);
        }
    }
    case In: case NotIn: {
        auto in = ast->tag == In ? Match(ast, In) : Match(ast, NotIn);
        sss_type_t *member_t = get_type(env, in->member);
        sss_type_t *container_t = get_type(env, in->container);

        sss_type_t *container_value_t = container_t;
        while (container_value_t->tag == PointerType)
            container_value_t = Match(container_value_t, PointerType)->pointed;

        gcc_rvalue_t *ret;
        if (container_value_t->tag == TableType) {
            if (!type_is_a(member_t, Match(container_value_t, TableType)->key_type))
                compiler_err(env, ast, "This is checking for the presence of a key with type %T, but the table has type %T",
                            member_t, container_value_t);

            gcc_rvalue_t *val_opt = table_lookup_optional(env, block, in->container, in->member, NULL, true);
            gcc_rvalue_t *missing = gcc_null(env->ctx, gcc_get_ptr_type(sss_type_to_gcc(env, Match(container_value_t, TableType)->value_type)));
            ret = gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, val_opt, missing);
        } else if (container_value_t->tag == ArrayType) {
            ret = array_contains(env, block, in->container, in->member);
        } else if (member_t->tag == TaggedUnionType && type_eq(member_t, container_t)) {
            gcc_type_t *gcc_tagged_t = sss_type_to_gcc(env, member_t);
            gcc_struct_t *gcc_tagged_s = gcc_type_if_struct(gcc_tagged_t);
            gcc_field_t *tag_field = gcc_get_field(gcc_tagged_s, 0);
            gcc_type_t *tag_gcc_t = get_tag_type(env, member_t);
            gcc_rvalue_t *member_val = compile_expr(env, block, in->member);
            gcc_rvalue_t *container_val = compile_expr(env, block, in->container);
            ret = gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE,
                                 gcc_zero(env->ctx, tag_gcc_t),
                                 gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_BITWISE_AND, tag_gcc_t,
                                               gcc_rvalue_access_field(member_val, loc, tag_field),
                                               gcc_rvalue_access_field(container_val, loc, tag_field)));
        } else if (container_value_t->tag == RangeType) {
            ret = range_contains(env, block, in->container, in->member);
        } else {
            compiler_err(env, ast, "'in' membership testing is only supported for Arrays and Tables, not %T", container_t);
        }
        if (ast->tag == NotIn)
            ret = gcc_unary_op(env->ctx, loc, GCC_UNOP_LOGICAL_NEGATE, gcc_type(env->ctx, BOOL), ret);
        return ret;
    }
    case TypeOf: {
        auto value = Match(ast, TypeOf)->value;
        sss_type_t *t = get_type(env, value);
        return gcc_str(env->ctx, type_to_typeof_string(t));
    }
    case SizeOf: {
        auto value = Match(ast, SizeOf)->value;
        sss_type_t *t = get_type(env, value);
        ssize_t size = gcc_sizeof(env, t);
        return gcc_int64(env->ctx, size);
    }
    case Cast: {
        auto cast = Match(ast, Cast);
        gcc_rvalue_t *val = compile_expr(env, block, cast->value);
        sss_type_t *src_t = get_type(env, cast->value);
        sss_type_t *cast_t = get_type(env, ast);
        if (type_eq(src_t, cast_t))
            return val;

        binding_t *convert_b = get_from_namespace(env, cast_t, heap_strf("#convert-from:%s", type_to_string(src_t)));
        if (convert_b)
            return gcc_callx(env->ctx, loc, convert_b->func, val);

        if (src_t->tag == TaggedUnionType && cast_t->tag == IntType) {
            auto tagged = Match(src_t, TaggedUnionType);
            int64_t max_tag = 0;
            foreach (tagged->members, member, _) {
                if (member->tag_value > max_tag)
                    max_tag = member->tag_value;
            }
            ssize_t size = 0;
            if (max_tag > INT32_MAX) size = 8;
            else if (max_tag > INT16_MAX) size = 4;
            else if (max_tag > INT8_MAX) size = 2;
            else size = 1;
            if (gcc_sizeof(env, cast_t) < size)
                compiler_err(env, ast, "This tagged enum cannot be converted to %T without loss of precision", cast_t);

            gcc_struct_t *tagged_struct = gcc_type_if_struct(sss_type_to_gcc(env, src_t));
            gcc_field_t *tag_field = gcc_get_field(tagged_struct, 0);
            gcc_rvalue_t *tag = gcc_rvalue_access_field(val, NULL, tag_field);
            return gcc_cast(env->ctx, loc, tag, sss_type_to_gcc(env, cast_t));
        }

        if (!((is_numeric(src_t) && is_numeric(cast_t))
              || (src_t->tag == PointerType && cast_t->tag == PointerType && can_promote(src_t, cast_t))
              || (is_numeric(src_t) && (cast_t->tag == BoolType || cast_t->tag == CharType))
              || (is_numeric(cast_t) && (src_t->tag == BoolType || src_t->tag == CharType))))
            compiler_err(env, ast, "I don't know how to convert %T to %T. "
                        "You should implement a `def x:%T as %T` conversion function or use 'bitcast'",
                        src_t, cast_t, src_t, cast_t);
        return gcc_cast(env->ctx, loc, val, sss_type_to_gcc(env, cast_t));
    }
    case Bitcast: {
        auto bitcast = Match(ast, Bitcast);
        sss_type_t *t = get_type(env, ast);
        if (gcc_sizeof(env, get_type(env, bitcast->value)) != gcc_sizeof(env, t))
            compiler_err(env, ast, "This value can't be cast to the given type, because it has a different size."); 
        gcc_rvalue_t *val = compile_expr(env, block, bitcast->value);
        return gcc_bitcast(env->ctx, loc, val, sss_type_to_gcc(env, t));
    }
    case Nil: {
        sss_type_t *t = get_type(env, ast);
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        if (gcc_type_if_pointer(gcc_t))
            return gcc_null(env->ctx, sss_type_to_gcc(env, t));
        else
            compiler_err(env, ast, "There is no nil value for %T", t);
    }
    case Not: {
        auto value = Match(ast, Not)->value;
        sss_type_t *t = get_type(env, value);
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        gcc_rvalue_t *val = compile_expr(env, block, value);
        if (t->tag == BoolType)
            return gcc_unary_op(env->ctx, ast_loc(env, ast), GCC_UNOP_LOGICAL_NEGATE, gcc_t, val);
        else if (is_integral(t))
            return gcc_unary_op(env->ctx, ast_loc(env, ast), GCC_UNOP_BITWISE_NEGATE, gcc_t, val);
        else if (t->tag == PointerType && Match(t, PointerType)->is_optional)
            return gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, val, gcc_null(env->ctx, gcc_t));
        else if (t->tag == TaggedUnionType) {
            gcc_type_t *gcc_tagged_t = sss_type_to_gcc(env, t);
            gcc_struct_t *gcc_tagged_s = gcc_type_if_struct(gcc_tagged_t);
            gcc_field_t *tag_field = gcc_get_field(gcc_tagged_s, 0);
            gcc_type_t *tag_gcc_t = get_tag_type(env, t);
            int64_t all_tags = 0;
            auto members = Match(t, TaggedUnionType)->members;
            for (int64_t i = 0; i < length(members); i++) {
                if (ith(members, i).type)
                    compiler_err(env, ast, "%T tagged union values can't be negated because some tags have data attached to them.", t);
                all_tags |= ith(members, i).tag_value;
            }
            gcc_rvalue_t *result_tag = gcc_binary_op(env->ctx, loc, GCC_BINOP_BITWISE_XOR, tag_gcc_t,
                                                     gcc_rvalue_access_field(val, loc, tag_field),
                                                     gcc_rvalue_from_long(env->ctx, tag_gcc_t, all_tags));
            return gcc_struct_constructor(env->ctx, NULL, gcc_tagged_t, 1, (gcc_field_t*[]){tag_field}, &result_tag);
        } else
            compiler_err(env, ast, "The 'not' operator isn't supported for values with type %T", t);
    }
    case Equal: case NotEqual:
    case Less: case LessEqual: case Greater: case GreaterEqual: {
        // Unsafe! This is a hack to avoid duplicate code, based on the assumption that each of these types
        // has the same struct layout:
        ast_t *lhs = ast->__data.Less.lhs,
              *rhs = ast->__data.Less.rhs;
        // End of unsafe
        gcc_comparison_e cmp;
        bool is_ordered;
        switch (ast->tag) {
        case Equal: cmp = GCC_COMPARISON_EQ; is_ordered = false; break;
        case NotEqual: cmp = GCC_COMPARISON_NE; is_ordered = false; break;
        case Less: cmp = GCC_COMPARISON_LT; is_ordered = true; break;
        case LessEqual: cmp = GCC_COMPARISON_LE; is_ordered = true; break;
        case Greater: cmp = GCC_COMPARISON_GT; is_ordered = true; break;
        case GreaterEqual: cmp = GCC_COMPARISON_GE; is_ordered = true; break;
        default: assert(false);
        }

        sss_type_t *lhs_t = get_type(env, lhs);
        sss_type_t *rhs_t = get_type(env, rhs);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);

        if (!promote(env, lhs_t, &lhs_val, rhs_t)
            && !promote(env, rhs_t, &rhs_val, lhs_t))
            compiler_err(env, ast, "I don't know how to do a comparison between a %T and a %T.", lhs_t, rhs_t);

        if (is_ordered && !is_orderable(lhs_t))
            compiler_err(env, ast, "I can't do ordered comparisons between values with type %T", lhs_t);

        if (is_numeric(lhs_t) || lhs_t->tag == PointerType)
            return gcc_comparison(env->ctx, loc, cmp, lhs_val, rhs_val);

        return gcc_comparison(env->ctx, loc, cmp, compare_values(env, lhs_t, lhs_val, rhs_val),
                              gcc_zero(env->ctx, gcc_type(env->ctx, INT)));
    }
    case Negative: {
        ast_t *value = Match(ast, Negative)->value;
        sss_type_t *t = get_type(env, value);
        if (!is_numeric(t))
            compiler_err(env, ast, "I only know how to get negative numbers, not %T", t);
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        gcc_rvalue_t *rval = compile_expr(env, block, value);
        return gcc_unary_op(env->ctx, loc, GCC_UNOP_MINUS, gcc_t, rval);
    }
    case And: {
        ast_t *lhs = Match(ast, And)->lhs,
              *rhs = Match(ast, And)->rhs;
        sss_type_t *t = get_type(env, ast);
        sss_type_t *lhs_t = get_type(env, lhs);
        sss_type_t *rhs_t = get_type(env, rhs);

        if (!((lhs_t->tag == BoolType && rhs_t->tag == BoolType) || lhs_t->tag == AbortType || rhs_t->tag == AbortType || lhs_t->tag == PointerType || rhs_t->tag == PointerType))
            return math_binop(env, block, ast);

        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *result = gcc_local(func, loc, sss_type_to_gcc(env, t), "_and_result");
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        gcc_assign(*block, NULL, result, lhs_val);
        lhs_val = gcc_rval(result);
        if (is_integral(lhs_t) && is_integral(rhs_t)) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
            // Numeric promotion:
            if (!promote(env, lhs_t, &lhs_val, rhs_t))
                assert(promote(env, rhs_t, &rhs_val, lhs_t));
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_BITWISE_AND, sss_type_to_gcc(env, t), lhs_val, rhs_val);
        }
        gcc_block_t *if_truthy = gcc_new_block(func, fresh("and_truthy"));
        gcc_block_t *done = gcc_new_block(func, fresh("and_done"));

        gcc_type_t *lhs_gcc_t = sss_type_to_gcc(env, lhs_t);
        gcc_rvalue_t *bool_val = lhs_val;
        if (t->tag != BoolType) {
            gcc_rvalue_t *zero = gcc_type_if_pointer(lhs_gcc_t) ? gcc_null(env->ctx, lhs_gcc_t) : gcc_zero(env->ctx, lhs_gcc_t);
            bool_val = gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, lhs_val, zero);
        }
        gcc_jump_condition(*block, loc, bool_val, if_truthy, done);

        if (rhs_t->tag == AbortType) {
            compile_statement(env, &if_truthy, rhs);
        } else {
            gcc_rvalue_t *rhs_val = compile_expr(env, &if_truthy, rhs);
            if (if_truthy)
                gcc_assign(if_truthy, loc, result, rhs_val);
        }
        if (if_truthy)
            gcc_jump(if_truthy, loc, done);

        *block = done;
        return gcc_rval(result);
    }
    case Or: {
        ast_t *lhs = Match(ast, Or)->lhs,
              *rhs = Match(ast, Or)->rhs;
        sss_type_t *t = get_type(env, ast);
        sss_type_t *lhs_t = get_type(env, lhs);
        sss_type_t *rhs_t = get_type(env, rhs);

        if (!((lhs_t->tag == BoolType && rhs_t->tag == BoolType) || lhs_t->tag == AbortType || rhs_t->tag == AbortType || lhs_t->tag == PointerType || rhs_t->tag == PointerType))
            return math_binop(env, block, ast);

        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *result = gcc_local(func, loc, sss_type_to_gcc(env, t), "_and_result");
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        gcc_assign(*block, loc, result, lhs_val);
        lhs_val = gcc_rval(result);
        if (is_integral(lhs_t) && is_integral(rhs_t)) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
            if (!promote(env, lhs_t, &lhs_val, rhs_t)
                && !promote(env, rhs_t, &rhs_val, lhs_t))
                compiler_err(env, ast, "I can't figure out how to combine a %T and a %T", lhs_t, rhs_t);
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_BITWISE_OR, sss_type_to_gcc(env, t), lhs_val, rhs_val);
        }
        gcc_block_t *if_falsey = gcc_new_block(func, fresh("or_falsey"));
        gcc_block_t *done = gcc_new_block(func, fresh("or_done"));

        gcc_type_t *lhs_gcc_t = sss_type_to_gcc(env, lhs_t);
        gcc_rvalue_t *bool_val = lhs_val;
        if (t->tag != BoolType) {
            gcc_rvalue_t *zero = gcc_type_if_pointer(lhs_gcc_t) ? gcc_null(env->ctx, lhs_gcc_t) : gcc_zero(env->ctx, lhs_gcc_t);
            bool_val = gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, lhs_val, zero);
        }
        gcc_jump_condition(*block, loc, bool_val, done, if_falsey);

        if (rhs_t->tag == AbortType) {
            compile_statement(env, &if_falsey, rhs);
        } else {
            gcc_rvalue_t *rhs_val = compile_expr(env, &if_falsey, rhs);
            if (if_falsey)
                gcc_assign(if_falsey, loc, result, rhs_val);
        }
        if (if_falsey)
            gcc_jump(if_falsey, loc, done);

        *block = done;
        return gcc_rval(result);
    }
    case Xor: {
        return math_binop(env, block, ast);
    }
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate: case OrUpdate: case AndUpdate: case XorUpdate: {
        return math_update(env, block, ast);
    }
    case Add: case Subtract: case Divide: case Multiply: case LeftShift: case RightShift: {
        return math_binop(env, block, ast);
    }
    case Concatenate: {
        auto concat = Match(ast, Concatenate);
        (void)get_type(env, ast);
        ast_t *lhs_loop = WrapAST(concat->lhs, For, .iter=concat->lhs,
                                  .value=WrapAST(concat->lhs, Var, .name="x"),
                                  .body=WrapAST(concat->lhs, Var, .name="x"));
        ast_t *rhs_loop = WrapAST(concat->rhs, For, .iter=concat->rhs,
                                  .value=WrapAST(concat->rhs, Var, .name="x"),
                                  .body=WrapAST(concat->rhs, Var, .name="x"));
        ast_t *concat_ast = WrapAST(ast, Array, .items=LIST(ast_t*, lhs_loop, rhs_loop));
        return compile_expr(env, block, concat_ast);
    }
    case ConcatenateUpdate: {
        auto concat = Match(ast, ConcatenateUpdate);
        sss_type_t *t_lhs = get_type(env, concat->lhs);
        sss_type_t *array_t = t_lhs;
        while (array_t->tag == PointerType) array_t = Match(array_t, PointerType)->pointed;
        binding_t *b = get_array_method(env, array_t, "insert_all");
        assert(b);
        gcc_type_t *i64 = gcc_type(env->ctx, INT64);
        if (t_lhs->tag == ArrayType) {
            gcc_lvalue_t *lval = get_lvalue(env, block, concat->lhs, false);
            gcc_rvalue_t *rhs_rval = set_pointer_level(env, block, concat->rhs, 0);
            gcc_rvalue_t *index_rval = gcc_binary_op(env->ctx, loc, GCC_BINOP_PLUS, i64, gcc_one(env->ctx, i64), compile_len(env, block, t_lhs, gcc_rval(lval)));
            gcc_eval(*block, loc, gcc_callx(env->ctx, loc, b->func, gcc_lvalue_address(lval, loc), rhs_rval, index_rval));
            return gcc_rval(lval);
        } else if (t_lhs->tag == PointerType) {
            gcc_rvalue_t *lhs_rval = set_pointer_level(env, block, concat->lhs, 1);
            gcc_rvalue_t *rhs_rval = set_pointer_level(env, block, concat->rhs, 0);
            gcc_rvalue_t *index_rval = gcc_binary_op(env->ctx, loc, GCC_BINOP_PLUS, i64, gcc_one(env->ctx, i64), compile_len(env, block, t_lhs, lhs_rval));
            gcc_eval(*block, loc, gcc_callx(env->ctx, loc, b->func, lhs_rval, rhs_rval, index_rval));
            return lhs_rval;
        } else {
            compiler_err(env, ast, "Concatenation update is only defined for array types and pointers to array types.");
        }
    }
    case Modulus: case Modulus1: {
        // UNSAFE: this works because the two tags have the same layout:
        ast_t *lhs = ast->__data.Modulus.lhs, *rhs = ast->__data.Modulus.rhs;
        // END UNSAFE
        sss_type_t *t = get_type(env, ast);
        sss_type_t *lhs_t = get_type(env, lhs);
        sss_type_t *rhs_t = get_type(env, rhs);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
        if (!promote(env, lhs_t, &lhs_val, rhs_t)
            && !promote(env, rhs_t, &rhs_val, lhs_t))
            compiler_err(env, ast, "The left hand side of this modulus has type %T, but the right hand side has type %T and I can't figure out how to combine them.",
                         lhs_t, rhs_t);

        if (ast->tag == Modulus1)
            lhs_val = gcc_binary_op(env->ctx, loc, GCC_BINOP_MINUS, sss_type_to_gcc(env, t), lhs_val, gcc_one(env->ctx, sss_type_to_gcc(env, t)));

        if (t->tag == NumType) {
            gcc_func_t *sane_fmod_func = get_function(env, "sane_fmod");
            if (Match(t, NumType)->bits != 64) {
                return gcc_cast(
                    env->ctx, loc,
                    gcc_callx(env->ctx, loc, sane_fmod_func,
                              gcc_cast(env->ctx, loc, lhs_val, gcc_type(env->ctx, DOUBLE)),
                              gcc_cast(env->ctx, loc, rhs_val, gcc_type(env->ctx, DOUBLE))),
                    gcc_type(env->ctx, FLOAT));
            }
            gcc_rvalue_t *result = gcc_callx(env->ctx, loc, sane_fmod_func, lhs_val, rhs_val);
            if (ast->tag == Modulus1)
                result = gcc_binary_op(env->ctx, loc, GCC_BINOP_PLUS, sss_type_to_gcc(env, t), result, gcc_one(env->ctx, sss_type_to_gcc(env, t)));
            return result;
        } else {
            gcc_rvalue_t *result = gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_MODULO, sss_type_to_gcc(env, t), lhs_val, rhs_val);
            // Ensure modulus result is positive (i.e. (-1 mod 10) == 9)
            result = gcc_binary_op(env->ctx, loc, GCC_BINOP_PLUS, sss_type_to_gcc(env, t), result, rhs_val);
            result = gcc_binary_op(env->ctx, loc, GCC_BINOP_MODULO, sss_type_to_gcc(env, t), result, rhs_val);
            if (ast->tag == Modulus1)
                result = gcc_binary_op(env->ctx, loc, GCC_BINOP_PLUS, sss_type_to_gcc(env, t), result, gcc_one(env->ctx, sss_type_to_gcc(env, t)));
            return result;
        }
    }
    case Power: {
        ast_t *base = Match(ast, Power)->lhs, *exponent = Match(ast, Power)->rhs;
        sss_type_t *t = get_type(env, ast);
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        sss_type_t *base_t = get_type(env, base);
        sss_type_t *rhs_t = get_type(env, exponent);
        gcc_rvalue_t *base_val = compile_expr(env, block, base);
        gcc_rvalue_t *exponent_val = compile_expr(env, block, exponent);
        if (!promote(env, base_t, &base_val, rhs_t)
            && !promote(env, rhs_t, &exponent_val, base_t))
            compiler_err(env, ast, "The base of this operation has type %T, but the exponent has type %T and I can't figure out how to combine them.",
                         base_t, rhs_t);

        gcc_type_t *double_t = gcc_type(env->ctx, DOUBLE);
        if (t->tag != NumType) {
            base_val = gcc_cast(env->ctx, loc, base_val, double_t);
            exponent_val = gcc_cast(env->ctx, loc, exponent_val, double_t);
        }
        gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, double_t,
                                        "pow", 2, (gcc_param_t*[]){
                                        gcc_new_param(env->ctx, NULL, double_t, "base"),
                                        gcc_new_param(env->ctx, NULL, double_t, "exponent")}, 0);
        gcc_rvalue_t *ret = gcc_callx(env->ctx, loc, func, base_val, exponent_val);
        if (t->tag != NumType)
            ret = gcc_cast(env->ctx, loc, ret, gcc_t);
        return ret;
    }
    case If: {
        auto if_ = Match(ast, If);
        sss_type_t *subject_t = if_->subject->tag == Declare ? get_type(env, Match(if_->subject, Declare)->value) : get_type(env, if_->subject);

        if (subject_t->tag != BoolType && ith(if_->patterns, 0)->tag == Bool)
            compiler_err(env, if_->subject, "This value's type is %T, but for it to work as a conditional, it should be a Bool", subject_t);

        gcc_rvalue_t *subject = compile_expr(env, block, if_->subject);
        gcc_type_t *gcc_t = sss_type_to_gcc(env, subject_t);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *subject_var = gcc_local(func, loc, gcc_t, "_when_subject");
        gcc_assign(*block, loc, subject_var, subject);
        if (if_->subject->tag == Declare) {
            env = fresh_scope(env);
            hset(env->bindings, Match(Match(if_->subject, Declare)->var, Var)->name,
                 new(binding_t, .type=subject_t, .lval=subject_var, .rval=gcc_rval(subject_var)));
        }
        subject = gcc_rval(subject_var);

        gcc_block_t *done = gcc_new_block(func, fresh("finished"));

        sss_type_t *result_t = get_type(env, ast);
        bool has_value = !(result_t->tag == GeneratorType || result_t->tag == AbortType || result_t->tag == VoidType);

        // Check exhaustiveness:
        if (has_value) {
            const char *missing = get_missing_pattern(env, subject_t, if_->patterns);
            if (missing) compiler_err(env, ast, missing);
        }

        gcc_lvalue_t *when_value = has_value ? gcc_local(func, loc, sss_type_to_gcc(env, result_t), "_when_value") : NULL;
        for (int64_t i = 0; i < LIST_LEN(if_->patterns); i++) {
            auto outcomes = perform_conditional_match(env, block, subject_t, subject, ith(if_->patterns, i));
            gcc_rvalue_t *result = compile_expr(outcomes.match_env, &outcomes.match_block, ith(if_->blocks, i));
            assert(*block == NULL);
            if (outcomes.match_block) {
                if (result) {
                    if (has_value)
                        gcc_assign(outcomes.match_block, loc, when_value, result);
                    else
                        gcc_eval(outcomes.match_block, loc, result);
                }
                gcc_jump(outcomes.match_block, loc, done);
            }
            *block = outcomes.no_match_block;
        }
        if (*block)
            gcc_jump(*block, loc, done);
        *block = done;
        // Ensure the (unreachable) done block is terminated when every branch aborts:
        if (*block && result_t->tag == AbortType) {
            // If there's a way out of this without aborting, nevermind.
            // *Technically* this should be typechecked as a generator type,
            // but the type checker doesn't check for exhaustiveness.
            if (!get_missing_pattern(env, subject_t, if_->patterns)) {
                gcc_jump(*block, loc, *block);
                *block = NULL;
            }
        }

        return has_value ? gcc_rval(when_value) : NULL;
    }
    case Range: {
        return compile_range(env, block, ast);
    }
    case Repeat: {
        auto loop = Match(ast, Repeat);
        compile_while_loop(env, block, "repeat", NULL, loop->body, loop->between);
        return NULL;
    }
    case While: {
        auto loop = Match(ast, While);
        compile_while_loop(env, block, "while", loop->condition, loop->body, loop->between);
        return NULL;
    }
    case For: {
        compile_for_loop(env, block, ast);
        return NULL;
    }
    case Skip: case Stop: {
        if (env->is_deferred)
            compiler_err(env, ast, "This '%s' is inside a 'defer' block, which is not allowed", ast->tag == Skip ? "skip" : "stop");

        gcc_block_t *jump_dest = NULL;
        defer_t *prev_deferred = env->deferred;
        const char* target = ast->tag == Skip ? Match(ast, Skip)->target : Match(ast, Stop)->target;
        if (target) {
            for (loop_label_t *lbl = env->loop_label; lbl; lbl = lbl->enclosing) {
                foreach (lbl->names, name, _) {
                    if (streq(*name, target)) {
                        if (ast->tag == Skip)
                            jump_dest = lbl->skip_label;
                        else
                            jump_dest = lbl->stop_label;
                        prev_deferred = lbl->deferred;
                        if (jump_dest)
                            goto found_label;
                    }
                }
            }
          found_label:;
        } else if (env->loop_label) {
            if (ast->tag == Skip)
                jump_dest = env->loop_label->skip_label;
            else
                jump_dest = env->loop_label->stop_label;
            prev_deferred = env->loop_label->deferred;
        }
        if (!jump_dest)
            compiler_err(env, ast, "I'm not sure what %s is referring to",
                        target ? target : (ast->tag == Skip ? "this 'skip'" : "this 'stop'"));

        insert_defers(env, block, prev_deferred);

        gcc_jump(*block, loc, jump_dest);
        *block = NULL;
        return NULL;
    }
    case Pass: return NULL;
    case Min: case Max: {
        ast_t *lhs_ast, *rhs_ast, *key;
        gcc_comparison_e cmp;
        if (ast->tag == Min) {
            cmp = GCC_COMPARISON_LE;
            lhs_ast = Match(ast, Min)->lhs;
            rhs_ast = Match(ast, Min)->rhs;
            key = Match(ast, Min)->key;
        } else {
            cmp = GCC_COMPARISON_GE;
            lhs_ast = Match(ast, Max)->lhs;
            rhs_ast = Match(ast, Max)->rhs;
            key = Match(ast, Max)->key;
        }
        gcc_func_t *func = gcc_block_func(*block);
        sss_type_t *t = get_type(env, ast);
        sss_type_t *lhs_t = get_type(env, lhs_ast),
                  *rhs_t = get_type(env, rhs_ast);
        gcc_lvalue_t *lhs = gcc_local(func, loc, sss_type_to_gcc(env, lhs_t), "_lhs"),
                     *rhs = gcc_local(func, loc, sss_type_to_gcc(env, rhs_t), "_rhs"),
                     *result = gcc_local(func, loc, sss_type_to_gcc(env, t), "_result");

        gcc_assign(*block, loc, lhs, compile_expr(env, block, lhs_ast));
        gcc_assign(*block, loc, rhs, compile_expr(env, block, rhs_ast));

        gcc_rvalue_t *lhs_val = gcc_rval(lhs),
                     *rhs_val = gcc_rval(rhs);

        if (!promote(env, lhs_t, &lhs_val, rhs_t)
            && !promote(env, rhs_t, &rhs_val, lhs_t))
            compiler_err(env, ast, "I don't know how to do a comparison between a %T and a %T.", lhs_t, rhs_t);

        if (!is_orderable(lhs_t))
            compiler_err(env, ast, "I can't do ordered comparisons between values with type %T", lhs_t);

        gcc_rvalue_t *should_choose_lhs;
        if (key) {
            env_t *lhs_env = fresh_scope(env), *rhs_env = fresh_scope(env);
            const char *var_name = (ast->tag == Min) ? "_min_" : "_max_";
            // Note: These both use 't' because promotion has already occurred.
            hset(lhs_env->bindings, var_name, new(binding_t, .type=t, .rval=lhs_val));
            hset(rhs_env->bindings, var_name, new(binding_t, .type=t, .rval=rhs_val));

            sss_type_t *cmp_lhs_t = get_type(lhs_env, key);
            gcc_rvalue_t *lhs_cmp_val = compile_expr(lhs_env, block, key),
                         *rhs_cmp_val = compile_expr(rhs_env, block, key);
            if (is_numeric(cmp_lhs_t) || cmp_lhs_t->tag == PointerType)
                should_choose_lhs = gcc_comparison(env->ctx, loc, cmp, lhs_cmp_val, rhs_cmp_val);
            else
                should_choose_lhs = gcc_comparison(env->ctx, loc, cmp, compare_values(env, cmp_lhs_t, lhs_cmp_val, rhs_cmp_val),
                                                   gcc_zero(env->ctx, gcc_type(env->ctx, INT)));
        } else {
            if (is_numeric(lhs_t) || lhs_t->tag == PointerType)
                should_choose_lhs = gcc_comparison(env->ctx, loc, cmp, lhs_val, rhs_val);
            else
                should_choose_lhs = gcc_comparison(env->ctx, loc, cmp, compare_values(env, lhs_t, lhs_val, rhs_val),
                                                   gcc_zero(env->ctx, gcc_type(env->ctx, INT)));
        }

        gcc_block_t *choose_lhs = gcc_new_block(func, fresh("choose_lhs")),
                    *choose_rhs = gcc_new_block(func, fresh("choose_rhs")),
                    *done = gcc_new_block(func, fresh("done"));

        gcc_jump_condition(*block, loc, should_choose_lhs, choose_lhs, choose_rhs);
        gcc_assign(choose_lhs, loc, result, lhs_val);
        gcc_jump(choose_lhs, loc, done);
        gcc_assign(choose_rhs, loc, result, rhs_val);
        gcc_jump(choose_rhs, loc, done);
        *block = done;
        return gcc_rval(result);
    }
    case Mix: {
        auto mix = Match(ast, Mix);
        sss_type_t *mix_t = get_type(env, mix->key);
        if (!is_numeric(mix_t))
            compiler_err(env, mix->key, "The mix amount here is not a numeric value.");

        // (1-amount)*lhs + amount*rhs
        ast_t *amount_var = WrapAST(mix->key, Var, "$mix_amount");
        ast_t *amount = mix->key;
        if (streq(type_units(mix_t), "%"))
            amount = WrapAST(mix->key, Divide, amount, WrapAST(mix->key, Num, .n=1.0, .precision=32, .units="%"));
        ast_t *mix_equation = WrapAST(ast, Block, LIST(ast_t*,
            WrapAST(mix->key, Declare, amount_var, amount),
            WrapAST(ast, Add,
                WrapAST(mix->lhs, Multiply,
                    WrapAST(mix->lhs, Subtract, WrapAST(mix->lhs, Num, .n=1.0, .precision=32), amount_var),
                    mix->lhs),
                WrapAST(mix->rhs, Multiply, amount_var, mix->rhs))));
        return compile_expr(env, block, mix_equation);
    }
    case Reduction: {
        auto reduction = Match(ast, Reduction);
        sss_type_t *t = get_type(env, ast);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *ret = gcc_local(func, loc, sss_type_to_gcc(env, t), fresh("reduction"));

        env = fresh_scope(env);

        ast_t *accum_var = WrapAST(ast, Var, .name="x");
        hset(env->bindings, Match(accum_var, Var)->name, new(binding_t, .lval=ret, .rval=gcc_rval(ret), .type=t));
        ast_t *incoming_var = WrapAST(ast, Var, .name="y");

        ast_t *index, *value, *iter, *first, *between, *empty;
        if (reduction->iter->tag == For) {
            auto loop = Match(reduction->iter, For);
            if (loop->index) index = WrapAST(ast, Var, .name=fresh("#i"));
            else index = NULL;
        } else {
            index = NULL;
        }
        value = incoming_var;
        iter = reduction->iter;
        first = WrapAST(reduction->combination, Assign, LIST(ast_t*, accum_var), LIST(ast_t*, value));
        between = WrapAST(reduction->combination, Assign, LIST(ast_t*, accum_var), LIST(ast_t*, reduction->combination));

        if (reduction->fallback)
            empty = WrapAST(reduction->fallback, Assign, LIST(ast_t*, accum_var), LIST(ast_t*, reduction->fallback));
        else
            empty = WrapAST(reduction->iter, Fail, .message=StringAST(reduction->iter, "This collection was empty"));

        ast_t *for_ast = WrapAST(ast, For, .index=index, .value=value, .iter=iter, .first=first, .between=between, .body=FakeAST(Skip), .empty=empty);
        compile_statement(env, block, for_ast);
        return gcc_rval(ret);
    }
    case Fail: {
        if (env->is_deferred)
            compiler_err(env, ast, "This 'fail' is inside a 'defer' block, which is not allowed");

        ast_t *message = Match(ast, Fail)->message;
        if (message) {
            gcc_rvalue_t *msg = compile_expr(env, block, message);
            insert_failure(env, block, &ast->span, "%#s", get_type(env, message), msg);
        } else {
            insert_failure(env, block, &ast->span, "A failure occurred");
        }
        return NULL;
    }
    case DocTest: {
        auto test = Match(ast, DocTest);
        ast_t *expr = test->expr;

        gcc_rvalue_t *use_color = get_binding(env, "USE_COLOR")->rval;
        gcc_rvalue_t *stderr_val = gcc_rval(gcc_global(env->ctx, NULL, GCC_GLOBAL_IMPORTED, gcc_type(env->ctx, FILE_PTR), "stderr"));
        gcc_func_t *fputs_fn = hget(&env->global->funcs, "fputs", gcc_func_t*);
        if (!test->skip_source) {
            const char* color_src = heap_strf("\x1b[33;1m>>> \x1b[0m%.*s\x1b[m\n", (int)(test->expr->span.end - test->expr->span.start), test->expr->span.start);
            const char* plain_src = heap_strf(">>> %.*s\n", (int)(test->expr->span.end - test->expr->span.start), test->expr->span.start);
            gcc_rvalue_t *source = ternary(block, use_color, gcc_type(env->ctx, STRING), gcc_str(env->ctx, color_src), gcc_str(env->ctx, plain_src));
            gcc_eval(*block, loc, gcc_callx(env->ctx, loc, fputs_fn, source, stderr_val)); 
        }

        if (expr->tag == Return && Match(expr, Return)->value) {
            if (test->output)
                compiler_err(env, ast, "Sorry, I don't support testing return values in doctests with '==='");
            ast_t *ret = Match(expr, Return)->value;
            sss_type_t *ret_t = get_type(env, ret);
            if (ret_t->tag == VoidType) return NULL;
            gcc_func_t *func = gcc_block_func(*block);
            gcc_lvalue_t *ret_var = gcc_local(func, loc, sss_type_to_gcc(env, ret_t), "_ret");
            gcc_assign(*block, loc, ret_var, compile_expr(env, block, ret));
            const char *info = heap_strf("%.*s = ", (int)(ret->span.end - ret->span.start), ret->span.start);
            print_doctest_value(env, block, loc, info, ret_t, gcc_rval(ret_var));
            gcc_return(*block, loc, gcc_rval(ret_var));
            *block = NULL;
            return NULL;
        }

        sss_type_t *t = get_type(env, expr);
        if (t->tag == VoidType || t->tag == AbortType) {
            if (test->output)
                compiler_err(env, ast, "There shouldn't be any output for a Void expression like this");

            assert(is_discardable(env, ast));
            gcc_rvalue_t *val = compile_expr(env, block, expr);
            if (val && *block)
                gcc_eval(*block, ast_loc(env, expr), val);
            else
                return NULL;

            // For declarations, assignment, and updates, even though it
            // doesn't evaluate to a value, it's helpful to print out the new
            // values of the variable. (For multi-assignments, the result is a
            // tuple)
            sss_type_t *lhs_t = NULL;
            const char *info = NULL;
            switch (expr->tag) {
            case AddUpdate: case SubtractUpdate: case MultiplyUpdate: case DivideUpdate: case AndUpdate: case OrUpdate: case XorUpdate: case ConcatenateUpdate:
            case Declare: {
                // UNSAFE: this assumes all these types have the same layout:
                ast_t *lhs_ast = expr->__data.AddUpdate.lhs;
                // END UNSAFE
                lhs_t = get_type(env, lhs_ast);
                info = heap_strf("%.*s = ", (int)(lhs_ast->span.end - lhs_ast->span.start), lhs_ast->span.start);
                break;
            }
            case Assign: {
                auto assign = Match(expr, Assign);
                ast_t *first = ith(assign->targets, 0);
                if (length(assign->targets) == 1) {
                    lhs_t = get_type(env, first);
                    info = heap_strf("%.*s = ", (int)(first->span.end - first->span.start), first->span.start);
                    gcc_type_t *gcc_struct_t = sss_type_to_gcc(env, Type(StructType, .field_types=LIST(sss_type_t*,lhs_t), .field_names=LIST(const char*, "_1")));
                    val = gcc_rvalue_access_field(
                        val, loc, gcc_get_field(gcc_type_if_struct(gcc_struct_t), 0));
                } else {
                    ast_t *last = ith(assign->targets, length(assign->targets)-1);
                    info = heap_strf("%.*s = ", (int)(last->span.end - first->span.start), first->span.start);
                    NEW_LIST(ast_t*, members);
                    for (int64_t i = 0; i < length(assign->targets); i++) {
                        APPEND(members, WrapAST(ith(assign->targets, i), StructField, .name=heap_strf("_%d", i+1), .value=ith(assign->targets, i)));
                    }
                    lhs_t = get_type(env, WrapAST(expr, Struct, .members=members));
                }
                break;
            }
            default: break;
            }

            if (info && lhs_t)
                print_doctest_value(env, block, loc, info, lhs_t, val);
            return NULL;
        } else {
            // Print "= <expr>"
            gcc_rvalue_t *val = compile_expr(env, block, expr);
            gcc_func_t *func = gcc_block_func(*block);
            gcc_lvalue_t *val_var = gcc_local(func, loc, sss_type_to_gcc(env, t), "_expression");
            gcc_assign(*block, loc, val_var, val);
            val = gcc_rval(val_var);
            print_doctest_value(env, block, loc, "= ", t, val);
            if (test->output) {
                gcc_func_t *open_memstream_fn = hget(&env->global->funcs, "open_memstream", gcc_func_t*);

                // char *buf; size_t size;
                // FILE *f = open_memstream(&buf, &size);
                gcc_lvalue_t *buf_var = gcc_local(func, loc, gcc_type(env->ctx, STRING), "buf");
                gcc_lvalue_t *size_var = gcc_local(func, loc, gcc_type(env->ctx, SIZE), "size");
                gcc_lvalue_t *file_var = gcc_local(func, loc, gcc_type(env->ctx, FILE_PTR), "file");
                gcc_assign(*block, loc, file_var,
                           gcc_callx(env->ctx, loc, open_memstream_fn, gcc_lvalue_address(buf_var, loc), gcc_lvalue_address(size_var, loc)));
                gcc_rvalue_t *file = gcc_rval(file_var);

                gcc_func_t *print_fn = get_print_func(env, t);
                assert(print_fn);
                
                gcc_rvalue_t *cycle_checker = make_cycle_checker(env, loc, *block);
                gcc_rvalue_t *print_call = gcc_callx(env->ctx, loc, print_fn, val, file, cycle_checker, gcc_rvalue_bool(env->ctx, false));
                gcc_eval(*block, loc, print_call);

                gcc_func_t *fflush_fn = hget(&env->global->funcs, "fflush", gcc_func_t*);
                gcc_eval(*block, loc, gcc_callx(env->ctx, loc, fflush_fn, file));

                // fail unless strcmp(output, "expected") == 0
                gcc_func_t *strcmp_fn = hget(&env->global->funcs, "strcmp", gcc_func_t*);
                gcc_block_t *done_block = gcc_new_block(func, fresh("test_done")),
                            *fail_block = gcc_new_block(func, fresh("test_failed"));
                gcc_jump_condition(*block, loc,
                    gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ,
                        gcc_callx(env->ctx, loc, strcmp_fn, gcc_rval(buf_var), gcc_str(env->ctx, test->output)),
                        gcc_zero(env->ctx, gcc_type(env->ctx, INT))),
                    done_block, fail_block);
                insert_failure(env, &fail_block, &ast->span, "Test failed!\nExpected: %s \nBut got:  %#s ",
                               test->output, Type(PointerType, Type(CStringCharType)), gcc_rval(buf_var));
                *block = done_block;

                // Cleanup
                gcc_func_t *fclose_fn = hget(&env->global->funcs, "fclose", gcc_func_t*);
                gcc_eval(*block, loc, gcc_callx(env->ctx, loc, fclose_fn, file));
                gcc_func_t *free_fn = hget(&env->global->funcs, "free", gcc_func_t*);
                gcc_eval(*block, loc, gcc_callx(env->ctx, loc, free_fn, gcc_rval(buf_var)));
            }
            return NULL;
        }
    }
    case Use: {
        return gcc_callx(env->ctx, NULL, prepare_use(env, ast));
    }
    case LinkerDirective: {
        auto directives = Match(ast, LinkerDirective)->directives;
        foreach (directives, d, _)
            gcc_add_driver_opt(env->ctx, *d);
        return NULL;
    }
    default: break;
    }
    compiler_err(env, ast, "I haven't yet implemented compiling for: %s", ast_to_str(ast)); 
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
