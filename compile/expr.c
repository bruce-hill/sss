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
        bl_type_t *fielded_t = get_type(env, access->fielded);
        binding_t *binding = get_ast_binding(env, ast);
        if (!binding) {
            compiler_err(env, ast, "I can't find any constant-value field or method called \"%s\" on a %s.", access->field, type_to_string(fielded_t));
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
        gcc_type_t *gcc_t = bl_type_to_gcc(env, get_type(env, ast));

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

        switch (intval->precision) {
        case 32:
            if (i < INT32_MIN)
                compiler_err(env, ast, "This integer literal is too small to fit in a %d bit integer %ld", intval->precision, i);
            else if (i > INT32_MAX)
                compiler_err(env, ast, "This integer literal is too big to fit in a %d bit integer %ld", intval->precision, i);
            break;
        case 16:
            if (i < INT16_MIN)
                compiler_err(env, ast, "This integer literal is too small to fit in a %d bit integer %ld", intval->precision, i);
            else if (i > INT16_MAX)
                compiler_err(env, ast, "This integer literal is too big to fit in a %d bit integer %ld", intval->precision, i);
            break;
        case 8:
            if (i < INT8_MIN)
                compiler_err(env, ast, "This integer literal is too small to fit in a %d bit integer %ld", intval->precision, i);
            else if (i > INT8_MAX)
                compiler_err(env, ast, "This integer literal is too big to fit in a %d bit integer %ld", intval->precision, i);
            break;
        default: break;
        }

        return gcc_rvalue_from_long(env->ctx, gcc_t, i);
    }
    case Char: {
        return gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, CHAR), Match(ast, Char)->c);
    }
    case Num: {
        gcc_type_t *gcc_t = bl_type_to_gcc(env, get_type(env, ast));
        double n = Match(ast, Num)->n;
        const char* units = Match(ast, Num)->units;
        units = unit_derive(units, &n, env->derived_units);
        return gcc_rvalue_from_double(env->ctx, gcc_t, n);
    }
    default: break;
    }
    compiler_err(env, ast, "I can't evaluate this value at compile-time. It needs to be a constant value.");
}

static gcc_rvalue_t *compile_len(env_t *env, gcc_block_t **block, bl_type_t *t, gcc_rvalue_t *obj)
{
    gcc_func_t *func = gcc_block_func(*block);
    switch (t->tag) {
    case ArrayType: {
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
        return gcc_cast(env->ctx, NULL, gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, 1)), gcc_type(env->ctx, INT64));
    }
    case TableType: {
        gcc_struct_t *table_struct = gcc_type_if_struct(bl_type_to_gcc(env, t));
        return gcc_cast(env->ctx, NULL, gcc_rvalue_access_field(obj, NULL, gcc_get_field(table_struct, TABLE_COUNT_FIELD)), gcc_type(env->ctx, INT64));
    }
    case PointerType: {
        auto ptr = Match(t, PointerType);
        if (ptr->is_optional) {
            gcc_lvalue_t *len_var = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("len"));
            gcc_block_t *if_nil = gcc_new_block(func, fresh("if_nil")),
                        *if_nonnil = gcc_new_block(func, fresh("if_nonnil")),
                        *done = gcc_new_block(func, fresh("if_nonnil"));
            gcc_jump_condition(*block, NULL,
                               gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, obj, gcc_null(env->ctx, bl_type_to_gcc(env, t))),
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
            gcc_func_t *len_func = hget(env->global_funcs, "strlen", gcc_func_t*);
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
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        return range_len(env, gcc_t, obj);
    }
    default: break;
    }
    return NULL;
}

static gcc_rvalue_t *set_pointer_level(env_t *env, gcc_block_t **block, ast_t *ast, unsigned int desired_level)
{
    bl_type_t *t = get_type(env, ast);
    unsigned int level = 0;
    for (bl_type_t *tmp = t; tmp->tag == PointerType; tmp = Match(tmp, PointerType)->pointed)
        ++level;

    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *lval = gcc_local(func, NULL, bl_type_to_gcc(env, t), fresh("val"));
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
        gcc_lvalue_t *lval = gcc_local(func, NULL, bl_type_to_gcc(env, t), fresh("val"));
        gcc_assign(*block, NULL, lval, rval);
        rval = gcc_lvalue_address(lval, NULL);
        t = Type(PointerType, .pointed=t, .is_optional=false);
        ++level;
    }
    return rval;
}

gcc_rvalue_t *compile_expr(env_t *env, gcc_block_t **block, ast_t *ast)
{
    gcc_loc_t *loc = ast_loc(env, ast);
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
        bl_type_t *t = get_type(env, decl->value);
        assert(t);
        if (t->tag == GeneratorType)
            compiler_err(env, decl->value, "This expression isn't guaranteed to have a single value, so you can't use it to initialize a variable."); 
        else if (t->tag == VoidType)
            compiler_err(env, decl->value, "This expression doesn't have a value (it has a Void type), so you can't store it in a variable."); 
        gcc_rvalue_t *rval = compile_expr(env, block, decl->value);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_lvalue_t *lval;
        const char* name = Match(decl->var, Var)->name;
        const char* sym_name = fresh(name);
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
        else if (clobbered && decl->is_global && clobbered->rval)
            compiler_err(env, ast, "This name is already being used for a global");
        hset(decl->is_global ? env->global_bindings : env->bindings, name,
             new(binding_t, .lval=lval, .rval=gcc_rval(lval), .type=t, .sym_name=decl->is_global ? sym_name : NULL));
        assert(rval);
        gcc_assign(*block, ast_loc(env, ast), lval, rval);
        return gcc_rval(lval);
    }

    case Extern: {
        auto ext = Match(ast, Extern);
        bl_type_t *t = parse_type_ast(env, ext->type);
        if (t->tag == FunctionType) {
            auto fn = Match(t, FunctionType); 
            gcc_type_t *gcc_ret_t = bl_type_to_gcc(env, fn->ret);
            NEW_LIST(gcc_param_t*, params);
            for (int64_t i = 0, len = length(fn->arg_types); i < len; i++) {
                gcc_type_t *arg_t = bl_type_to_gcc(env, ith(fn->arg_types, i));
                const char* arg_name = fn->arg_names ? ith(fn->arg_names, i) : NULL;
                if (!arg_name) arg_name = fresh("arg");
                APPEND(params, gcc_new_param(env->ctx, loc, arg_t, arg_name));
            }
            gcc_func_t *func = gcc_new_func(
                env->ctx, loc, GCC_FUNCTION_IMPORTED, gcc_ret_t, ext->name, length(params), params[0], 0);
            return gcc_get_func_address(func, loc);
        } else {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
            return gcc_rval(gcc_global(env->ctx, ast_loc(env, ast), GCC_GLOBAL_IMPORTED, gcc_t, ext->name));
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
            bl_type_t *type;
            bool is_table;
            union {
                gcc_lvalue_t *lval;
                struct {
                    gcc_rvalue_t *table;
                    gcc_lvalue_t *key;
                    bl_type_t *table_type;
                };
            };
        } assign_target_t;
        NEW_LIST(assign_target_t, lvals);
        foreach (targets, lhs, _) {
            bl_type_t *t_lhs = get_type(env, *lhs);
            assign_target_t target = {.type=t_lhs};
            if ((*lhs)->tag == Index && value_type(get_type(env, Match(*lhs, Index)->indexed))->tag == TableType) {
                target.is_table = true;
                bl_type_t *table_t = get_type(env, Match(*lhs, Index)->indexed);
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
                target.key = gcc_local(func, loc, bl_type_to_gcc(env, Match(target.table_type, TableType)->key_type), fresh("key"));
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
            bl_type_t *t_lhs = target.type;
            ast_t *rhs = ith(values, i);
            bl_type_t *t_rhs = get_type(env, rhs);
            // TODO: maybe allow generators to assign the *last* value, if any
            if (t_rhs->tag == GeneratorType)
                compiler_err(env, rhs, "This expression isn't guaranteed to have a single value, so you can't assign it to a variable."); 
            gcc_rvalue_t *rval = compile_expr(env, block, ith(values, i));

            if (!promote(env, t_rhs, &rval, t_lhs))
                compiler_err(env, rhs, "You're assigning this %s value to a variable with type %s and I can't figure out how to make that work.",
                      type_to_string(t_rhs), type_to_string(t_lhs));

            gcc_lvalue_t *tmp = gcc_local(func, loc, bl_type_to_gcc(env, t_rhs), fresh("to_assign"));
            assert(rval);
            gcc_assign(*block, loc, tmp, rval);
            append(rvals, gcc_rval(tmp));
        }
        NEW_LIST(bl_type_t*, target_types);
        NEW_LIST(const char*, field_names);
        for (int64_t i = 0; i < len; i++) {
            auto target = ith(lvals, i);
            if (target.is_table) {
                bl_type_t *key_t = Match(target.table_type, TableType)->key_type;
                bl_type_t *value_t = Match(target.table_type, TableType)->value_type;

                size_t key_size = gcc_sizeof(env, key_t);
                size_t value_align = gcc_alignof(env, value_t);
                size_t value_offset = key_size;
                if (value_align > 0 && value_offset % value_align != 0) value_offset = (value_offset - (value_offset % value_align) + value_align);
                gcc_rvalue_t *entry_offset = gcc_rvalue_size(env->ctx, value_offset);

                gcc_func_t *hashmap_set_fn = get_function(env, "bl_hashmap_set");
                gcc_func_t *key_hash = get_hash_func(env, key_t);
                gcc_func_t *key_cmp = get_indirect_compare_func(env, key_t);
                gcc_lvalue_t *val_lval = gcc_local(func, loc, bl_type_to_gcc(env, value_t), fresh("value"));
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
        bl_type_t *values_tuple = Type(StructType, .field_types=target_types, .field_names=field_names);
        gcc_type_t *values_gcc_t = bl_type_to_gcc(env, values_tuple);
        gcc_field_t *fields[len];
        for (int64_t i = 0; i < len; i++)
            fields[i] = gcc_get_field(gcc_type_if_struct(values_gcc_t), i);
        return gcc_struct_constructor(env->ctx, loc, values_gcc_t, len, fields, rvals[0]);
    }
    case Delete: {
        ast_t *to_delete = Match(ast, Delete)->value;
        if (to_delete->tag != Index)
            compiler_err(env, ast, "The 'del' statement is only supported for indexed values like val[x]");
        auto index = Match(to_delete, Index);
        bl_type_t *t = get_type(env, index->indexed);
        if (t->tag != PointerType)
            compiler_err(env, ast, "Only mutable tables can be used with 'del', not %s", type_to_string(t));
        if (Match(t, PointerType)->is_optional)
            compiler_err(env, index->indexed, "This value is optional and can't be safely dereferenced");
        bl_type_t *table_t = Match(t, PointerType)->pointed;
        if (table_t->tag != TableType)
            compiler_err(env, ast, "Only mutable tables can be used with 'del', not %s", type_to_string(t));

        bl_type_t *key_t = get_type(env, index->index);
        if (!type_is_a(key_t, Match(table_t, TableType)->key_type))
            compiler_err(env, index->index, "This key has type %s, but this table has a different key type: %s",
                        type_to_string(key_t), type_to_string(Match(table_t, TableType)->key_type));

        table_remove(env, block, table_t, compile_expr(env, block, index->indexed), compile_expr(env, block, index->index));
        return NULL;
    }
    case Do: {
        auto do_ = Match(ast, Do);
        gcc_func_t *func = gcc_block_func(*block);
        bl_type_t *t = get_type(env, ast);
        gcc_lvalue_t *result = (t->tag != VoidType && t->tag != AbortType)? gcc_local(func, loc, bl_type_to_gcc(env, t), fresh("do_result")) : NULL;

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
            gcc_lvalue_t *cached = gcc_local(func, loc, bl_type_to_gcc(env, entry->value->type), fresh(heap_strf("deferred_%s", entry->key)));
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

        if (env->return_type->tag == VoidType) {
            if (ret->value) {
                bl_type_t *child_type = get_type(env, ret->value);
                if (child_type->tag != VoidType)
                    compiler_err(env, ast, "I was expecting a plain `return` with no expression here or a Void-type function call, because the function this is inside has no declared return type. If you want to return a value, please change the function's definition to include a return type.");
                compile_statement(env, block, ret->value);
            }
            insert_defers(env, block, NULL);
            gcc_return_void(*block, loc);
        } else {
            if (!ret->value)
                compiler_err(env, ast, "I was expecting this `return` to have a value of type %s because of the function's type signature, but no value is being returned here.",
                      type_to_string(env->return_type));
            bl_type_t *t = get_type(env, ret->value);
            gcc_rvalue_t *val = compile_expr(env, block, ret->value);
            if (!promote(env, t, &val, env->return_type))
                compiler_err(env, ast, "I was expecting this `return` to have value of type %s because of the function's type signature, but this value has type %s",
                      type_to_string(env->return_type), type_to_string(t));

            // Put in a temporary variable so defers can run after evaluating return expr:
            gcc_func_t *func = gcc_block_func(*block);
            gcc_lvalue_t *return_var = gcc_local(func, loc, bl_type_to_gcc(env, t), fresh("return_val"));
            gcc_assign(*block, loc, return_var, val);

            // Now run defers:
            insert_defers(env, block, NULL);
            gcc_return(*block, loc, gcc_rval(return_var));
        }
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
        gcc_struct_constructor(env->ctx, loc, gcc_t, 3, (gcc_field_t*[]){ \
                                          gcc_get_field(gcc_type_if_struct(gcc_t), 0), \
                                          gcc_get_field(gcc_type_if_struct(gcc_t), 1), \
                                          gcc_get_field(gcc_type_if_struct(gcc_t), 2), \
                                      }, (gcc_rvalue_t*[]){ \
                                          str_rval, \
                                          len_rval, \
                                          stride_rval, \
                                      })
    case StringLiteral: {
        const char* str = Match(ast, StringLiteral)->str;
        gcc_rvalue_t *str_rval = gcc_str(env->ctx, str);
        gcc_rvalue_t *len_rval = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, INT32), strlen(str));
        gcc_rvalue_t *stride_rval = gcc_one(env->ctx, gcc_type(env->ctx, INT32));
        gcc_type_t *gcc_t = bl_type_to_gcc(env, Type(ArrayType, .item_type=Type(CharType)));
        return STRING_STRUCT(env, gcc_t, str_rval, len_rval, stride_rval);
    }
    case StringJoin: {
        auto string_join = Match(ast, StringJoin);
        auto chunks = string_join->children;
        foreach (chunks, chunk, _) {
            bl_type_t *t = get_type(env, *chunk);
            if (t->tag == VoidType)
                compiler_err(env, *chunk, "This expression doesn't have a value (it has a Void type), so you can't use it in a string."); 
        }

        bl_type_t *string_t = Type(ArrayType, .item_type=Type(CharType), .dsl=string_join->dsl);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, string_t);
        gcc_type_t *i32_t = gcc_type(env->ctx, INT32);

        // Optimize the case of empty strings
        if (length(chunks) == 0) {
            return STRING_STRUCT(env, gcc_t, gcc_null(env->ctx, gcc_type(env->ctx, STRING)), gcc_zero(env->ctx, i32_t), gcc_one(env->ctx, i32_t));
        } else if (length(chunks) == 1 && LIST_ITEM(chunks, 0)->tag == StringLiteral) {
            // Optimize the case of a single string literal
            return compile_expr(env, block, LIST_ITEM(chunks, 0));
        }

        gcc_func_t *open_memstream_fn = hget(env->global_funcs, "open_memstream", gcc_func_t*);
        gcc_func_t *free_fn = hget(env->global_funcs, "free", gcc_func_t*);
        gcc_func_t *fputs_fn = hget(env->global_funcs, "fputs", gcc_func_t*);
        gcc_func_t *fflush_fn = hget(env->global_funcs, "fflush", gcc_func_t*);
        gcc_func_t *fclose_fn = hget(env->global_funcs, "fclose", gcc_func_t*);

        // char *buf; size_t size;
        // FILE *f = open_memstream(&buf, &size);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *buf_var = gcc_local(func, loc, gcc_type(env->ctx, STRING), fresh("buf"));
        gcc_lvalue_t *size_var = gcc_local(func, loc, gcc_type(env->ctx, SIZE), fresh("size"));
        gcc_lvalue_t *file_var = gcc_local(func, loc, gcc_type(env->ctx, FILE_PTR), fresh("file"));
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
            if (string_join->dsl) {
                interp_value = NewAST(env->file, interp_value->span.start, interp_value->span.end, Cast,
                                      .value=interp_value, .type=NewAST(env->file, interp_value->span.start, interp_value->span.start,
                                                                        TypeDSL, .name=string_join->dsl));
            }
            bl_type_t *t = get_type(env, interp_value);

            if (!interp->quote_string && t->tag == ArrayType && Match(t, ArrayType)->item_type->tag == CharType && !Match(t, ArrayType)->dsl) {
                gcc_lvalue_t *interp_var = gcc_local(func, loc, bl_type_to_gcc(env, t), fresh("interp_str"));
                gcc_assign(*block, loc, interp_var, compile_expr(env, block, interp_value));
                gcc_rvalue_t *obj = gcc_rval(interp_var);
                // i = 1
                gcc_type_t *i32 = gcc_type(env->ctx, INT32);
                gcc_lvalue_t *i = gcc_local(func, loc, i32, fresh("i"));
                gcc_assign(*block, loc, i, gcc_zero(env->ctx, i32));
                gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
                gcc_rvalue_t *items = gcc_rvalue_access_field(obj, loc, gcc_get_field(array_struct, 0));
                gcc_rvalue_t *len = gcc_rvalue_access_field(obj, loc, gcc_get_field(array_struct, 1));
                gcc_rvalue_t *stride = gcc_rvalue_access_field(obj, loc, gcc_get_field(array_struct, 2));

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
                                                  gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, i32, gcc_rval(i), stride))),
                        gcc_rval(file_var)));
                
                // i += 1
                gcc_update(add_next_item, loc, i, GCC_BINOP_PLUS, gcc_one(env->ctx, i32));
                // if (i < len) goto add_next_item;
                gcc_jump_condition(add_next_item, loc, 
                              gcc_comparison(env->ctx, loc, GCC_COMPARISON_LT, gcc_rval(i), len),
                              add_next_item, done);

                *block = done;
                continue;
            }

            gcc_func_t *print_fn = get_print_func(env, t);
            assert(print_fn);
            
            // Do bl_hashmap_t rec = {0}; def = 0; rec->default = &def; print(obj, &rec)
            bl_type_t *cycle_checker_t = Type(TableType, .key_type=Type(PointerType, .pointed=Type(VoidType)), .value_type=Type(IntType, .bits=64));
            gcc_type_t *hashmap_gcc_t = bl_type_to_gcc(env, cycle_checker_t);
            gcc_lvalue_t *cycle_checker = gcc_local(func, loc, hashmap_gcc_t, fresh("rec"));
            gcc_assign(*block, loc, cycle_checker, gcc_struct_constructor(env->ctx, loc, hashmap_gcc_t, 0, NULL, NULL));
            gcc_lvalue_t *next_index = gcc_local(func, loc, gcc_type(env->ctx, INT64), fresh("index"));
            gcc_assign(*block, loc, next_index, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
            gcc_assign(*block, loc, gcc_lvalue_access_field(
                    cycle_checker, loc, gcc_get_field(gcc_type_if_struct(hashmap_gcc_t), TABLE_DEFAULT_FIELD)),
                gcc_lvalue_address(next_index, loc));

            gcc_type_t *void_star = gcc_type(env->ctx, VOID_PTR);
            gcc_rvalue_t *print_call = gcc_callx(
                env->ctx, chunk_loc, print_fn, 
                compile_expr(env, block, interp_value),
                file,
                gcc_cast(env->ctx, loc, gcc_lvalue_address(cycle_checker, loc), void_star),
                gcc_rvalue_bool(env->ctx, interp->colorize));
            assert(print_call);
            gcc_eval(*block, chunk_loc, print_call);
        }
        gcc_eval(*block, loc, gcc_callx(env->ctx, loc, fflush_fn, file));
        gcc_lvalue_t *str_struct_var = gcc_local(func, loc, gcc_t, fresh("str_final"));
        gcc_rvalue_t *len32 = gcc_cast(env->ctx, loc, gcc_rval(size_var), i32_t);

        gcc_func_t *alloc_fn = hget(env->global_funcs, "GC_malloc_atomic", gcc_func_t*);
        gcc_rvalue_t *size = gcc_rval(size_var);
        gcc_rvalue_t *str = gcc_callx(
            env->ctx, loc, alloc_fn,
            gcc_binary_op(env->ctx, loc, GCC_BINOP_PLUS, gcc_type(env->ctx, SIZE),
                          size, gcc_one(env->ctx, gcc_type(env->ctx, SIZE))));
        gcc_func_t *memcpy_fn = hget(env->global_funcs, "memcpy", gcc_func_t*);
        str = gcc_callx(env->ctx, loc, memcpy_fn, str, gcc_rval(buf_var), size);
        str = gcc_cast(env->ctx, loc, str, gcc_type(env->ctx, STRING));
        gcc_assign(*block, loc, str_struct_var, STRING_STRUCT(env, gcc_t, str, len32, gcc_one(env->ctx, i32_t)));
        gcc_eval(*block, loc, gcc_callx(env->ctx, loc, fclose_fn, file));
        gcc_eval(*block, loc, gcc_callx(env->ctx, loc, free_fn, gcc_rval(buf_var)));
        return gcc_rval(str_struct_var);
    }
    case Array: {
        return compile_array(env, block, ast);
    }
    case Table: {
        return compile_table(env, block, ast);
    }
    case TableEntry: {
        auto entry = Match(ast, TableEntry);
        bl_type_t *key_t = get_type(env, entry->key);
        bl_type_t *value_t = get_type(env, entry->value);
        bl_type_t *entry_t = Type(StructType, .field_names=LIST(const char*, "key", "value"),
                                  .field_types=LIST(bl_type_t*, key_t, value_t));
        gcc_type_t *entry_gcc_t = bl_type_to_gcc(env, entry_t);
        gcc_field_t *fields[] = {
            gcc_get_field(gcc_type_if_struct(entry_gcc_t), 0),
            gcc_get_field(gcc_type_if_struct(entry_gcc_t), 1),
        };

        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *key_lval = gcc_local(func, loc, bl_type_to_gcc(env, key_t), fresh("key"));
        gcc_assign(*block, loc, key_lval, compile_expr(env, block, entry->key));
        flatten_arrays(env, block, key_t, gcc_lvalue_address(key_lval, loc));
        gcc_rvalue_t *rvals[] = {
            gcc_rval(key_lval),
            compile_expr(env, block, entry->value),
        };
        return gcc_struct_constructor(env->ctx, loc, bl_type_to_gcc(env, entry_t), 2, fields, rvals);
    }
    case TaggedUnionDef: {
        return NULL;
    }
    case UnitDef: {
        return NULL;
    }
    case ConvertDef: {
        auto convert = Match(ast, ConvertDef);
        bl_type_t *src_t = parse_type_ast(env, convert->source_type);
        bl_type_t *target_t = parse_type_ast(env, convert->target_type);

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
        bl_hashmap_t *ns = get_namespace(env, target_t);
        hset(ns, name, new(binding_t, .type=Type(FunctionType, .arg_types=LIST(bl_type_t*, src_t), .ret=target_t),
                           .func=func, .rval=gcc_get_func_address(func, NULL), .visible_in_closures=true));
        return NULL;
    }
    case StructDef: case Extend: {
        bl_type_t *t;
        List(ast_t*) members;
        if (ast->tag == StructDef) {
            auto struct_def = Match(ast, StructDef);
            members = struct_def->definitions;
            binding_t *b = get_binding(env, struct_def->name);
            if (!b) compiler_err(env, ast, "I couldn't find the type for this struct name");
            t = Match(b->type, TypeType)->type;
        } else {
            auto extend = Match(ast, Extend);
            members = Match(extend->body, Block)->statements;
            t = parse_type_ast(env, extend->type);
        }
        env = get_type_env(env, t);
        foreach (members, member, _) {
            if ((*member)->tag == Declare) {
                auto decl = Match((*member), Declare);
                gcc_rvalue_t *rval = compile_expr(env, block, decl->value);
                bl_type_t *t = get_type(env, decl->value);
                assert(t);
                gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
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
        bl_type_t *t;
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

        size_t num_values = length(struct_->members);

        auto struct_type = Match(t, StructType);
        if (length(struct_type->field_names) == 0) {
            // GCC doesn't allow empty constructors for empty structs, but for
            // some reason, it's perfectly fine to just declare an empty struct
            // variable and use that as an rvalue.
            gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
            gcc_lvalue_t *lval = gcc_local(gcc_block_func(*block), loc, gcc_t, fresh("empty_singleton"));
            return gcc_rval(lval);
        }
        if (length(struct_->members) > length(struct_type->field_names))
            compiler_err(env, ast, "I expected this %s literal to only have %ld fields, but you provided %ld fields.",
                  type_to_string(t),
                  length(struct_type->field_names), length(struct_->members));

        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
        size_t num_fields = gcc_field_count(gcc_struct);
        assert(num_fields == (size_t)length(struct_type->field_names));
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
        for (size_t field_index = 0; field_index < num_fields; field_index++) {
            bl_type_t *ft = ith(struct_type->field_types, field_index);
            ast_t *def = struct_type->field_defaults ? ith(struct_type->field_defaults, field_index) : NULL;
            if (def != NULL) {
                entries[num_values].field = unused_fields[field_index];
                entries[num_values].field_num = field_index;
                entries[num_values].ast = def;
                gcc_rvalue_t *def_val = compile_expr(env, block, def);
                if (!promote(env, get_type(env, def), &def_val, ft))
                    compiler_err(env, def, "I couldn't make this default value work as a %s",
                                type_to_string(ft));
                entries[num_values].value = def_val;
                unused_fields[field_index] = NULL;
                ++num_values;
                continue;
            }

            if (!can_leave_uninitialized(ft) && unused_fields[field_index]) {
                compiler_err(env, ast, "%s structs are supposed to have a non-optional field '%s' (%s), but you didn't provide a value for it.",
                      type_to_string(t),
                      ith(struct_type->field_names, field_index), type_to_string(ft));
            }
        }

        // GCC is dumb and requires sorting the fields:
        qsort_r(entries, num_values, sizeof(entries[0]), (int(*)(const void*,const void*,void*))(void*)memcmp, (void*)sizeof(size_t));
        gcc_field_t *populated_fields[num_values];
        gcc_rvalue_t *rvalues[num_values];
        for (size_t i = 0; i < num_values; i++) {
            // Check type:
            bl_type_t *expected = ith(struct_type->field_types, entries[i].field_num);
            bl_type_t *actual = get_type(env, entries[i].ast);
            rvalues[i] = entries[i].value;
            if (!promote(env, actual, &rvalues[i], expected))
                compiler_err(env, entries[i].ast, "I was expecting a value of type %s for the %s.%s field, but this value is a %s.", 
                      type_to_string(expected), type_to_string(t), ith(struct_type->field_names, entries[i].field_num),
                      type_to_string(actual));

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
        bl_type_t *fn_bl_t = get_type(env, call->fn);
        if (fn_bl_t->tag != FunctionType)
            compiler_err(env, call->fn, "This is not a callable function (it's a %s)", type_to_string(fn_bl_t));
        auto fn_t = Match(fn_bl_t, FunctionType);

        int64_t num_args = length(fn_t->arg_types);
        gcc_rvalue_t **arg_vals = GC_MALLOC(sizeof(gcc_rvalue_t*)*num_args);
        // method calls:
        if (call->fn->tag == FieldAccess) { // method call (foo.method())
            auto access = Match(call->fn, FieldAccess);
            ast_t *self = access->fielded;
            bl_type_t *self_t = get_type(env, self);
            if (streq(access->field, "__hash"))
                (void)get_hash_func(env, self_t); 
            else if (streq(access->field, "__compare"))
                (void)get_compare_func(env, self_t); 
            else if (streq(access->field, "__print"))
                (void)get_print_func(env, self_t); 
            bl_type_t *value_type = self_t;
            while (value_type->tag == PointerType)
                value_type = Match(value_type, PointerType)->pointed;
            switch (value_type->tag) {
            case ModuleType: goto non_method_fncall;
            case TypeType: {
                if (access->fielded->tag != Var)
                    compiler_err(env, call->fn, "I only know how to access type members by referencing the type directly like foo.baz()");
                bl_type_t *fielded_type = Match(value_type, TypeType)->type;
                binding_t *binding = get_from_namespace(env, fielded_type, access->field);
                if (!binding)
                    compiler_err(env, call->fn, "I couldn't find any method called %s for %s.", access->field, type_to_string(fielded_type));
                fn = binding->func;
                fn_ptr = binding->rval;
                break;
            }
            default: {
                if (value_type->tag == ArrayType)
                    define_array_methods(env, value_type);

                binding_t *binding = get_from_namespace(env, self_t, access->field);
                if (!binding)
                    binding = get_from_namespace(env, value_type, access->field);
                if (!binding)
                    goto non_method_fncall;
                if (binding->type->tag != FunctionType)
                    compiler_err(env, call->fn, "This value isn't a function, it's a %s", type_to_string(binding->type));
                auto fn_info = Match(binding->type, FunctionType);
                if (length(fn_info->arg_types) < 1)
                    compiler_err(env, call->fn, "This function doesn't take any arguments. If you want to call it anyways, use the class name like %s.%s()",
                          type_to_string(value_type), access->field);

                gcc_rvalue_t *self_val = compile_expr(env, block, self);
                bl_type_t *expected_self = ith(fn_info->arg_types, 0);
                if (!type_eq(self_t, expected_self) && !promote(env, self_t, &self_val, expected_self))
                    compiler_err(env, ast, "The method %s.%s(...) is being called on a %s, but it wants a %s.",
                          type_to_string(self_t), access->field, type_to_string(self_t), type_to_string(expected_self));
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
        bl_hashmap_t arg_positions = {0};
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
            gcc_rvalue_t *val = compile_expr(env, block, kwarg->arg);
            bl_type_t *actual = get_type(env, *arg);
            bl_type_t *expected = ith(fn_t->arg_types, arg_index);
            if (!promote(env, actual, &val, expected))
                compiler_err(env, *arg, "This function expected this argument to have type %s, but this value is a %s",
                      type_to_string(expected), type_to_string(actual));
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
            gcc_rvalue_t *val = compile_expr(env, block, arg);
            bl_type_t *actual = get_type(env, arg);
            bl_type_t *expected = ith(fn_t->arg_types, pos);
            if (!promote(env, actual, &val, expected))
                compiler_err(env, arg, "This function expected this argument to have type %s, but this value is a %s",
                      type_to_string(expected), type_to_string(actual));
            arg_vals[pos] = val;
        }

        env_t default_arg_env = fn_t->env ? *(env_t*)fn_t->env : *global_scope(env);
        default_arg_env.bindings = new(bl_hashmap_t, .fallback=default_arg_env.bindings);
        gcc_func_t *func = gcc_block_func(*block);

        if (fn_t->arg_names && fn_t->arg_defaults) {
            // Stash args in local variables so we don't evaluate them multiple times (e.g. def foo(x:Int,y=x)... foo(Int.random()))
            for (int64_t i = 0; i < num_args; i++) {
                if (arg_vals[i]) {
                    const char *arg_name = ith(fn_t->arg_names, i);
                    gcc_lvalue_t *tmp = gcc_local(func, loc, bl_type_to_gcc(env, ith(fn_t->arg_types, i)), fresh(arg_name ? arg_name : "arg"));
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
                    gcc_lvalue_t *tmp = gcc_local(func, loc, bl_type_to_gcc(env, ith(fn_t->arg_types, pos)), fresh(arg_name ? arg_name : "arg"));
                    gcc_assign(*block, loc, tmp, compile_expr(&default_arg_env, block, default_val));
                    arg_vals[pos] = gcc_rval(tmp);
                    hset(default_arg_env.bindings, ith(fn_t->arg_names, pos), new(binding_t, .type=ith(fn_t->arg_types, pos), .rval=arg_vals[pos]));
                    continue;
                }
            }
            bl_type_t *arg_t = ith(fn_t->arg_types, pos);
            if (arg_t->tag != PointerType || !Match(arg_t, PointerType)->is_optional) {
                const char *arg_name = fn_t->arg_names ? ith(fn_t->arg_names, pos) : NULL;
                compiler_err(env, ast, "The non-optional argument %s was not provided",
                      arg_name ? arg_name : heap_strf("%ld", pos));
            }
            arg_vals[pos] = gcc_null(env->ctx, bl_type_to_gcc(env, arg_t));
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
    case HeapAllocate: {
        ast_t *value = Match(ast, HeapAllocate)->value;
        gcc_rvalue_t *rval = compile_expr(env, block, value);
        bl_type_t *t = get_type(env, value);
        if (t->tag == VoidType)
            compiler_err(env, value, "This expression is a Void type, which can't be heap allocated");
        gcc_func_t *func = gcc_block_func(*block);
        ssize_t gcc_size = gcc_sizeof(env, t);
        if (t->tag == ArrayType)
            gcc_size += 4; // Hidden "capacity" field
        gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), gcc_size);
        gcc_type_t *gcc_t = gcc_get_ptr_type(bl_type_to_gcc(env, t));
        gcc_lvalue_t *tmp = gcc_local(func, loc, gcc_t, fresh(heap_strf("heap_%s", type_to_string(t))));
        gcc_func_t *alloc_func = get_function(env, has_heap_memory(t) ? "GC_malloc" : "GC_malloc_atomic");
        gcc_assign(*block, loc, tmp, gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, size), gcc_t));
        gcc_assign(*block, loc, gcc_rvalue_dereference(gcc_rval(tmp), loc), rval);
        return gcc_rval(tmp);
    }
    case Dereference: {
        bl_type_t *t = get_type(env, ast); // Check this is a pointer type
        ast_t *value = Match(ast, Dereference)->value;
        gcc_rvalue_t *obj = compile_expr(env, block, value);
        if (t->tag == ArrayType)
            mark_array_cow(env, block, obj);
        else if (t->tag == TableType)
            gcc_eval(*block, NULL, gcc_callx(env->ctx, NULL, get_function(env, "bl_hashmap_mark_cow"), obj));
        return gcc_rval(gcc_rvalue_dereference(obj, loc));
    }
    case Maybe: {
        ast_t *value = Match(ast, Maybe)->value;
        return compile_expr(env, block, value);
    }
    case Len: {
        ast_t *value = Match(ast, Len)->value;
        bl_type_t *t = get_type(env, value);
        gcc_rvalue_t *obj = compile_expr(env, block, value);
        gcc_rvalue_t *len = compile_len(env, block, t, obj);
        if (!len)
            compiler_err(env, ast, "I don't know how to get the length of a %s", type_to_string(t));
        return len;
    }
    case FieldAccess: {
        (void)get_type(env, ast); // typecheck
        auto access = Match(ast, FieldAccess);
        bl_type_t *fielded_t = get_type(env, access->fielded);
        gcc_rvalue_t *obj = compile_expr(env, block, access->fielded);
      get_field:
        switch (fielded_t->tag) {
        case PointerType: {
            auto ptr = Match(fielded_t, PointerType);
            if (ptr->is_optional)
                compiler_err(env, ast, "This field access is unsafe because the value may be nil");
            if (ptr->pointed->tag == ArrayType)
                mark_array_cow(env, block, obj);
            else if (ptr->pointed->tag == TableType)
                gcc_eval(*block, NULL, gcc_callx(env->ctx, NULL, get_function(env, "bl_hashmap_mark_cow"), obj));
            obj = gcc_rval(gcc_rvalue_dereference(obj, loc));
            fielded_t = ptr->pointed;
            goto get_field;
        }
        case StructType: {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, fielded_t);
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
        case ArrayType: { // [Vec{1,2}, Vec{3,4}].x ==> [1,3]
            auto array = Match(fielded_t, ArrayType);
            if (array->item_type->tag != StructType)
                break; // TODO: support pointers?

            gcc_type_t *array_gcc_t = bl_type_to_gcc(env, fielded_t);
            gcc_struct_t *gcc_array_struct = gcc_type_if_struct(array_gcc_t);

            gcc_type_t *item_gcc_t = bl_type_to_gcc(env, array->item_type);
            gcc_struct_t *gcc_item_struct = gcc_type_if_struct(item_gcc_t);

            auto struct_type = Match(array->item_type, StructType);
            for (int64_t i = 0, len = length(struct_type->field_names); i < len; i++) {
                if (!streq(ith(struct_type->field_names, i), access->field))  continue;
                bl_type_t *field_type = ith(struct_type->field_types, i);

                // items = &(array.items->field)
                gcc_field_t *items_field = gcc_get_field(gcc_array_struct, 0);
                gcc_rvalue_t *items = gcc_rvalue_access_field(obj, loc, items_field);
                gcc_field_t *struct_field = gcc_get_field(gcc_item_struct, (size_t)i);
                gcc_lvalue_t *field = gcc_rvalue_dereference_field(items, loc, struct_field);
                gcc_rvalue_t *field_addr = gcc_lvalue_address(field, loc);

                // length = array->length
                gcc_rvalue_t *len = gcc_rvalue_access_field(obj, loc, gcc_get_field(gcc_array_struct, 1));

                // stride = array->stride * sizeof(array->items[0]) / sizeof(array->items[0].field)
                gcc_rvalue_t *stride = gcc_rvalue_access_field(obj, loc, gcc_get_field(gcc_array_struct, 2));
                stride = gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, gcc_type(env->ctx, INT32), stride,
                                       gcc_rvalue_int32(env->ctx, gcc_sizeof(env, array->item_type)));
                stride = gcc_binary_op(env->ctx, loc, GCC_BINOP_DIVIDE, gcc_type(env->ctx, INT32), stride,
                                       gcc_rvalue_int32(env->ctx, gcc_sizeof(env, field_type)));

                gcc_type_t *slice_gcc_t = bl_type_to_gcc(env, Type(ArrayType, .item_type=field_type));
                gcc_struct_t *slice_struct = gcc_type_if_struct(slice_gcc_t);
                gcc_field_t *fields[3] = {
                    gcc_get_field(slice_struct, 0),
                    gcc_get_field(slice_struct, 1),
                    gcc_get_field(slice_struct, 2),
                };

                gcc_rvalue_t *rvals[3] = {
                    field_addr,
                    len,
                    stride,
                };

                return gcc_struct_constructor(env->ctx, loc, slice_gcc_t, 3, fields, rvals);
            }
            break;
        }
        case TypeType: {
            bl_type_t *t = Match(fielded_t, TypeType)->type;
            binding_t *val_binding = get_from_namespace(env, t, access->field);
            if (val_binding)
                return val_binding->rval;
            else
                compiler_err(env, ast, "I can't find any field or method called \"%s\" on the type %s.", access->field, type_to_string(t));
            break;
        }
        default: break;
        }
        // Class lookup:
        binding_t *binding = get_from_namespace(env, fielded_t, access->field);
        if (binding)
            return binding->rval;
        else {
            compiler_err(env, ast, "I can't find any field or method called \"%s\" on the type %s.", access->field, type_to_string(fielded_t));
        }
    }
    case Index: {
        auto indexing = Match(ast, Index);

        bl_type_t *t = get_type(env, indexing->indexed);
        while (t->tag == PointerType)
            t = Match(t, PointerType)->pointed;

        if (t->tag == ArrayType) {
            return gcc_rval(array_index(env, block, indexing->indexed, indexing->index, indexing->type, ACCESS_READ));
        } else if (t->tag == TableType) {
            gcc_rvalue_t *val_opt = table_lookup_optional(env, block, indexing->indexed, indexing->index);
            if (indexing->type == INDEX_UNCHECKED)
                return gcc_rval(gcc_rvalue_dereference(val_opt, loc));

            gcc_func_t *func = gcc_block_func(*block);
            gcc_type_t *gcc_value_t = bl_type_to_gcc(env, Match(t, TableType)->value_type);
            gcc_lvalue_t *value_var = gcc_local(func, loc, gcc_value_t, fresh("value"));
            gcc_block_t *if_nil = gcc_new_block(func, fresh("if_nil")),
                        *if_nonnil = gcc_new_block(func, fresh("if_nonnil")),
                        *done = gcc_new_block(func, fresh("if_nonnil"));
            gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, val_opt, gcc_null(env->ctx, gcc_get_ptr_type(gcc_value_t))),
                               if_nil, if_nonnil);
            *block = if_nil;

            if (indexing->type == INDEX_NORMAL && env->loop_label) {
                gcc_block_t *skip_dest = env->loop_label->skip_label;
                insert_defers(env, block, env->loop_label->deferred);
                gcc_jump(*block, loc, skip_dest);
            } else {
                // ast_t *str_ast = WrapAST(indexing->index, StringJoin, .children=LIST(ast_t*, WrapAST(indexing->index, Interp, .value=indexing->index)));
                // insert_failure(env, block, ast->span, "Error: this table does not have the given key: %#s", NULL, compile_expr(env, block, str_ast));
                insert_failure(env, block, ast->span, "Error: this table does not have the given key");
                gcc_jump(*block, loc, done);
            }

            *block = if_nonnil;
            gcc_assign(*block, loc, value_var, gcc_rval(gcc_rvalue_dereference(val_opt, loc)));
            gcc_jump(*block, loc, done);

            *block = done;
            return gcc_rval(value_var);
        } else {
            compiler_err(env, ast, "I only know how to index Arrays and Tables, not %s", type_to_string(t));
        }
    }
    case In: {
        auto in = Match(ast, In);
        bl_type_t *member_t = get_type(env, in->member);
        bl_type_t *container_t = get_type(env, in->container);

        while (container_t->tag == PointerType)
            container_t = Match(container_t, PointerType)->pointed;

        if (container_t->tag == TableType) {
            if (!type_is_a(member_t, Match(container_t, TableType)->key_type))
                compiler_err(env, ast, "This is checking for the presence of a key with type %s, but the table has type %s",
                            type_to_string(member_t), type_to_string(container_t));

            gcc_rvalue_t *val_opt = table_lookup_optional(env, block, in->container, in->member);
            return gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, val_opt, gcc_null(env->ctx, gcc_get_ptr_type(bl_type_to_gcc(env, member_t))));
        } else if (container_t->tag == ArrayType) {
            return array_contains(env, block, in->container, in->member);
        } else {
            compiler_err(env, ast, "'in' membership testing is only supported for Tables, not %s", type_to_string(container_t));
        }
    }
    case TypeOf: {
        auto value = Match(ast, TypeOf)->value;
        bl_type_t *t = get_type(env, value);
        return gcc_str(env->ctx, type_to_string(t));
    }
    case SizeOf: {
        auto value = Match(ast, SizeOf)->value;
        bl_type_t *t = get_type(env, value);
        ssize_t size = gcc_sizeof(env, t);
        return gcc_int64(env->ctx, size);
    }
    case Cast: {
        auto cast = Match(ast, Cast);
        gcc_rvalue_t *val = compile_expr(env, block, cast->value);
        bl_type_t *src_t = get_type(env, cast->value);
        bl_type_t *cast_t = get_type(env, ast);

        binding_t *convert_b = get_from_namespace(env, cast_t, heap_strf("#convert-from:%s", type_to_string(src_t)));
        if (convert_b)
            return gcc_callx(env->ctx, loc, convert_b->func, val);

        if (!((is_numeric(src_t) && is_numeric(cast_t))
              || (is_numeric(src_t) && (cast_t->tag == BoolType || cast_t->tag == CharType))
              || (is_numeric(cast_t) && (src_t->tag == BoolType || src_t->tag == CharType))))
            compiler_err(env, ast, "I don't know how to convert %s to %s. "
                        "You should implement a `def x:%s->%s` conversion function or use 'bitcast'",
                        type_to_string(src_t), type_to_string(cast_t),
                        type_to_string(src_t), type_to_string(cast_t));
        return gcc_cast(env->ctx, loc, val, bl_type_to_gcc(env, cast_t));
    }
    case Bitcast: {
        auto bitcast = Match(ast, Bitcast);
        gcc_rvalue_t *val = compile_expr(env, block, bitcast->value);
        bl_type_t *t = get_type(env, ast);
        return gcc_bitcast(env->ctx, loc, val, bl_type_to_gcc(env, t));
    }
    case Nil: {
        bl_type_t *t = get_type(env, ast);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        if (gcc_type_if_pointer(gcc_t))
            return gcc_null(env->ctx, bl_type_to_gcc(env, t));
        else
            return gcc_zero(env->ctx, bl_type_to_gcc(env, t));
    }
    case Not: {
        auto value = Match(ast, Not)->value;
        bl_type_t *t = get_type(env, value);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_rvalue_t *val = compile_expr(env, block, value);
        if (t->tag == BoolType)
            return gcc_unary_op(env->ctx, ast_loc(env, ast), GCC_UNOP_LOGICAL_NEGATE, gcc_t, val);
        else if (is_integral(t))
            return gcc_unary_op(env->ctx, ast_loc(env, ast), GCC_UNOP_BITWISE_NEGATE, gcc_t, val);
        else if (t->tag == PointerType && Match(t, PointerType)->is_optional)
            return gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, val, gcc_null(env->ctx, gcc_t));
        else
            compiler_err(env, ast, "The 'not' operator isn't supported for values with type %s.", type_to_string(t));
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

        bl_type_t *lhs_t = get_type(env, lhs);
        bl_type_t *rhs_t = get_type(env, rhs);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);

        if (!promote(env, lhs_t, &lhs_val, rhs_t)
            && !promote(env, rhs_t, &rhs_val, lhs_t))
            compiler_err(env, ast, "I don't know how to do a comparison between a %s and a %s.", type_to_string(lhs_t), type_to_string(rhs_t));

        if (is_ordered && !is_orderable(lhs_t))
            compiler_err(env, ast, "I can't do ordered comparisons between values with type %s",
                  type_to_string(lhs_t));

        if (is_numeric(lhs_t) || lhs_t->tag == PointerType)
            return gcc_comparison(env->ctx, loc, cmp, lhs_val, rhs_val);

        return gcc_comparison(env->ctx, loc, cmp, compare_values(env, lhs_t, lhs_val, rhs_val),
                              gcc_zero(env->ctx, gcc_type(env->ctx, INT)));
    }
    case Negative: {
        ast_t *value = Match(ast, Negative)->value;
        bl_type_t *t = get_type(env, value);
        if (!is_numeric(t))
            compiler_err(env, ast, "I only know how to negate numbers, not %s", type_to_string(t));
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_rvalue_t *rval = compile_expr(env, block, value);
        return gcc_unary_op(env->ctx, loc, GCC_UNOP_MINUS, gcc_t, rval);
    }
    case And: {
        ast_t *lhs = Match(ast, And)->lhs,
              *rhs = Match(ast, And)->rhs;
        bl_type_t *t = get_type(env, ast);
        bl_type_t *lhs_t = get_type(env, lhs);
        bl_type_t *rhs_t = get_type(env, rhs);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *result = gcc_local(func, loc, bl_type_to_gcc(env, t), fresh("and_result"));
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        gcc_assign(*block, NULL, result, lhs_val);
        lhs_val = gcc_rval(result);
        if (t->tag == BoolType && rhs_t->tag == BoolType) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_LOGICAL_AND, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        } else if (is_integral(lhs_t) && is_integral(rhs_t)) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
            // Numeric promotion:
            if (!promote(env, lhs_t, &lhs_val, rhs_t))
                assert(promote(env, rhs_t, &rhs_val, lhs_t));
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_BITWISE_AND, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        }
        gcc_block_t *if_truthy = gcc_new_block(func, fresh("and_truthy"));
        gcc_block_t *done = gcc_new_block(func, fresh("and_done"));

        gcc_type_t *lhs_gcc_t = bl_type_to_gcc(env, lhs_t);
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
        bl_type_t *t = get_type(env, ast);
        bl_type_t *lhs_t = get_type(env, lhs);
        bl_type_t *rhs_t = get_type(env, rhs);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *result = gcc_local(func, loc, bl_type_to_gcc(env, t), fresh("and_result"));
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        gcc_assign(*block, loc, result, lhs_val);
        lhs_val = gcc_rval(result);
        if (t->tag == BoolType && rhs_t->tag == BoolType) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_LOGICAL_OR, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        } else if (is_integral(lhs_t) && is_integral(rhs_t)) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
            if (!promote(env, lhs_t, &lhs_val, rhs_t)
                && !promote(env, rhs_t, &rhs_val, lhs_t))
                compiler_err(env, ast, "I can't figure out how to combine a %s and a %s", type_to_string(lhs_t), type_to_string(rhs_t));
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_BITWISE_OR, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        }
        gcc_block_t *if_falsey = gcc_new_block(func, fresh("or_falsey"));
        gcc_block_t *done = gcc_new_block(func, fresh("or_done"));

        gcc_type_t *lhs_gcc_t = bl_type_to_gcc(env, lhs_t);
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
        ast_t *lhs = Match(ast, Xor)->lhs, *rhs = Match(ast, Xor)->rhs;
        bl_type_t *t = get_type(env, ast);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
        return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_BITWISE_XOR, bl_type_to_gcc(env, t), lhs_val, rhs_val);
    }
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate: {
        return math_update(env, block, ast);
    }
    case Add: case Subtract: case Divide: case Multiply: {
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
        bl_type_t *t_lhs = get_type(env, concat->lhs);
        bl_type_t *array_t = t_lhs;
        while (array_t->tag == PointerType) array_t = Match(array_t, PointerType)->pointed;
        define_array_methods(env, array_t);
        binding_t *b = get_from_namespace(env, array_t, "insert_all");
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
    case Modulus: {
        ast_t *lhs = Match(ast, Modulus)->lhs, *rhs = Match(ast, Modulus)->rhs;
        bl_type_t *t = get_type(env, ast);
        bl_type_t *lhs_t = get_type(env, lhs);
        bl_type_t *rhs_t = get_type(env, rhs);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
        if (!promote(env, lhs_t, &lhs_val, rhs_t)
            && !promote(env, rhs_t, &rhs_val, lhs_t))
            compiler_err(env, ast, "The left hand side of this modulus has type %s, but the right hand side has type %s and I can't figure out how to combine them.",
                  type_to_string(lhs_t), type_to_string(rhs_t));
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
            return gcc_callx(env->ctx, loc, sane_fmod_func, lhs_val, rhs_val);
        } else {
            gcc_rvalue_t *result = gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_MODULO, bl_type_to_gcc(env, t), lhs_val, rhs_val);
            // Ensure modulus result is positive (i.e. (-1 mod 10) == 9)
            result = gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_PLUS, bl_type_to_gcc(env, t), result, rhs_val);
            result = gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_MODULO, bl_type_to_gcc(env, t), result, rhs_val);
            return result;
        }
    }
    case Power: {
        ast_t *base = Match(ast, Power)->lhs, *exponent = Match(ast, Power)->rhs;
        bl_type_t *t = get_type(env, ast);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        bl_type_t *base_t = get_type(env, base);
        bl_type_t *rhs_t = get_type(env, exponent);
        gcc_rvalue_t *base_val = compile_expr(env, block, base);
        gcc_rvalue_t *exponent_val = compile_expr(env, block, exponent);
        if (!promote(env, base_t, &base_val, rhs_t)
            && !promote(env, rhs_t, &exponent_val, base_t))
            compiler_err(env, ast, "The base of this operation has type %s, but the exponent has type %s and I can't figure out how to combine them.",
                  type_to_string(base_t), type_to_string(rhs_t));

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
        bl_type_t *if_t = get_type(env, ast);
        auto if_ = Match(ast, If);
        bool has_value = !(if_t->tag == GeneratorType || if_t->tag == AbortType || if_t->tag == VoidType);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *if_ret = has_value ? gcc_local(func, loc, bl_type_to_gcc(env, if_t), fresh("if_value")) : NULL;

        gcc_block_t *end_if = if_t->tag == AbortType ? NULL : gcc_new_block(func, fresh("endif"));

        ast_t *condition = if_->condition;
        ast_t *body = if_->body;
        gcc_block_t *if_truthy = gcc_new_block(func, fresh("if_true"));
        gcc_block_t *if_falsey = gcc_new_block(func, fresh("else"));

        env_t *branch_env = fresh_scope(env);
        check_truthiness(branch_env, block, condition, if_truthy, if_falsey);
        *block = NULL;

        if (if_t->tag == GeneratorType && env->comprehension_callback) {
            if (!env->comprehension_callback)
                compiler_err(env, ast, "This 'if' is being used as a value, but it might not have a value");
            env->comprehension_callback(branch_env, &if_truthy, body, env->comprehension_userdata);
            assert(end_if);
            if (if_truthy)
                gcc_jump(if_truthy, loc, end_if);
        } else {
            gcc_rvalue_t *branch_val = compile_expr(branch_env, &if_truthy, body);
            if (if_truthy) {
                if (branch_val) {
                    if (has_value) {
                        bl_type_t *actual = get_type(env, body);
                        if (!promote(env, actual, &branch_val, if_t))
                            compiler_err(env, body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
                                  type_to_string(if_t), type_to_string(actual));
                        gcc_assign(if_truthy, loc, if_ret, branch_val);
                    } else {
                        gcc_eval(if_truthy, loc, branch_val);
                    }
                }
                assert(end_if);
                gcc_jump(if_truthy, loc, end_if);
            }
        }

        *block = if_falsey;
        if (if_->else_body) {
            gcc_rvalue_t *branch_val = compile_expr(env, block, if_->else_body);
            if (branch_val) {
                if (has_value) {
                    assert(*block);
                    bl_type_t *actual = get_type(env, if_->else_body);
                    if (!promote(env, actual, &branch_val, if_t))
                        compiler_err(env, if_->else_body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
                              type_to_string(if_t), type_to_string(actual));
                    gcc_assign(*block, loc, if_ret, branch_val);
                } else if (*block) {
                    gcc_eval(*block, loc, branch_val);
                }
            }
        }

        if (*block) {
            assert(end_if);
            gcc_jump(*block, loc, end_if);
        }
        *block = end_if;
        return has_value ? gcc_rval(if_ret) : NULL;
    }
    case When: {
        auto when = Match(ast, When);
        gcc_rvalue_t *subject = compile_expr(env, block, when->subject);
        bl_type_t *subject_t = get_type(env, when->subject);
        while (subject_t->tag == PointerType) {
            auto ptr = Match(subject_t, PointerType);
            if (ptr->is_optional)
                compiler_err(env, when->subject, "This value may be nil, so it can't be safely dereferenced");
            subject_t = ptr->pointed;
            subject = gcc_rval(gcc_rvalue_dereference(subject, ast_loc(env, when->subject)));
        }
        if (subject_t->tag != TaggedUnionType)
            compiler_err(env, when->subject, "'when' blocks must use enums, but this is a %s", type_to_string(subject_t));
        gcc_type_t *gcc_t = bl_type_to_gcc(env, subject_t);

        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *subject_var = gcc_local(func, loc, gcc_t, fresh("when_subject"));
        gcc_assign(*block, loc, subject_var, subject);
        subject = gcc_rval(subject_var);

        bl_type_t *result_t = get_type(env, ast);
        bool has_value = !(result_t->tag == GeneratorType || result_t->tag == AbortType || result_t->tag == VoidType);
        gcc_lvalue_t *when_value = has_value ? gcc_local(func, loc, bl_type_to_gcc(env, result_t), fresh("when_value")) : NULL;
        gcc_block_t *end_when = result_t->tag == AbortType ? NULL : gcc_new_block(func, fresh("endif"));

        gcc_type_t *gcc_union_t = get_union_type(env, subject_t);

        auto tagged = Match(subject_t, TaggedUnionType);
        NEW_LIST(gcc_case_t*, gcc_cases);
        foreach (when->cases, case_, _) {
            const char* tag_name = Match(case_->tag, Var)->name;
            gcc_loc_t *case_loc = ast_loc(env, case_->tag);
            env_t *case_env = env;
            int64_t tag_value = 0;
            if (case_->var) {
                case_env = fresh_scope(env);

                gcc_rvalue_t *case_rval = NULL;
                bl_type_t *case_t = NULL;
                for (int64_t i = 0, len = length(tagged->members); i < len; i++) {
                    auto member = ith(tagged->members, i);
                    if (streq(member.name, tag_name)) {
                        if (!member.type)
                            compiler_err(env, case_->var, "This tagged union tag has no value, so it can't be bound to anything");
                        gcc_rvalue_t *data = gcc_rvalue_access_field(subject, case_loc, gcc_get_field(gcc_type_if_struct(gcc_t), 1));
                        case_rval = gcc_rvalue_access_field(data, case_loc, gcc_get_union_field(gcc_union_t, i));
                        case_t = member.type;
                        tag_value = member.tag_value;
                        goto found_tag;
                    }
                }
                compiler_err(env, case_->var, "I couldn't find a tag with this name");

              found_tag:
                binding_t *case_binding = new(binding_t, .type=case_t, .rval=case_rval);
                hset(case_env->bindings, Match(case_->var, Var)->name, case_binding);
            } else {
                foreach (tagged->members, member, _) {
                    if (streq(member->name, tag_name)) {
                        tag_value = member->tag_value;
                        goto found_tag_without_value;
                    }
                }
                compiler_err(env, case_->var, "I couldn't find a tag with this name");
              found_tag_without_value:;
            }

            gcc_block_t *case_block = gcc_new_block(func, fresh(heap_strf("case_%s", tag_name)));
            gcc_rvalue_t *tag_rval = gcc_rvalue_from_long(env->ctx, get_tag_type(env, subject_t), tag_value);
            gcc_case_t *gcc_case = gcc_new_case(env->ctx, tag_rval, tag_rval, case_block);
            APPEND(gcc_cases, gcc_case);

            gcc_rvalue_t *branch_val = compile_expr(case_env, &case_block, case_->body);
            if (branch_val) {
                if (has_value) {
                    bl_type_t *actual = get_type(case_env, case_->body);
                    if (!promote(case_env, actual, &branch_val, result_t))
                        compiler_err(case_env, case_->body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
                              type_to_string(result_t), type_to_string(actual));
                    gcc_assign(case_block, case_loc, when_value, branch_val);
                } else {
                    gcc_eval(case_block, case_loc, branch_val);
                }
            }
            if (case_block)
                gcc_jump(case_block, case_loc, end_when);
        }
        gcc_block_t *default_block;
        if (when->default_body) {
            default_block = gcc_new_block(func, fresh("default"));
            gcc_block_t *end_of_default = default_block;
            gcc_rvalue_t *branch_val = compile_expr(env, &end_of_default, when->default_body);
            gcc_loc_t *else_loc = ast_loc(env, when->default_body);
            if (branch_val && end_of_default) {
                if (has_value) {
                    bl_type_t *actual = get_type(env, when->default_body);
                    if (!promote(env, actual, &branch_val, result_t))
                        compiler_err(env, when->default_body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
                              type_to_string(result_t), type_to_string(actual));
                    gcc_assign(end_of_default, else_loc, when_value, branch_val);
                } else {
                    gcc_eval(end_of_default, else_loc, branch_val);
                }
            }
            if (end_of_default)
                gcc_jump(end_of_default, else_loc, end_when);
        } else {
            default_block = end_when;
        }

        gcc_rvalue_t *tag_val = gcc_rvalue_access_field(subject, loc, gcc_get_field(gcc_type_if_struct(gcc_t), 0));
        gcc_switch(*block, loc, tag_val, default_block, length(gcc_cases), gcc_cases[0]);
        *block = end_when;
        return when_value ? gcc_rval(when_value) : NULL;
    }
    case Range: {
        return compile_range(env, block, ast);
    }
    case Ellipsis: return compile_range(env, block, WrapAST(ast, Range));
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
    case Min: case Max: {
        ast_t *lhs_ast, *rhs_ast;
        gcc_comparison_e cmp;
        if (ast->tag == Min) {
            cmp = GCC_COMPARISON_LE;
            lhs_ast = Match(ast, Min)->lhs;
            rhs_ast = Match(ast, Min)->rhs;
        } else {
            cmp = GCC_COMPARISON_GE;
            lhs_ast = Match(ast, Max)->lhs;
            rhs_ast = Match(ast, Max)->rhs;
        }
        gcc_func_t *func = gcc_block_func(*block);
        bl_type_t *t = get_type(env, ast);
        bl_type_t *lhs_t = get_type(env, lhs_ast),
                  *rhs_t = get_type(env, rhs_ast);
        gcc_lvalue_t *lhs = gcc_local(func, loc, bl_type_to_gcc(env, lhs_t), fresh("lhs")),
                     *rhs = gcc_local(func, loc, bl_type_to_gcc(env, rhs_t), fresh("rhs")),
                     *result = gcc_local(func, loc, bl_type_to_gcc(env, t), fresh("result"));

        gcc_assign(*block, loc, lhs, compile_expr(env, block, lhs_ast));
        gcc_assign(*block, loc, rhs, compile_expr(env, block, rhs_ast));

        gcc_rvalue_t *lhs_val = gcc_rval(lhs),
                     *rhs_val = gcc_rval(rhs);

        if (!promote(env, lhs_t, &lhs_val, rhs_t)
            && !promote(env, rhs_t, &rhs_val, lhs_t))
            compiler_err(env, ast, "I don't know how to do a comparison between a %s and a %s.", type_to_string(lhs_t), type_to_string(rhs_t));

        if (!is_orderable(lhs_t))
            compiler_err(env, ast, "I can't do ordered comparisons between values with type %s",
                  type_to_string(lhs_t));

        gcc_rvalue_t *should_choose_lhs;
        if (is_numeric(lhs_t) || lhs_t->tag == PointerType)
            should_choose_lhs = gcc_comparison(env->ctx, loc, cmp, lhs_val, rhs_val);
        else
            should_choose_lhs = gcc_comparison(env->ctx, loc, cmp, compare_values(env, lhs_t, lhs_val, rhs_val),
                                               gcc_zero(env->ctx, gcc_type(env->ctx, INT)));

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
    case Reduction: {
        auto reduction = Match(ast, Reduction);
        bl_type_t *t = get_type(env, ast);
        const char* name = fresh("reduction");
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *ret = gcc_local(func, loc, bl_type_to_gcc(env, t), name);

        env = fresh_scope(env);

        ast_t *prev_var = WrapAST(ast, Var, .name="x");
        ast_t *iter_var = WrapAST(ast, Var, .name="y");
        hset(env->bindings, "x", new(binding_t, .lval=ret, .rval=gcc_rval(ret), .type=t));
        if (reduction->fallback) {
            ast_t *body = WrapAST(reduction->combination, Assign, LIST(ast_t*, prev_var), LIST(ast_t*, reduction->combination));
            ast_t *for_ast = WrapAST(ast, For, .value=iter_var, .iter=reduction->iter, .body=body);
            for_ast = WrapAST(ast, Block, .statements=LIST(ast_t*, WrapAST(reduction->fallback, Assign, LIST(ast_t*, prev_var), LIST(ast_t*, reduction->fallback)), for_ast));
            compile_statement(env, block, for_ast);
        } else {
            ast_t *first = WrapAST(reduction->combination, Assign, LIST(ast_t*, prev_var), LIST(ast_t*, iter_var));
            ast_t *between = WrapAST(reduction->combination, Assign, LIST(ast_t*, prev_var), LIST(ast_t*, reduction->combination));
            ast_t *empty = WrapAST(reduction->iter, Fail, .message=StringAST(reduction->iter, "This collection was empty"));
            ast_t *for_ast = WrapAST(ast, For, .value=iter_var, .iter=reduction->iter, .first=first, .between=between, .empty=empty);
            compile_statement(env, block, for_ast);
        }

        return gcc_rval(ret);
    }
    case Fail: {
        if (env->is_deferred)
            compiler_err(env, ast, "This 'fail' is inside a 'defer' block, which is not allowed");

        ast_t *message = Match(ast, Fail)->message;
        gcc_rvalue_t *msg;
        gcc_rvalue_t *len;
        if (message) {
            msg = compile_expr(env, block, message);
            gcc_type_t *gcc_t = bl_type_to_gcc(env, Type(ArrayType, .item_type=Type(CharType)));
            gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
            len = gcc_rvalue_access_field(msg, loc, gcc_get_field(array_struct, 1));
            msg = gcc_rvalue_access_field(msg, loc, gcc_get_field(array_struct, 0));
        } else {
            const char *default_msg = "A failure occurred";
            msg = gcc_str(env->ctx, default_msg);
            len = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), strlen(default_msg));
        }

        gcc_rvalue_t *fmt = gcc_str(env->ctx, "\x1b[31;1;7m%s: %.*s\x1b[m\n\n%s");
        gcc_rvalue_t *loc_info = gcc_str(
            env->ctx, heap_strf("%s:%ld.%ld",
                                ast->span.file->relative_filename,
                                bl_get_line_number(ast->span.file, ast->span.start),
                                bl_get_line_column(ast->span.file, ast->span.start)));

        char *info = NULL;
        size_t size = 0;
        FILE *f = open_memstream(&info, &size);
        fprint_span(f, ast->span, "\x1b[31;1m", 2, true);
        fputc('\0', f);
        fflush(f);
        gcc_rvalue_t *callstack = gcc_str(env->ctx, info);
        gcc_func_t *fail = get_function(env, "fail");
        gcc_rvalue_t *ret = gcc_callx(env->ctx, loc, fail, fmt, loc_info, len, msg, callstack);
        gcc_eval(*block, loc, ret);
        fclose(f);
        free(info);
        gcc_jump(*block, loc, *block);
        *block = NULL;
        return NULL;
    }
    case DocTest: {
        auto test = Match(ast, DocTest);
        ast_t *expr = test->expr;

        if (!test->skip_source) {
            // Print source code of the doctest:
            const char* src = heap_strf("\x1b[33;1m>>> \x1b[0m%.*s\x1b[m", (int)(test->expr->span.end - test->expr->span.start), test->expr->span.start);
            ast_t *say_src = WrapAST(ast, FunctionCall, .fn=WrapAST(ast, Var, .name="say"), .args=LIST(ast_t*, StringAST(expr, src)));
            compile_statement(env, block, say_src);
        }

        bl_type_t *t = get_type(env, expr);
        if (t->tag == VoidType) {
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
            bl_type_t *lhs_t = NULL;
            const char *info = NULL;
            switch (expr->tag) {
            case AddUpdate: case SubtractUpdate: case MultiplyUpdate: case DivideUpdate: case AndUpdate: case OrUpdate: case ConcatenateUpdate:
            case Declare: {
                // UNSAFE: this assumes all these types have the same layout:
                ast_t *lhs_ast = expr->__data.AddUpdate.lhs;
                // END UNSAFE
                lhs_t = get_type(env, lhs_ast);
                info = heap_strf("\x1b[0;2m%.*s = \x1b[0;35m", (int)(lhs_ast->span.end - lhs_ast->span.start), lhs_ast->span.start);
                break;
            }
            case Assign: {
                auto assign = Match(expr, Assign);
                if (length(assign->targets) == 1) {
                    lhs_t = get_type(env, ith(assign->targets, 0));
                    expr = WrapAST(expr, FieldAccess, .fielded=expr, .field="_1");
                    break;
                }
                ast_t *first = ith(assign->targets, 0);
                ast_t *last = ith(assign->targets, length(assign->targets)-1);
                info = heap_strf("\x1b[0;2m%.*s = \x1b[0;35m", (int)(last->span.end - first->span.start), first->span.start);
                NEW_LIST(ast_t*, members);
                for (int64_t i = 0; i < length(assign->targets); i++) {
                    APPEND(members, WrapAST(ith(assign->targets, i), StructField, .name=heap_strf("_%d", i+1), .value=ith(assign->targets, i)));
                }
                lhs_t = get_type(env, WrapAST(expr, Struct, .members=members));
                break;
            }
            default: break;
            }

            if (info && lhs_t) {
                gcc_rvalue_t *stdout_val = gcc_rval(gcc_global(env->ctx, NULL, GCC_GLOBAL_IMPORTED, gcc_type(env->ctx, FILE_PTR), "stdout"));
                gcc_func_t *fputs_fn = hget(env->global_funcs, "fputs", gcc_func_t*);
                gcc_eval(*block, loc, gcc_callx(env->ctx, loc, fputs_fn, gcc_str(env->ctx, info), stdout_val)); 
                gcc_func_t *print_fn = get_print_func(env, lhs_t);
                bl_type_t *cycle_checker_t = Type(TableType, .key_type=Type(PointerType, .pointed=Type(VoidType)), .value_type=Type(IntType, .bits=64));
                gcc_type_t *hashmap_gcc_t = bl_type_to_gcc(env, cycle_checker_t);
                gcc_lvalue_t *cycle_checker = gcc_local(gcc_block_func(*block), loc, hashmap_gcc_t, fresh("rec"));
                gcc_assign(*block, loc, cycle_checker, gcc_struct_constructor(env->ctx, loc, hashmap_gcc_t, 0, NULL, NULL));
                gcc_lvalue_t *next_index = gcc_local(gcc_block_func(*block), loc, gcc_type(env->ctx, INT64), fresh("index"));
                gcc_assign(*block, loc, next_index, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
                gcc_assign(*block, loc, gcc_lvalue_access_field(
                        cycle_checker, loc, gcc_get_field(gcc_type_if_struct(hashmap_gcc_t), TABLE_DEFAULT_FIELD)),
                    gcc_lvalue_address(next_index, loc));
                gcc_eval(*block, loc, gcc_callx(env->ctx, loc, print_fn, val, stdout_val,
                                               gcc_cast(env->ctx, loc, gcc_lvalue_address(cycle_checker, loc), gcc_type(env->ctx, VOID_PTR)),
                                               gcc_rvalue_bool(env->ctx, true)));
                gcc_eval(*block, loc, gcc_callx(env->ctx, loc, fputs_fn, gcc_str(env->ctx, "\n"), stdout_val)); 
            }
            return NULL;
        } else {
            // Print "= <expr>"
            NEW_LIST(ast_t*, statements);
            ast_t *stmt = WrapAST(expr, Declare, .var=WrapAST(expr, Var, .name="=expr"), .value=expr);
            APPEND(statements, stmt);
            stmt = WrapAST(expr, FunctionCall, .fn=WrapAST(expr, Var, .name="say"), .args=LIST(ast_t*,
                WrapAST(expr, StringJoin, .children=LIST(ast_t*, 
                    WrapAST(expr, StringLiteral, .str="\x1b[0;2m= \x1b[0;35m"),
                    WrapAST(expr, Interp, .value=WrapAST(expr, Var, .name="=expr"), .colorize=true, .quote_string=true),
                    WrapAST(expr, StringLiteral, .str=heap_strf("\x1b[0;2m : %s\x1b[m", type_to_string(t)))))));
            APPEND(statements, stmt);
            if (test->output) {
                ast_t *message = WrapAST(
                    ast, StringJoin, .children=LIST(
                        ast_t*,
                        WrapAST(ast, StringLiteral, .str=heap_strf("Test failed, expected: %s, but got: ", test->output)), 
                        WrapAST(ast, Interp, .value=WrapAST(expr, Var, .name="=expr"))));
                stmt = WrapAST(expr, If,
                   .condition=WrapAST(expr, NotEqual, .lhs=WrapAST(expr, Interp, .quote_string=true, .value=WrapAST(expr, Var, .name="=expr")),
                                                      .rhs=WrapAST(expr, StringLiteral, .str=test->output)),
                   .body=WrapAST(expr, Fail, .message=message));
                APPEND(statements, stmt);
            }
            compile_statement(env, block, WrapAST(expr, Block, .statements=statements));
            return NULL;
        }
    }
    case Use: {
        return gcc_callx(env->ctx, NULL, prepare_use(env, ast));
    }
    default: break;
    }
    compiler_err(env, ast, "I haven't yet implemented compiling for: %s", ast_to_str(ast)); 
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
