// Compilation logic for expressions
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
#include "../span.h"
#include "../typecheck.h"
#include "../types.h"
#include "../units.h"
#include "../util.h"
#include "compile.h"
#include "libgccjit_abbrev.h"

gcc_rvalue_t *add_tag_to_value(env_t *env, bl_type_t *tagged_union_t, bl_type_t *field_type, gcc_rvalue_t *rval, gcc_rvalue_t *tag_rval)
{
    gcc_type_t *gcc_tagged_t = bl_type_to_gcc(env, tagged_union_t);
    gcc_struct_t *gcc_tagged_s = gcc_type_if_struct(gcc_tagged_t);

    auto tagged = Match(tagged_union_t, TaggedUnionType);
    bl_type_t *union_type = tagged->data;
    auto union_ = Match(union_type, UnionType);

    int64_t field_index = 0;
    for (int64_t i = 0, len = length(union_->field_types); i < len; i++) {
        if (ith(union_->field_types, i) == field_type) {
            field_index = i;
            break;
        }
    }
    gcc_field_t *union_field = ith(union_->fields, field_index);
    gcc_type_t *gcc_union_t = bl_type_to_gcc(env, union_type);
    gcc_rvalue_t *data_union = gcc_union_constructor(env->ctx, NULL, gcc_union_t, union_field, rval);
    gcc_field_t *fields[] = {
        gcc_get_field(gcc_tagged_s, 0),
        gcc_get_field(gcc_tagged_s, 1),
    };
    rval = gcc_struct_constructor(env->ctx, NULL, gcc_tagged_t, 2, fields, (gcc_rvalue_t*[]){tag_rval, data_union});
    assert(rval);
    return rval;
}

gcc_rvalue_t *compile_constant(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case Var: {
        auto var = Match(ast, Var);
        binding_t *binding = hashmap_get(env->bindings, var->name);
        if (!binding) {
            compile_err(env, ast, "I can't find a definition for this variable"); 
        } else if (!binding->is_constant) {
            compile_err(env, ast, "This variable is not a constant, but I need a constant value here that is known at compile-time and can't change."); 
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
            compile_err(env, ast, "I can't find any constant-value field or method called \"%s\" on a %s.", access->field, type_to_string(fielded_t));
        } else if (!binding->is_constant) {
            compile_err(env, ast, "This variable is not a constant, but I need a constant value here that is known at compile-time and can't change."); 
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
        istr_t units = intval->units;
        if (units) {
            units = unit_derive(units, &n, env->derived_units);
            if ((double)((int64_t)n) == n) // no floating point rounding
                i = (int64_t)n;
            else
                compile_err(env, ast, "This is an integer with a derived unit of measure (%s) which is %g<%s> in base units, "
                            "but that can't be represented as an integer without rounding errors",
                            intval->units, n, units);
        }

        switch (intval->precision) {
        case 32:
            if (i < INT32_MIN)
                compile_err(env, ast, "This integer literal is too small to fit in a %d bit integer %ld", intval->precision, i);
            else if (i > INT32_MAX)
                compile_err(env, ast, "This integer literal is too big to fit in a %d bit integer %ld", intval->precision, i);
            break;
        case 16:
            if (i < INT16_MIN)
                compile_err(env, ast, "This integer literal is too small to fit in a %d bit integer %ld", intval->precision, i);
            else if (i > INT16_MAX)
                compile_err(env, ast, "This integer literal is too big to fit in a %d bit integer %ld", intval->precision, i);
            break;
        case 8:
            if (i < INT8_MIN)
                compile_err(env, ast, "This integer literal is too small to fit in a %d bit integer %ld", intval->precision, i);
            else if (i > INT8_MAX)
                compile_err(env, ast, "This integer literal is too big to fit in a %d bit integer %ld", intval->precision, i);
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
        istr_t units = Match(ast, Num)->units;
        units = unit_derive(units, &n, env->derived_units);
        return gcc_rvalue_from_double(env->ctx, gcc_t, n);
    }
    default: break;
    }
    compile_err(env, ast, "I can't evaluate this value at compile-time. It needs to be a constant value.");
}

gcc_rvalue_t *compile_expr(env_t *env, gcc_block_t **block, ast_t *ast)
{
    gcc_loc_t *loc = ast_loc(env, ast);
    switch (ast->tag) {
    case Var: {
        auto var = Match(ast, Var);
        binding_t *binding = hashmap_get(env->bindings, var->name);
        if (binding) {
            if (binding->rval)
                return binding->rval;
            if (binding->lval)
                return gcc_rval(binding->lval);
            if (binding->func)
                return gcc_get_func_address(binding->func, loc);
        }
        compile_err(env, ast, "I can't find a definition for this variable"); 
    }
    case Declare: {
        auto decl = Match(ast, Declare);
        bl_type_t *t = get_type(env, decl->value);
        assert(t);
        if (t->tag == GeneratorType)
            compile_err(env, decl->value, "This expression isn't guaranteed to have a single value, so you can't use it to initialize a variable."); 
        else if (t->tag == VoidType)
            compile_err(env, decl->value, "This expression doesn't have a value (it has a Void type), so you can't store it in a variable."); 
        gcc_rvalue_t *rval = compile_expr(env, block, decl->value);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_lvalue_t *lval;
        istr_t name = Match(decl->var, Var)->name;
        if (decl->is_global) {
            lval = gcc_global(env->ctx, ast_loc(env, ast), GCC_GLOBAL_EXPORTED, gcc_t, name);
        } else {
            gcc_func_t *func = gcc_block_func(*block);
            lval = gcc_local(func, ast_loc(env, ast), gcc_t, name);
        }
        binding_t *clobbered = hashmap_get_raw(env->bindings, name);
        if (clobbered && clobbered->type->tag == TypeType)
            compile_err(env, ast, "This name is already being used for the name of a type (struct or enum) in the same block, "
                  "and I get confused if you try to redeclare the name of a namespace.");
        else if (clobbered && decl->is_global)
            compile_err(env, ast, "This name is already being used for a global");
        hashmap_set(decl->is_global ? env->global_bindings : env->bindings, name,
                    new(binding_t, .lval=lval, .rval=gcc_rval(lval), .type=t));
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
                istr_t arg_name = fn->arg_names ? ith(fn->arg_names, i) : fresh("arg");
                APPEND(params, gcc_new_param(env->ctx, loc, arg_t, arg_name));
            }
            gcc_func_t *func = gcc_new_func(
                env->ctx, loc, GCC_FUNCTION_IMPORTED, gcc_ret_t, ext->name, length(params), params[0], 0);
            hashmap_set(env->global_bindings, ext->bl_name ? ext->bl_name : ext->name, new(binding_t, .func=func, .type=t));
        } else {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
            gcc_rvalue_t *glob = gcc_rval(gcc_global(env->ctx, ast_loc(env, ast), GCC_GLOBAL_IMPORTED, gcc_t, ext->name));
            hashmap_set(env->global_bindings, ext->bl_name ? ext->bl_name : ext->name, new(binding_t, .rval=glob, .type=t));
        }
        return NULL;
    }

    case Assign: {
        auto assignment = Match(ast, Assign);
        List(ast_t*) targets = assignment->targets;
        List(ast_t*) values = assignment->values;
        int64_t len = length(targets);
        NEW_LIST(gcc_lvalue_t*, lvals);
        foreach (targets, lhs, _) {
            append(lvals, get_lvalue(env, block, *lhs, false));
        }
        gcc_func_t *func = gcc_block_func(*block);
        NEW_LIST(gcc_rvalue_t*, rvals);
        for (int64_t i = 0; i < len; i++) {
            ast_t *rhs = ith(values, i);
            bl_type_t *t_lhs = get_type(env, ith(targets, i)),
                      *t_rhs = get_type(env, rhs);
            // TODO: maybe allow generators to assign the *last* value, if any
            if (t_rhs->tag == GeneratorType)
                compile_err(env, rhs, "This expression isn't guaranteed to have a single value, so you can't assign it to a variable."); 
            gcc_rvalue_t *rval = compile_expr(env, block, ith(values, i));

            if (!promote(env, t_rhs, &rval, t_lhs))
                compile_err(env, rhs, "You're assigning this value with type %s to a variable with type %s and I can't figure out how to make that work.",
                      type_to_string(t_rhs), type_to_string(t_lhs));

            if (len > 1) {
                gcc_lvalue_t *tmp = gcc_local(func, loc, bl_type_to_gcc(env, t_rhs), fresh("tmp"));
                assert(rval);
                gcc_assign(*block, loc, tmp, rval);
                append(rvals, gcc_rval(tmp));
            } else {
                append(rvals, rval);
            }
        }
        for (int64_t i = 0; i < len; i++)
            gcc_assign(*block, ast_loc(env, ast), ith(lvals, i), ith(rvals, i));
        return ith(rvals, length(rvals)-1);
    }
    case Do: {
        auto blocks = Match(ast, Do)->blocks;
        // TODO: support do/else?
        if (length(blocks) > 1)
            compile_err(env, ith(blocks, 1), "`do` statments with else clauses are not currently supported");
        return compile_expr(env, block, ith(blocks, 0));
    }
    case Using: {
        auto using = Match(ast, Using);
        env_t *env2 = global_scope(env);
        for (int64_t i = 0, len = length(using->vars); i < len; i++) {
            ast_t *var = ith(using->vars, i);
            binding_t *b = hashmap_get(env->bindings, Match(var, Var)->name);
            if (!b) compile_err(env, var, "This variable is not defined");
            hashmap_set(env2->bindings, Match(var, Var)->name, b);
        }
        return compile_expr(env2, block, using->body);
    }
    case Block: {
        // Create scope:
        return compile_block_expr(fresh_scope(env), block, ast);
    }
    case FunctionDef: {
        auto fn = Match(ast, FunctionDef);
        binding_t *binding = hashmap_get(env->bindings, fn->name);
        assert(binding && binding->func);
        compile_function(env, binding->func, ast);
        return binding->rval;
    }
    case Lambda: {
        gcc_func_t *func = get_function_def(env, ast, fresh("lambda"), false);
        compile_function(env, func, ast);
        return gcc_get_func_address(func, loc);
    }
    case Return: {
        auto ret = Match(ast, Return);
        assert(env->return_type);
        if (env->return_type->tag == VoidType) {
            if (ret->value) {
                bl_type_t *child_type = get_type(env, ret->value);
                if (child_type->tag != VoidType)
                    compile_err(env, ast, "I was expecting a plain `return` with no expression here or a Void-type function call, because the function this is inside has no declared return type. If you want to return a value, please change the function's definition to include a return type.");
                compile_statement(env, block, ret->value);
            }
            gcc_return_void(*block, loc);
        } else {
            if (!ret->value)
                compile_err(env, ast, "I was expecting this `return` to have a value of type %s because of the function's type signature, but no value is being returned here.",
                      type_to_string(env->return_type));
            bl_type_t *t = get_type(env, ret->value);
            gcc_rvalue_t *val = compile_expr(env, block, ret->value);
            if (!promote(env, t, &val, env->return_type))
                compile_err(env, ast, "I was expecting this `return` to have value of type %s because of the function's type signature, but this value has type %s",
                      type_to_string(env->return_type), type_to_string(t));

            gcc_return(*block, loc, val);
        }
        *block = NULL;
        return NULL;
    }
    case Int: case Num: case Char: {
        return compile_constant(env, ast);
    }
    case Interp: {
        return compile_expr(env, block, Match(ast, Interp)->value);
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
        istr_t str = Match(ast, StringLiteral)->str;
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
                compile_err(env, *chunk, "This expression doesn't have a value (it has a Void type), so you can't use it in a string."); 
        }

        bl_type_t *string_t = Type(ArrayType, .item_type=Type(CharType), .dsl=string_join->dsl);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, string_t);
        gcc_type_t *i32_t = gcc_type(env->ctx, INT32);

        gcc_func_t *open_memstream_fn = hashmap_gets(env->global_funcs, "open_memstream");
        gcc_func_t *free_fn = hashmap_gets(env->global_funcs, "free");
        gcc_func_t *fputs_fn = hashmap_gets(env->global_funcs, "fputs");
        gcc_func_t *fflush_fn = hashmap_gets(env->global_funcs, "fflush");
        gcc_func_t *fclose_fn = hashmap_gets(env->global_funcs, "fclose");
        // Optimize the case of empty strings
        if (length(chunks) == 0) {
            return STRING_STRUCT(env, gcc_t, gcc_null(env->ctx, gcc_type(env->ctx, STRING)), gcc_zero(env->ctx, i32_t), gcc_one(env->ctx, i32_t));
        }

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
                istr_t str = Match(*chunk, StringLiteral)->str;
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
            gcc_func_t *print_fn = get_print_func(env, t);
            assert(print_fn);
            
            // Hack: instead of hashmap_t rec = {0}; print(obj, &rec), I'm using char[sizeof(hashmap_t)] = {0}
            // and passing that instead. Basically, it's just a zeroed out stack value of the appropriate size
            // which saves a call to hashmap_new() and an allocation.
            gcc_type_t *hashmap_gcc_t = gcc_array_type(env->ctx, loc, gcc_type(env->ctx, CHAR), sizeof(hashmap_t));
            gcc_lvalue_t *cycle_checker = gcc_local(func, loc, hashmap_gcc_t, fresh("rec"));
            gcc_assign(*block, loc, cycle_checker, gcc_array_constructor(env->ctx, loc, hashmap_gcc_t, 0, NULL));
            gcc_type_t *void_star = gcc_type(env->ctx, VOID_PTR);

            gcc_eval(*block, chunk_loc,
                     gcc_callx(env->ctx, chunk_loc, print_fn, 
                               compile_expr(env, block, interp_value),
                               file,
                               gcc_cast(env->ctx, loc, gcc_lvalue_address(cycle_checker, loc), void_star)));

        }
        gcc_eval(*block, loc, gcc_callx(env->ctx, loc, fflush_fn, file));
        gcc_lvalue_t *str_struct_var = gcc_local(func, loc, gcc_t, fresh("str_final"));
        gcc_rvalue_t *len32 = gcc_cast(env->ctx, loc, gcc_rval(size_var), i32_t);

        gcc_func_t *alloc_fn = hashmap_gets(env->global_funcs, "GC_malloc_atomic");
        gcc_rvalue_t *size = gcc_rval(size_var);
        gcc_rvalue_t *str = gcc_callx(
            env->ctx, loc, alloc_fn,
            gcc_binary_op(env->ctx, loc, GCC_BINOP_PLUS, gcc_type(env->ctx, SIZE),
                          size, gcc_one(env->ctx, gcc_type(env->ctx, SIZE))));
        gcc_func_t *memcpy_fn = hashmap_gets(env->global_funcs, "memcpy");
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
            .arg_names=LIST(istr_t, convert->var),
            .arg_types=LIST(ast_t*, convert->source_type),
            .ret_type=convert->target_type,
            .body=convert->body);
        gcc_func_t *func = get_function_def(env, def, fresh("convert"), false);
        compile_function(env, func, def);
        env->conversions = new(conversions_t, .src=src_t, .dest=target_t, .func=func, .next=env->conversions);
        return NULL;
    }
    case StructDef: {
        auto struct_def = Match(ast, StructDef);
        bl_type_t *struct_t = Match(get_binding(env, struct_def->name)->type, TypeType)->type;

        env = get_type_env(env, struct_t);

        foreach (struct_def->members, member, _) {
            if ((*member)->tag == FunctionDef) {
                auto fndef = Match((*member), FunctionDef);
                binding_t *binding = hashmap_get(env->bindings, fndef->name);
                assert(binding);
                compile_function(env, binding->func, *member);
            } else if ((*member)->tag == Declare) {
                auto decl = Match((*member), Declare);
                gcc_rvalue_t *rval = compile_expr(env, block, decl->value);
                bl_type_t *t = get_type(env, decl->value);
                assert(t);
                gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
                istr_t name = Match(decl->var, Var)->name;
                istr_t global_name = intern_strf("%s__%s", struct_def->name, name);
                gcc_lvalue_t *lval = gcc_global(env->ctx, ast_loc(env, (*member)), GCC_GLOBAL_INTERNAL, gcc_t, global_name);
                hashmap_set(env->bindings, name,
                            new(binding_t, .lval=lval, .rval=gcc_rval(lval), .type=t));
                assert(rval);
                gcc_assign(*block, ast_loc(env, (*member)), lval, rval);
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
                compile_err(env, struct_->type, "This isn't a struct type that I recognize");
            t = Match(binding->type, TypeType)->type;
        } else {
            binding = NULL;
            t = get_type(env, ast);
        }

        size_t num_values = length(struct_->members);

        bl_type_t *tagged_union_t = NULL;
        if (struct_->type && struct_->type->tag == FieldAccess) {
            bl_type_t *fielded_t = get_type(env, Match(struct_->type, FieldAccess)->fielded);
            if (fielded_t->tag == TypeType && Match(fielded_t, TypeType)->type->tag == TaggedUnionType)
                tagged_union_t = Match(fielded_t, TypeType)->type;
        }

        if (t->tag != StructType) {
            // For tagged enums, you can initialize with curly brace syntax (`Foo.Text{"hi"}`)
            // when it's just a value and not a struct
            if (num_values != 1)
                compile_err(env, ast, "I expected this to have exactly one value");
            ast_t *member_ast = ith(struct_->members, 0);
            ast_t *value = Match(member_ast, StructField)->value;
            gcc_rvalue_t *rval = compile_expr(env, block, value);
            if (!promote(env, get_type(env, value), &rval, t))
                compile_err(env, value, "I couldn't promote this value to %s, which is what this struct needs", type_to_string(t));
            if (tagged_union_t)
                rval = add_tag_to_value(env, tagged_union_t, t, rval, binding->tag_rval);
            return rval;
        }

        auto struct_type = Match(t, StructType);
        if (length(struct_type->field_names) == 0)
            compile_err(env, ast, "This struct type has no members and I'm not able to handle creating a struct with no members.");
        if (length(struct_->members) > length(struct_type->field_names))
            compile_err(env, ast, "I expected this %s literal to only have %ld fields, but you provided %ld fields.",
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
                if (ith(struct_type->field_names, field_index) != member->name)
                    continue;

                gcc_field_t *field = unused_fields[field_index];
                if (!field)
                    compile_err(env, member_ast, "You already provided a value for this field earlier in this struct.");

                // Found the field:
                entries[value_index].field = field;
                entries[value_index].field_num = field_index;
                unused_fields[field_index] = NULL;
                goto found_name;
            }

            compile_err(env, member_ast, "There is no struct field with this name");

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
            compile_err(env, member_ast, "This field is beyond the number of fields in this struct.");

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
                    compile_err(env, def, "I couldn't make this default value work as a %s",
                                type_to_string(ft));
                entries[num_values].value = def_val;
                unused_fields[field_index] = NULL;
                ++num_values;
                continue;
            }

            if (!can_leave_uninitialized(ft) && unused_fields[field_index]) {
                compile_err(env, ast, "%s structs are supposed to have a non-optional field '%s' (%s), but you didn't provide a value for it.",
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
                compile_err(env, entries[i].ast, "I was expecting a value of type %s for the %s.%s field, but this value is a %s.", 
                      type_to_string(expected), type_to_string(t), ith(struct_type->field_names, entries[i].field_num),
                      type_to_string(actual));

            populated_fields[i] = entries[i].field;
        }

        gcc_rvalue_t *rval = gcc_struct_constructor(env->ctx, loc, gcc_t, num_values, populated_fields, rvalues);
        assert(rval);

        if (tagged_union_t)
            rval = add_tag_to_value(env, tagged_union_t, t, rval, binding->tag_rval);

        return rval;
    }
    case FunctionCall: {
        auto call = Match(ast, FunctionCall);
        gcc_rvalue_t *fn_ptr = NULL;
        gcc_func_t *fn = NULL;
        bl_type_t *fn_bl_t = get_type(env, call->fn);
        if (fn_bl_t->tag != FunctionType)
            compile_err(env, call->fn, "This is not a callable function (it's a %s)", type_to_string(fn_bl_t));
        auto fn_t = Match(fn_bl_t, FunctionType);

        int64_t num_args = length(fn_t->arg_types);
        gcc_rvalue_t **arg_vals = GC_MALLOC(sizeof(gcc_rvalue_t*)*num_args);
        // method calls:
        if (call->fn->tag == FieldAccess) { // method call (foo.method())
            auto access = Match(call->fn, FieldAccess);
            ast_t *self = access->fielded;
            bl_type_t *self_t = get_type(env, self);
            bl_type_t *value_type = self_t;
            while (value_type->tag == PointerType)
                value_type = Match(value_type, PointerType)->pointed;
            switch (value_type->tag) {
            case TypeType: {
                if (access->fielded->tag != Var)
                    compile_err(env, call->fn, "I only know how to access type members by referencing the type directly like foo.baz()");
                bl_type_t *fielded_type = Match(value_type, TypeType)->type;
                binding_t *binding = get_from_namespace(env, fielded_type, access->field);
                if (!binding)
                    compile_err(env, call->fn, "I couldn't find any method called %s for %s.", access->field, type_to_string(fielded_type));
                fn = binding->func;
                fn_ptr = binding->rval;
                break;
            }
            default: {
                binding_t *binding = get_from_namespace(env, self_t, access->field);
                if (!binding)
                    binding = get_from_namespace(env, value_type, access->field);
                if (!binding)
                    compile_err(env, call->fn, "I couldn't find a method with this name defined for a %s.", type_to_string(self_t));
                if (binding->type->tag != FunctionType)
                    compile_err(env, call->fn, "This value isn't a function, it's a %s", type_to_string(binding->type));
                auto fn_info = Match(binding->type, FunctionType);
                if (length(fn_info->arg_types) < 1)
                    compile_err(env, call->fn, "This function doesn't take any arguments. If you want to call it anyways, use the class name like %s.%s()",
                          type_to_string(value_type), access->field);

                gcc_rvalue_t *self_val = compile_expr(env, block, self);
                bl_type_t *expected_self = ith(fn_info->arg_types, 0);
                if (self_t != expected_self && !promote(env, self_t, &self_val, expected_self))
                    compile_err(env, ast, "The method %s.%s(...) is being called on a %s, but it wants a %s.",
                          type_to_string(self_t), access->field, type_to_string(self_t), type_to_string(expected_self));
                arg_vals[0] = self_val;
                fn = binding->func;
                fn_ptr = binding->rval;
                break;
            }
            }
        } else {
            fn_ptr = compile_expr(env, block, call->fn);
            fn = NULL;
        }

        const int64_t MASK = -1; // mask is because the hashmap api returns 0 for missing keys
        hashmap_t *arg_positions = hashmap_new();
        if (fn_t->arg_names) {
            for (int64_t i = 0; i < num_args; i++)
                hashmap_set(arg_positions, ith(fn_t->arg_names, i), (void*)(i^MASK));
        }

        // // First: keyword args
        foreach (call->args, arg, _) {
            if ((*arg)->tag != KeywordArg) continue;
            auto kwarg = Match((*arg), KeywordArg);
            int64_t arg_index = (int64_t)hashmap_get(arg_positions, kwarg->name);
            if (arg_index == 0) // missing key
                compile_err(env, *arg, "\"%s\" is not the name of any argument for this function that I'm aware of",
                      kwarg->name);
            arg_index ^= MASK;
            if (arg_vals[arg_index])
                compile_err(env, *arg, "This argument was already passed in earlier to this function call");
            gcc_rvalue_t *val = compile_expr(env, block, kwarg->arg);
            bl_type_t *actual = get_type(env, *arg);
            bl_type_t *expected = ith(fn_t->arg_types, arg_index);
            if (!promote(env, actual, &val, expected))
                compile_err(env, *arg, "This function expected this argument to have type %s, but this value is a %s",
                      type_to_string(expected), type_to_string(actual));
            arg_vals[arg_index] = val;
        }

        // Then positional args
        int64_t pos = 0;
        for (int64_t call_index = 0, len = length(call->args); call_index < len; call_index++) {
            ast_t *arg = ith(call->args, call_index);
            if (arg->tag == KeywordArg) continue;
            // Find the next unspecified arg:
            while (arg_vals[pos]) {
                ++pos;
                if (pos >= num_args)
                    compile_err(env, arg, "This is one argument too many for this function call");
                assert(pos < num_args);
            }
            gcc_rvalue_t *val = compile_expr(env, block, arg);
            bl_type_t *actual = get_type(env, arg);
            bl_type_t *expected = ith(fn_t->arg_types, pos);
            if (!promote(env, actual, &val, expected))
                compile_err(env, arg, "This function expected this argument to have type %s, but this value is a %s",
                      type_to_string(expected), type_to_string(actual));
            arg_vals[pos] = val;
        }

        // Optional values get passed as nil or default values are used:
        for (int64_t len = num_args; pos < len; pos++) {
            if (arg_vals[pos]) continue;
            if (fn_t->arg_defaults) {
                ast_t *default_val = ith(fn_t->arg_defaults, pos);
                if (default_val) {
                    // TODO: maybe manually restrict the bindings in `env`
                    arg_vals[pos] = compile_expr(env, block, default_val);
                    continue;
                }
            }
            bl_type_t *arg_t = ith(fn_t->arg_types, pos);
            if (arg_t->tag != PointerType || !Match(arg_t, PointerType)->is_optional)
                compile_err(env, ast, "The non-optional argument %s was not provided",
                      fn_t->arg_names ? ith(fn_t->arg_names, pos) : intern_strf("%ld", pos));
            arg_vals[pos] = gcc_null(env->ctx, bl_type_to_gcc(env, arg_t));
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
        gcc_func_t *func = gcc_block_func(*block);
        gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), gcc_sizeof(env, t));
        gcc_type_t *gcc_t = gcc_get_ptr_type(bl_type_to_gcc(env, t));
        gcc_lvalue_t *tmp = gcc_local(func, loc, gcc_t, fresh(intern_strf("heap_%s", type_to_string(t))));
        gcc_func_t *alloc_func = hashmap_gets(env->global_funcs, has_heap_memory(t) ? "GC_malloc" : "GC_malloc_atomic");
        gcc_assign(*block, loc, tmp, gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, size), gcc_t));
        gcc_assign(*block, loc, gcc_rvalue_dereference(gcc_rval(tmp), loc), rval);
        return gcc_rval(tmp);
    }
    case Dereference: {
        (void)get_type(env, ast); // Check this is a pointer type
        ast_t *value = Match(ast, Dereference)->value;
        gcc_rvalue_t *rval = compile_expr(env, block, value);
        return gcc_rval(gcc_rvalue_dereference(rval, ast_loc(env, ast)));
    }
    case Maybe: {
        ast_t *value = Match(ast, Maybe)->value;
        return compile_expr(env, block, value);
    }
    case Len: {
        ast_t *value = Match(ast, Len)->value;
        bl_type_t *t = get_type(env, value);
        gcc_rvalue_t *obj = compile_expr(env, block, value);
        switch (t->tag) {
        case ArrayType: {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
            gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
            return gcc_cast(env->ctx, loc, gcc_rvalue_access_field(obj, loc, gcc_get_field(array_struct, 1)), gcc_type(env->ctx, INT64));
        }
        case PointerType: {
            auto ptr = Match(t, PointerType);
            if (ptr->pointed == Type(CharType)) {
                // String length
                gcc_func_t *len_func = hashmap_gets(env->global_funcs, "strlen");
                gcc_rvalue_t *len = gcc_callx(env->ctx, ast_loc(env, ast), len_func, obj);
                return gcc_cast(env->ctx, ast_loc(env, ast), len, gcc_type(env->ctx, INT64));
            } else {
                goto unknown_len;
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
        default: {
          unknown_len:
            compile_err(env, ast, "I don't know how to get the length of a %s", type_to_string(t));
        }
        }
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
                compile_err(env, ast, "This field access is unsafe because the value may be nil");
            obj = gcc_rval(gcc_rvalue_dereference(obj, loc));
            fielded_t = ptr->pointed;
            goto get_field;
        }
        case StructType: {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, fielded_t);
            gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
            auto struct_type = Match(fielded_t, StructType);
            for (int64_t i = 0, len = length(struct_type->field_names); i < len; i++) {
                if (ith(struct_type->field_names, i) == access->field) {
                    gcc_field_t *field = gcc_get_field(gcc_struct, (size_t)i);
                    return gcc_rvalue_access_field(obj, loc, field);
                }
            }
            break;
        }
        case ArrayType: { // [Vec{1,2}, Vec{3,4]].x ==> [1,3]
            auto array = Match(fielded_t, ArrayType);
            if (array->item_type->tag != StructType)
                break; // TODO: support pointers?

            gcc_type_t *array_gcc_t = bl_type_to_gcc(env, fielded_t);
            gcc_struct_t *gcc_array_struct = gcc_type_if_struct(array_gcc_t);

            gcc_type_t *item_gcc_t = bl_type_to_gcc(env, array->item_type);
            gcc_struct_t *gcc_item_struct = gcc_type_if_struct(item_gcc_t);

            auto struct_type = Match(array->item_type, StructType);
            for (int64_t i = 0, len = length(struct_type->field_names); i < len; i++) {
                if (ith(struct_type->field_names, i) == access->field) {
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
                                           gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, INT32), gcc_sizeof(env, array->item_type)));
                    stride = gcc_binary_op(env->ctx, loc, GCC_BINOP_DIVIDE, gcc_type(env->ctx, INT32), stride,
                                           gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, INT32), gcc_sizeof(env, field_type)));

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
            }
            break;
        }
        case TaggedUnionType: {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, fielded_t);
            bl_type_t* tag_bl_type = Match(fielded_t, TaggedUnionType)->tag_type;
            auto tag_type = Match(tag_bl_type, TagType);
            auto union_type = Match(Match(fielded_t, TaggedUnionType)->data, UnionType);
            for (int64_t i = 0, len = length(union_type->field_names); i < len; i++) {
                if (ith(union_type->field_names, i) == access->field) {
                    // Step 1: check tag and fail if it's the wrong one:
                    gcc_rvalue_t *tagged_tag = gcc_rvalue_access_field(obj, loc, gcc_get_field(gcc_type_if_struct(gcc_t), 0));
                    gcc_rvalue_t *field_tag = NULL;
                    for (int64_t tag_index = 0, len = length(tag_type->names); tag_index < len; tag_index++) {
                        if (ith(tag_type->names, tag_index) == access->field) {
                            gcc_type_t *tag_t = bl_type_to_gcc(env, tag_bl_type);
                            field_tag = gcc_rvalue_from_long(env->ctx, tag_t, ith(tag_type->values, i));
                            break;
                        }
                    }
                    gcc_func_t *func = gcc_block_func(*block);
                    gcc_block_t *tag_ok = gcc_new_block(func, fresh("tag_ok")),
                                *tag_wrong = gcc_new_block(func, fresh("tag_wrong"));
                    gcc_rvalue_t *is_ok = gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, tagged_tag, field_tag);
                    gcc_jump_condition(*block, loc, is_ok, tag_ok, tag_wrong);

                    *block = tag_wrong;
                    char *info = NULL;
                    size_t size = 0;
                    FILE *f = open_memstream(&info, &size);
                    fprint_span(f, ast->span, "\x1b[31;1m", 2, true);
                    fputc('\0', f);
                    fflush(f);
                    gcc_rvalue_t *callstack = gcc_str(env->ctx, info);
                    gcc_func_t *fail = hashmap_gets(env->global_funcs, "fail");
                    istr_t fmt_str = intern_strf("\x1b[31;1;7mError: this tagged union is not a %s\x1b[m\n\n%%s", access->field);
                    gcc_rvalue_t *fmt = gcc_str(env->ctx, fmt_str);
                    gcc_rvalue_t *failure = gcc_callx(env->ctx, loc, fail, fmt, callstack);
                    gcc_eval(tag_wrong, loc, failure);
                    fclose(f);
                    free(info);
                    gcc_jump(tag_wrong, loc, tag_wrong);

                    // Step 2: access tagged.__data.TagName
                    *block = tag_ok;
                    gcc_field_t *field = ith(union_type->fields, i);
                    gcc_rvalue_t *data = gcc_rvalue_access_field(obj, loc, gcc_get_field(gcc_type_if_struct(gcc_t), 1));
                    return gcc_rvalue_access_field(data, loc, field);
                }
            }
            break;
        }
        case TypeType: {
            bl_type_t *t = Match(fielded_t, TypeType)->type;
            binding_t *val_binding = get_from_namespace(env, t, access->field);
            if (val_binding)
                return val_binding->rval;
            else
                compile_err(env, ast, "I can't find any field or method called \"%s\" on the type %s.", access->field, type_to_string(t));
            break;
        }
        default: break;
        }
        // Class lookup:
        binding_t *binding = get_from_namespace(env, fielded_t, access->field);
        if (binding)
            return binding->rval;
        else {
            compile_err(env, ast, "I can't find any field or method called \"%s\" on the type %s.", access->field, type_to_string(fielded_t));
        }
    }
    case Index: {
        auto indexing = Match(ast, Index);
        bl_type_t *indexed_t = get_type(env, indexing->indexed);
        if (indexed_t->tag != ArrayType)
            compile_err(env, ast, "I only know how to index into lists, but this is a %s", type_to_string(indexed_t));

        bl_type_t *index_t = get_type(env, indexing->index);
        if (is_integral(index_t)) {
            return gcc_rval(get_lvalue(env, block, ast, true));
        } else if (index_t->tag == RangeType) {
            // Slicing (currently not implemented as an Lvalue)
            gcc_rvalue_t *obj = compile_expr(env, block, indexing->indexed);

            gcc_type_t *array_gcc_t = bl_type_to_gcc(env, indexed_t);
            if (indexing->index->tag == Range) {
                auto range = Match(indexing->index, Range);
                if (!range->step || (range->step->tag == Int && Match(range->step, Int)->i == 1)) {
                    gcc_struct_t *gcc_array_struct = gcc_type_if_struct(array_gcc_t);
                    gcc_type_t *i32_t = gcc_type(env->ctx, INT32);
#define SUB(a,b) gcc_binary_op(env->ctx, loc, GCC_BINOP_MINUS, i32_t, a, b)
                    gcc_func_t *func = gcc_block_func(*block);
                    gcc_rvalue_t *old_items = gcc_rvalue_access_field(obj, loc, gcc_get_field(gcc_array_struct, 0));
                    gcc_rvalue_t *offset;
                    if (range->first)
                        offset = SUB(gcc_cast(env->ctx, loc, compile_expr(env, block, range->first), i32_t), gcc_one(env->ctx, i32_t));
                    else
                        offset = gcc_zero(env->ctx, i32_t);
                    gcc_rvalue_t *items = gcc_lvalue_address(gcc_array_access(env->ctx, loc, old_items, offset), loc);
                    gcc_lvalue_t *slice = gcc_local(func, loc, array_gcc_t, fresh("slice"));
                    // assign slice.items and slice.stride
                    gcc_assign(*block, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, 0)), items);
                    gcc_rvalue_t *old_stride = gcc_rvalue_access_field(obj, loc, gcc_get_field(gcc_array_struct, 2));
                    gcc_assign(*block, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, 2)),
                               old_stride);

                    // len = MIN(array_len, range.last)-first
                    gcc_rvalue_t *array_len = gcc_rvalue_access_field(obj, loc, gcc_get_field(gcc_array_struct, 1));
                    if (range->last) {
                        gcc_block_t *array_shorter = gcc_new_block(func, "array_shorter"),
                                    *range_shorter = gcc_new_block(func, "range_shorter"),
                                    *len_assigned = gcc_new_block(func, "len_assigned");
                        gcc_rvalue_t *range_len = gcc_cast(env->ctx, loc, compile_expr(env, block, range->last), i32_t);

                        gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_LT, array_len, range_len), array_shorter, range_shorter);
                        
                        gcc_assign(array_shorter, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, 1)),
                                   SUB(array_len, offset));
                        gcc_jump(array_shorter, loc, len_assigned);

                        gcc_assign(range_shorter, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, 1)),
                                   SUB(range_len, offset));
                        gcc_jump(range_shorter, loc, len_assigned);

                        *block = len_assigned;
                    } else {
                        gcc_assign(*block, loc, gcc_lvalue_access_field(slice, loc, gcc_get_field(gcc_array_struct, 1)),
                                   SUB(array_len, offset));
                    }

                    return gcc_rval(slice);
#undef SUB
                }
            }

            // If we're not in the optimized case, fall back to the C function:
            gcc_rvalue_t *index = compile_expr(env, block, indexing->index);
            gcc_type_t *str_gcc_t = bl_type_to_gcc(env, Type(ArrayType, .item_type=Type(CharType)));
            gcc_func_t *slice_fn = hashmap_gets(env->global_funcs, "range_slice");
            return gcc_bitcast(
                env->ctx, loc,
                gcc_callx(
                    env->ctx, loc, slice_fn,
                    gcc_bitcast(env->ctx, loc, obj, str_gcc_t),
                    index,
                    gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), gcc_sizeof(env, Match(indexed_t, ArrayType)->item_type))),
                array_gcc_t);
        } else {
            compile_err(env, indexing->index, "I only support indexing arrays by integers, not %s", type_to_string(index_t));
        }
    }
    case TypeOf: {
        auto value = Match(ast, TypeOf)->value;
        gcc_func_t *intern_str_func = hashmap_gets(env->global_funcs, "intern_str");
        bl_type_t *t = get_type(env, value);
        return gcc_callx(env->ctx, loc, intern_str_func, gcc_str(env->ctx, type_to_string(t)));
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

        for (auto convert = env->conversions; convert; convert = convert->next) {
            if (convert->src == src_t && convert->dest == cast_t) {
                return gcc_callx(env->ctx, loc, convert->func, val);
            }
        }
        if (!((is_numeric(src_t) && is_numeric(cast_t))
              || (is_numeric(src_t) && (cast_t->tag == BoolType || cast_t->tag == CharType))
              || (is_numeric(cast_t) && (src_t->tag == BoolType || src_t->tag == CharType))))
            compile_err(env, ast, "I don't know how to convert %s to %s. "
                        "Maybe you could use 'bitcast' instead or implement a conversion function.",
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
            compile_err(env, ast, "The 'not' operator isn't supported for values with type %s.", type_to_string(t));
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
        coerce_numbers(env, &lhs_t, &lhs_val, &rhs_t, &rhs_val);
        if (lhs_t != rhs_t)
            compile_err(env, ast, "I don't know how to do a comparison between a %s and a %s.", type_to_string(lhs_t), type_to_string(rhs_t));

        if (is_ordered && !is_comparable(lhs_t))
            compile_err(env, ast, "I don't have a good way to do ordered comparisons when pointers are involved (this has type %s), because this could be a recursive structure",
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
            compile_err(env, ast, "I only know how to negate numbers, not %s", type_to_string(t));
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
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
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
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *result = gcc_local(func, loc, bl_type_to_gcc(env, t), fresh("and_result"));
        gcc_block_t *if_truthy = gcc_new_block(func, fresh("and_truthy"));
        gcc_block_t *if_falsey = gcc_new_block(func, fresh("and_falsey"));
        gcc_block_t *done = gcc_new_block(func, fresh("and_done"));

        gcc_type_t *lhs_gcc_t = bl_type_to_gcc(env, lhs_t);
        gcc_rvalue_t *bool_val = lhs_val;
        if (t->tag != BoolType) {
            gcc_rvalue_t *zero = gcc_type_if_pointer(lhs_gcc_t) ? gcc_null(env->ctx, lhs_gcc_t) : gcc_zero(env->ctx, lhs_gcc_t);
            bool_val = gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, lhs_val, zero);
        }
        gcc_jump_condition(*block, loc, bool_val, if_truthy, if_falsey);

        if (rhs_t->tag == AbortType) {
            compile_statement(env, &if_truthy, rhs);
        } else {
            gcc_rvalue_t *rhs_val = compile_expr(env, &if_truthy, rhs);
            if (if_truthy)
                gcc_assign(if_truthy, loc, result, rhs_val);
        }
        if (if_truthy)
            gcc_jump(if_truthy, loc, done);

        gcc_assign(if_falsey, loc, result, lhs_val);
        gcc_jump(if_falsey, loc, done);

        *block = done;
        return gcc_rval(result);
    }
    case Or: {
        ast_t *lhs = Match(ast, Or)->lhs,
              *rhs = Match(ast, Or)->rhs;
        bl_type_t *t = get_type(env, ast);
        bl_type_t *lhs_t = get_type(env, lhs);
        bl_type_t *rhs_t = get_type(env, rhs);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        if (t->tag == BoolType && rhs_t->tag == BoolType) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_LOGICAL_OR, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        } else if (is_integral(lhs_t) && is_integral(rhs_t)) {
            gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
            if (!promote(env, lhs_t, &lhs_val, rhs_t)
                && !promote(env, rhs_t, &rhs_val, lhs_t))
                compile_err(env, ast, "I can't figure out how to combine a %s and a %s", type_to_string(lhs_t), type_to_string(rhs_t));
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_BITWISE_OR, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        }
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *result = gcc_local(func, loc, bl_type_to_gcc(env, t), fresh("and_result"));
        gcc_block_t *if_truthy = gcc_new_block(func, fresh("or_truthy"));
        gcc_block_t *if_falsey = gcc_new_block(func, fresh("or_falsey"));
        gcc_block_t *done = gcc_new_block(func, fresh("or_done"));

        gcc_type_t *lhs_gcc_t = bl_type_to_gcc(env, lhs_t);
        gcc_rvalue_t *bool_val = lhs_val;
        if (t->tag != BoolType) {
            gcc_rvalue_t *zero = gcc_type_if_pointer(lhs_gcc_t) ? gcc_null(env->ctx, lhs_gcc_t) : gcc_zero(env->ctx, lhs_gcc_t);
            bool_val = gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, lhs_val, zero);
        }
        gcc_jump_condition(*block, loc, bool_val, if_truthy, if_falsey);

        gcc_assign(if_truthy, loc, result, lhs_val);
        gcc_jump(if_truthy, loc, done);

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
        math_update(env, block, ast);
        return NULL;
    }
    case Add: case Subtract: case Divide: case Multiply: {
        return math_binop(env, block, ast);
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
            compile_err(env, ast, "The left hand side of this modulus has type %s, but the right hand side has type %s and I can't figure out how to combine them.",
                  type_to_string(lhs_t), type_to_string(rhs_t));
        if (t->tag == NumType) {
            gcc_func_t *sane_fmod_func = hashmap_gets(env->global_funcs, "sane_fmod");
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
            compile_err(env, ast, "The base of this operation has type %s, but the exponent has type %s and I can't figure out how to combine them.",
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
                compile_err(env, ast, "This 'if' is being used as a value, but it might not have a value");
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
                            compile_err(env, body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
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
                        compile_err(env, if_->else_body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
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
                compile_err(env, when->subject, "This value may be nil, so it can't be safely dereferenced");
            subject_t = ptr->pointed;
            subject = gcc_rval(gcc_rvalue_dereference(subject, ast_loc(env, when->subject)));
        }
        if (subject_t->tag != TaggedUnionType)
            compile_err(env, when->subject, "'when' blocks must use enums, but this is a %s", type_to_string(subject_t));
        gcc_type_t *gcc_t = bl_type_to_gcc(env, subject_t);

        binding_t *tags_binding = get_from_namespace(env, subject_t, "__Tag");
        assert(tags_binding);
        bl_type_t *tag_t = Match(tags_binding->type, TypeType)->type;

        bl_type_t *result_t = get_type(env, ast);
        bool has_value = !(result_t->tag == GeneratorType || result_t->tag == AbortType || result_t->tag == VoidType);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *when_value = has_value ? gcc_local(func, loc, bl_type_to_gcc(env, result_t), fresh("when_value")) : NULL;
        gcc_block_t *end_when = result_t->tag == AbortType ? NULL : gcc_new_block(func, fresh("endif"));

        auto union_type = Match(Match(subject_t, TaggedUnionType)->data, UnionType);
        NEW_LIST(gcc_case_t*, gcc_cases);
        foreach (when->cases, case_, _) {
            istr_t tag_name = Match(case_->tag, Var)->name;
            gcc_loc_t *case_loc = ast_loc(env, case_->tag);
            env_t *case_env = env;
            if (case_->var) {
                case_env = fresh_scope(env);

                gcc_rvalue_t *case_rval = NULL;
                bl_type_t *case_t = NULL;
                for (int64_t i = 0, len = length(union_type->field_names); i < len; i++) {
                    if (ith(union_type->field_names, i) == tag_name) {
                        gcc_rvalue_t *data = gcc_rvalue_access_field(subject, case_loc, gcc_get_field(gcc_type_if_struct(gcc_t), 1));
                        case_rval = gcc_rvalue_access_field(data, case_loc, ith(union_type->fields, i));
                        case_t = ith(union_type->field_types, i);
                        break;
                    }
                }
                if (!case_rval)
                    compile_err(env, case_->var, "I couldn't figure out how to bind this variable (maybe the tag doesn't have a value associated with it)");

                binding_t *case_binding = new(binding_t, .type=case_t, .rval=case_rval);
                hashmap_set(case_env->bindings, Match(case_->var, Var)->name, case_binding);
            }

            gcc_block_t *case_block = gcc_new_block(func, fresh(intern_strf("case_%s", tag_name)));
            gcc_rvalue_t *tag_rval = get_from_namespace(env, tag_t, tag_name)->rval;
            gcc_case_t *gcc_case = gcc_new_case(env->ctx, tag_rval, tag_rval, case_block);
            APPEND(gcc_cases, gcc_case);

            gcc_rvalue_t *branch_val = compile_expr(case_env, &case_block, case_->body);
            if (branch_val) {
                if (has_value) {
                    bl_type_t *actual = get_type(env, case_->body);
                    if (!promote(env, actual, &branch_val, result_t))
                        compile_err(env, case_->body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
                              type_to_string(result_t), type_to_string(actual));
                    gcc_assign(case_block, case_loc, when_value, branch_val);
                } else {
                    gcc_eval(case_block, case_loc, branch_val);
                }
            }
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
                        compile_err(env, when->default_body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
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
    case Repeat: {
        auto loop = Match(ast, Repeat);
        compile_while_loop(env, block, intern_str("repeat"), NULL, loop->body, loop->between);
        return NULL;
    }
    case While: {
        auto loop = Match(ast, While);
        compile_while_loop(env, block, intern_str("while"), loop->condition, loop->body, loop->between);
        return NULL;
    }
    case For: {
        compile_for_loop(env, block, ast);
        return NULL;
    }
    case Skip: case Stop: {
        gcc_block_t *jump_dest = NULL;
        istr_t target = ast->tag == Skip ? Match(ast, Skip)->target : Match(ast, Stop)->target;
        if (target) {
            for (loop_label_t *lbl = env->loop_label; lbl; lbl = lbl->enclosing) {
                foreach (lbl->names, name, _) {
                    if (*name == target) {
                        if (ast->tag == Skip) {
                            jump_dest = lbl->skip_label;
                        } else {
                            jump_dest = lbl->stop_label;
                        }
                        goto found_label;
                    }
                }
            }
          found_label:;
        } else {
            if (env->loop_label) {
                if (ast->tag == Skip) {
                    jump_dest = env->loop_label->skip_label;
                } else {
                    jump_dest = env->loop_label->stop_label;
                }
            }
        }
        if (!jump_dest) compile_err(env, ast, "I'm not sure what %s is referring to", target);
        gcc_jump(*block, loc, jump_dest);
        *block = NULL;
        return NULL;
    }
    case Reduction: {
        auto reduction = Match(ast, Reduction);
        bl_type_t *t = get_type(env, ast);
        istr_t name = fresh("reduction");
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *ret = gcc_local(func, loc, bl_type_to_gcc(env, t), name);

        env = fresh_scope(env);

        hashmap_set(env->bindings, intern_str("x"), new(binding_t, .lval=ret, .rval=gcc_rval(ret), .type=t));
        ast_t *prev_var = WrapAST(ast, Var, .name=intern_str("x"));
        ast_t *iter_var = WrapAST(ast, Var, .name=intern_str("y"));
        ast_t *first_block = WrapAST(ast, Assign, LIST(ast_t*, prev_var), LIST(ast_t*, iter_var));
        ast_t *between_block = WrapAST(ast, Assign, LIST(ast_t*, prev_var), LIST(ast_t*, reduction->combination));
        ast_t *empty_block = NULL;
        if (reduction->fallback) {
            bl_type_t *empty_type = get_type(env, reduction->fallback);
            if (empty_type == t) {
                empty_block = WrapAST(ast, Assign, LIST(ast_t*, prev_var), LIST(ast_t*, reduction->fallback));
            } else if (empty_type->tag == AbortType) {
                empty_block = reduction->fallback;
            } else {
                compile_err(env, reduction->fallback, "This fallback value has type %s, but I need a %s",
                            type_to_string(empty_type), type_to_string(t));
            }
        } else {
            empty_block = WrapAST(reduction->iter, Fail, .message=StringAST(reduction->iter, "This collection was empty"));
        }
        ast_t *for_ast = WrapAST(ast, For, .value=iter_var, .iter=reduction->iter, .first=first_block, .between=between_block, .empty=empty_block);
        compile_statement(env, block, for_ast);

        return gcc_rval(ret);
    }
    case Fail: {
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

        gcc_rvalue_t *fmt = gcc_str(env->ctx, "\x1b[31;1;7mError: %.*s\x1b[m\n\n%s");

        char *info = NULL;
        size_t size = 0;
        FILE *f = open_memstream(&info, &size);
        fprint_span(f, ast->span, "\x1b[31;1m", 2, true);
        fputc('\0', f);
        fflush(f);
        gcc_rvalue_t *callstack = gcc_str(env->ctx, info);
        gcc_func_t *fail = hashmap_gets(env->global_funcs, "fail");
        gcc_rvalue_t *ret = gcc_callx(env->ctx, loc, fail, fmt, len, msg, callstack);
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

        // Print test:
        istr_t src = intern_strf("\x1b[33;1m>>> \x1b[0m%.*s\x1b[m", (int)(test->expr->span.end - test->expr->span.start), test->expr->span.start);
        ast_t *say_src = WrapAST(ast, FunctionCall, .fn=WrapAST(ast, Var, .name=intern_str("say")), .args=LIST(ast_t*, StringAST(expr, src)));
        compile_statement(env, block, say_src);

        bl_type_t *t = get_type(env, expr);
        if (t->tag == VoidType) {
            if (test->output)
                compile_err(env, ast, "There shouldn't be any output for a Void expression like this");

            compile_statement(env, block, expr);
            return NULL;
        }
        if (t == Type(ArrayType, .item_type=Type(CharType))) {
            if (Match(t, ArrayType)->dsl)
                expr = WrapAST(ast, StringJoin, .children=LIST(ast_t*, WrapAST(ast, Interp, .value=expr)));
            List(ast_t*) args = LIST(ast_t*, WrapAST(ast, KeywordArg, .name=intern_str("colorize"), .arg=WrapAST(ast, Bool, .b=false)));
            expr = WrapAST(ast, FunctionCall, .fn=WrapAST(ast, FieldAccess, .fielded=expr, .field=intern_str("quoted")), .args=args);
        }

        ast_t *prefix = WrapAST(ast, StringLiteral, .str=intern_str("\x1b[0;2m= \x1b[0;35m"));
        ast_t *type_info = WrapAST(ast, StringLiteral, .str=intern_strf("\x1b[0;2m : %s\x1b[m", type_to_string(t)));
        // Stringify and add type info:
        ast_t *result_str = WrapAST(ast, StringJoin, .children=LIST(ast_t*, prefix, WrapAST(ast, Interp, .value=expr), type_info));
        ast_t *result_str_plain = WrapAST(ast, StringJoin, .children=LIST(ast_t*, WrapAST(ast, Interp, .value=expr)));

        // Call say(str):
        ast_t *say_result = WrapAST(ast, FunctionCall, .fn=WrapAST(ast, Var, .name=intern_str("say")), .args=LIST(ast_t*, result_str));
        compile_statement(env, block, say_result);

        if (test->output) {
            ast_t *expected = StringAST(ast, test->output);
            List(ast_t*) chunks = LIST(ast_t*,
                                       WrapAST(ast, StringLiteral, .str=intern_str("Test failed, expected: ")), 
                                       WrapAST(ast, Interp, .value=expected),
                                       WrapAST(ast, StringLiteral, .str=intern_str(", but got: ")), 
                                       WrapAST(ast, Interp, .value=result_str_plain));
            compile_statement(
                env, block, WrapAST(
                    ast, If, .condition=WrapAST(ast, NotEqual, .lhs=result_str_plain, .rhs=expected),
                    .body=WrapAST(ast, Block, .statements=LIST(ast_t*, WrapAST(ast, Fail, .message=WrapAST(ast, StringJoin, .children=chunks))))));

        }

        return NULL;
    }
    default: break;
    }
    compile_err(env, ast, "I haven't yet implemented compiling for: %s", ast_to_str(ast)); 
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
