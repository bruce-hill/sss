// Logic for compiling SSS functions
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

static bool is_cacheable(sss_type_t *t)
{
    switch (t->tag) {
    case ArrayType: return true;
    case TableType: return true;
    case PointerType: return Match(t, PointerType)->pointed->tag == MemoryType;
    case GeneratorType: return false;
    case StructType: {
        auto field_types = Match(t, StructType)->field_types;
        for (int64_t i = 0; i < LENGTH(field_types); i++) {
            if (!is_cacheable(ith(field_types, i)))
                return false;
        }
        return true;
    }
    case TaggedUnionType: {
        auto members = Match(t, TaggedUnionType)->members;
        for (int64_t i = 0; i < LENGTH(members); i++) {
            auto member = ith(members, i);
            if (member.type && !is_cacheable(member.type))
                return false;
        }
        return true;
    }
    default: return true;
    }
}

// Given an unpopulated function, populate its body with code that checks if
// the arguments are in a cache, and if not, populates the cache by calling an
// inline function with the real computational work (the returned func).
static gcc_func_t *add_cache(env_t *env, gcc_loc_t *loc, sss_type_t *fn_t, gcc_func_t *func, const char *name, ARRAY_OF(const char*) arg_names, ast_t *max_cache_size)
{
    auto fn_info = Match(fn_t, FunctionType);
    auto params = EMPTY_ARRAY(gcc_param_t*);
    for (int64_t i = 0; i < LENGTH(arg_names); i++) {
        sss_type_t *argtype = ith(fn_info->arg_types, i);
        gcc_param_t *param = gcc_new_param(env->ctx, NULL, sss_type_to_gcc(env, argtype), ith(arg_names, i));
        append(params, param);
        Table_str_set(env->bindings, ith(arg_names, i), new(binding_t, .type=argtype, .lval=gcc_param_as_lvalue(param), .rval=gcc_param_as_rvalue(param)));
    }

    gcc_func_t *inner_func = gcc_new_func(
        env->ctx, loc, GCC_FUNCTION_ALWAYS_INLINE,
        sss_type_to_gcc(env, fn_info->ret), name, LENGTH(params), params[0], 0);

    gcc_block_t *block = gcc_new_block(func, fresh("cache_wrapper"));
    sss_type_t *arg_tuple_t = Type(StructType, .field_names=arg_names, .field_types=fn_info->arg_types);
    gcc_type_t *arg_tuple_gcc_t = sss_type_to_gcc(env, arg_tuple_t);
    sss_type_t *cache_t = Type(TableType, .key_type=arg_tuple_t, .value_type=fn_info->ret);
    gcc_lvalue_t *cache = gcc_global(env->ctx, loc, GCC_GLOBAL_INTERNAL, sss_type_to_gcc(env, cache_t), fresh("cache"));
    gcc_jit_lvalue_set_tls_model(cache, GCC_JIT_TLS_MODEL_INITIAL_EXEC);

    gcc_lvalue_t *arg_tuple = gcc_local(func, loc, arg_tuple_gcc_t, "_cache_key");
    gcc_assign(block, loc, arg_tuple, gcc_struct_constructor(env->ctx, loc, arg_tuple_gcc_t, 0, NULL, NULL));
    auto arg_rvals = EMPTY_ARRAY(gcc_rvalue_t*);
    for (int64_t i = 0; i < LENGTH(arg_names); i++) {
        gcc_assign(block, loc, gcc_lvalue_access_field(arg_tuple, loc, gcc_get_field(gcc_type_as_struct(arg_tuple_gcc_t), i)),
                   gcc_param_as_rvalue(gcc_func_get_param(func, i)));
        append(arg_rvals, gcc_param_as_rvalue(gcc_func_get_param(func, i)));
    }

    binding_t *get_fn_binding = get_from_namespace(env, cache_t, "get");
    gcc_lvalue_t *cached_ptr = gcc_local(func, loc, gcc_get_ptr_type(sss_type_to_gcc(env, fn_info->ret)), "_cached_ptr");
    gcc_assign(block, loc, cached_ptr, gcc_callx(
            env->ctx, loc, get_fn_binding->func,
            gcc_lvalue_address(cache, loc),
            gcc_lvalue_address(arg_tuple, loc), // key
            get_type_pointer(env, cache_t),
    ));
    gcc_block_t *if_cached = gcc_new_block(func, fresh("cached")),
                *if_not_cached = gcc_new_block(func, fresh("not_cached"));
    gcc_jump_condition(block, loc,
                       gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, gcc_rval(cached_ptr), gcc_null(env->ctx, gcc_get_ptr_type(sss_type_to_gcc(env, fn_info->ret)))),
                       if_cached, if_not_cached);

    block = if_cached;
    gcc_return(block, loc, gcc_rval(gcc_rvalue_dereference(gcc_rval(cached_ptr), loc)));

    block = if_not_cached;

    if (max_cache_size && (max_cache_size->tag != Int || Match(max_cache_size, Int)->i < UINT32_MAX)) {
        gcc_rvalue_t *max_val = compile_constant(env, max_cache_size);
        sss_type_t *max_t = get_type(env, max_cache_size);
        if (!promote(env, max_t, &max_val, Type(IntType, .bits=64)))
            compiler_err(env, max_cache_size, "Cache maximum size must be an integer value, not %T", max_t);

        gcc_block_t *needs_pop = gcc_new_block(func, fresh("needs_pop")),
                    *populate_cache = gcc_new_block(func, fresh("populate_cache"));
        gcc_jump_condition(block, loc,
                           gcc_comparison(env->ctx, loc, GCC_COMPARISON_LT, compile_len(env, &block, cache_t, gcc_rval(cache)), max_val),
                           populate_cache, needs_pop);

        block = needs_pop;
        binding_t *remove_fn_binding = get_from_namespace(env, cache_t, "remove");
        gcc_eval(block, loc,
                 gcc_callx(env->ctx, loc, remove_fn_binding->func,
                           gcc_lvalue_address(cache, loc),
                           gcc_null(env->ctx, gcc_get_ptr_type(arg_tuple_gcc_t)),
                           get_type_pointer(env, cache_t)
                 ));
        gcc_jump(block, loc, populate_cache);
        block = populate_cache;
    }

    gcc_lvalue_t *cached_var = gcc_local(func, loc, sss_type_to_gcc(env, fn_info->ret), "_cached");
    gcc_assign(block, loc, cached_var, gcc_call(env->ctx, loc, inner_func, LENGTH(arg_rvals), arg_rvals[0]));
    binding_t *set_fn_binding = get_from_namespace(env, cache_t, "set");
    gcc_eval(block, loc,
             gcc_callx(env->ctx, loc, set_fn_binding->func,
                       gcc_lvalue_address(cache, loc), // table
                       gcc_lvalue_address(arg_tuple, loc), // key
                       gcc_lvalue_address(cached_var, loc), // value
                       get_type_pointer(env, cache_t) // type
             ));
    gcc_return(block, loc, gcc_rval(cached_var));
    return inner_func;
}

void compile_function(env_t *env, gcc_func_t *func, ast_t *def)
{
    sss_type_t * fn_t = get_type(env, def);
    auto fn_info = Match(fn_t, FunctionType);

    // Use a set of bindings that don't include any closures
    env = file_scope(env);
    env->return_type = fn_info->ret;
    env = scope_with_type(env, fn_info->ret);

    args_t args = def->tag == FunctionDef ? Match(def, FunctionDef)->args : Match(def, Lambda)->args;

    // Populate bindings for the arguments:
    for (int64_t i = 0; i < LENGTH(args.names); i++) {
        const char* argname = ith(args.names, i);
        sss_type_t *argtype = ith(fn_info->arg_types, i);
        if (argtype->tag == VoidType)
            compiler_err(env, ith(args.types, i), "'Void' can't be used as the type of an argument because there is no value that could be passed as a Void argument.");
        gcc_param_t *param = gcc_func_get_param(func, i);
        gcc_lvalue_t *lv = gcc_param_as_lvalue(param);
        gcc_rvalue_t *rv = gcc_param_as_rvalue(param);
        Table_str_set(env->bindings, argname, new(binding_t, .type=argtype, .lval=lv, .rval=rv));
    }

    ast_t *max_cache_size = def->tag == FunctionDef ? Match(def, FunctionDef)->cache : NULL;
    if (max_cache_size) {
        if (fn_info->ret->tag == AbortType || fn_info->ret->tag == VoidType || fn_info->ret->tag == GeneratorType)
            compiler_err(env, def, "Functions can't be cached unless they have a return value.");
        for (int64_t i = 0; i < LENGTH(fn_info->arg_types); i++) {
            sss_type_t *arg_t = ith(fn_info->arg_types, i);
            if (has_stack_memory(arg_t))
                compiler_err(env, def, "Functions can't be cached if they take a pointer to stack memory, like the argument '%s' (type: %T)",
                             ith(args.names, i), arg_t);
            else if (!is_cacheable(arg_t))
                compiler_err(env, def, "Functions can't be cached if they take a pointer to mutable heap memory, like the argument '%s' (type: %T)",
                             ith(args.names, i), arg_t);
        }
        const char *name = def->tag == FunctionDef ? fresh(heap_strf("%s__inner", Match(def, FunctionDef)->name)) : fresh("lambda__inner");
        func = add_cache(env, ast_loc(env, def), fn_t, func, name, args.names, max_cache_size);
    }

    gcc_block_t *block = gcc_new_block(func, fresh("func"));
    auto body = def->tag == FunctionDef ? Match(def, FunctionDef)->body : FakeAST(Return, .value=Match(def, Lambda)->body);
    compile_statement(env, &block, body);
    if (block) {
        if (fn_info->ret->tag != VoidType) {
            compiler_err(env, def,
                         "This function should return a %T, but I think you can reach the end without returning a value."
                         "\nSuggestion: if the end of the function is unreachable, put a `fail` statement there so the compiler knows it doesn't have to return a value at the end.",
                         fn_info->ret);
        }
        gcc_return_void(block, NULL);
    }
}

gcc_func_t *get_function_def(env_t *env, ast_t *def, const char* name)
{
    const char *key = heap_strf("%p", def);
    func_context_t *func_context = Table_str_get(&env->global->ast_functions, key);
    if (func_context) return func_context->func;

    auto t = Match(get_type(env, def), FunctionType);
    auto params = EMPTY_ARRAY(gcc_param_t*);
    auto arg_names = def->tag == FunctionDef ? Match(def, FunctionDef)->args.names : Match(def, Lambda)->args.names;
    for (int64_t i = 0; i < LENGTH(arg_names); i++) {
        const char* argname = ith(arg_names, i);
        sss_type_t *argtype = ith(t->arg_types, i);
        gcc_param_t *param = gcc_new_param(env->ctx, NULL, sss_type_to_gcc(env, argtype), fresh(argname));
        append(params, param);
    }

    bool is_inline = def->tag == FunctionDef && Match(def, FunctionDef)->is_inline;
    gcc_func_t *func = gcc_new_func(
        env->ctx, ast_loc(env, def), is_inline ? GCC_FUNCTION_ALWAYS_INLINE : GCC_FUNCTION_EXPORTED,
        sss_type_to_gcc(env, t->ret), name, LENGTH(params), params[0], 0);
    func_context = new(func_context_t, .func=func, .env=*env);
    Table_str_set(&env->global->ast_functions, key, func_context);
    return func;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
