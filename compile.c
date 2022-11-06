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

#include "ast.h"
#include "libgccjit_abbrev.h"
#include "typecheck.h"
#include "types.h"
#include "util.h"

typedef struct loop_label_s {
    struct loop_label_s *enclosing;
    gcc_block_t *skip_label, *stop_label;
    istr_t name;
} loop_label_t;

typedef struct {
    gcc_ctx_t *ctx;
    file_t *file;
    hashmap_t *tostring_funcs;
    hashmap_t *bindings;
    hashmap_t *gcc_types;
    hashmap_t *global_funcs;
    loop_label_t *loop_label;
    bool debug;
} env_t;

#define ERROR(env, ast, fmt, ...) { fprintf(stderr, "\x1b[31;7;1m" fmt "\x1b[m\n\n" __VA_OPT__(,) __VA_ARGS__); \
            highlight_match(stderr, env->file, (ast)->match, 2); \
            exit(1); }

static inline gcc_loc_t *ast_loc(env_t *env, ast_t *ast)
{
    return gcc_new_location(
        env->ctx, env->file->filename,
        (int)get_line_number(env->file, ast->match->start),
        (int)get_line_column(env->file, ast->match->start));
}

static inline istr_t fresh(istr_t name)
{
    static int id = 0;
    CORD ret;
    CORD_sprintf(&ret, "%s__%d", name, id++);
    return intern_str(CORD_to_char_star(ret));
}

#define foreach LIST_FOR
#define length LIST_LEN
#define ith LIST_ITEM
#define append APPEND

#define gcc_type(ctx, t) gcc_get_type(ctx, GCC_T_ ## t)
#define gcc_int64(ctx,i) gcc_rvalue_from_long(ctx, gcc_type(ctx, INT64), i)

// This must be memoized because GCC JIT doesn't do structural equality
static inline gcc_type_t *bl_type_to_gcc(env_t *env, bl_type_t *t)
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

static gcc_rvalue_t *add_value(env_t *env, gcc_block_t **block, ast_t *ast);
static void add_statement(env_t *env, gcc_block_t **block, ast_t *ast);

static void check_truthiness(env_t *env, gcc_block_t **block, ast_t *obj, gcc_block_t *if_truthy, gcc_block_t *if_falsey)
{
    bl_type_t *t = get_type(env->file, env->bindings, obj);
    gcc_rvalue_t *bool_val = add_value(env, block, obj); 
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

static gcc_func_t *get_tostring_func(env_t *env, bl_type_t *t)
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

    gcc_func_t *CORD_cat_func = hashmap_get(env->global_funcs, "CORD_cat");
    gcc_func_t *CORD_to_char_star_func = hashmap_get(env->global_funcs, "CORD_to_char_star");
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
        gcc_func_t *cord_sprintf = hashmap_get(env->global_funcs, "CORD_sprintf");
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
        gcc_lvalue_t *str = gcc_local(func, NULL, gcc_type(env->ctx, STRING), fresh("str"));
        gcc_assign(block, NULL, str,
                   gcc_call(env->ctx, NULL, CORD_cat_func, 2, (gcc_rvalue_t*[]){
                            gcc_null(env->ctx, gcc_type(env->ctx, STRING)),
                            gcc_new_string(env->ctx, "["),
                   }));
        gcc_lvalue_t *i = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("i"));
        gcc_assign(block, NULL, i, gcc_zero(env->ctx, gcc_type(env->ctx, INT64)));
        gcc_struct_t *list_struct = gcc_type_if_struct(gcc_type_if_pointer(gcc_t));
        gcc_rvalue_t *items = gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(obj, NULL, gcc_get_field(list_struct, 0)));
        gcc_rvalue_t *len = gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(obj, NULL, gcc_get_field(list_struct, 1)));

        gcc_block_t *add_comma = gcc_new_block(func, NULL);
        gcc_block_t *add_next_item = gcc_new_block(func, NULL);
        gcc_block_t *end = gcc_new_block(func, NULL);

        // if (i < len) goto add_next_item;
        gcc_jump_condition(block, NULL, 
                      gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_lvalue_as_rvalue(i), len),
                      add_next_item, end);

        // add_next_item:
        gcc_rvalue_t *item = gcc_lvalue_as_rvalue(gcc_array_access(env->ctx, NULL, items, gcc_lvalue_as_rvalue(i)));
        gcc_rvalue_t *item_str;
        gcc_func_t *item_tostring = get_tostring_func(env, t->item_type);
        gcc_rvalue_t *args[] = {
            item,
            gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR)),
        };
        item_str = item_tostring ? gcc_call(env->ctx, NULL, item_tostring, 2, args) : item;
        gcc_assign(add_next_item, NULL, str,
                   gcc_call(env->ctx, NULL, CORD_cat_func, 2, (gcc_rvalue_t*[]){
                            gcc_lvalue_as_rvalue(str),
                            item_str,
                   }));
        
        // i += 1
        gcc_update(add_next_item, NULL, i, GCC_BINOP_PLUS, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
        // if (i < len) goto add_comma;
        gcc_jump_condition(add_next_item, NULL, 
                      gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, gcc_lvalue_as_rvalue(i), len),
                      add_comma, end);

        // add_comma:
        gcc_assign(add_comma, NULL, str,
                   gcc_call(env->ctx, NULL, CORD_cat_func, 2, (gcc_rvalue_t*[]){
                            gcc_lvalue_as_rvalue(str),
                            gcc_new_string(env->ctx, ", "),
                   }));
        // goto add_next_item;
        gcc_jump(add_comma, NULL, add_next_item);

        // end:
        gcc_rvalue_t *ret = gcc_call(env->ctx, NULL, CORD_cat_func, 2, (gcc_rvalue_t*[]){
                                     gcc_lvalue_as_rvalue(str),
                                     gcc_new_string(env->ctx, "]"),
                            });
        gcc_return(end, NULL, CORD_str(ret));
        break;
    }
    default: {
        fprintf(stderr, "\x1b[31;1mtostring(%s) function is not yet implemented!\n", type_to_string(t));
        exit(1);
    }
    }
    return func;
}

static void compile_function(env_t *env, gcc_func_t *func, ast_t *def)
{
    bl_type_t *t = get_type(env->file, env->bindings, def);

    env_t body_env = *env;
    // Use a set of bindings that don't include any closures
    body_env.bindings = global_bindings(env->bindings);

    for (int64_t i = 0; i < length(def->fn.arg_names); i++) {
        istr_t argname = ith(def->fn.arg_names, i);
        bl_type_t *argtype = ith(t->args, i);
        gcc_param_t *param = gcc_func_get_param(func, i);
        gcc_lvalue_t *lv = gcc_param_as_lvalue(param);
        gcc_rvalue_t *rv = gcc_param_as_rvalue(param);
        hashmap_set(body_env.bindings, argname, new(binding_t, .type=argtype, .lval=lv, .rval=rv));
    }

    gcc_block_t *block = gcc_new_block(func, NULL);
    add_statement(&body_env, &block, def->fn.body);
    if (block)
        gcc_return_void(block, NULL);
}

static void coerce_numbers(env_t *env, bl_type_t *lhs_type, gcc_rvalue_t **lhs, bl_type_t *rhs_type, gcc_rvalue_t **rhs)
{
    if (numtype_priority(lhs_type) < numtype_priority(rhs_type))
        *lhs = gcc_cast(env->ctx, NULL, *lhs, bl_type_to_gcc(env, rhs_type));
    else if (numtype_priority(lhs_type) > numtype_priority(rhs_type))
        *rhs = gcc_cast(env->ctx, NULL, *rhs, bl_type_to_gcc(env, lhs_type));
}

static gcc_func_t *get_function_def(env_t *env, ast_t *def, bool is_global)
{
    istr_t name;
    if (def->fn.name)
        name = is_global ? def->fn.name : fresh(def->fn.name);
    else
        name = fresh("lambda");

    bl_type_t *t = get_type(env->file, env->bindings, def);
    NEW_LIST(gcc_param_t*, params);
    for (int64_t i = 0; i < length(def->fn.arg_names); i++) {
        istr_t argname = ith(def->fn.arg_names, i);
        bl_type_t *argtype = ith(t->args, i);
        gcc_param_t *param = gcc_new_param(env->ctx, NULL, bl_type_to_gcc(env, argtype), fresh(argname));
        append(params, param);
    }

    return gcc_new_func(
        env->ctx, ast_loc(env, def), is_global ? GCC_FUNCTION_EXPORTED : GCC_FUNCTION_INTERNAL,
        bl_type_to_gcc(env, t->ret), name, length(params), params[0], 0);
}

static gcc_rvalue_t *add_block(env_t *env, gcc_block_t **block, ast_t *ast, bool return_value)
{
    // Function defs are visible in the entire block (allowing corecursive funcs)
    foreach (ast->children, stmt, last_stmt) {
        if ((*stmt)->kind == FunctionDef) {
            bl_type_t *t = get_type(env->file, env->bindings, *stmt);
            gcc_func_t *func = get_function_def(env, *stmt, false);
            gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
            hashmap_set(env->bindings, (*stmt)->fn.name,
                        new(binding_t, .type=t, .is_global=true, .func=func, .rval=fn_ptr));
        }
    }

    foreach (ast->children, stmt, last_stmt) {
        // Declarations are visible from here onwards:
        if ((*stmt)->kind == FunctionDef) {
            binding_t *binding = hashmap_get(env->bindings, (*stmt)->fn.name);
            assert(binding);
            // Compile the function here instead of above because we need the type information
            // from the other functions in the block.
            compile_function(env, binding->func, *stmt);
        }
        if (stmt == last_stmt && return_value) {
            return add_value(env, block, *stmt);
        } else {
            add_statement(env, block, *stmt);
        }
    }
    return NULL;
}

static gcc_rvalue_t *add_list_for_loop(env_t *env, gcc_block_t **block, ast_t *ast)
{

    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *loop_body = gcc_new_block(func, NULL),
                *loop_between = ast->for_loop.between ? gcc_new_block(func, NULL) : NULL,
                *loop_next = gcc_new_block(func, NULL),
                *loop_end = gcc_new_block(func, NULL);

    env_t loop_env = *env;
    loop_env.bindings = hashmap_new();
    loop_env.bindings->fallback = env->bindings;
    loop_env.loop_label = &(loop_label_t){
        .enclosing = env->loop_label,
        .name = intern_str("for"),
        .skip_label = loop_next,
        .stop_label = loop_end,
    };
    env = &loop_env;

    // Preamble:
    gcc_rvalue_t *list = add_value(env, block, ast->for_loop.iter);
    bl_type_t *list_t = get_type(env->file, env->bindings, ast->for_loop.iter);
    assert(list_t->kind == ListType);
    gcc_type_t *gcc_list_t = bl_type_to_gcc(env, list_t);
    bl_type_t *item_t = list_t->item_type;
    gcc_type_t *gcc_item_t = bl_type_to_gcc(env, item_t);

    // item_ptr = list->items
    gcc_struct_t *list_struct = gcc_type_if_struct(gcc_type_if_pointer(gcc_list_t));
    assert(list_struct);
    gcc_lvalue_t *item_ptr = gcc_local(func, NULL, gcc_get_ptr_type(gcc_item_t), fresh("item_ptr"));
    gcc_assign(*block, NULL, item_ptr,
               gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(list, NULL, gcc_get_field(list_struct, 0))));
    // len = list->len
    gcc_lvalue_t *len = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("len"));
    gcc_assign(*block, NULL, len,
               gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(list, NULL, gcc_get_field(list_struct, 1))));

    gcc_lvalue_t *item_var = NULL;
    if (ast->for_loop.value) {
        if (ast->for_loop.value->kind != Var)
            ERROR(env, ast->for_loop.value, "This needs to be a variable");
        item_var = gcc_local(func, ast_loc(env, ast->for_loop.value), gcc_item_t, fresh(ast->for_loop.value->str));
        hashmap_set(env->bindings, ast->for_loop.value->str, new(binding_t, .rval=gcc_lvalue_as_rvalue(item_var), .type=item_t));
    }

    // index = 1
    gcc_lvalue_t *index_var;
    if (ast->for_loop.key) {
        index_var = gcc_local(func, ast_loc(env, ast->for_loop.key), gcc_type(env->ctx, INT64), fresh(ast->for_loop.key->str));
        if (ast->for_loop.key->kind != Var)
            ERROR(env, ast->for_loop.key, "This needs to be a variable");
        hashmap_set(env->bindings, ast->for_loop.key->str, new(binding_t, .rval=gcc_lvalue_as_rvalue(index_var), .type=Type(IntType)));
    } else {
        index_var = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("i"));
    }
    gcc_rvalue_t *one64 = gcc_one(env->ctx, gcc_type(env->ctx, INT64));
    gcc_assign(*block, NULL, index_var, one64);

    // goto (index > len) ? end : body
    gcc_rvalue_t *is_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, gcc_lvalue_as_rvalue(index_var), gcc_lvalue_as_rvalue(len));
    gcc_jump_condition(*block, NULL, is_done, loop_end, loop_body);
    *block = NULL;

    // body:
    gcc_block_t *loop_body_end = loop_body;
    // item = *item_ptr
    if (item_var)
        gcc_assign(loop_body_end, ast_loc(env, ast->for_loop.value), item_var,
                   gcc_lvalue_as_rvalue(gcc_jit_rvalue_dereference(gcc_lvalue_as_rvalue(item_ptr), NULL)));

    // body block
    (void)add_block(env, &loop_body_end, ast->for_loop.body, false);
    if (loop_body_end)
        gcc_jump(loop_body_end, NULL, loop_next);

    // next: index++, item_ptr++
    gcc_update(loop_next, NULL, index_var, GCC_BINOP_PLUS, one64);
    gcc_assign(loop_next, NULL, item_ptr,
               gcc_lvalue_address(gcc_array_access(env->ctx, NULL, gcc_lvalue_as_rvalue(item_ptr), one64), NULL));

    if (loop_between) {
        // goto is_done ? end : between
        gcc_jump_condition(loop_next, NULL, is_done, loop_end, loop_between);
        // between:
        if (item_var)
            gcc_assign(loop_between, ast_loc(env, ast->for_loop.value), item_var,
                       gcc_lvalue_as_rvalue(gcc_jit_rvalue_dereference(gcc_lvalue_as_rvalue(item_ptr), NULL)));
        (void)add_block(env, &loop_between, ast->for_loop.between, false);
        if (loop_between)
            gcc_jump(loop_between, NULL, loop_body); // goto body
    } else {
        // goto is_done ? end : body
        gcc_jump_condition(loop_next, NULL, is_done, loop_end, loop_body);
    }

    *block = loop_end;
    return NULL;
}

static gcc_rvalue_t *add_range_for_loop(env_t *env, gcc_block_t **block, ast_t *ast)
{

    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *loop_body = gcc_new_block(func, NULL),
                *loop_between = ast->for_loop.between ? gcc_new_block(func, NULL) : NULL,
                *loop_next = gcc_new_block(func, NULL),
                *loop_end = gcc_new_block(func, NULL);

    env_t loop_env = *env;
    loop_env.bindings = hashmap_new();
    loop_env.bindings->fallback = env->bindings;
    loop_env.loop_label = &(loop_label_t){
        .enclosing = env->loop_label,
        .name = intern_str("for"),
        .skip_label = loop_next,
        .stop_label = loop_end,
    };
    env = &loop_env;

    // Preamble:
    bl_type_t *range_t = get_type(env->file, env->bindings, ast->for_loop.iter);
    assert(range_t->kind == RangeType);
    gcc_rvalue_t *range = add_value(env, block, ast->for_loop.iter);
    gcc_type_t *gcc_range_t = bl_type_to_gcc(env, range_t);
    gcc_type_t *i64_t = gcc_type(env->ctx, INT64);

    // val = range.first
    gcc_struct_t *range_struct = gcc_type_if_struct(gcc_range_t);
    assert(range_struct);
    gcc_lvalue_t *val;
    if (ast->for_loop.value) {
        if (ast->for_loop.value->kind != Var)
            ERROR(env, ast->for_loop.value, "This needs to be a variable");
        val = gcc_local(func, ast_loc(env, ast->for_loop.value), i64_t, fresh(ast->for_loop.value->str));
        hashmap_set(env->bindings, ast->for_loop.value->str, new(binding_t, .rval=gcc_lvalue_as_rvalue(val), .type=Type(IntType)));
    } else {
        val = gcc_local(func, NULL, i64_t, fresh("val"));
    }
    gcc_assign(*block, NULL, val,
               gcc_rvalue_access_field(range, NULL, gcc_get_field(range_struct, 0)));

    // step = range.step
    gcc_lvalue_t *step = gcc_local(func, NULL, i64_t, fresh("step"));
    gcc_assign(*block, NULL, step,
               gcc_rvalue_access_field(range, NULL, gcc_get_field(range_struct, 1)));

    // sign = step / abs(step)
    gcc_lvalue_t *sign = gcc_local(func, NULL, i64_t, fresh("sign"));
    gcc_assign(*block, NULL, sign,
               gcc_binary_op(env->ctx, NULL, GCC_BINOP_DIVIDE, i64_t,
                             gcc_lvalue_as_rvalue(step),
                             gcc_unary_op(env->ctx, NULL, GCC_UNOP_ABS, i64_t, gcc_lvalue_as_rvalue(step))));

    // last = range.last
    gcc_lvalue_t *last = gcc_local(func, NULL, i64_t, fresh("last"));
    gcc_assign(*block, NULL, last,
               gcc_rvalue_access_field(range, NULL, gcc_get_field(range_struct, 2)));

    // index = 1
    gcc_rvalue_t *one64 = gcc_one(env->ctx, gcc_type(env->ctx, INT64));
    gcc_lvalue_t *index_var = NULL;
    if (ast->for_loop.key) {
        index_var = gcc_local(func, ast_loc(env, ast->for_loop.key), i64_t, fresh(ast->for_loop.key->str));
        if (ast->for_loop.key->kind != Var)
            ERROR(env, ast->for_loop.key, "This needs to be a variable");
        hashmap_set(env->bindings, ast->for_loop.key->str, new(binding_t, .rval=gcc_lvalue_as_rvalue(index_var), .type=Type(IntType)));
        gcc_assign(*block, NULL, index_var, one64);
    }

    // goto ((last - val)*sign < 0) ? body : end
    gcc_rvalue_t *is_done = gcc_comparison(
        env->ctx, NULL, GCC_COMPARISON_LT,
        gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, i64_t,
                      gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, i64_t,
                                    gcc_lvalue_as_rvalue(last),
                                    gcc_lvalue_as_rvalue(val)),
                      gcc_lvalue_as_rvalue(sign)),
        gcc_zero(env->ctx, i64_t));
    gcc_jump_condition(*block, NULL, is_done, loop_end, loop_body);
    *block = NULL;

    // body:
    gcc_block_t *loop_body_end = loop_body;
    // body block
    (void)add_block(env, &loop_body_end, ast->for_loop.body, false);
    if (loop_body_end)
        gcc_jump(loop_body_end, NULL, loop_next);

    // next:
    // index++, val+=step
    gcc_update(loop_next, NULL, index_var, GCC_BINOP_PLUS, one64);
    gcc_update(loop_next, NULL, val, GCC_BINOP_PLUS, gcc_lvalue_as_rvalue(step));
    if (loop_between) {
        // goto is_done ? end : between
        gcc_jump_condition(loop_next, NULL, is_done, loop_end, loop_between);
        // between:
        (void)add_block(env, &loop_between, ast->for_loop.between, false);
        if (loop_between)
            gcc_jump(loop_between, NULL, loop_body); // goto body
    } else {
        // goto body
        gcc_jump_condition(loop_next, NULL, is_done, loop_end, loop_body);
    }

    *block = loop_end;
    return NULL;
}

// static void set_nil(CORD *code, bl_type_t *t, CORD reg)
// {
//     add_line(code, "%r =%c copy %s", reg, base_type_for(t), nil_value(t));
// }

static gcc_lvalue_t *get_lvalue(env_t *env, gcc_block_t **block, ast_t *ast)
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

gcc_rvalue_t *add_value(env_t *env, gcc_block_t **block, ast_t *ast)
{
    switch (ast->kind) {
    case Var: {
        binding_t *binding = hashmap_get(env->bindings, ast->str);
        if (binding) {
            return binding->rval;
        } else {
            ERROR(env, ast, "Error: variable is not defined"); 
        }
    }
    case Declare: {
        gcc_rvalue_t *rval = add_value(env, block, ast->rhs);

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
            gcc_rvalue_t *rval = add_value(env, block, ith(ast->multiassign.rhs, i));

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
        return add_block(&block_env, block, ast, true);
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
            gcc_rvalue_t *val = add_value(env, block, ast->child);
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
        gcc_func_t *intern_str_func = hashmap_get(env->global_funcs, "intern_str");
        // Optimize to avoid using cords in the cases of 0 or 1 string chunks/interpolations
        if (length(ast->children) == 0) {
            gcc_rvalue_t *empty = gcc_new_string(env->ctx, "");
            return gcc_call(env->ctx, NULL, intern_str_func, 1, &empty);
        } else if (length(ast->children) == 1) {
            ast_t *child = ith(ast->children, 0);
            gcc_rvalue_t *str = add_value(env, block, child);
            bl_type_t *t = get_type(env->file, env->bindings, child);
            gcc_func_t *tostring = get_tostring_func(env, t);
            gcc_rvalue_t *args[] = {str, gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR))};
            str = tostring ? gcc_call(env->ctx, ast_loc(env, child), tostring, 2, args) : args[0];
            return gcc_call(env->ctx, NULL, intern_str_func, 1, &str);
        }
        gcc_rvalue_t *str = gcc_null(env->ctx, gcc_type(env->ctx, STRING));
        gcc_func_t *CORD_cat_func = hashmap_get(env->global_funcs, "CORD_cat");
        gcc_func_t *CORD_to_char_star_func = hashmap_get(env->global_funcs, "CORD_to_char_star");

        foreach (ast->children, chunk, _) {
            gcc_rvalue_t *val;
            bl_type_t *t = get_type(env->file, env->bindings, *chunk);
            if (t->kind == StringType) {
                val = add_value(env, block, *chunk);
            } else {
                gcc_func_t *tostring = get_tostring_func(env, t);
                gcc_rvalue_t *args[] = {
                    add_value(env, block, *chunk),
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
        gcc_ctx_t *ctx = env->ctx;
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_type_t *item_gcc_type = bl_type_to_gcc(env, t->item_type);
        gcc_lvalue_t *list = gcc_local(gcc_block_func(*block), NULL, bl_type_to_gcc(env, t), fresh("list"));

#define PARAM(_t, _name) gcc_new_param(ctx, NULL, gcc_type(ctx, _t), _name)
        gcc_param_t *list_params[] = {PARAM(SIZE, "item_size"), PARAM(SIZE, "min_items")};
        gcc_func_t *new_list_func = gcc_new_func(
            env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_t, "list_new", 2, list_params, 0);

        gcc_param_t *list_insert_params[] = {
            gcc_new_param(ctx, NULL, gcc_t, "list"), PARAM(SIZE,"item_size"), PARAM(INT64,"index"),
            gcc_new_param(ctx, NULL, gcc_get_ptr_type(item_gcc_type), "item"), PARAM(STRING,"err_msg"),
        };
        gcc_func_t *list_insert_func = gcc_new_func(
            ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, VOID), "list_insert", 5, list_insert_params, 0);
#undef PARAM

        ssize_t item_size = gcc_type_is_integral(item_gcc_type) ? gcc_type_size(item_gcc_type) : 8;
        gcc_rvalue_t *new_list_args[] = {
            gcc_rvalue_from_long(ctx, gcc_type(ctx, SIZE), (long)item_size),
            gcc_rvalue_from_long(ctx, gcc_type(ctx, SIZE), ast->list.items ? length(ast->list.items): 0),
        };
        gcc_assign(*block, ast_loc(env, ast), list, gcc_call(ctx, NULL, new_list_func, 2, new_list_args));

        if (ast->list.items) {
            gcc_lvalue_t *item = gcc_local(gcc_block_func(*block), NULL, item_gcc_type, fresh("item"));
            gcc_rvalue_t *item_addr = gcc_lvalue_address(item, NULL);
            foreach (ast->list.items, item_ast, _) {
                switch ((*item_ast)->kind) {
                case For: case While: case Repeat: case If: {
                    errx(1, "Comprehensions not yet implemented");
                }
                default: {
                    gcc_rvalue_t *val = add_value(env, block, (*item_ast));
                    gcc_assign(*block, NULL, item, val);

                    gcc_rvalue_t *insert_args[] = {
                        gcc_lvalue_as_rvalue(list),
                        gcc_rvalue_from_long(ctx, gcc_type(ctx, SIZE), (long)item_size),
                        gcc_int64(ctx, INT_NIL),
                        item_addr,
                        gcc_null(ctx, gcc_type(ctx, STRING)),
                    };
                    gcc_eval(*block, NULL, gcc_call(ctx, NULL, list_insert_func, 5, insert_args));
                }
                }
            }
        }
        return gcc_lvalue_as_rvalue(list);
    }
    case FunctionCall: {
        gcc_rvalue_t *fn = add_value(env, block, ast->call.fn);
        NEW_LIST(gcc_rvalue_t*, arg_vals);
        // TODO: keyword args
        foreach (ast->call.args, arg, _) {
            // TODO: coerce numeric args? sqrt(5) -> sqrt(5.0)
            gcc_rvalue_t *val = add_value(env, block, *arg);
            append(arg_vals, val);
        }
        gcc_rvalue_t *call = gcc_call_ptr(env->ctx, ast_loc(env, ast), fn, length(arg_vals), arg_vals[0]);
        return call;
    }
    case KeywordArg: {
        return add_value(env, block, ast->named.value);
    }
    case Bool: {
        return gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, BOOL), ast->b ? 1 : 0);
    }
    case Maybe: {
        return add_value(env, block, ast->child);
    }
    case Len: {
        bl_type_t *t = get_type(env->file, env->bindings, ast->child);
        printf("Got type: %s\n", type_to_string(t));
        gcc_rvalue_t *obj = add_value(env, block, ast->child);
        switch (t->kind) {
        case ListType: {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
            gcc_struct_t *list_struct = gcc_type_if_struct(gcc_type_if_pointer(gcc_t));
            return gcc_lvalue_as_rvalue(gcc_rvalue_dereference_field(obj, NULL, gcc_get_field(list_struct, 1)));
        }
        case StringType: case DSLType: case TypeType: {
            gcc_func_t *strlen_func = hashmap_get(env->global_funcs, "strlen");
            gcc_rvalue_t *len = gcc_call(env->ctx, ast_loc(env, ast), strlen_func, 1, &obj);
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
    case TypeOf: {
        gcc_func_t *intern_str_func = hashmap_get(env->global_funcs, "intern_str");
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        return gcc_call(env->ctx, NULL, intern_str_func, 1,
                        (gcc_rvalue_t*[]){gcc_new_string(env->ctx, type_to_string(t->type))});
    }
    case Cast: {
        gcc_rvalue_t *val = add_value(env, block, ast->expr);
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        return gcc_bitcast(env->ctx, NULL, val, bl_type_to_gcc(env, t));
    }
    case As: {
        gcc_rvalue_t *val = add_value(env, block, ast->expr);
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
    case Equal: case NotEqual: {
        (void)get_type(env->file, env->bindings, ast); // Check type
        gcc_rvalue_t *lhs_val = add_value(env, block, ast->lhs);
        gcc_rvalue_t *rhs_val = add_value(env, block, ast->rhs);
        coerce_numbers(
            env, get_type(env->file, env->bindings, ast->lhs), &lhs_val,
            get_type(env->file, env->bindings, ast->rhs), &rhs_val);
        return gcc_comparison(env->ctx, NULL, ast->kind == Equal ? GCC_COMPARISON_EQ : GCC_COMPARISON_NE, lhs_val, rhs_val);
    }
    case Less: case LessEqual: case Greater: case GreaterEqual: {
        bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
        bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
        gcc_rvalue_t *lhs_val = add_value(env, block, ast->lhs);
        gcc_rvalue_t *rhs_val = add_value(env, block, ast->rhs);
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
            gcc_rvalue_t *rval = add_value(env, block, ast->rhs);

            bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
            bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
            if (numtype_priority(lhs_t) > numtype_priority(rhs_t))
                rval = gcc_cast(env->ctx, NULL, rval, bl_type_to_gcc(env, lhs_t));
            gcc_update(*block, ast_loc(env, ast), lval, op, rval);
            return gcc_lvalue_as_rvalue(lval);
        } else {
            gcc_rvalue_t *lhs_val = add_value(env, block, ast->lhs);
            gcc_rvalue_t *rhs_val = add_value(env, block, ast->rhs);
            return gcc_binary_op(env->ctx, ast_loc(env, ast), op, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        }
    }
    // case Power: {
    //     bl_type_t *t = get_type(env->file, env->bindings, ast->lhs);
    //     bl_type_t *t2 = get_type(env->file, env->bindings, ast->rhs);
    //     CORD lhs_reg = add_value(env, code, ast->lhs);
    //     CORD rhs_reg = add_value(env, code, ast->rhs);
    //     CORD result = fresh_local(env, "result");
    //     if (t == t2 && base_type_for(t) == 'd') {
    //         add_line(code, "%r =d pow(d %r, d %r)", result, lhs_reg, rhs_reg);
    //         return result;
    //     } else if (t == t2 && base_type_for(t) == 's') {
    //         add_line(code, "%r =s powf(s %r, s %r)", result, lhs_reg, rhs_reg);
    //         return result;
    //     }
    //     ERROR(env, ast, "Exponentiation is not supported for %s and %s", type_to_string(t), type_to_string(t2));
    //     break;
    // }
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
            gcc_rvalue_t *branch_val = add_value(&branch_env, &if_truthy, body);

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
            gcc_rvalue_t *branch_val = add_value(env, block, ast->else_body);
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
        gcc_type_t *range_t = bl_type_to_gcc(env, Type(RangeType));
        gcc_struct_t *range_struct = gcc_type_if_struct(range_t);
        assert(range_struct);
        gcc_rvalue_t *values[] = {
            ast->range.first ? add_value(env, block, ast->range.first) : gcc_int64(env->ctx, INT64_MIN),
            ast->range.step ? add_value(env, block, ast->range.step) : gcc_int64(env->ctx, 1),
            ast->range.last ? add_value(env, block, ast->range.last) : gcc_int64(env->ctx, INT64_MAX),
        };
        return gcc_struct_constructor(env->ctx, NULL, range_t, 3, NULL, values);

    }
    case While: case Repeat: {
        gcc_func_t *func = gcc_block_func(*block);
        gcc_block_t *loop_top = gcc_new_block(func, NULL),
                    *loop_body = gcc_new_block(func, NULL),
                    *loop_between = ast->loop.between ? gcc_new_block(func, NULL) : NULL,
                    *loop_end = gcc_new_block(func, NULL);

        env_t loop_env = *env;
        loop_env.bindings = hashmap_new();
        loop_env.bindings->fallback = env->bindings;
        loop_env.loop_label = &(loop_label_t){
            .enclosing = env->loop_label,
            .name = intern_str(ast->kind == While ? "while" : "repeat"),
            .skip_label = loop_top,
            .stop_label = loop_end,
        };
        env = &loop_env;

        gcc_jump(*block, NULL, loop_top);
        *block = NULL;

        gcc_comment(loop_top, NULL, "Loop");
        ast_t *cond = ast->loop.condition;
        if (cond)
            check_truthiness(env, &loop_top, cond, loop_body, loop_end);
        else
            gcc_jump(loop_top, NULL, loop_body);

        gcc_block_t *loop_body_orig = loop_body;
        (void)add_block(env, &loop_body, ast->loop.body, false);

        if (loop_body) {
            if (loop_between) {
                if (cond)
                    check_truthiness(env, &loop_body, cond, loop_between, loop_end);
                else
                    gcc_jump(loop_body, NULL, loop_between);
                (void)add_block(env, &loop_between, ast->loop.between, false);
                if (loop_between)
                    gcc_jump(loop_between, NULL, loop_body_orig);
            } else {
                gcc_jump(loop_body_orig, NULL, loop_top);
            }
        }

        *block = loop_end;
        return NULL;
    }
    case For: {
        bl_type_t *iter_t = get_type(env->file, env->bindings, ast->for_loop.iter);
        switch (iter_t->kind) {
        case ListType: return add_list_for_loop(env, block, ast);
        case RangeType: return add_range_for_loop(env, block, ast);
        default: ERROR(env, ast, "Not implemented");
        }
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
        gcc_func_t *cat = hashmap_get(env->global_funcs, "CORD_cat");
        if (ast->child) {
            msg = gcc_call(env->ctx, NULL, cat, 2, (gcc_rvalue_t*[]){
                           gcc_new_string(env->ctx, "\x1b[41;30m Error: \x1b[0;31;1m "),
                           add_value(env, block, ast->child),
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
        gcc_func_t *to_char_star = hashmap_get(env->global_funcs, "CORD_to_char_star");
        msg = gcc_call(env->ctx, NULL, to_char_star, 1, (gcc_rvalue_t*[]){msg});
        gcc_func_t *intern = hashmap_get(env->global_funcs, "intern_str");
        msg = gcc_call(env->ctx, NULL, intern, 1, (gcc_rvalue_t*[]){msg});
        gcc_func_t *fail = hashmap_get(env->global_funcs, "fail");
        msg = gcc_call(env->ctx, NULL, fail, 1, (gcc_rvalue_t*[]){msg});
        fclose(f);
        return msg;
    }
    default: break;
    }
    ERROR(env, ast, "Error: compiling is not yet implemented for %s", get_ast_kind_name(ast->kind)); 
}

void add_statement(env_t *env, gcc_block_t **block, ast_t *ast) {
    check_discardable(env->file, env->bindings, ast);
    gcc_rvalue_t *val = add_value(env, block, ast);
    if (val)
        gcc_eval(*block, ast_loc(env, ast), val);
}

gcc_result_t *compile_file(gcc_ctx_t *ctx, file_t *f, ast_t *ast, bool debug) {
    env_t env = {
        .ctx = ctx,
        .file = f,
        .bindings = hashmap_new(),
        .tostring_funcs = hashmap_new(),
        .gcc_types = hashmap_new(),
        .global_funcs = hashmap_new(),
        .debug = debug,
    };

    bl_type_t *string_type = Type(StringType);
    bl_type_t *say_type = Type(
        FunctionType,
        .args=LIST(bl_type_t*, string_type, Type(OptionalType, .nonnil=string_type)),
        .ret=Type(VoidType));

    gcc_param_t *gcc_str_param = gcc_new_param(ctx, NULL, gcc_type(ctx, STRING), "str");
    gcc_func_t *puts_func = gcc_new_func(ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(ctx, INT), "puts", 1, &gcc_str_param, 0);
    gcc_rvalue_t *puts_rvalue = gcc_get_func_address(puts_func, NULL);
    hashmap_set(env.bindings, intern_str("say"), new(binding_t, .rval=puts_rvalue, .type=say_type, .is_global=true));
#define DEFTYPE(t) hashmap_set(env.bindings, intern_str(#t), new(binding_t, .is_global=true, .rval=gcc_new_string(ctx, #t), .type=Type(TypeType, .type=Type(t##Type))));
    // Primitive types:
    DEFTYPE(Bool); DEFTYPE(Void); DEFTYPE(Abort);
    DEFTYPE(Int); DEFTYPE(Int32); DEFTYPE(Int16); DEFTYPE(Int8);
    DEFTYPE(Num); DEFTYPE(Num32);
    DEFTYPE(String);
#undef DEFTYPE

    { // Global funcs:
        gcc_param_t *cord_cat_params[2] = {
            gcc_new_param(env.ctx, NULL, gcc_type(env.ctx, STRING), "str"),
            gcc_new_param(env.ctx, NULL, gcc_type(env.ctx, STRING), "str2"),
        };
        gcc_func_t *CORD_cat_func = gcc_new_func(
            env.ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(env.ctx, STRING),
            "CORD_cat", 2, cord_cat_params, 0);
        hashmap_set(env.global_funcs, "CORD_cat", CORD_cat_func);

        gcc_type_t *str_t = gcc_type(env.ctx, STRING);
        gcc_type_t *str_ptr_t = gcc_get_ptr_type(str_t);
        gcc_param_t *sprintf_args[] = {
            gcc_new_param(env.ctx, NULL, str_ptr_t, "cord"),
            gcc_new_param(env.ctx, NULL, str_t, "fmt"),
        };
        gcc_func_t *CORD_sprintf_func = gcc_new_func(
            env.ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(env.ctx, INT), "CORD_sprintf", 2, sprintf_args, 1);
        hashmap_set(env.global_funcs, "CORD_sprintf", CORD_sprintf_func);

        gcc_func_t *CORD_to_char_star_func = gcc_new_func(
            env.ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(env.ctx, STRING),
            "CORD_to_char_star", 1, (gcc_param_t*[]){gcc_new_param(env.ctx, NULL, gcc_type(env.ctx, STRING), "str")}, 0);
        hashmap_set(env.global_funcs, "CORD_to_char_star", CORD_to_char_star_func);

        gcc_func_t *intern_str_func = gcc_new_func(
            env.ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(env.ctx, STRING),
            "intern_str", 1, (gcc_param_t*[]){gcc_new_param(env.ctx, NULL, gcc_type(env.ctx, STRING), "str")}, 0);
        hashmap_set(env.global_funcs, "intern_str", intern_str_func);

        gcc_func_t *strlen_func = gcc_new_func(
            env.ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(env.ctx, SIZE),
            "strlen", 1, (gcc_param_t*[]){gcc_new_param(env.ctx, NULL, gcc_type(env.ctx, STRING), "str")}, 0);
        hashmap_set(env.global_funcs, "strlen", strlen_func);

        gcc_func_t *fail_func = gcc_new_func(
            env.ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(env.ctx, VOID),
            "fail", 1, (gcc_param_t*[]){gcc_new_param(env.ctx, NULL, gcc_type(env.ctx, STRING), "message")}, 0);
        hashmap_set(env.global_funcs, "fail", fail_func);

        gcc_func_t *range_tostring_func = gcc_new_func(
            env.ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(env.ctx, STRING),
            "range_tostring", 2, (gcc_param_t*[]){
                gcc_new_param(env.ctx, NULL, bl_type_to_gcc(&env, Type(RangeType)), "range"),
                gcc_new_param(env.ctx, NULL, gcc_type(env.ctx, VOID_PTR), "range"),
            }, 0);
        hashmap_set(env.tostring_funcs, Type(RangeType), range_tostring_func);
    }

    gcc_func_t *main_func = gcc_new_func(
        ctx, NULL, GCC_FUNCTION_EXPORTED, gcc_type(ctx, VOID),
        "main", 0, NULL, 0);
    gcc_block_t *block = gcc_new_block(main_func, NULL);
    add_statement(&env, &block, ast);
    gcc_return_void(block, NULL);

    return gcc_compile(ctx);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
