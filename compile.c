#include <assert.h>
#include <bhash.h>
#include <libgccjit.h>
#include <bp/files.h>
#include <ctype.h>
#include <err.h>
#include <gc/cord.h>
#include <stdarg.h>
#include <stdint.h>

#include "ast.h"
#include "typecheck.h"
#include "types.h"
#include "util.h"

typedef struct loop_label_s {
    struct loop_label_s *enclosing;
    CORD skip_label, stop_label;
    istr_t name;
} loop_label_t;

typedef struct {
    gcc_jit_context *ctx;
    file_t *file;
    hashmap_t *tostring_funcs;
    hashmap_t *bindings;
    loop_label_t *loop_label;
    bool debug;
} env_t;

gcc_jit_function *gcc_CORD_sprintf = NULL;
gcc_jit_function *gcc_CORD_cat = NULL;
gcc_jit_function *gcc_CORD_to_char_star = NULL;
gcc_jit_function *gcc_puts = NULL;
gcc_jit_function *gcc_intern_str = NULL;

#define ERROR(env, ast, fmt, ...) { fprintf(stderr, "\x1b[31;7;1m" fmt "\x1b[m\n\n" __VA_OPT__(,) __VA_ARGS__); \
            highlight_match(stderr, env->file, (ast)->match); \
            exit(1); }

#define foreach LIST_FOR
#define length LIST_LEN
#define ith LIST_ITEM
#define append APPEND
#define gcc_type(ctx,e) gcc_jit_context_get_type(ctx, GCC_JIT_TYPE_ ## e)

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

static gcc_jit_rvalue *add_value(env_t *env, gcc_jit_block **block, ast_t *ast);
static void add_statement(env_t *env, gcc_jit_block **block, ast_t *ast);

static gcc_jit_rvalue *add_fncall(env_t *env, gcc_jit_block **block, ast_t *ast)
{
    gcc_jit_rvalue *fn = add_value(env, block, ast->call.fn);
    NEW_LIST(gcc_jit_rvalue*, arg_vals);
    // TODO: keyword args
    foreach (ast->call.args, arg, _) {
        gcc_jit_rvalue *val = add_value(env, block, *arg);
        append(arg_vals, val);
    }
    gcc_jit_rvalue *call = gcc_jit_context_new_call_through_ptr(
        env->ctx, NULL, fn, length(arg_vals), arg_vals[0]);
    return call;
}

static void check_nil(env_t *env, gcc_jit_block *block, bl_type_t *t, gcc_jit_rvalue *obj, gcc_jit_block *nil_block, gcc_jit_block *nonnil_block)
{
    if (t->kind != OptionalType) {
        gcc_jit_block_end_with_jump(block, NULL, nonnil_block);
        return;
    }

    gcc_jit_function *func = gcc_jit_block_get_function(block);
    gcc_jit_lvalue *nil = gcc_jit_function_new_local(
        func, NULL, bl_type_to_gcc(env->ctx, t), "nil");
    gcc_jit_block_add_assignment(block, NULL, nil, nil_value(env->ctx, t));

    gcc_jit_rvalue *is_nil = gcc_jit_context_new_comparison(
        env->ctx, NULL, GCC_JIT_COMPARISON_EQ, obj, gcc_jit_lvalue_as_rvalue(nil));
    gcc_jit_block_end_with_conditional(block, NULL, is_nil, nil_block, nonnil_block);
}

// static CORD check_truthiness(env_t *env, CORD *code, ast_t *val, CORD truthy_label, CORD falsey_label)
// {
//     bl_type_t *t = get_type(env->file, env->bindings, val);
//     add_line(code, "# Check truthiness of:");
//     CORD val_reg = add_value(env, code, val);
//     if (t->kind == OptionalType) {
//         check_nil(env, code, t, val_reg, falsey_label, truthy_label);
//         return val_reg;
//     }

//     if (t->kind != BoolType)
//         ERROR(env, val, "This value has type %s, which means it will always be truthy", type_to_string(t)); 
    
//     add_line(code, "jnz %r, %r, %r", val_reg, truthy_label, falsey_label);
//     return val_reg;
// }

static gcc_jit_function *get_tostring_func(env_t *env, bl_type_t *t)
{
    gcc_jit_function *func = hashmap_get(env->tostring_funcs, t);
    if (func) return func;

    gcc_jit_type *gcc_t = bl_type_to_gcc(env->ctx, t);

    gcc_jit_param *params[2] = {
        gcc_jit_context_new_param(env->ctx, NULL, gcc_t, "obj"),
        gcc_jit_context_new_param(env->ctx, NULL, gcc_type(env->ctx, VOID_PTR), "stack"),
    };
    func = gcc_jit_context_new_function(
        env->ctx, NULL, GCC_JIT_FUNCTION_INTERNAL, gcc_type(env->ctx, CONST_CHAR_PTR),
        "tostring", 2, params, 0);
    hashmap_set(env->tostring_funcs, t, func);

    gcc_jit_block *block = gcc_jit_function_new_block(func, NULL);
    gcc_jit_rvalue *obj = gcc_jit_param_as_rvalue(params[0]);
    
    switch (t->kind) {
    case NilType: {
        gcc_jit_block_end_with_return(block, NULL,
            gcc_jit_context_new_string_literal(env->ctx, "(nil)"));
        break;
    }
    case BoolType: {
        gcc_jit_block *yes_block = gcc_jit_function_new_block(func, NULL);
        gcc_jit_block *no_block = gcc_jit_function_new_block(func, NULL);
        gcc_jit_block_end_with_conditional(block, NULL, obj, yes_block, no_block);
        gcc_jit_block_end_with_return(yes_block, NULL,
            gcc_jit_context_new_string_literal(env->ctx, "yes"));
        gcc_jit_block_end_with_return(no_block, NULL,
            gcc_jit_context_new_string_literal(env->ctx, "no"));
        break;
    }
    case IntType: case Int32Type: case Int16Type: case Int8Type: case NumType: case Num32Type: {
        const char *fmt;
        switch (t->kind) {
        case Int32Type: case Int16Type: case Int8Type: fmt = "%d"; break;
        case NumType: case Num32Type: fmt = "%g"; break;
        default: fmt = "%ld"; break;
        }

        gcc_jit_lvalue *str = gcc_jit_function_new_local(
            func, NULL, gcc_type(env->ctx, CONST_CHAR_PTR), "str");
        gcc_jit_rvalue *args[] = {
            gcc_jit_lvalue_get_address(str, NULL),
            gcc_jit_context_new_string_literal(env->ctx, fmt),
            obj,
        };
        (void)gcc_jit_context_new_call(env->ctx, NULL, gcc_CORD_sprintf, 3, args);
        gcc_jit_block_end_with_return(block, NULL, gcc_jit_lvalue_as_rvalue(str));
        break;
    }
    case OptionalType: {
        gcc_jit_block *nil_block = gcc_jit_function_new_block(func, NULL);
        gcc_jit_block *nonnil_block = gcc_jit_function_new_block(func, NULL);
        check_nil(env, block, t, obj, nil_block, nonnil_block);


        gcc_jit_block_end_with_return(nil_block, NULL,
            gcc_jit_context_new_string_literal(env->ctx, "(nil)"));

        gcc_jit_rvalue *args[] = {
            obj,
            gcc_jit_param_as_rvalue(params[1]),
        };
        gcc_jit_rvalue *ret = gcc_jit_context_new_call(env->ctx, NULL, get_tostring_func(env, t->nonnil), 2, args);
        gcc_jit_block_end_with_return(nonnil_block, NULL, ret);
        break;
    }
    default: {
        fprintf(stderr, "\x1b[31;1mtostring(%s) function is not yet implemented!\n", type_to_string(t));
        exit(1);
    }
    }
    return func;
}

// static CORD compile_function(env_t *env, CORD *code, ast_t *def)
// {
//     binding_t *binding = hashmap_get(env->bindings, def->fn.name);

//     env_t body_env = *env;
//     // Use a set of bindings that don't include any closures
//     body_env.bindings = global_bindings(env->bindings);

//     bl_type_t *t;
//     CORD fn_reg;
//     if (binding) {
//         hashmap_set(body_env.bindings, def->fn.name, binding);
//         fn_reg = binding->reg;
//         t = binding->type;
//     } else {
//         fn_reg = fresh_global(&body_env, "lambda");
//         t = get_type(body_env.file, body_env.bindings, def);
//     }
//     add_line(code, "# %s", type_to_string(t));
//     addf(code, "function %c %r(", base_type_for(t->ret), fn_reg);
//     for (int64_t i = 0; i < length(def->fn.arg_names); i++) {
//         if (i > 0) addf(code, ", ");
//         istr_t argname = ith(def->fn.arg_names, i);
//         bl_type_t *argtype = ith(t->args, i);
//         CORD argreg = fresh_local(&body_env, argname);
//         addf(code, "%c %s", base_type_for(argtype), argreg);
//         assert(argtype);
//         hashmap_set(body_env.bindings, argname, new(binding_t, .reg=argreg, .type=argtype));
//     }
//     addf(code, ") {\n@start\n");
//     add_statement(&body_env, code, def->fn.body);
//     if (!ends_with_jump(code))
//         add_line(code, "ret 0");
//     add_line(code, "}");
//     return fn_reg;
// }

// static void set_nil(CORD *code, bl_type_t *t, CORD reg)
// {
//     add_line(code, "%r =%c copy %s", reg, base_type_for(t), nil_value(t));
// }

static gcc_jit_lvalue *get_lvalue(env_t *env, gcc_jit_block **block, ast_t *ast)
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

gcc_jit_rvalue *add_value(env_t *env, gcc_jit_block **block, ast_t *ast)
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
        binding_t *binding = hashmap_get(env->bindings, ast->lhs->str);
        assert(binding);
        gcc_jit_rvalue *rval = add_value(env, block, ast->rhs);
        gcc_jit_block_add_assignment(*block, NULL, binding->lval, rval);
        return gcc_jit_lvalue_as_rvalue(binding->lval);
    }
    case Assign: {
        int64_t len = length(ast->multiassign.lhs);
        NEW_LIST(gcc_jit_lvalue*, lvals);
        foreach (ast->multiassign.lhs, lhs, _) {
            append(lvals, get_lvalue(env, block, *lhs));
        }
        NEW_LIST(gcc_jit_rvalue*, rvals);
        for (int64_t i = 0; i < len; i++) {
            bl_type_t *t_lhs = get_type(env->file, env->bindings, ith(ast->multiassign.lhs, i));
            bl_type_t *t_rhs = get_type(env->file, env->bindings, ith(ast->multiassign.rhs, i));
            if (!type_is_a(t_rhs, t_lhs)) {
                ERROR(env, ith(ast->multiassign.rhs, i), "This value is a %s, but it needs to be a %s",
                      type_to_string(t_rhs), type_to_string(t_lhs));
            }

            gcc_jit_rvalue *rval = add_value(env, block, ith(ast->multiassign.rhs, i));
            if (len > 1) {
                gcc_jit_function *func = gcc_jit_block_get_function(*block);
                gcc_jit_lvalue *tmp = gcc_jit_function_new_local(
                    func, NULL, bl_type_to_gcc(env->ctx, t_rhs), "tmp");
                gcc_jit_block_add_assignment(*block, NULL, tmp, rval);
                append(rvals, gcc_jit_lvalue_as_rvalue(tmp));
            } else {
                append(rvals, rval);
            }
        }
        for (int64_t i = 0; i < len; i++)
            gcc_jit_block_add_assignment(*block, NULL, ith(lvals, i), ith(rvals, i));
        return ith(rvals, length(rvals)-1);
    }
    case Block: {
        env_t block_env = *env;
        block_env.bindings = hashmap_new();
        block_env.bindings->fallback = env->bindings;

        // Function defs are visible in the entire block (allowing corecursive funcs)
        // foreach (ast->children, stmt, last_stmt) {
        //     if ((*stmt)->kind == FunctionDef) {
        //         CORD reg = fresh_global(env, (*stmt)->fn.name);
        //         bl_type_t *t = get_type(env->file, block_env.bindings, *stmt);
        //         assert(t);
        //         hashmap_set(block_env.bindings, (*stmt)->fn.name, new(binding_t, .reg=reg, .type=t, .is_global=true));
        //     }
        // }

        foreach (ast->children, stmt, last_stmt) {
            // Declarations are visible from here onwards:
            if ((*stmt)->kind == Declare) {
                bl_type_t *t = get_type(block_env.file, block_env.bindings, (*stmt)->rhs);
                assert(t);
                gcc_jit_type *gcc_t = bl_type_to_gcc(env->ctx, t);
                gcc_jit_function *func = gcc_jit_block_get_function(*block);
                gcc_jit_lvalue *lval = gcc_jit_function_new_local(
                    func, NULL, gcc_t, (*stmt)->lhs->str);
                hashmap_set(block_env.bindings, (*stmt)->lhs->str,
                            new(binding_t, .lval=lval, .rval=gcc_jit_lvalue_as_rvalue(lval), .type=t));
            }
            if (stmt == last_stmt) {
                return add_value(&block_env, block, *stmt);
            } else {
                add_statement(&block_env, block, *stmt);
            }
        }
        errx(1, "Unreachable");
    }
    // case FunctionDef: case Lambda: {
    //     CORD fn_code = NULL;
    //     CORD fn_reg = compile_function(env, &fn_code, ast);
    //     addf(env->fn_code, "\n%r\n", fn_code);
    //     return fn_reg;
    // }
    // case Return: {
    //     if (ast->child) {
    //         CORD ret = add_value(env, code, ast->child);
    //         add_line(code, "ret %r", ret);
    //     } else {
    //         add_line(code, "ret");
    //     }
    //     add_line(code, "%r", fresh_label(env, "unreachable_after_ret"));
    //     return "0";
    // }
    case Int: {
        return gcc_jit_context_new_rvalue_from_long(
            env->ctx, gcc_type(env->ctx, INT64_T), ast->i);
    }
    case Num: {
        return gcc_jit_context_new_rvalue_from_long(
            env->ctx, gcc_type(env->ctx, DOUBLE), ast->n);
    }
    case StringLiteral: {
        return gcc_jit_context_new_string_literal(env->ctx, ast->str);
    }
    case StringJoin: {
        gcc_jit_rvalue *str = gcc_jit_context_null(env->ctx, gcc_type(env->ctx, CONST_CHAR_PTR));
        foreach (ast->children, chunk, _) {
            gcc_jit_rvalue *val;
            bl_type_t *t = get_type(env->file, env->bindings, *chunk);
            if (t->kind == StringType) {
                val = add_value(env, block, *chunk);
            } else {
                gcc_jit_function *tostring = get_tostring_func(env, t);
                gcc_jit_rvalue *args[] = {
                    add_value(env, block, *chunk),
                    gcc_jit_context_null(env->ctx, gcc_type(env->ctx, VOID_PTR)),
                };
                val = gcc_jit_context_new_call(env->ctx, NULL, tostring, 2, args);
            }
            str = gcc_jit_context_new_call(
                env->ctx, NULL, gcc_CORD_cat, 2, (gcc_jit_rvalue*[]){str, val});
        }
        str = gcc_jit_context_new_call(
            env->ctx, NULL, gcc_CORD_to_char_star, 1, &str);
        str = gcc_jit_context_new_call(
            env->ctx, NULL, gcc_intern_str, 1, &str);
        return str;
    }
    // case List: {
    //     CORD list = fresh_local(env, "list");
    //     bl_type_t *t = get_type(env->f, env->bindings, ast);
    //     size_t item_size;
    //     switch (abi_type_for(t->item_type)) {
    //     case 'd': case 'l': item_size = 8; break;
    //     case 's': case 'w': item_size = 4; break;
    //     case 'h': item_size = 2; break;
    //     case 'b': item_size = 1; break;
    //     default: errx(1, "Unknown abi type: %c", abi_type_for(t->item_type));
    //     }
    //     add_line(code, "%r =l call $GC_malloc(l %ld) # Allocate list", list, sizeof(list_t));
    //     if (!ast->list.items)
    //         return list;

    //     CORD item_addr = fresh_local(env, "item_addr");
    //     foreach (ast->list.items, item, _) {
    //         switch (item->kind) {
    //         case For: case While: case Repeat: case If: {
    //             errx(1, "Comprehensions not yet implemented");
    //         }
    //         default: {
    //             CORD item_reg = add_value(env, code, item);
    //             add_line(code, "call $list_insert(l %r, l %ld, l %ld, l %r, l 0)",
    //                      list, item_size, INT_NIL, item_loc);
    //         }
    //         }
    //     }
    //     return list;
    // }
    case FunctionCall: {
        return add_fncall(env, block, ast);
    }
    case KeywordArg: {
        return add_value(env, block, ast->named.value);
    }
    // case Bool: {
    //     return ast->b ? "1" : "0";
    // }
    // case Cast: {
    //     bl_type_t *raw_t = get_type(env->file, env->bindings, ast->expr);
    //     bl_type_t *cast_t = get_type(env->file, env->bindings, ast->type);
    //     CORD raw_reg = add_value(env, code, ast->expr);
    //     char raw_base = base_type_for(raw_t);
    //     char cast_base = base_type_for(cast_t);
    //     if (raw_base == cast_base)
    //         return raw_reg;
    //     CORD cast_reg = fresh_local(env, "cast");
    //     if (raw_base == 's' && cast_base == 'd')
    //         add_line(code, "%r =d exts %r", cast_reg, raw_reg);
    //     else if (raw_base == 'd' && cast_base == 's')
    //         add_line(code, "%r =s truncd %r", cast_reg, raw_reg);
    //     else if (raw_base == 's' || raw_base == 'd')
    //         add_line(code, "%r =%c %ctosi %r", cast_reg, cast_base, raw_base, raw_reg);
    //     else if (cast_base == 's' || cast_base == 'd')
    //         add_line(code, "%r =%c s%ctof %r", cast_reg, cast_base, raw_base, raw_reg);
    //     else if (raw_base == 'w' && cast_base == 'l')
    //         add_line(code, "%r =%c exts%c %r", cast_reg, cast_base, abi_type_for(raw_t), raw_reg);
    //     else if (raw_base == 'l' && cast_base == 'w')
    //         add_line(code, "%r =%c copy %r", cast_reg, cast_base, raw_reg);
    //     return cast_reg;
    // }
    // case Nil: {
    //     // TODO: different nil type values
    //     return "0";
    // }
    // case Equal: case NotEqual: {
    //     bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
    //     bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
    //     if (!(type_is_a(lhs_t, rhs_t) || type_is_a(rhs_t, lhs_t)))
    //         ERROR(env, ast, "These two values have incompatible types: %s vs %s", type_to_string(lhs_t), type_to_string(rhs_t));
    //     CORD lhs_reg = add_value(env, code, ast->lhs);
    //     CORD rhs_reg = add_value(env, code, ast->rhs);
    //     add_line(code, "# Not Equals RHS: %r", rhs_reg);
    //     CORD result = fresh_local(env, "comparison");
    //     char b = base_type_for(lhs_t);
    //     add_line(code, "%r =w c%s%c %r, %r", result, ast->kind == Equal ? "eq" : "ne", b, lhs_reg, rhs_reg);
    //     return result;
    // }
    // case Less: case LessEqual: case Greater: case GreaterEqual: {
    //     bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
    //     bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
    //     CORD lhs_reg = add_value(env, code, ast->lhs);
    //     CORD rhs_reg = add_value(env, code, ast->rhs);
    //     CORD result = fresh_local(env, "comparison");
    //     const char *cmp = NULL;
    //     switch (ast->kind) {
    //     case Less: cmp = "lt"; break;
    //     case LessEqual: cmp = "le"; break;
    //     case Greater: cmp = "gt"; break;
    //     case GreaterEqual: cmp = "ge"; break;
    //     default: break;
    //     }
    //     if (lhs_t == rhs_t && (lhs_t->kind == StringType || lhs_t->kind == DSLType)) {
    //         add_line(code, "%r =w call $strcmp(l %r, l %r)", result, lhs_reg, rhs_reg);
    //         add_line(code, "%r =w cs%sw %r, 0", result, cmp, result);
    //         return result;
    //     } else if (lhs_t == rhs_t && is_numeric(lhs_t)) {
    //         char b = base_type_for(lhs_t);
    //         const char *sign = (b == 's' || b == 'd') ? "" : "s";
    //         add_line(code, "%r =w c%s%s%c %r, %r", result, sign, cmp, b, lhs_reg, rhs_reg);
    //         return result;
    //     }
    //     ERROR(env, ast, "Ordered comparison is not supported for %s and %s", type_to_string(lhs_t), type_to_string(rhs_t));
    // }
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate:
    case Add: case Subtract: case Divide: case Multiply: case Modulus: {
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        static enum gcc_jit_binary_op ops[NUM_TYPES] = {
#define OP(bl,gcc) [bl]=GCC_JIT_BINARY_OP_ ## gcc
            OP(Add,PLUS), OP(Subtract,MINUS), OP(Multiply,MULT), OP(Divide,DIVIDE), OP(Modulus,MODULO),
            OP(AddUpdate,PLUS), OP(SubtractUpdate,MINUS), OP(MultiplyUpdate,MULT), OP(DivideUpdate,DIVIDE),
#undef OP
        };
        enum gcc_jit_binary_op op = ops[ast->kind];
        if (!op) ERROR(env, ast, "Unsupported math operation");

        if (AddUpdate <= ast->kind && ast->kind <= DivideUpdate) {
            gcc_jit_lvalue *lval = get_lvalue(env, block, ast->lhs);
            gcc_jit_rvalue *rval = add_value(env, block, ast->rhs);
            gcc_jit_block_add_assignment_op(*block, NULL, lval, op, rval);
            return gcc_jit_lvalue_as_rvalue(lval);
        } else {
            gcc_jit_rvalue *lhs_val = add_value(env, block, ast->lhs);
            gcc_jit_rvalue *rhs_val = add_value(env, block, ast->rhs);
            return gcc_jit_context_new_binary_op(env->ctx, NULL, op, bl_type_to_gcc(env->ctx, t), lhs_val, rhs_val);
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
    // case If: {
    //     CORD if_ret = fresh_local(env, "if_result");
    //     CORD endif = fresh_label(env, "endif");
    //     bl_type_t *t = get_type(env->file, env->bindings, ast);
    //     foreach (ast->clauses, clause, last_clause) {
    //         CORD iffalse = (clause < last_clause || ast->else_body) ? fresh_label(env, "if_false") : endif;

    //         CORD truthy_label = fresh_label(env, "if_true");
    //         ast_t *condition = clause->condition, *body = clause->body;
    //         CORD branch_val;
    //         if (condition->kind == Declare) {
    //             env_t branch_env = *env;
    //             branch_env.bindings = hashmap_new();
    //             branch_env.bindings->fallback = env->bindings;
    //             bl_type_t *t = get_type(env->file, env->bindings, condition);
    //             assert(t);
    //             CORD cond_reg = fresh_local(&branch_env, condition->lhs->str);
    //             binding_t b = {.reg=cond_reg, .type=t};
    //             hashmap_set(branch_env.bindings, condition->lhs->str, &b);
    //             check_truthiness(&branch_env, code, condition, truthy_label, iffalse);
    //             add_line(code, "%r", truthy_label);
    //             branch_val = add_value(&branch_env, code, body);
    //         } else {
    //             (void)check_truthiness(env, code, condition, truthy_label, iffalse);
    //             add_line(code, "%r", truthy_label);
    //             branch_val = add_value(env, code, body);
    //         }

    //         if (!ends_with_jump(code)) {
    //             if (t->kind != AbortType)
    //                 add_line(code, "%r =%c copy %r", if_ret, base_type_for(t), branch_val);
    //             add_line(code, "jmp %r", endif);
    //         }
    //         if (iffalse != endif)
    //             add_line(code, "%r", iffalse);
    //     }
    //     if (ast->else_body) {
    //         CORD branch_val = add_value(env, code, ast->else_body);
    //         if (t->kind != AbortType)
    //             add_line(code, "%r =%c copy %r", if_ret, base_type_for(t), branch_val);
    //     }
    //     add_line(code, "%r", endif);
    //     return t->kind == AbortType ? "0" : if_ret;
    // }
    // case While: case Repeat: {
    //     add_line(code, "\n# Loop");
    //     CORD loop_top = fresh_label(env, "loop_top");
    //     CORD loop_body = fresh_label(env, "loop_body");
    //     CORD loop_between = ast->loop.between ? fresh_label(env, "between") : NULL;
    //     CORD loop_end = fresh_label(env, "end_loop");

    //     add_line(code, "%r", loop_top);

    //     bl_type_t *t = get_type(env->file, env->bindings, ast);
    //     CORD ret_reg = (t->kind == NilType || t->kind == AbortType) ? NULL : fresh_local(env, "loop_value");
    //     if (ret_reg)
    //         set_nil(code, t, ret_reg);

    //     env_t loop_env = *env;
    //     loop_env.bindings = hashmap_new();
    //     loop_env.bindings->fallback = env->bindings;
    //     loop_env.loop_label = &(loop_label_t){
    //         .enclosing = env->loop_label,
    //         .name = intern_str(ast->kind == While ? "while" : "repeat"),
    //         .skip_label = loop_top,
    //         .stop_label = loop_end,
    //     };

    //     ast_t *cond = ast->loop.condition;
    //     binding_t cond_binding;
    //     if (cond && cond->kind == Declare) {
    //         bl_type_t *t = get_type(env->file, env->bindings, cond);
    //         assert(t);
    //         CORD cond_reg = fresh_local(&loop_env, cond->lhs->str);
    //         cond_binding = (binding_t){.reg=cond_reg, .type=t};
    //         hashmap_set(loop_env.bindings, cond->lhs->str, &cond_binding);
    //         check_truthiness(&loop_env, code, cond, loop_body, loop_end);
    //     } else if (cond) {
    //         (void)check_truthiness(&loop_env, code, cond, loop_body, loop_end);
    //     } else {
    //         add_line(code, "jmp %r", loop_body);
    //     }

    //     add_line(code, "%r", loop_body);
    //     ast_t *body = ast->loop.body;
    //     // Function defs are visible in the entire block (allowing corecursive funcs)
    //     foreach (body->children, stmt, last_stmt) {
    //         if ((*stmt)->kind == FunctionDef) {
    //             CORD reg = fresh_global(env, (*stmt)->fn.name);
    //             bl_type_t *t = get_type(env->file, loop_env.bindings, *stmt);
    //             assert(t);
    //             hashmap_set(loop_env.bindings, (*stmt)->fn.name, new(binding_t, .reg=reg, .type=t, .is_global=true));
    //         }
    //     }

    //     foreach (body->children, stmt, last_stmt) {
    //         // Declarations are visible from here onwards:
    //         if ((*stmt)->kind == Declare) {
    //             CORD reg = fresh_local(&loop_env, (*stmt)->lhs->str);
    //             bl_type_t *t = get_type(loop_env.file, loop_env.bindings, (*stmt)->rhs);
    //             assert(t);
    //             hashmap_set(loop_env.bindings, (*stmt)->lhs->str, new(binding_t, .reg=reg, .type=t));
    //         }
    //         if (stmt == last_stmt) {
    //             if (loop_env.debug)
    //                 add_line(code, "loc %d, %d", 1, get_line_number(loop_env.file, (*stmt)->match->start));
    //             CORD block_val = add_value(&loop_env, code, *stmt);
    //             if (ret_reg)
    //                 add_line(code, "%r =%c copy %r", ret_reg, base_type_for(t), block_val);
    //         } else {
    //             add_statement(&loop_env, code, *stmt);
    //         }
    //     }

    //     if (loop_between) {
    //         if (cond)
    //             (void)check_truthiness(&loop_env, code, cond, loop_between, loop_end);
    //         else
    //             add_line(code, "jmp %r", loop_between);

    //         add_line(code, "%r", loop_between);
    //         add_statement(&loop_env, code, ast->loop.between);
    //         add_line(code, "jmp %r", loop_body);
    //     } else {
    //         add_line(code, "jmp %r", loop_top);
    //     }
    //     add_line(code, "%r", loop_end);
    //     return ret_reg ? ret_reg : "0";
    // }
    // case Skip: case Stop: {
    //     CORD label = NULL;
    //     if (ast->str) {
    //         for (loop_label_t *lbl = env->loop_label; lbl; lbl = lbl->enclosing) {
    //             if (lbl->name == ast->str) {
    //                 label = ast->kind == Skip ? lbl->skip_label : lbl->stop_label;
    //                 break;
    //             }
    //         }
    //     } else {
    //         if (env->loop_label)
    //             label = ast->kind == Skip ? env->loop_label->skip_label : env->loop_label->stop_label;
    //     }
    //     if (!label) ERROR(env, ast, "I'm not sure what %s is referring to", ast->str);
    //     add_line(code, "jmp %r", label);
    //     return "0";
    // }
    // case Fail: {
    //     CORD reg = fresh_local(env, "fail_msg");
    //     if (ast->child) {
    //         add_line(code, "%r =l copy %r", reg, get_string_reg(env, intern_str("\x1b[31;7m")));
    //         CORD msg_reg = add_value(env, code, ast->child);
    //         add_line(code, "%r =l call $CORD_cat(l %r, l %r)", reg, reg, msg_reg);
    //         add_line(code, "%r =l call $CORD_cat(l %r, l %r)", reg, reg, get_string_reg(env, intern_str("\x1b[m\n\n")));
    //     } else {
    //         add_line(code, "%r =l copy %r", get_string_reg(env, intern_str("\x1b[31;7mA failure occurred\x1b[m\n\n")));
    //     }

    //     char *info = NULL;
    //     size_t size = 0;
    //     FILE *f = open_memstream(&info, &size);
    //     highlight_match(f, env->file, ast->match);
    //     fputc('\0', f);
    //     fflush(f);
    //     add_line(code, "%r =l call $CORD_cat(l %r, l %r)", reg, reg, get_string_reg(env, intern_str(info)));
    //     add_line(code, "%r =l call $CORD_to_char_star(l %r)", reg, reg);
    //     add_line(code, "call $fail(l %r)", reg, reg);
    //     fclose(f);
    //     return "0";
    // }
    default: break;
    }
    ERROR(env, ast, "Error: compiling is not yet implemented for %s", get_ast_kind_name(ast->kind)); 
}

void add_statement(env_t *env, gcc_jit_block **block, ast_t *ast) {
    check_discardable(env->file, env->bindings, ast);
    gcc_jit_rvalue *val = add_value(env, block, ast);
    gcc_jit_block_add_eval(*block, NULL, val);
}

gcc_jit_result *compile_file(gcc_jit_context *ctx, file_t *f, ast_t *ast, bool debug) {
    env_t env = {
        .ctx = ctx,
        .file = f,
        .bindings = hashmap_new(),
        .debug = debug,
    };

    bl_type_t *string_type = Type(StringType);
    bl_type_t *nil_type = Type(NilType);
    bl_type_t *say_type = Type(
        FunctionType,
        .args=LIST(bl_type_t*, string_type, Type(OptionalType, .nonnil=string_type)),
        .ret=nil_type);


    if (!gcc_CORD_sprintf) {
        gcc_jit_param *params[] = {
            gcc_jit_context_new_param(ctx, NULL, gcc_jit_type_get_pointer(gcc_type(ctx, CONST_CHAR_PTR)), "cord"),
            gcc_jit_context_new_param(ctx, NULL, gcc_type(ctx, CONST_CHAR_PTR), "fmt"),
        };
        gcc_CORD_sprintf = gcc_jit_context_new_function(
            ctx, NULL, GCC_JIT_FUNCTION_IMPORTED,
            gcc_type(ctx, INT),
            "CORD_sprintf", 2, params, 1);
    }

    if (!gcc_CORD_cat) {
        gcc_jit_param *params[] = {
            gcc_jit_context_new_param(ctx, NULL, gcc_type(ctx, CONST_CHAR_PTR), "a"),
            gcc_jit_context_new_param(ctx, NULL, gcc_type(ctx, CONST_CHAR_PTR), "b"),
        };
        gcc_CORD_cat = gcc_jit_context_new_function(
            ctx, NULL, GCC_JIT_FUNCTION_IMPORTED,
            gcc_type(ctx, CONST_CHAR_PTR),
            "CORD_cat", 2, params, 0);
    }

    if (!gcc_CORD_to_char_star) {
        gcc_jit_param *params[] = {
            gcc_jit_context_new_param(ctx, NULL, gcc_type(ctx, CONST_CHAR_PTR), "cord"),
        };
        gcc_CORD_to_char_star = gcc_jit_context_new_function(
            ctx, NULL, GCC_JIT_FUNCTION_IMPORTED,
            gcc_type(ctx, CONST_CHAR_PTR),
            "CORD_to_char_star", 1, params, 0);
    }

    if (!gcc_intern_str) {
        gcc_jit_param *params[] = {
            gcc_jit_context_new_param(ctx, NULL, gcc_type(ctx, CONST_CHAR_PTR), "str"),
        };
        gcc_intern_str = gcc_jit_context_new_function(
            ctx, NULL, GCC_JIT_FUNCTION_IMPORTED,
            gcc_type(ctx, CONST_CHAR_PTR),
            "intern_str", 1, params, 0);
    }

    if (!gcc_puts) {
        gcc_jit_param *param_str = gcc_jit_context_new_param(ctx, NULL, gcc_type(ctx, CONST_CHAR_PTR), "str");
        gcc_puts = gcc_jit_context_new_function(
            ctx, NULL, GCC_JIT_FUNCTION_IMPORTED,
            gcc_type(ctx, INT),
            "puts", 1, &param_str, 1);
    }

    gcc_jit_rvalue *puts_rvalue = gcc_jit_function_get_address(gcc_puts, NULL);
    hashmap_set(env.bindings, intern_str("say"), new(binding_t, .rval=puts_rvalue, .type=say_type));
#define DEFTYPE(t) hashmap_set(env.bindings, intern_str(#t), new(binding_t, .is_global=true, .rval=gcc_jit_context_new_string_literal(ctx, #t), .type=Type(TypeType, .type=Type(t##Type))));
    // Primitive types:
    DEFTYPE(Bool); DEFTYPE(Nil); DEFTYPE(Abort);
    DEFTYPE(Int); DEFTYPE(Int32); DEFTYPE(Int16); DEFTYPE(Int8);
    DEFTYPE(Num); DEFTYPE(Num32);
    DEFTYPE(String);
#undef DEFTYPE

    gcc_jit_function *main_func = gcc_jit_context_new_function(
        ctx, NULL, GCC_JIT_FUNCTION_EXPORTED, gcc_type(ctx, VOID),
        "main", 0, NULL, 0);

    gcc_jit_block *block = gcc_jit_function_new_block(main_func, NULL);
    add_statement(&env, &block, ast);
    gcc_jit_block_end_with_void_return(block, NULL);

    return gcc_jit_context_compile(ctx);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
