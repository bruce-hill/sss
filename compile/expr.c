// Compilation logic for expressions
#include <assert.h>
#include <bhash.h>
#include <libgccjit.h>
#include <bp/files.h>
#include <bp/printmatch.h>
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

static void compile_for_body(env_t *env, gcc_block_t **block, iterator_info_t *info, void *data) {
    auto for_loop = Match((ast_t*)data, For);
    if (for_loop->key)
        hashmap_set(env->bindings, for_loop->key, new(binding_t, .rval=info->key_rval, .type=info->key_type));

    if (for_loop->value)
        hashmap_set(env->bindings, for_loop->value, new(binding_t, .rval=info->value_rval, .type=info->value_type));

    if (for_loop->body) {
        if (get_type(env, for_loop->body)->tag != GeneratorType && env->comprehension_callback)
            env->comprehension_callback(env, block, for_loop->body, env->comprehension_userdata);
        else
            compile_block_statement(env, block, for_loop->body);
    }
}

static void compile_for_between(env_t *env, gcc_block_t **block, iterator_info_t *info, void *data) {
    (void)info;
    auto for_loop = Match((ast_t*)data, For);
    if (for_loop->between)
        compile_block_statement(env, block, for_loop->between);
}

static void compile_while_body(env_t *env, gcc_block_t **block, iterator_info_t *info, void *data) {
    (void)info;
    ast_t *ast = (ast_t*)data;
    ast_t *body = ast->tag == While ? Match(ast, While)->body : Match(ast, Repeat)->body;
    if (body) {
        if (get_type(env, body)->tag != GeneratorType && env->comprehension_callback)
            env->comprehension_callback(env, block, body, env->comprehension_userdata);
        else
            compile_block_statement(env, block, body);
    }
}

static void compile_while_between(env_t *env, gcc_block_t **block, iterator_info_t *info, void *data) {
    (void)info;
    ast_t *ast = (ast_t*)data;
    if (ast->tag == While) {
        auto loop = Match(ast, While);
        if (loop->between)
            compile_block_statement(env, block, loop->between);
    } else {
        auto loop = Match(ast, Repeat);
        if (loop->between)
            compile_block_statement(env, block, loop->between);
    }
}

gcc_rvalue_t *add_tag_to_value(env_t *env, bl_type_t *enum_type, bl_type_t *field_type, gcc_rvalue_t *rval, gcc_rvalue_t *tag_rval)
{
    gcc_type_t *gcc_tagged_t = bl_type_to_gcc(env, enum_type);
    gcc_struct_t *gcc_tagged_s = gcc_type_if_struct(gcc_tagged_t);

    auto tagged = Match(enum_type, TaggedUnionType);
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
            return gcc_lvalue_as_rvalue(binding->lval);
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
            return gcc_lvalue_as_rvalue(binding->lval);
        } else if (binding->func) {
            return gcc_get_func_address(binding->func, NULL);
        }

        return binding->rval;
    }
    case Int: {
        // TODO: check if literal exceeds numeric precision
        gcc_type_t *gcc_t = bl_type_to_gcc(env, get_type(env, ast));
        return gcc_rvalue_from_long(env->ctx, gcc_t, Match(ast, Int)->i);
    }
    case Num: {
        gcc_type_t *gcc_t = bl_type_to_gcc(env, get_type(env, ast));
        return gcc_rvalue_from_double(env->ctx, gcc_t, Match(ast, Num)->n);
    }
    default: break;
    }
    compile_err(env, ast, "I can't evaluate this value at compile-time. It needs to be a constant value.");
}

gcc_rvalue_t *compile_expr(env_t *env, gcc_block_t **block, ast_t *ast)
{
    switch (ast->tag) {
    case Var: {
        auto var = Match(ast, Var);
        binding_t *binding = hashmap_get(env->bindings, var->name);
        if (binding) {
            if (binding->rval)
                return binding->rval;
            if (binding->lval)
                return gcc_lvalue_as_rvalue(binding->lval);
            if (binding->func)
                return gcc_get_func_address(binding->func, NULL);
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
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *lval = gcc_local(func, ast_loc(env, ast), gcc_t, fresh(decl->name));
        binding_t *clobbered = hashmap_get_raw(env->bindings, decl->name);
        if (clobbered && clobbered->type_value)
            compile_err(env, ast, "This name is already being used for the name of a type (struct or enum) in the same block, "
                  "and I get confused if you try to redeclare the name of a namespace.");
        hashmap_set(env->bindings, decl->name,
                    new(binding_t, .lval=lval, .rval=gcc_lvalue_as_rvalue(lval), .type=t));
        assert(rval);
        gcc_assign(*block, ast_loc(env, ast), lval, rval);
        return gcc_lvalue_as_rvalue(lval);
    }

    case Extern: {
        auto ext = Match(ast, Extern);
        bl_type_t *t = parse_type(env, ext->type);
        if (t->tag == FunctionType) {
            auto fn = Match(t, FunctionType); 
            gcc_type_t *gcc_ret_t = bl_type_to_gcc(env, fn->ret);
            NEW_LIST(gcc_param_t*, params);
            for (int64_t i = 0, len = length(fn->arg_types); i < len; i++) {
                gcc_type_t *arg_t = bl_type_to_gcc(env, ith(fn->arg_types, i));
                istr_t arg_name = fn->arg_names ? ith(fn->arg_names, i) : fresh("arg");
                APPEND(params, gcc_new_param(env->ctx, NULL, arg_t, arg_name));
            }
            gcc_func_t *func = gcc_new_func(
                env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_ret_t, ext->name, length(params), params[0], 0);
            hashmap_set(env->bindings, ext->bl_name ? ext->bl_name : ext->name, new(binding_t, .func=func, .type=t, .is_global=true));
        } else {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
            gcc_rvalue_t *glob = gcc_lvalue_as_rvalue(gcc_global(env->ctx, ast_loc(env, ast), GCC_GLOBAL_IMPORTED, gcc_t, ext->name));
            hashmap_set(env->bindings, ext->bl_name ? ext->bl_name : ext->name, new(binding_t, .rval=glob, .type=t, .is_global=true));
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
            append(lvals, get_lvalue(env, block, *lhs));
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
        auto blocks = Match(ast, Do)->blocks;
        // TODO: support do/else?
        if (length(blocks) > 1)
            compile_err(env, ith(blocks, 1), "`do` statments with else clauses are not currently supported");
        return compile_expr(env, block, ith(blocks, 0));
    }
    case Block: {
        // Create scope:
        env_t block_env = *env;
        block_env.bindings = hashmap_new();
        block_env.bindings->fallback = env->bindings;
        return compile_block_expr(&block_env, block, ast);
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
        return gcc_get_func_address(func, NULL);
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
            gcc_return_void(*block, NULL);
        } else {
            if (!ret->value)
                compile_err(env, ast, "I was expecting this `return` to have a value of type %s because of the function's type signature, but no value is being returned here.",
                      type_to_string(env->return_type));
            bl_type_t *t = get_type(env, ret->value);
            gcc_rvalue_t *val = compile_expr(env, block, ret->value);
            if (!promote(env, t, &val, env->return_type))
                compile_err(env, ast, "I was expecting this `return` to have value of type %s because of the function's type signature, but this value has type %s",
                      type_to_string(env->return_type), type_to_string(t));

            gcc_return(*block, NULL, val);
        }
        *block = NULL;
        return NULL;
    }
    case Int: case Num: {
        return compile_constant(env, ast);
    }
    case Interp: {
        return compile_expr(env, block, Match(ast, Interp)->value);
    }
#define STRING_STRUCT(env, gcc_t, str_rval, len_rval, stride_rval) \
        gcc_struct_constructor(env->ctx, NULL, gcc_t, 3, (gcc_field_t*[]){ \
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
        gcc_rvalue_t *str_rval = gcc_new_string(env->ctx, str);
        gcc_rvalue_t *len_rval = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, INT32), strlen(str));
        gcc_func_t *intern_strn_func = hashmap_gets(env->global_funcs, "intern_strn");
        str_rval = gcc_call(env->ctx, NULL, intern_strn_func, 2,
                            (gcc_rvalue_t*[]){str_rval, gcc_cast(env->ctx, NULL, len_rval, gcc_type(env->ctx, SIZE))});
        gcc_rvalue_t *stride_rval = gcc_one(env->ctx, gcc_type(env->ctx, INT32));
        gcc_type_t *gcc_t = bl_type_to_gcc(env, Type(ArrayType, .item_type=Type(CharType)));
        return STRING_STRUCT(env, gcc_t, str_rval, len_rval, stride_rval);
    }
    case StringJoin: {
        auto chunks = Match(ast, StringJoin)->children;
        foreach (chunks, chunk, _) {
            bl_type_t *t = get_type(env, *chunk);
            if (t->tag == VoidType)
                compile_err(env, *chunk, "This expression doesn't have a value (it has a Void type), so you can't use it in a string."); 
        }

        gcc_type_t *gcc_t = bl_type_to_gcc(env, Type(ArrayType, .item_type=Type(CharType)));
        gcc_type_t *i32_t = gcc_type(env->ctx, INT32);

        gcc_func_t *open_memstream_fn = hashmap_gets(env->global_funcs, "open_memstream");
        gcc_func_t *fputs_fn = hashmap_gets(env->global_funcs, "fputs");
        gcc_func_t *fflush_fn = hashmap_gets(env->global_funcs, "fflush");
        gcc_func_t *fclose_fn = hashmap_gets(env->global_funcs, "fclose");
        gcc_func_t *intern_strn_func = hashmap_gets(env->global_funcs, "intern_strn");
        // Optimize the cases of 0 or 1 string chunks/interpolations
        if (length(chunks) == 0) {
            return STRING_STRUCT(env, gcc_t, gcc_null(env->ctx, gcc_type(env->ctx, STRING)), gcc_zero(env->ctx, i32_t), gcc_one(env->ctx, i32_t));
        } else if (length(chunks) == 1 && ith(chunks, 0)->tag == StringLiteral) {
            ast_t *child = ith(chunks, 0);
            return compile_expr(env, block, child);
        }

        // char *buf; size_t size;
        // FILE *f = open_memstream(&buf, &size);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *buf_var = gcc_local(func, NULL, gcc_type(env->ctx, STRING), fresh("buf"));
        gcc_lvalue_t *size_var = gcc_local(func, NULL, gcc_type(env->ctx, SIZE), fresh("size"));
        gcc_lvalue_t *file_var = gcc_local(func, NULL, gcc_type(env->ctx, FILE_PTR), fresh("file"));
        gcc_assign(*block, NULL, file_var,
                   gcc_call(env->ctx, NULL, open_memstream_fn, 2, (gcc_rvalue_t*[]){
                                gcc_lvalue_address(buf_var, NULL),
                                gcc_lvalue_address(size_var, NULL),
                            }));
        gcc_rvalue_t *file = gcc_lvalue_as_rvalue(file_var);

        foreach (chunks, chunk, _) {
            if ((*chunk)->tag == Interp) {
                auto interp = Match(*chunk, Interp);
                if (interp->labelled) {
                    match_t *m = interp->value->match;
                    char *buf; size_t size;
                    FILE *f = open_memstream(&buf, &size);
                    fprint_match(f, m->start, m, NULL);
                    fprintf(f, ": ");
                    fflush(f);
                    gcc_eval(*block, NULL, gcc_call(env->ctx, NULL, fputs_fn, 2, (gcc_rvalue_t*[]){gcc_new_string(env->ctx, buf), file}));
                    fclose(f);
                }
            }
            bl_type_t *t = get_type(env, *chunk);
            gcc_func_t *print_fn = get_print_func(env, t);
            assert(print_fn);
            gcc_rvalue_t *args[] = {
                compile_expr(env, block, *chunk),
                file,
                gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR)),
            };
            gcc_eval(*block, NULL, gcc_call(env->ctx, ast_loc(env, *chunk), print_fn, 3, args));
        }
        gcc_eval(*block, NULL, gcc_call(env->ctx, NULL, fflush_fn, 1, &file));
        gcc_rvalue_t *str = gcc_call(env->ctx, ast_loc(env, ast), intern_strn_func, 2, (gcc_rvalue_t*[]){
                                         gcc_lvalue_as_rvalue(buf_var),
                                         gcc_lvalue_as_rvalue(size_var),
                                     });
        gcc_lvalue_t *str_struct_var = gcc_local(func, NULL, gcc_t, fresh("str_final"));
        gcc_rvalue_t *len32 = gcc_cast(env->ctx, NULL, gcc_lvalue_as_rvalue(size_var), i32_t);
        gcc_assign(*block, NULL, str_struct_var, STRING_STRUCT(env, gcc_t, str, len32, gcc_one(env->ctx, i32_t)));
        gcc_eval(*block, NULL, gcc_call(env->ctx, NULL, fclose_fn, 1, &file));
        return gcc_lvalue_as_rvalue(str_struct_var);
    }
    case Array: {
        return compile_array(env, block, ast);
    }
    case EnumDef: {
        return NULL;
    }
    case StructDef: {
        auto struct_def = Match(ast, StructDef);
        bl_type_t *struct_t = get_binding(env, struct_def->name)->type_value;
        hashmap_t *namespace = get_namespace(env, struct_t);
        hashmap_t *globals = global_bindings(env->bindings);
        namespace->fallback = globals;

        env_t struct_env = *env;
        struct_env.bindings = namespace;
        env = &struct_env;

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
                istr_t global_name = intern_strf("%s__%s", struct_def->name, decl->name);
                gcc_lvalue_t *lval = gcc_global(env->ctx, ast_loc(env, (*member)), GCC_GLOBAL_INTERNAL, gcc_t, global_name);
                hashmap_set(env->bindings, decl->name,
                            new(binding_t, .lval=lval, .rval=gcc_lvalue_as_rvalue(lval), .type=t, .is_global=true));
                assert(rval);
                gcc_assign(*block, ast_loc(env, (*member)), lval, rval);
            }
        }

        return NULL;
    }
    case Struct: {
        auto struct_ = Match(ast, Struct);
        binding_t *binding = get_ast_binding(env, struct_->type);
        bl_type_t *t = binding->type_value;
        if (!t)
            compile_err(env, struct_->type, "This isn't a struct type that I recognize");

        size_t num_values = length(struct_->members);

        if (t->tag != StructType) {
            // For tagged enums, you can initialize with curly brace syntax (`Foo.Text{"hi"}`)
            // when it's just a value and not a struct
            if (num_values != 1)
                compile_err(env, ast, "I expected this to have exactly one value");
            ast_t *member_ast = ith(struct_->members, 0);
            gcc_rvalue_t *rval = compile_expr(env, block, Match(member_ast, StructField)->value);
            if (binding->enum_type)
                rval = add_tag_to_value(env, binding->enum_type, t, rval, binding->tag_rval);
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
        } entries[num_values];

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

        for (size_t field_index = 0; field_index < num_fields; field_index++) {
            bl_type_t *ft = ith(struct_type->field_types, field_index);
            if (ft->tag == PointerType && Match(ft, PointerType)->is_optional) continue;
            if (unused_fields[field_index])
                compile_err(env, ast, "%s structs are supposed to have a non-optional field '%s' (%s), but you didn't provide a value for it.",
                      type_to_string(t),
                      ith(struct_type->field_names, field_index), type_to_string(ft));
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

        gcc_rvalue_t *rval = gcc_struct_constructor(env->ctx, NULL, gcc_t, num_values, populated_fields, rvalues);
        assert(rval);

        if (binding->enum_type)
            rval = add_tag_to_value(env, binding->enum_type, t, rval, binding->tag_rval);

        return rval;
    }
    case FunctionCall: {
        auto call = Match(ast, FunctionCall);
        gcc_rvalue_t *fn_ptr = NULL;
        gcc_func_t *fn = NULL;
        auto fn_t = Match(get_type(env, call->fn), FunctionType);
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
                bl_type_t *fielded_type = parse_type(env, access->fielded);
                binding_t *type_binding = hashmap_get(env->bindings, fielded_type);
                binding_t *binding = (type_binding && type_binding->type_value) ? get_from_namespace(env, type_binding->type_value, access->field) : NULL;
                if (!binding)
                    compile_err(env, call->fn, "I couldn't find any method called %s for %s.", access->field, type_to_string(fielded_type));
                fn = binding->func;
                fn_ptr = binding->rval;
                break;
            }
            default: {
                binding_t *binding = get_from_namespace(env, value_type, access->field);
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
        bl_type_t *nonnil_t = get_type(env, value);
        return move_to_heap(env, block, nonnil_t, rval);
    }
    case Dereference: {
        (void)get_type(env, ast); // Check this is a pointer type
        ast_t *value = Match(ast, Dereference)->value;
        gcc_rvalue_t *rval = compile_expr(env, block, value);
        return gcc_lvalue_as_rvalue(gcc_rvalue_dereference(rval, ast_loc(env, ast)));
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
            return gcc_cast(env->ctx, NULL, gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, 1)), gcc_type(env->ctx, INT64));
        }
        case PointerType: {
            auto ptr = Match(t, PointerType);
            if (ptr->pointed == Type(CharType))
                goto string_len;
            else
                goto unknown_len;
        }
        case DSLType: case TypeType: {
          string_len:;
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
            obj = gcc_lvalue_as_rvalue(gcc_rvalue_dereference(obj, NULL));
            fielded_t = ptr->pointed;
            goto get_field;
        }
        case StructType: {
            gcc_type_t *gcc_t = bl_type_to_gcc(env, fielded_t);
            auto struct_type = Match(fielded_t, StructType);
            for (int64_t i = 0, len = length(struct_type->field_names); i < len; i++) {
                if (ith(struct_type->field_names, i) == access->field) {
                    gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);
                    gcc_field_t *field = gcc_get_field(gcc_struct, (size_t)i);
                    return gcc_rvalue_access_field(obj, NULL, field);
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
                    gcc_rvalue_t *tagged_tag = gcc_rvalue_access_field(obj, NULL, gcc_get_field(gcc_type_if_struct(gcc_t), 0));
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
                    gcc_rvalue_t *is_ok = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, tagged_tag, field_tag);
                    gcc_jump_condition(*block, NULL, is_ok, tag_ok, tag_wrong);

                    *block = tag_wrong;
                    char *info = NULL;
                    size_t size = 0;
                    FILE *f = open_memstream(&info, &size);
                    highlight_match(f, env->file, ast->match, 2);
                    fputc('\0', f);
                    fflush(f);
                    gcc_rvalue_t *callstack = gcc_new_string(env->ctx, info);
                    gcc_func_t *fail_array = hashmap_gets(env->global_funcs, "fail_array");
                    istr_t fmt_str = intern_strf("\x1b[31;1;7mError: this tagged union is a %%s, but you are treating it like a %s\x1b[m\n\n%%s", access->field);
                    gcc_rvalue_t *fmt = gcc_new_string(env->ctx, fmt_str);
                    gcc_func_t *print_fn = get_print_func(env, fielded_t);
                    assert(print_fn);
                    gcc_rvalue_t *args[] = {obj, gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR))};
                    gcc_rvalue_t *tag_str = gcc_call(env->ctx, NULL, print_fn, 2, args);
                    gcc_rvalue_t *failure = gcc_call(env->ctx, NULL, fail_array, 3, (gcc_rvalue_t*[]){fmt, tag_str, callstack});
                    gcc_eval(tag_wrong, NULL, failure);
                    fclose(f);
                    gcc_jump(tag_wrong, NULL, tag_wrong);

                    // Step 2: access tagged.__data.TagName
                    *block = tag_ok;
                    gcc_field_t *field = ith(union_type->fields, i);
                    gcc_rvalue_t *data = gcc_rvalue_access_field(obj, NULL, gcc_get_field(gcc_type_if_struct(gcc_t), 1));
                    return gcc_rvalue_access_field(data, NULL, field);
                }
            }
            break;
        }
        case TypeType: {
            binding_t *type_binding = get_ast_binding(env, access->fielded);
            bl_type_t *t = type_binding->type_value;
            assert(t);
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
        // (void)get_type(env, ast); // typecheck
        auto indexing = Match(ast, Index);
        bl_type_t *indexed_t = get_type(env, indexing->indexed);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, indexed_t);
        gcc_rvalue_t *obj = compile_expr(env, block, indexing->indexed);
        if (indexed_t->tag != ArrayType)
            compile_err(env, ast, "I only know how to index into lists, but this is a %s", type_to_string(indexed_t));

        bl_type_t *index_t = get_type(env, indexing->index);
        gcc_type_t *i64_t = gcc_type(env->ctx, INT64);
        gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
        gcc_rvalue_t *items = gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, 0));
        gcc_rvalue_t *index = compile_expr(env, block, indexing->index);
        gcc_rvalue_t *stride64 = gcc_cast(env->ctx, NULL, gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, 2)), i64_t);

        if (index_t->tag == RangeType) {
            // Slicing
            gcc_type_t *str_gcc_t = bl_type_to_gcc(env, Type(ArrayType, .item_type=Type(CharType)));
            gcc_func_t *slice_fn = hashmap_gets(env->global_funcs, "range_slice");
            return gcc_bitcast(
                env->ctx, NULL,
                gcc_call(
                    env->ctx, NULL, slice_fn, 3, (gcc_rvalue_t*[]){
                        gcc_bitcast(env->ctx, NULL, obj, str_gcc_t),
                        index,
                        gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), gcc_sizeof(env, Match(indexed_t, ArrayType)->item_type)),
                    }),
                gcc_t);
        } else {
            // Bounds check:
            gcc_rvalue_t *big_enough = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GE, index, gcc_one(env->ctx, i64_t));
            gcc_rvalue_t *len64 = gcc_cast(env->ctx, NULL, gcc_rvalue_access_field(obj, NULL, gcc_get_field(array_struct, 1)), i64_t);
            gcc_rvalue_t *small_enough = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LE, index, len64);
            gcc_rvalue_t *ok = gcc_binary_op(env->ctx, NULL, GCC_BINOP_LOGICAL_AND, gcc_type(env->ctx, BOOL), big_enough, small_enough);

            gcc_func_t *func = gcc_block_func(*block);
            gcc_block_t *bounds_safe = gcc_new_block(func, fresh("bounds_safe")),
                        *bounds_unsafe = gcc_new_block(func, fresh("bounds_unsafe"));
            gcc_jump_condition(*block, NULL, ok, bounds_safe, bounds_unsafe);

            // Bounds check failure:
            gcc_rvalue_t *fmt = gcc_new_string(env->ctx, "\x1b[31;1;7mError: invalid list index: %ld (list is size %ld)\x1b[m\n\n%s");
            char *info = NULL;
            size_t size = 0;
            FILE *f = open_memstream(&info, &size);
            highlight_match(f, env->file, ast->match, 2);
            fputc('\0', f);
            fflush(f);
            gcc_rvalue_t *callstack = gcc_new_string(env->ctx, info);
            gcc_func_t *fail = hashmap_gets(env->global_funcs, "fail");
            gcc_eval(bounds_unsafe, NULL, gcc_call(env->ctx, NULL, fail, 4, (gcc_rvalue_t*[]){fmt, index, len64, callstack}));
            fclose(f);
            gcc_jump(bounds_unsafe, NULL, bounds_unsafe);

            // Bounds check success:
            *block = bounds_safe;
            gcc_rvalue_t *index0 = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, i64_t, index, gcc_one(env->ctx, i64_t));
            index0 = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, i64_t, index0, stride64);
            return gcc_lvalue_as_rvalue(gcc_array_access(env->ctx, NULL, items, index0));
        }
    }
    case TypeOf: {
        auto value = Match(ast, TypeOf)->value;
        gcc_func_t *intern_str_func = hashmap_gets(env->global_funcs, "intern_str");
        bl_type_t *t = get_type(env, value);
        return gcc_call(env->ctx, NULL, intern_str_func, 1,
                        (gcc_rvalue_t*[]){gcc_new_string(env->ctx, type_to_string(t))});
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
        bl_type_t *t = get_type(env, ast);
        return gcc_bitcast(env->ctx, NULL, val, bl_type_to_gcc(env, t));
    }
    case As: {
        auto as = Match(ast, As);
        gcc_rvalue_t *val = compile_expr(env, block, as->value);
        bl_type_t *t = get_type(env, ast);
        return gcc_cast(env->ctx, NULL, val, bl_type_to_gcc(env, t));
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
        bool pointers_allowed = false;
        switch (ast->tag) {
        case Equal: cmp = GCC_COMPARISON_EQ; pointers_allowed = true; break;
        case NotEqual: cmp = GCC_COMPARISON_NE; pointers_allowed = true; break;
        case Less: cmp = GCC_COMPARISON_LT; break;
        case LessEqual: cmp = GCC_COMPARISON_LE; break;
        case Greater: cmp = GCC_COMPARISON_GT; break;
        case GreaterEqual: cmp = GCC_COMPARISON_GE; break;
        default: assert(false);
        }

        bl_type_t *lhs_t = get_type(env, lhs);
        bl_type_t *rhs_t = get_type(env, rhs);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
        coerce_numbers(env, &lhs_t, &lhs_val, &rhs_t, &rhs_val);
        if (lhs_t != rhs_t)
            compile_err(env, ast, "I don't know how to do a comparison between a %s and a %s.", type_to_string(lhs_t), type_to_string(rhs_t));

        if (!pointers_allowed && has_pointer(lhs_t))
            compile_err(env, ast, "Ordered comparisons are only supported between value types (structs, integers, etc.), but this isn't a value type: %s",
                  type_to_string(lhs_t));

        if (is_numeric(lhs_t) || lhs_t->tag == PointerType)
            return gcc_comparison(env->ctx, NULL, cmp, lhs_val, rhs_val);

        return gcc_comparison(env->ctx, NULL, cmp, compare_values(env, lhs_t, lhs_val, rhs_val),
                              gcc_zero(env->ctx, gcc_type(env->ctx, INT)));
    }
    case Negative: {
        ast_t *value = Match(ast, Negative)->value;
        bl_type_t *t = get_type(env, value);
        if (!is_numeric(t))
            compile_err(env, ast, "I only know how to negate numbers, not %s", type_to_string(t));
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_rvalue_t *rval = compile_expr(env, block, value);
        return gcc_unary_op(env->ctx, NULL, GCC_UNOP_MINUS, gcc_t, rval);
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
        gcc_lvalue_t *result = gcc_local(func, NULL, bl_type_to_gcc(env, t), fresh("and_result"));
        gcc_block_t *if_truthy = gcc_new_block(func, fresh("and_truthy"));
        gcc_block_t *if_falsey = gcc_new_block(func, fresh("and_falsey"));
        gcc_block_t *done = gcc_new_block(func, fresh("and_done"));

        gcc_type_t *lhs_gcc_t = bl_type_to_gcc(env, lhs_t);
        gcc_rvalue_t *bool_val = lhs_val;
        if (t->tag != BoolType) {
            gcc_rvalue_t *zero = gcc_type_if_pointer(lhs_gcc_t) ? gcc_null(env->ctx, lhs_gcc_t) : gcc_zero(env->ctx, lhs_gcc_t);
            bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, lhs_val, zero);
        }
        gcc_jump_condition(*block, NULL, bool_val, if_truthy, if_falsey);

        if (rhs_t->tag == AbortType) {
            compile_statement(env, &if_truthy, rhs);
        } else {
            gcc_rvalue_t *rhs_val = compile_expr(env, &if_truthy, rhs);
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
        gcc_lvalue_t *result = gcc_local(func, NULL, bl_type_to_gcc(env, t), fresh("and_result"));
        gcc_block_t *if_truthy = gcc_new_block(func, fresh("or_truthy"));
        gcc_block_t *if_falsey = gcc_new_block(func, fresh("or_falsey"));
        gcc_block_t *done = gcc_new_block(func, fresh("or_done"));

        gcc_type_t *lhs_gcc_t = bl_type_to_gcc(env, lhs_t);
        gcc_rvalue_t *bool_val = lhs_val;
        if (t->tag != BoolType) {
            gcc_rvalue_t *zero = gcc_type_if_pointer(lhs_gcc_t) ? gcc_null(env->ctx, lhs_gcc_t) : gcc_zero(env->ctx, lhs_gcc_t);
            bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, lhs_val, zero);
        }
        gcc_jump_condition(*block, NULL, bool_val, if_truthy, if_falsey);

        gcc_assign(if_truthy, NULL, result, lhs_val);
        gcc_jump(if_truthy, NULL, done);

        if (rhs_t->tag == AbortType) {
            compile_statement(env, &if_falsey, rhs);
        } else {
            gcc_rvalue_t *rhs_val = compile_expr(env, &if_falsey, rhs);
            if (if_falsey)
                gcc_assign(if_falsey, NULL, result, rhs_val);
        }
        if (if_falsey)
            gcc_jump(if_falsey, NULL, done);

        *block = done;
        return gcc_lvalue_as_rvalue(result);
    }
    case Xor: {
        ast_t *lhs = Match(ast, Xor)->lhs, *rhs = Match(ast, Xor)->rhs;
        bl_type_t *t = get_type(env, ast);
        gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
        gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
        return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_BITWISE_XOR, bl_type_to_gcc(env, t), lhs_val, rhs_val);
    }
    case AddUpdate: case SubtractUpdate: case DivideUpdate: case MultiplyUpdate:
    case Add: case Subtract: case Divide: case Multiply: {
        bool is_update = false;
        gcc_binary_op_e op;
        switch (ast->tag) {
        case AddUpdate: op = GCC_BINOP_PLUS; is_update = true; break;
        case Add: op = GCC_BINOP_PLUS; break;
        case SubtractUpdate: op = GCC_BINOP_MINUS; is_update = true; break;
        case Subtract: op = GCC_BINOP_MINUS; break;
        case MultiplyUpdate: op = GCC_BINOP_MULT; is_update = true; break;
        case Multiply: op = GCC_BINOP_MULT; break;
        case DivideUpdate: op = GCC_BINOP_DIVIDE; is_update = true; break;
        case Divide: op = GCC_BINOP_DIVIDE; break;
        default: compile_err(env, ast, "Unsupported math operation");
        }

        // Unsafe! This assumes each of these types has the same tagged union struct layout. It saves some duplicated code.
        ast_t *lhs = ast->__data.Add.lhs, *rhs = ast->__data.Add.rhs;
        // End unsafe

        bl_type_t *lhs_t = get_type(env, lhs);
        bl_type_t *rhs_t = get_type(env, rhs);
        if (is_update) {
            gcc_lvalue_t *lhs_val = get_lvalue(env, block, lhs);
            gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);

            if (!promote(env, rhs_t, &rhs_val, lhs_t))
                compile_err(env, ast, "The left hand side of this assignment has type %s, but the right hand side has type %s and I can't figure out how to combine them without losing precision.",
                      type_to_string(lhs_t), type_to_string(rhs_t));
            if (!is_numeric(lhs_t))
                compile_err(env, lhs, "I've only implemented numeric math operations, but this type is %s", type_to_string(lhs_t));
            gcc_update(*block, ast_loc(env, ast), lhs_val, op, rhs_val);
            return gcc_lvalue_as_rvalue(lhs_val);
        } else {
            gcc_rvalue_t *lhs_val = compile_expr(env, block, lhs);
            gcc_rvalue_t *rhs_val = compile_expr(env, block, rhs);
            // Numeric promotion:
            bl_type_t *t = NULL;
            if (promote(env, lhs_t, &lhs_val, rhs_t))
                t = rhs_t;
            else if (promote(env, rhs_t, &rhs_val, lhs_t))
                t = lhs_t;
            else
                compile_err(env, ast, "The left hand side of this assignment has type %s, but the right hand side has type %s and I can't figure out how to combine them.",
                      type_to_string(lhs_t), type_to_string(rhs_t));
            if (!is_numeric(t))
                compile_err(env, ast, "I've only implemented numeric math operations, but this type is %s", type_to_string(t));
            return gcc_binary_op(env->ctx, ast_loc(env, ast), op, bl_type_to_gcc(env, t), lhs_val, rhs_val);
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
            compile_err(env, ast, "The left hand side of this assignment has type %s, but the right hand side has type %s and I can't figure out how to combine them.",
                  type_to_string(lhs_t), type_to_string(rhs_t));
        if (t->tag == NumType || t->tag == Num32Type) {
            gcc_func_t *sane_fmod_func = hashmap_gets(env->global_funcs, "sane_fmod");
            return gcc_call(env->ctx, NULL, sane_fmod_func, 2, (gcc_rvalue_t*[]){lhs_val, rhs_val});
        } else {
            return gcc_binary_op(env->ctx, ast_loc(env, ast), GCC_BINOP_MODULO, bl_type_to_gcc(env, t), lhs_val, rhs_val);
        }
    }
    case If: {
        bl_type_t *if_t = get_type(env, ast);
        auto if_ = Match(ast, If);
        bool has_value = !(if_t->tag == GeneratorType || if_t->tag == AbortType || if_t->tag == VoidType);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *if_ret = has_value ? gcc_local(func, NULL, bl_type_to_gcc(env, if_t), fresh("if_value")) : NULL;

        gcc_block_t *end_if = if_t->tag == AbortType ? NULL : gcc_new_block(func, fresh("endif"));

        foreach (if_->clauses, clause, last_clause) {
            gcc_block_t *if_truthy = gcc_new_block(func, fresh("if_true"));
            gcc_block_t *if_falsey = gcc_new_block(func, fresh("elseif"));

            ast_t *condition = clause->condition, *body = clause->body;
            env_t branch_env = *env;
            branch_env.bindings = hashmap_new();
            branch_env.bindings->fallback = env->bindings;
            assert(if_truthy && if_falsey);
            check_truthiness(&branch_env, block, condition, if_truthy, if_falsey);

            if (if_t->tag == GeneratorType && env->comprehension_callback) {
                if (!env->comprehension_callback)
                    compile_err(env, ast, "This 'if' is being used as a value, but it might not have a value");
                env->comprehension_callback(&branch_env, &if_truthy, body, env->comprehension_userdata);
                assert(end_if);
                if (if_truthy)
                    gcc_jump(if_truthy, NULL, end_if);
                *block = if_falsey;
                continue;
            }

            gcc_rvalue_t *branch_val = compile_expr(&branch_env, &if_truthy, body);
            if (if_truthy) {
                if (branch_val) {
                    if (has_value) {
                        bl_type_t *actual = get_type(env, body);
                        if (!promote(env, actual, &branch_val, if_t))
                            compile_err(env, body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
                                  type_to_string(if_t), type_to_string(actual));
                        gcc_assign(if_truthy, NULL, if_ret, branch_val);
                    } else {
                        gcc_eval(if_truthy, NULL, branch_val);
                    }
                }
                assert(end_if);
                gcc_jump(if_truthy, NULL, end_if);
            }
            *block = if_falsey;
        }
        if (if_->else_body) {
            gcc_rvalue_t *branch_val = compile_expr(env, block, if_->else_body);
            if (branch_val) {
                if (has_value) {
                    bl_type_t *actual = get_type(env, if_->else_body);
                    if (!promote(env, actual, &branch_val, if_t))
                        compile_err(env, if_->else_body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
                              type_to_string(if_t), type_to_string(actual));
                    gcc_assign(*block, NULL, if_ret, branch_val);
                } else if (*block) {
                    gcc_eval(*block, NULL, branch_val);
                }
            }
            if (*block) {
                assert(end_if); // AbortTypes shouldn't leave *block as non-null when compiling
                gcc_jump(*block, NULL, end_if);
            }
            *block = NULL;
        } else {
            if (*block)
                gcc_jump(*block, NULL, end_if);
            *block = NULL;
        }
        *block = end_if;
        return has_value ? gcc_lvalue_as_rvalue(if_ret) : NULL;
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
            subject = gcc_lvalue_as_rvalue(gcc_rvalue_dereference(subject, ast_loc(env, when->subject)));
        }
        if (subject_t->tag != TaggedUnionType)
            compile_err(env, when->subject, "'when' blocks must use enums, but this is a %s", type_to_string(subject_t));
        gcc_type_t *gcc_t = bl_type_to_gcc(env, subject_t);

        binding_t *tags_binding = get_from_namespace(env, subject_t, "__Tag");
        assert(tags_binding && tags_binding->type_value);
        bl_type_t *tag_t = tags_binding->type_value;

        bl_type_t *result_t = get_type(env, ast);
        bool has_value = !(result_t->tag == GeneratorType || result_t->tag == AbortType || result_t->tag == VoidType);
        gcc_func_t *func = gcc_block_func(*block);
        gcc_lvalue_t *when_value = has_value ? gcc_local(func, NULL, bl_type_to_gcc(env, result_t), fresh("when_value")) : NULL;
        gcc_block_t *end_when = result_t->tag == AbortType ? NULL : gcc_new_block(func, fresh("endif"));

        auto union_type = Match(Match(subject_t, TaggedUnionType)->data, UnionType);
        NEW_LIST(gcc_case_t*, gcc_cases);
        foreach (when->cases, case_, _) {
            istr_t tag_name = Match(case_->tag, Var)->name;
            env_t case_env = *env;
            if (case_->var) {
                case_env.bindings = hashmap_new();
                case_env.bindings->fallback = env->bindings;

                gcc_rvalue_t *case_rval = NULL;
                bl_type_t *case_t = NULL;
                for (int64_t i = 0, len = length(union_type->field_names); i < len; i++) {
                    if (ith(union_type->field_names, i) == tag_name) {
                        gcc_rvalue_t *data = gcc_rvalue_access_field(subject, NULL, gcc_get_field(gcc_type_if_struct(gcc_t), 1));
                        case_rval = gcc_rvalue_access_field(data, NULL, ith(union_type->fields, i));
                        case_t = ith(union_type->field_types, i);
                        break;
                    }
                }
                if (!case_rval)
                    compile_err(env, case_->var, "I couldn't figure out how to bind this variable (maybe the tag doesn't have a value associated with it)");

                binding_t *case_binding = new(binding_t, .type=case_t, .rval=case_rval);
                hashmap_set(case_env.bindings, Match(case_->var, Var)->name, case_binding);
            }

            gcc_block_t *case_block = gcc_new_block(func, fresh(intern_strf("case_%s", tag_name)));
            gcc_rvalue_t *tag_rval = get_from_namespace(env, tag_t, tag_name)->rval;
            gcc_case_t *gcc_case = gcc_new_case(env->ctx, tag_rval, tag_rval, case_block);
            APPEND(gcc_cases, gcc_case);

            gcc_rvalue_t *branch_val = compile_expr(&case_env, &case_block, case_->body);
            if (branch_val) {
                if (has_value) {
                    bl_type_t *actual = get_type(env, case_->body);
                    if (!promote(env, actual, &branch_val, result_t))
                        compile_err(env, case_->body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
                              type_to_string(result_t), type_to_string(actual));
                    gcc_assign(case_block, NULL, when_value, branch_val);
                } else {
                    gcc_eval(case_block, NULL, branch_val);
                }
            }
            gcc_jump(case_block, NULL, end_when);
        }
        gcc_block_t *default_block;
        if (when->default_body) {
            default_block = gcc_new_block(func, fresh("default"));
            gcc_block_t *end_of_default = default_block;
            gcc_rvalue_t *branch_val = compile_expr(env, &end_of_default, when->default_body);
            if (branch_val && end_of_default) {
                if (has_value) {
                    bl_type_t *actual = get_type(env, when->default_body);
                    if (!promote(env, actual, &branch_val, result_t))
                        compile_err(env, when->default_body, "The earlier branches of this conditional have type %s, but this branch has type %s and I can't figure out how to make that work.",
                              type_to_string(result_t), type_to_string(actual));
                    gcc_assign(end_of_default, NULL, when_value, branch_val);
                } else {
                    gcc_eval(end_of_default, NULL, branch_val);
                }
            }
            if (end_of_default)
                gcc_jump(end_of_default, NULL, end_when);
        } else {
            default_block = end_when;
        }

        gcc_rvalue_t *tag_val = gcc_rvalue_access_field(subject, NULL, gcc_get_field(gcc_type_if_struct(gcc_t), 0));
        gcc_switch(*block, NULL, tag_val, default_block, length(gcc_cases), gcc_cases[0]);
        *block = end_when;
        return when_value ? gcc_lvalue_as_rvalue(when_value) : NULL;
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
        istr_t target = ast->tag == Skip ? Match(ast, Skip)->target : Match(ast, Stop)->target;
        if (target) {
            for (loop_label_t *lbl = env->loop_label; lbl; lbl = lbl->enclosing) {
                foreach (lbl->names, name, _) {
                    if (*name == target) {
                        if (ast->tag == Skip) {
                            jump_dest = lbl->skip_label;
                            lbl->skip_reachable = true;
                        } else {
                            jump_dest = lbl->stop_label;
                            lbl->stop_reachable = true;
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
                    env->loop_label->skip_reachable = true;
                } else {
                    jump_dest = env->loop_label->stop_label;
                    env->loop_label->stop_reachable = true;
                }
            }
        }
        if (!jump_dest) compile_err(env, ast, "I'm not sure what %s is referring to", target);
        gcc_jump(*block, NULL, jump_dest);
        *block = NULL;
        return NULL;
    }
    case Fail: {
        ast_t *message = Match(ast, Fail)->message;
        gcc_rvalue_t *msg;
        if (message) {
            msg = compile_expr(env, block, message);
        } else {
            msg = gcc_new_string(env->ctx, "A failure occurred");
        }

        gcc_rvalue_t *fmt = gcc_new_string(env->ctx, "\x1b[31;1;7mError: %s\x1b[m\n\n%s");

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
        gcc_jump(*block, NULL, *block);
        *block = NULL;
        return ret;
    }
    default: break;
    }
    compile_err(env, ast, "I haven't yet implemented compiling for %s", get_ast_tag_name(ast->tag)); 
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
