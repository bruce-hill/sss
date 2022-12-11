// Logic for compiling Blang blocks
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

void compile_statement(env_t *env, gcc_block_t **block, ast_t *ast)
{
    check_discardable(env->file, env->bindings, ast);
    gcc_rvalue_t *val = compile_expr(env, block, ast);
    if (val)
        gcc_eval(*block, ast_loc(env, ast), val);
}

static void predeclare_def_types(env_t *env, ast_t *def)
{
    if (def->kind == StructDef) {
        istr_t name = def->struct_def.name;
        NEW_LIST(istr_t, field_names);
        NEW_LIST(bl_type_t*, field_types);
        // Placeholder type, will be populated later:
        if (hashmap_get(env->bindings, name))
            ERROR(env, def, "Something called %s is already defined.", name);
        bl_type_t *t = Type(StructType, .struct_.name=name, .struct_.field_names=field_names, .struct_.field_types=field_types);
        gcc_lvalue_t *lval = gcc_global(env->ctx, ast_loc(env, def), GCC_GLOBAL_EXPORTED, gcc_type(env->ctx, STRING), name);
        lval = gcc_global_set_initializer_rvalue(lval, gcc_new_string(env->ctx, name));

        hashmap_t *namespace = hashmap_new();
        namespace->fallback = env->bindings;
        hashmap_set(env->bindings, name, new(binding_t, .type=Type(TypeType), .type_value=t, .is_global=true, .rval=gcc_lvalue_as_rvalue(lval), .namespace=namespace));

        env_t struct_env = *env;
        struct_env.bindings = namespace;
        foreach (def->struct_def.members, member, _) {
            predeclare_def_types(&struct_env, *member);
        }
    } else if (def->kind == EnumDef) {
        istr_t enum_name = def->enum_def.name;
        if (hashmap_get(env->bindings, enum_name))
            ERROR(env, def, "Something called %s is already defined.", enum_name);
        NEW_LIST(bl_type_t*, field_types);
        NEW_LIST(istr_t, field_names);
        for (int64_t i = 0, len = length(def->enum_def.tag_names); i < len; i++) {
            if (ith(def->enum_def.tag_types, i))
                APPEND(field_names, ith(def->enum_def.tag_names, i));
        }
        bl_type_t *union_t = Type(UnionType, .union_.field_names=field_names, .union_.field_types=field_types, .union_.fields=LIST(gcc_field_t*));
        bl_type_t *t = Type(TaggedUnionType, .tagged.name=enum_name, .tagged.tag_names=def->enum_def.tag_names,
                            .tagged.tag_values=def->enum_def.tag_values, .tagged.data=union_t);
        gcc_rvalue_t *rval = gcc_new_string(env->ctx, enum_name);

        hashmap_t *namespace = hashmap_new();
        namespace->fallback = env->bindings;

        // Populate union fields
        binding_t *binding = new(binding_t, .type=Type(TypeType), .type_value=t, .is_global=true, .rval=rval, .namespace=namespace);
        hashmap_set(env->bindings, enum_name, binding);

        // Bind tag values:
        for (int64_t i = 0, len = length(def->enum_def.tag_names); i < len; i++) {
            istr_t tag_name = ith(def->enum_def.tag_names, i);
            gcc_rvalue_t *tag_val = gcc_int64(env->ctx, ith(def->enum_def.tag_values, i));
            binding_t *b = new(binding_t, .type=Type(IntType), .is_constant=true, .is_global=true, .rval=tag_val, .enum_type=t);

            ast_t *field_type_ast = ith(def->enum_def.tag_types, i);
            if (field_type_ast) {
                NEW_LIST(bl_type_t*, union_fields);
                bl_type_t *field_type = Type(StructType, .struct_.name=tag_name, .struct_.field_names=LIST(istr_t), .struct_.field_types=union_fields);
                b->type_value = field_type;
                APPEND(field_types, field_type);
            }

            hashmap_set(namespace, tag_name, b);
            // Default to also visible outside the enum's namespace:
            hashmap_set(env->bindings, tag_name, b);
        }
    }
}

static void populate_def_members(env_t *env, ast_t *def)
{
    if (def->kind == StructDef) {
        istr_t name = def->struct_def.name;
        binding_t *binding = hashmap_get(env->bindings, name);
        assert(binding && binding->type->kind == TypeType && binding->type_value);
        bl_type_t *t = binding->type_value;
        if (length(def->struct_def.members) == 0)
            ERROR(env, def, "This struct has no fields, which is currently not supported");

        env_t inner_env = *env;
        inner_env.bindings = binding->namespace;

        foreach (def->struct_def.members, member, _) {
            if ((*member)->kind == StructFieldDef) {
                bl_type_t *ft = parse_type(env->file, inner_env.bindings, (*member)->fields.type);
                if (ft->kind == VoidType)
                    ERROR(env, (*member)->fields.type, "This field is a Void type, but that isn't supported for struct members.");
                foreach((*member)->fields.names, fname, __) {
                    APPEND(t->struct_.field_names, *fname);
                    APPEND(t->struct_.field_types, ft);
                }
            } else {
                populate_def_members(&inner_env, *member);
            }
        }
    } else if (def->kind == EnumDef) {
        istr_t name = def->enum_def.name;
        binding_t *binding = hashmap_get(env->bindings, name);
        assert(binding && binding->type->kind == TypeType && binding->type_value);
        bl_type_t *t = binding->type_value;
        if (length(def->struct_def.members) == 0)
            ERROR(env, def, "This struct has no fields, which is currently not supported");

        env_t inner_env = *env;
        inner_env.bindings = binding->namespace;

        // populate the unioned structs for the tagged data:
        bl_type_t *union_t = t->tagged.data;
        int64_t field_index = 0;
        for (int64_t i = 0, len = length(def->enum_def.tag_names); i < len; i++) {
            ast_t *fields_ast = ith(def->enum_def.tag_types, i);
            if (!fields_ast)
                continue;

            assert(fields_ast->kind == StructDef);

            bl_type_t *field_struct = ith(union_t->union_.field_types, field_index);
            foreach (fields_ast->struct_def.members, member, _) {
                assert((*member)->kind == StructFieldDef);
                bl_type_t *ft = parse_type(env->file, inner_env.bindings, (*member)->fields.type);
                if (ft->kind == VoidType)
                    ERROR(env, *member, "This field is a Void type, but that isn't supported for struct members.");
                foreach((*member)->fields.names, fname, __) {
                    APPEND(field_struct->struct_.field_names, *fname);
                    APPEND(field_struct->struct_.field_types, ft);
                }
            }
            gcc_type_t *field_type = bl_type_to_gcc(env, field_struct);
            assert(field_type);
            gcc_field_t *field = gcc_new_field(env->ctx, NULL, field_type, ith(def->enum_def.tag_names, i));
            APPEND(union_t->union_.fields, field);
            ++field_index;
        }
    }
}

static void predeclare_def_funcs(env_t *env, ast_t *def)
{
    if (def->kind == FunctionDef) {
        bl_type_t *t = get_type(env->file, env->bindings, def);
        gcc_func_t *func = get_function_def(env, def, fresh(def->fn.name), false);
        gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
        hashmap_set(env->bindings, def->fn.name,
                    new(binding_t, .type=t, .is_global=true, .func=func, .rval=fn_ptr));
    } else if (def->kind == StructDef) {
        binding_t *b = hashmap_get(env->bindings, def->struct_def.name);
        assert(b && b->namespace);
        env_t inner_env = *env;
        inner_env.bindings = b->namespace;
        env = &inner_env;
        // Struct methods:
        foreach (def->struct_def.members, member, _) {
            if ((*member)->kind == FunctionDef) {
                bl_type_t *t = get_type(env->file, env->bindings, *member);
                gcc_func_t *func = get_function_def(env, *member, intern_strf("%s__%s", def->struct_def.name, (*member)->fn.name), true);
                gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
                hashmap_set(env->bindings, (*member)->fn.name,
                            new(binding_t, .type=t, .is_global=true, .func=func, .rval=fn_ptr));
            } else {
                predeclare_def_funcs(env, *member);
            }
        }
    }
    // TODO: enum methods
}

gcc_rvalue_t *_compile_block(env_t *env, gcc_block_t **block, ast_t *ast, bool give_expression)
{
    assert(ast->kind == Block);

    // Struct and enum defs are visible in the entire block (allowing corecursive structs)
    foreach (ast->children, stmt, _) {
        predeclare_def_types(env, *stmt);
    }
    // Populate struct fields:
    foreach (ast->children, stmt, _) {
        populate_def_members(env, *stmt);
    }
    // Function defs are visible in the entire block (allowing corecursive funcs)
    foreach (ast->children, stmt, _) {
        predeclare_def_funcs(env, *stmt);
    }

    foreach (ast->children, stmt, last_stmt) {
        if (!*block)
            ERROR(env, *stmt, "This code can never be reached because there is an unconditional control flow statement before it.");
        if (stmt == last_stmt && give_expression) {
            return compile_expr(env, block, *stmt);
        } else {
            compile_statement(env, block, *stmt);
        }
    }
    return NULL;
}

gcc_rvalue_t *compile_block_expr(env_t *env, gcc_block_t **block, ast_t *ast)
{
    return _compile_block(env, block, ast, true);
}

void compile_block_statement(env_t *env, gcc_block_t **block, ast_t *ast)
{
    if (ast)
        (void)_compile_block(env, block, ast, false);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
