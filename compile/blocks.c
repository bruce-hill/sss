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
    if (def->tag == StructDef) {
        auto struct_def = Match(def, StructDef);
        istr_t name = struct_def->name;
        NEW_LIST(istr_t, field_names);
        NEW_LIST(bl_type_t*, field_types);
        // Placeholder type, will be populated later:
        if (hashmap_get(env->bindings, name))
            ERROR(env, def, "Something called %s is already defined.", name);
        bl_type_t *t = Type(StructType, .name=name, .field_names=field_names, .field_types=field_types);
        gcc_lvalue_t *lval = gcc_global(env->ctx, ast_loc(env, def), GCC_GLOBAL_EXPORTED, gcc_type(env->ctx, STRING), name);
        lval = gcc_global_set_initializer_rvalue(lval, gcc_new_string(env->ctx, name));

        hashmap_t *namespace = hashmap_new();
        namespace->fallback = env->bindings;
        hashmap_set(env->bindings, name, new(binding_t, .type=Type(TypeType), .type_value=t, .is_global=true, .rval=gcc_lvalue_as_rvalue(lval), .namespace=namespace));

        env_t struct_env = *env;
        struct_env.bindings = namespace;
        foreach (struct_def->members, member, _) {
            predeclare_def_types(&struct_env, *member);
        }
    } else if (def->tag == EnumDef) {
        auto enum_def = Match(def, EnumDef);
        istr_t enum_name = enum_def->name;
        if (hashmap_get(env->bindings, enum_name))
            ERROR(env, def, "Something called %s is already defined.", enum_name);
        NEW_LIST(bl_type_t*, field_types);
        NEW_LIST(istr_t, field_names);
        for (int64_t i = 0, len = length(enum_def->tag_names); i < len; i++) {
            if (ith(enum_def->tag_types, i))
                APPEND(field_names, ith(enum_def->tag_names, i));
        }
        bl_type_t *union_t = Type(UnionType, .field_names=field_names, .field_types=field_types, .fields=LIST(gcc_field_t*));
        bl_type_t *tag_t = Type(TagType, .name=enum_name, .names=enum_def->tag_names, .values=enum_def->tag_values);
        bl_type_t *t = Type(TaggedUnionType, .name=enum_name, .tag_type=tag_t, .data=union_t);
        gcc_rvalue_t *rval = gcc_new_string(env->ctx, enum_name);

        hashmap_t *namespace = hashmap_new();
        namespace->fallback = env->bindings;

        // Populate union fields
        binding_t *binding = new(binding_t, .type=Type(TypeType), .type_value=t, .is_global=true, .rval=rval, .namespace=namespace);
        hashmap_set(env->bindings, enum_name, binding);

        hashmap_t *tag_namespace = hashmap_new();
        tag_namespace->fallback = namespace;
        hashmap_set(namespace, intern_str("Tag"), new(binding_t, .type=Type(TypeType), .type_value=tag_t, .is_global=true, .namespace=tag_namespace, .rval=rval));

        gcc_type_t *tag_gcc_t = bl_type_to_gcc(env, tag_t);

        // Bind tag values:
        for (int64_t i = 0, len = length(enum_def->tag_names); i < len; i++) {
            istr_t tag_name = ith(enum_def->tag_names, i);
            gcc_rvalue_t *tag_val = gcc_rvalue_from_long(env->ctx, tag_gcc_t, ith(enum_def->tag_values, i));
            binding_t *b = new(binding_t, .type=tag_t, .is_constant=true, .is_global=true, .rval=tag_val, .enum_type=t);

            ast_t *field_type_ast = ith(enum_def->tag_types, i);
            if (field_type_ast) {
                NEW_LIST(bl_type_t*, union_fields);
                bl_type_t *field_type = Type(StructType, .name=tag_name, .field_names=LIST(istr_t), .field_types=union_fields);
                b->type_value = field_type;
                APPEND(field_types, field_type);
            }

            hashmap_set(namespace, tag_name, b);
            hashmap_set(tag_namespace, tag_name, b);
            // Default to also visible outside the enum's namespace:
            hashmap_set(env->bindings, tag_name, b);
        }
    }
}

static void populate_def_members(env_t *env, ast_t *def)
{
    if (def->tag == StructDef) {
        auto struct_def = Match(def, StructDef);
        istr_t name = struct_def->name;
        binding_t *binding = hashmap_get(env->bindings, name);
        assert(binding && binding->type->tag == TypeType && binding->type_value);
        bl_type_t *t = binding->type_value;
        if (length(struct_def->members) == 0)
            ERROR(env, def, "This struct has no fields, which is currently not supported");

        env_t inner_env = *env;
        inner_env.bindings = binding->namespace;

        foreach (struct_def->members, member, _) {
            if ((*member)->tag == StructFieldDef) {
                auto field_def = Match((*member), StructFieldDef);
                bl_type_t *ft = parse_type(env->file, inner_env.bindings, field_def->type);
                if (ft->tag == VoidType)
                    ERROR(env, field_def->type, "This field is a Void type, but that isn't supported for struct members.");
                auto struct_type = Match(t, StructType);
                foreach(field_def->names, fname, __) {
                    APPEND(struct_type->field_names, *fname);
                    APPEND(struct_type->field_types, ft);
                }
            } else {
                populate_def_members(&inner_env, *member);
            }
        }
    } else if (def->tag == EnumDef) {
        auto enum_def = Match(def, EnumDef);
        istr_t name = enum_def->name;
        binding_t *binding = hashmap_get(env->bindings, name);
        assert(binding && binding->type->tag == TypeType && binding->type_value);
        bl_type_t *t = binding->type_value;
        if (length(enum_def->tag_names) == 0)
            ERROR(env, def, "This enum has no fields, which is currently not supported");

        env_t inner_env = *env;
        inner_env.bindings = binding->namespace;

        // populate the unioned structs for the tagged data:
        auto union_t = Match(Match(t, TaggedUnionType)->data, UnionType);
        int64_t field_index = 0;
        for (int64_t i = 0, len = length(enum_def->tag_names); i < len; i++) {
            ast_t *fields_ast = ith(enum_def->tag_types, i);
            if (!fields_ast)
                continue;

            assert(fields_ast->tag == StructDef);

            bl_type_t *field_struct = ith(union_t->field_types, field_index);
            auto field_struct_info = Match(field_struct, StructType);
            auto struct_def = Match(fields_ast, StructDef);
            foreach (struct_def->members, member, _) {
                auto field_def = Match((*member), StructFieldDef);
                bl_type_t *ft = parse_type(env->file, inner_env.bindings, field_def->type);
                if (ft->tag == VoidType)
                    ERROR(env, *member, "This field is a Void type, but that isn't supported for struct members.");
                foreach(field_def->names, fname, __) {
                    APPEND(field_struct_info->field_names, *fname);
                    APPEND(field_struct_info->field_types, ft);
                }
            }
            gcc_type_t *field_type = bl_type_to_gcc(env, field_struct);
            assert(field_type);
            gcc_field_t *field = gcc_new_field(env->ctx, NULL, field_type, ith(enum_def->tag_names, i));
            APPEND(union_t->fields, field);
            ++field_index;
        }
    }
}

static void predeclare_def_funcs(env_t *env, ast_t *def)
{
    if (def->tag == FunctionDef) {
        auto fndef = Match(def, FunctionDef);
        bl_type_t *t = get_type(env->file, env->bindings, def);
        gcc_func_t *func = get_function_def(env, def, fresh(fndef->name), false);
        gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
        hashmap_set(env->bindings, fndef->name,
                    new(binding_t, .type=t, .is_global=true, .func=func, .rval=fn_ptr));
    } else if (def->tag == StructDef) {
        auto struct_def = Match(def, StructDef);
        binding_t *b = hashmap_get(env->bindings, struct_def->name);
        assert(b && b->namespace);
        env_t inner_env = *env;
        inner_env.bindings = b->namespace;
        env = &inner_env;
        // Struct methods:
        foreach (struct_def->members, member, _) {
            if ((*member)->tag == FunctionDef) {
                auto fndef = Match((*member), FunctionDef);
                bl_type_t *t = get_type(env->file, env->bindings, *member);
                gcc_func_t *func = get_function_def(env, *member, intern_strf("%s__%s", struct_def->name, fndef->name), true);
                gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
                hashmap_set(env->bindings, fndef->name,
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
    auto statements = Match(ast, Block)->statements;

    // Struct and enum defs are visible in the entire block (allowing corecursive structs)
    foreach (statements, stmt, _) {
        predeclare_def_types(env, *stmt);
    }
    // Populate struct fields:
    foreach (statements, stmt, _) {
        populate_def_members(env, *stmt);
    }
    // Function defs are visible in the entire block (allowing corecursive funcs)
    foreach (statements, stmt, _) {
        predeclare_def_funcs(env, *stmt);
    }

    foreach (statements, stmt, last_stmt) {
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
