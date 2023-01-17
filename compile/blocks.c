// Logic for compiling Blang blocks
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
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

void compile_statement(env_t *env, gcc_block_t **block, ast_t *ast)
{
    check_discardable(env, ast);
    gcc_rvalue_t *val = compile_expr(env, block, ast);
    if (val && *block)
        gcc_eval(*block, ast_loc(env, ast), val);
}

static bl_type_t *predeclare_def_types(env_t *env, ast_t *def)
{
    if (def->tag == StructDef) {
        auto struct_def = Match(def, StructDef);
        istr_t name = struct_def->name;
        NEW_LIST(istr_t, field_names);
        NEW_LIST(bl_type_t*, field_types);
        // Placeholder type, will be populated later:
        if (hashmap_get(env->bindings, name))
            compile_err(env, def, "Something called %s is already defined.", name);
        bl_type_t *t = Type(StructType, .name=name, .field_names=field_names, .field_types=field_types);
        gcc_lvalue_t *lval = gcc_global(env->ctx, ast_loc(env, def), GCC_GLOBAL_EXPORTED, gcc_get_ptr_type(gcc_type(env->ctx, CHAR)), name);
        lval = gcc_global_set_initializer_rvalue(lval, gcc_str(env->ctx, name));

        hashmap_t *namespace = hashmap_new();
        namespace->fallback = env->bindings;
        binding_t *b = new(binding_t, .type=Type(TypeType), .type_value=t, .is_global=true, .rval=gcc_rval(lval));
        hashmap_set(env->bindings, name, b);
        hashmap_set(env->type_namespaces, t, namespace);

        env_t struct_env = *env;
        struct_env.bindings = namespace;
        foreach (struct_def->members, member, _) {
            predeclare_def_types(&struct_env, *member);
        }
        return t;
    } else if (def->tag == EnumDef) {
        auto enum_def = Match(def, EnumDef);
        istr_t enum_name = enum_def->name;
        if (hashmap_get(env->bindings, enum_name))
            compile_err(env, def, "Something called %s is already defined.", enum_name);
        NEW_LIST(bl_type_t*, union_field_types);
        NEW_LIST(istr_t, union_field_names);
        bl_type_t *tag_t = Type(TagType, .name=enum_name, .names=enum_def->tag_names, .values=enum_def->tag_values);
        bl_type_t *union_t = Type(UnionType, .field_names=union_field_names, .field_types=union_field_types, .fields=LIST(gcc_field_t*));
        bl_type_t *t = Type(TaggedUnionType, .name=enum_name, .tag_type=tag_t, .data=union_t);
        gcc_rvalue_t *rval = gcc_str(env->ctx, enum_name);

        hashmap_t *type_ns = get_namespace(env, t);
        type_ns->fallback = env->bindings;

        // Populate union fields
        binding_t *binding = new(binding_t, .type=Type(TypeType), .type_value=t, .is_global=true, .rval=rval);
        hashmap_set(env->bindings, enum_name, binding);
        hashmap_set(env->type_namespaces, t, type_ns);

        hashmap_t *tag_namespace = get_namespace(env, tag_t);
        tag_namespace->fallback = type_ns;
        binding_t *tag_binding = new(binding_t, .type=Type(TypeType), .type_value=tag_t, .is_global=true,
                                     .rval=gcc_str(env->ctx, intern_strf("%s.__Tag", enum_name)));
        hashmap_set(type_ns, intern_str("__Tag"), tag_binding);
        hashmap_set(env->type_namespaces, tag_t, tag_namespace);

        gcc_type_t *tag_gcc_t = bl_type_to_gcc(env, tag_t);

        // Bind tag values:
        env_t type_env = *env;
        type_env.bindings = type_ns;
        for (int64_t i = 0, len = length(enum_def->tag_names); i < len; i++) {
            istr_t tag_name = ith(enum_def->tag_names, i);
            gcc_rvalue_t *tag_val = gcc_rvalue_from_long(env->ctx, tag_gcc_t, ith(enum_def->tag_values, i));
            // Bind the tag:
            hashmap_set(tag_namespace, tag_name, new(binding_t, .type=tag_t, .is_constant=true, .is_global=true, .rval=tag_val));

            ast_t *field_type_ast = ith(enum_def->tag_types, i);
            if (field_type_ast) {
                bl_type_t *field_t = predeclare_def_types(&type_env, field_type_ast);
                assert(field_t || field_type_ast->tag != StructDef);
                APPEND(union_field_names, tag_name);
                if (!field_t)
                    field_t = parse_type_ast(env, field_type_ast);
                APPEND(union_field_types, field_t);

                // Bind the struct type:
                binding_t *b = new(binding_t, .type=Type(TypeType), .is_constant=true, .is_global=true, .enum_type=t, .tag_rval=tag_val,
                                   .type_value=field_t, .rval=gcc_str(env->ctx, intern_strf("%s.%s", enum_name, tag_name)));
                hashmap_set(type_ns, tag_name, b);
                // Also visible in the enclosing namespace:
                // hashmap_set(env->bindings, tag_name, b);
            }
        }
        return t;
    } else {
        return NULL;
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
            compile_err(env, def, "This struct has no fields, which is currently not supported");

        env_t inner_env = *env;
        inner_env.bindings = get_namespace(env, t);

        foreach (struct_def->members, member, _) {
            if ((*member)->tag == StructFieldDef) {
                auto field_def = Match((*member), StructFieldDef);
                bl_type_t *ft = parse_type_ast(&inner_env, field_def->type);
                if (ft->tag == VoidType)
                    compile_err(env, field_def->type, "This field is a Void type, but that isn't supported for struct members.");
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
            compile_err(env, def, "This enum has no fields, which is currently not supported");

        env_t inner_env = *env;
        inner_env.bindings = get_namespace(env, t);

        // populate the unioned structs for the tagged data:
        auto union_t = Match(Match(t, TaggedUnionType)->data, UnionType);
        int64_t field_index = 0;
        for (int64_t i = 0, len = length(enum_def->tag_names); i < len; i++) {
            ast_t *field_type_ast = ith(enum_def->tag_types, i);
            if (!field_type_ast)
                continue;

            populate_def_members(&inner_env, field_type_ast);
            bl_type_t *bl_field_type = ith(union_t->field_types, field_index);
            gcc_type_t *field_type = bl_type_to_gcc(env, bl_field_type);
            gcc_field_t *field = gcc_new_field(env->ctx, NULL, field_type, ith(enum_def->tag_names, i));
            APPEND(union_t->fields, field);
            ++field_index;
        }

        bl_type_t *tag_t = Match(t, TaggedUnionType)->tag_type;
        gcc_type_t *tag_gcc_t = bl_type_to_gcc(env, tag_t);

        // Bind any tagged union fields without data (X is Foo.Tag.X)
        for (int64_t i = 0, len = length(enum_def->tag_names); i < len; i++) {
            ast_t *field_type_ast = ith(enum_def->tag_types, i);
            if (field_type_ast) continue;
            gcc_type_t *gcc_tagged_t = bl_type_to_gcc(env, t);
            gcc_struct_t *gcc_tagged_s = gcc_type_if_struct(gcc_tagged_t);
            gcc_field_t *tag_field = gcc_get_field(gcc_tagged_s, 0);
            gcc_rvalue_t *tag_val = gcc_rvalue_from_long(env->ctx, tag_gcc_t, ith(enum_def->tag_values, i));
            gcc_rvalue_t *singleton = gcc_struct_constructor(env->ctx, NULL, gcc_tagged_t, 1, &tag_field, &tag_val);
            binding_t *singleton_binding = new(binding_t, .type=t, .rval=singleton, .is_global=true);
            istr_t tag_name = ith(enum_def->tag_names, i);
            hashmap_set(inner_env.bindings, tag_name, singleton_binding);
            // hashmap_set(env->bindings, tag_name, singleton_binding);
        }
    }
}

static void predeclare_def_funcs(env_t *env, ast_t *def)
{
    if (def->tag == FunctionDef) {
        auto fndef = Match(def, FunctionDef);
        bl_type_t *t = get_type(env, def);
        gcc_func_t *func = get_function_def(env, def, fndef->is_exported ? fndef->name : fresh(fndef->name), fndef->is_exported);
        gcc_rvalue_t *fn_ptr = gcc_get_func_address(func, NULL);
        hashmap_set(env->bindings, fndef->name,
                    new(binding_t, .type=t, .is_global=true, .func=func, .rval=fn_ptr));
    } else if (def->tag == StructDef) {
        auto struct_def = Match(def, StructDef);
        binding_t *b = hashmap_get(env->bindings, struct_def->name);
        assert(b && b->type_value);
        env_t inner_env = *env;
        inner_env.bindings = get_namespace(env, b->type_value);
        env = &inner_env;
        // Struct methods:
        foreach (struct_def->members, member, _) {
            if ((*member)->tag == FunctionDef) {
                auto fndef = Match((*member), FunctionDef);
                bl_type_t *t = get_type(env, *member);
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
    auto statements = ast->tag == Block ? Match(ast, Block)->statements : LIST(ast_t*, ast);

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
            compile_err(env, *stmt, "This code can never be reached because there is an unconditional control flow statement before it.");
        if (stmt == last_stmt && give_expression) {
            return compile_expr(env, block, *stmt);
        } else {
            env_t tmp = *env;
            tmp.comprehension_callback = NULL;
            compile_statement(&tmp, block, *stmt);
            env->derived_units = tmp.derived_units;
            env->conversions = tmp.conversions;
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
    if (!ast) return;
    gcc_rvalue_t *rval = _compile_block(env, block, ast, env->comprehension_callback != NULL);
    if (rval && *block) gcc_eval(*block, NULL, rval);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
