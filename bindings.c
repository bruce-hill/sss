// Logic for applying bindings to all variables and typevariables.
#include <assert.h>
#include <err.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include "ast.h"
#include "typecheck.h"
#include "types.h"
#include "util.h"
#include "compile/libgccjit_abbrev.h"

void bind_types(env_t *env, table_t *bindings, ast_t *ast)
{
    if (!ast) return;

    switch (ast->tag) {
    case TypeDef: {
        auto def = Match(ast, TypeDef);
        sss_type_t *t = Type(PlaceholderType, .filename=env->file->filename, .name=def->name);
        Table_str_set(bindings, def->name, t);
        bind_types(env, bindings, def->type);
        bind_types(env, bindings, def->namespace);
        break;
    }
    case TypeVar: {
        auto var = Match(ast, TypeVar);
        sss_type_t *t = Table_str_get(bindings, var->name);
        if (!t)
            compiler_err(env, ast, "I don't know what this type is");
        var->type = t;
        break;
    }
    default: {
        auto children = get_ast_children(ast);
        foreach (children, child, _)
            bind_types(env, bindings, *child);
        break;
    }
    }
}

void populate_types(env_t *env, table_t *bindings, ast_t *ast)
{
    switch (ast->tag) {
    case TypeDef: {
        auto def = Match(ast, TypeDef);
        sss_type_t *t = Table_str_get(bindings, def->name);
        // Mutate placeholder type to hold resolved type: 
        sss_type_t *resolved_t = parse_type_ast(env, def->type);
        *(struct sss_type_s*)t = *Type(VariantType, .filename=env->file->filename, .name=def->name, .variant_of=resolved_t);
        populate_types(env, bindings, def->namespace);
        break;
    }
    default: {
        auto children = get_ast_children(ast);
        foreach (children, child, _)
            populate_types(env, bindings, *child);
        break;
    }
    }
}

void bind_variables(env_t *env, table_t *bindings, ast_t *ast)
{
    switch (ast->tag) {
    case Block: {
        bindings = new(table_t, .fallback=bindings);
        auto statements = Match(ast, Block)->statements;
        foreach (statements, stmt, _)
            bind_variables(env, bindings, *stmt);

        foreach (statements, _stmt, _) {
            ast_t *stmt = *_stmt;
            while (stmt->tag == DocTest)
                stmt = Match(stmt, DocTest)->expr;

            if (stmt->tag == FunctionDef) {
                auto fndef = Match(stmt, FunctionDef);
                bind_variables(env, bindings, fndef->body);
            }
        }
        break;
    }
    case Declare: {
        auto decl = Match(ast, Declare);
        bind_variables(env, bindings, decl->value);

        auto var = Match(decl->var, Var);
        assert(!var->binding);
        var->binding = new(binding_t, .type=get_type(env, decl->value));
        Table_str_set(bindings, var->name, var->binding);
        break;
    }
    case FunctionDef: {
        auto fndef = Match(ast, FunctionDef);
        if (fndef->args.defaults) {
            table_t arg_bindings = {0};
            for (int64_t i = 0; i < LENGTH(fndef->args.names); i++) {
                const char *name = ith(fndef->args.names, i);
                ast_t *def = ith(fndef->args.defaults, i);
                ast_t *type = ith(fndef->args.types, i);
                binding_t *arg_binding = new(binding_t);
                if (def) {
                    bind_variables(env, &arg_bindings, def);
                }
                arg_binding->type = type ? parse_type_ast(env, type) : get_type(env, def);
                Table_str_set(&arg_bindings, name, arg_binding);
            }
        }
        binding_t *fn_binding = new(binding_t, .type=get_type(env, ast));
        Table_str_set(bindings, fndef->name, fn_binding);
        break;
    }
    case TypeDef: {
        auto def = Match(ast, TypeDef);
        bind_variables(env, bindings, def->namespace);
        sss_type_t *def_t = Table_str_get(bindings, def->name);
        sss_type_t *ns_t = get_namespace_type(env, def->namespace, def_t);
        binding_t *b = new(binding_t, .type=ns_t);
        Table_str_set(bindings, def->name, b);
        break;
    }
    default: {
        auto children = get_ast_children(ast);
        foreach (children, child, _)
            bind_variables(env, bindings, *child);
        break;
    }
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
