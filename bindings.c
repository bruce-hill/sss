// Logic for applying bindings to all variables and typevariables.
#include <assert.h>
#include <err.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include "ast.h"
#include "args.h"
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
        auto var = Match(def->name, TypeVar);
        sss_type_t *t = Type(PlaceholderType, .filename=env->file->filename, .name=var->name);
        Table_str_set(bindings, var->name, t);
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
        auto var = Match(def->name, TypeVar);
        bind_types(env, bindings, def->type);
        populate_types(env, bindings, def->namespace);
        // Mutate placeholder type to hold resolved type: 
        sss_type_t *resolved_t = parse_type_ast(env, def->type);
        sss_type_t *t = Table_str_get(bindings, var->name);
        *(struct sss_type_s*)t = *Type(VariantType, .filename=env->file->filename, .name=var->name, .variant_of=resolved_t);
        var->type = t;
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

static void bind_match_patterns(env_t *env, table_t *bindings, sss_type_t *t, ast_t *pattern)
{
    while (t->tag == VariantType)
        t = Match(t, VariantType)->variant_of;

    switch (pattern->tag) {
    case Wildcard: {
        const char *name = Match(pattern, Wildcard)->name;
        if (!name) return;
        Table_str_set(bindings, name, new(binding_t, .type=t));
        return;
    }
    case Var: {
        const char *name = Match(pattern, Var)->name;
        if (t->tag == TaggedUnionType) {
            auto tu_t = Match(t, TaggedUnionType);
            foreach (tu_t->members, member, _) {
                if (streq(member->name, name))
                    return;
            }
        }
        return;
    }
    case HeapAllocate: {
        if (t->tag != PointerType) return;
        bind_match_patterns(env, bindings, Match(t, PointerType)->pointed, Match(pattern, HeapAllocate)->value);
        return;
    }
    case Struct: {
        if (t->tag != StructType)
            return;
        auto struct_type = Match(t, StructType);
        auto pat_struct = Match(pattern, Struct);
        if (pat_struct->type) {
            sss_type_t *pat_t = parse_type_ast(env, pat_struct->type);
            if (!type_eq(t, pat_t)) return;
        }

        auto arg_infos = bind_arguments(env, pat_struct->members, struct_type->field_names,
                                        struct_type->field_types, struct_type->field_defaults);
        foreach (arg_infos, arg_info, _) {
            if (arg_info->ast)
                bind_match_patterns(env, bindings, arg_info->type, arg_info->ast);
        }
        return;
    }
    case FunctionCall: {
        auto call = Match(pattern, FunctionCall);
        if (call->fn->tag != Var) return;

        const char *fn_name = Match(call->fn, Var)->name;
        if (t->tag != TaggedUnionType) return;

        // Tagged Union Constructor:
        auto tu_t = Match(t, TaggedUnionType);
        int64_t tag_index = -1;
        for (int64_t i = 0; i < LENGTH(tu_t->members); i++) {
            if (streq(ith(tu_t->members, i).name, fn_name)) {
                tag_index = i;
                break;
            }
        }
        if (tag_index < 0) return;

        auto member = ith(tu_t->members, tag_index);
        if (!member.type)
            compiler_err(env, pattern, "This tagged union member doesn't have any value");

        ast_t *m_pat = WrapAST(pattern, Struct, .members=call->args);
        bind_match_patterns(env, bindings, member.type, m_pat);
        return;
    }
    default: return;
    }
}

void bind_variables(env_t *env, table_t *bindings, ast_t *ast)
{
    if (!ast) return;
    switch (ast->tag) {
    case Var: {
        auto var = Match(ast, Var);
        if (var->binding) return;
        var->binding = Table_str_get(bindings, var->name);
        break;
    }
    case TypeVar: {
        auto var = Match(ast, TypeVar);
        if (var->binding) return;
        var->binding = Table_str_get(bindings, var->name);
        break;
    }
    case Block: {
        if (!Match(ast, Block)->keep_scope)
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
        gcc_func_t *func = get_function_def(env, ast, Match(fndef->name, Var)->name);
        table_t fn_bindings = {0};
        if (fndef->args.defaults) {
            for (int64_t i = 0; i < LENGTH(fndef->args.args); i++) {
                auto var = Match(ith(fndef->args.args, i), Var);
                ast_t *def = ith(fndef->args.defaults, i);
                ast_t *type = ith(fndef->args.types, i);
                var->binding = new(binding_t);
                if (def)
                    bind_variables(env, &fn_bindings, def);
                var->binding->type = type ? parse_type_ast(env, type) : get_type(env, def);
                var->binding->lval = gcc_param_as_lvalue(gcc_func_get_param(func, i));
                var->binding->rval = gcc_rval(var->binding->lval);
                Table_str_set(&fn_bindings, var->name, var->binding);
            }
        }
        binding_t *fn_binding = new(binding_t, .type=get_type(env, ast));
        fn_binding->func = func;
        Table_str_set(bindings, Match(fndef->name, Var)->name, fn_binding);
        Table_str_set(&fn_bindings, Match(fndef->name, Var)->name, fn_binding);
        bind_variables(env, &fn_bindings, fndef->body);
        break;
    }
    case TypeDef: {
        auto def = Match(ast, TypeDef);
        auto var = Match(def->name, TypeVar);
        bind_variables(env, bindings, def->namespace);
        sss_type_t *def_t = var->type;
        assert(def_t);
        sss_type_t *ns_t = get_namespace_type(env, def->namespace, def_t);
        binding_t *b = new(binding_t, .type=ns_t);
        var->binding = b;
        Table_str_set(bindings, var->name, b);
        break;
    }
    case If: {
        auto if_ = Match(ast, If);
        sss_type_t *subject_t;
        if (if_->subject->tag == Declare) {
            subject_t = get_type(env, Match(if_->subject, Declare)->value);
            bindings = new(table_t, .fallback=bindings);
            bind_variables(env, bindings, if_->subject);
        } else {
            bind_variables(env, bindings, if_->subject);
            subject_t = get_type(env, if_->subject);
        }
        for (int64_t i = 0; i < LENGTH(if_->patterns); i++) {
            table_t *case_bindings = new(table_t, .fallback=bindings);
            bind_match_patterns(env, case_bindings, subject_t, ith(if_->patterns, i));
            bind_variables(env, case_bindings, ith(if_->blocks, i));
        }
        break;
    }
    case For: {
        auto for_ = Match(ast, For);
        table_t *for_bindings = new(table_t, .fallback=bindings);
        bind_variables(env, for_bindings, for_->iter);
        if (for_->index) {
            auto index = Match(for_->index, Var);
            index->binding = new(binding_t, .type=Type(IntType, .bits=64));
            Table_str_set(for_bindings, index->name, index->binding);
        }
        if (for_->value) {
            auto value = Match(for_->value, Var);
            sss_type_t *iter_t = get_iter_type(env, for_->iter);
            value->binding = new(binding_t, .type=iter_t);
            Table_str_set(for_bindings, value->name, value->binding);
        }
        table_t *nonempty_bindings = new(table_t, .fallback=for_bindings);
        bind_variables(env, nonempty_bindings, for_->first);
        bind_variables(env, nonempty_bindings, for_->body);
        bind_variables(env, nonempty_bindings, for_->between);
        table_t *empty_bindings = new(table_t, .fallback=for_bindings);
        bind_variables(env, empty_bindings, for_->empty);
        break;
    }
    case Reduction: {
        auto reduction = Match(ast, Reduction);
        bindings = new(table_t, .fallback=bindings);
        bind_variables(env, bindings, reduction->iter);
        sss_type_t *item_type = get_iter_type(env, reduction->iter);
        Table_str_set(bindings, "x.0", new(binding_t, .type=item_type));
        Table_str_set(bindings, "y.0", new(binding_t, .type=item_type));
        bind_variables(env, bindings, reduction->combination);
        bind_variables(env, bindings, reduction->fallback);
        break;
    }
    case With: {
        auto with = Match(ast, With);
        bindings = new(table_t, .fallback=bindings);
        bind_variables(env, bindings, with->expr);
        if (with->var) {
            auto var = Match(with->var, Var);
            var->binding = new(binding_t, .type=get_type(env, with->expr));
            Table_str_set(bindings, var->name, var->binding);
        }
        bind_variables(env, bindings, with->body);
        bind_variables(env, bindings, with->cleanup);
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
