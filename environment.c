#include <intern.h>
#include <stdarg.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>

#include "ast.h"
#include "environment.h"

void compile_err(env_t *env, ast_t *ast, const char *fmt, ...)
{
    fputs("\x1b[31;7;1m", stderr);
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputs("\x1b[m\n\n", stderr);
    if (ast)
        highlight_match(stderr, env->file, (ast)->match, 2);
    raise(SIGABRT);
    exit(1);
}

binding_t *get_binding(env_t *env, const char *name)
{
    return hashmap_get(env->bindings, intern_str(name));
}

binding_t *get_ast_binding(env_t *env, ast_t *ast)
{
    switch (ast->tag) {
    case Var: {
        return get_binding(env, Match(ast, Var)->name);
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        binding_t *b = get_ast_binding(env, access->fielded);
        if (!b) return NULL;
        if (b->type->tag == TypeType)
            return get_from_namespace(env, b->type_value, access->field);
        else
            return get_from_namespace(env, b->type, access->field);
    }
    default: compile_err(env, ast, "I can't figure out at compile-time what this refers to");
    }
}

hashmap_t *get_namespace(env_t *env, bl_type_t *t)
{
    hashmap_t *ns = hashmap_get(env->type_namespaces, t);
    if (!ns) {
        ns = hashmap_new();
        hashmap_set(env->type_namespaces, t, ns);
    }
    return ns;
}

binding_t *get_from_namespace(env_t *env, bl_type_t *t, const char *name)
{
    return hashmap_get(get_namespace(env, t), intern_str(name));
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
