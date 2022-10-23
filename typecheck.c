#include <bhash.h>
#include <gc.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "typecheck.h"
#include "types.h"
#include "util.h"

static inline bl_type_t *new_type(bl_type_t init) {
    bl_type_t *ret = new(bl_type_t);
    *ret = init;
    return ret;
}
#define TYPE(mykind, ...) (new_type((bl_type_t){.kind=mykind, __VA_ARGS__}))

bl_type_t *get_type(hashmap_t *bindings, ast_t *ast)
{
    switch (ast->kind) {
        case Nil: return TYPE(NilType, .name="Nil");
        case Bool: return TYPE(BoolType, .name="Bool");
        case Int: return TYPE(IntType, .name="Int");
        case Num: return TYPE(NumType, .name="Num");
        case StringJoin: case StringLiteral: return TYPE(StringType, .name="String");
        case Var: {
            binding_t *binding = hashmap_get(bindings, ast->str);
            if (binding) {
                return binding->type;
            } else {
                return TYPE(UnknownType);
            }
        }
        case KeywordArg: return get_type(bindings, ast->named.value);

        default: break;
    }
    fprintf(stderr, "\x1b[31;7;1mCouldn't figure out type for %s:\x1b[m\n\n", get_ast_kind_name(ast->kind)); 
    // highlight_match(NULL, ast->match);
    exit(1);
    return TYPE(UnknownType);
}

const char *get_base_type(hashmap_t *bindings, ast_t *ast)
{
    bl_type_t *t = get_type(bindings, ast);
    switch (t->kind) {
    case BoolType: case Int8Type: case Int16Type: case Int32Type: return "w";
    case NumType: return "d";
    case Num32Type: return "s";
    default: return "l";
    }
}

const char *get_abi_type(hashmap_t *bindings, ast_t *ast)
{
    bl_type_t *t = get_type(bindings, ast);
    switch (t->kind) {
    case BoolType: case Int8Type: return "b";
    case Int16Type: return "h";
    case Int32Type: return "w";
    case NumType: return "d";
    case Num32Type: return "s";
    default: return "l";
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
