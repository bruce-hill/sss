#include <gc/cord.h>
#include <intern.h>
#include <libgccjit.h>

#include "libgccjit_abbrev.h"
#include "types.h"

static CORD type_to_cord(bl_type_t *t) {
    switch (t->kind) {
        case UnknownType: return "???";
        case AbortType: return "Abort";
        case NilType: return "Nil";
        case BoolType: return "Bool";
        case IntType: return "Int";
        case Int32Type: return "Int32";
        case Int16Type: return "Int16";
        case Int8Type: return "Int8";
        case NumType: return "Num";
        case Num32Type: return "Num32";
        case StringType: return "String";
        case NamedType: return t->name;
        case TypeType: return CORD_cat("Type(", CORD_cat(type_to_cord(t->type), ")"));
        case ListType: return CORD_cat("[", CORD_cat(type_to_cord(t->item_type), "]"));
        case TableType: {
            CORD c = "{";
            c = CORD_cat(c, type_to_cord(t->key));
            c = CORD_cat(c, "=");
            c = CORD_cat(c, type_to_cord(t->value));
            c = CORD_cat(c, "}");
            return c;
        }
        case FunctionType: {
            CORD c = "(";
            for (int64_t i = 0; i < LIST_LEN(t->args); i++) {
                if (i > 0) c = CORD_cat(c, ",");
                c = CORD_cat(c, type_to_cord(LIST_ITEM(t->args, i)));
            }
            c = CORD_cat(c, ")=>");
            c = CORD_cat(c, type_to_cord(t->value));
            return c;
        }
        case OptionalType: return CORD_cat(type_to_cord(t->nonnil), "?");
        default: return "?!?!?";
    }
}

istr_t type_to_string(bl_type_t *t) {
    return intern_str(CORD_to_char_star(type_to_cord(t)));
}

bool type_is_a(bl_type_t *t, bl_type_t *req)
{
    if (t == req) return true;
    if (req->kind == OptionalType && (t->kind == NilType || type_is_a(t, req->nonnil)))
        return true;
    return false;
}

bl_type_t *type_or_type(bl_type_t *a, bl_type_t *b)
{
    if (!a) return b;
    if (!b) return a;
    if (type_is_a(b, a)) return a;
    if (type_is_a(a, b)) return b;
    if (a->kind == AbortType && b->kind == OptionalType) return b->nonnil;
    if (b->kind == AbortType && a->kind == OptionalType) return a->nonnil;
    if (a->kind == AbortType) return b;
    if (b->kind == AbortType) return a;
    if (a->kind == NilType) return Type(OptionalType, .nonnil=b);
    if (b->kind == NilType) return Type(OptionalType, .nonnil=a);
    return NULL;
}

bool is_numeric(bl_type_t *t)
{
    switch (t->kind) {
    case IntType: case Int16Type: case Int8Type:
    case NumType: case Num32Type:
        return true;
    default:
        return false;
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
