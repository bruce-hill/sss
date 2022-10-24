#include <gc/cord.h>
#include <intern.h>

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
    }
    return "";
}

istr_t type_to_string(bl_type_t *t) {
    return intern_str(CORD_to_char_star(type_to_cord(t)));
}
