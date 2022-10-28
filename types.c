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

char base_type_for(bl_type_t *t)
{
    switch (t->kind) {
    case NilType: case BoolType: case Int8Type: case Int16Type: case Int32Type: return 'w';
    case NumType: return 'd';
    case Num32Type: return 's';
    case OptionalType: {
        switch (t->nonnil->kind) {
        case Int16Type: case Int8Type: return 'w';
        case BoolType: return 'w';
        case NumType: return 'd';
        case Num32Type: return 's';
        default: return 'l';
        }
    }
    default: return 'l';
    }
}

char abi_type_for(bl_type_t *t)
{
    switch (t->kind) {
    case NilType: case BoolType: case Int8Type: return 'b';
    case Int16Type: return 'h';
    case Int32Type: return 'w';
    case NumType: return 'd';
    case Num32Type: return 's';
    case OptionalType: {
        switch (t->nonnil->kind) {
        case Int32Type: case Int16Type: return 'w';
        case Int8Type: return 'h';
        case BoolType: return 'b';
        case NumType: return 'd';
        case Num32Type: return 's';
        default: return 'l';
        }
    }
    default: return 'l';
    }
}

const char* nil_value(bl_type_t *t)
{
    switch (t->kind) {
    case OptionalType: return nil_value(t->nonnil);
    case IntType: return "0x7FFFFFFFFFFFFFFFF";
    case Int16Type: case Int8Type: return "0x7FFFFFFFF";
    case BoolType: return "0x7F";
    case NumType: return "d_0x8000000000000.p7ff";
    case Num32Type: return "s_0x800000.pff";
    default: return "0";
    }
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
