#include <gc/cord.h>
#include <intern.h>

#include "util.h"
#include "types.h"

static CORD type_to_cord(bl_type_t *t) {
    switch (t->tag) {
        case UnknownType: return "???";
        case AbortType: return "Abort";
        case VoidType: return "Void";
        case BoolType: return "Bool";
        case IntType: return "Int";
        case Int32Type: return "Int32";
        case Int16Type: return "Int16";
        case Int8Type: return "Int8";
        case NumType: return "Num";
        case Num32Type: return "Num32";
        case StringType: return "String";
        case NamedType: {
            auto named = Match(t, NamedType);
            return named->name;
        }
        case TypeType: return "Type";
        case RangeType: return "Range";
        case ListType: {
            auto list = Match(t, ListType);
            return CORD_cat("[", CORD_cat(type_to_cord(list->item_type), "]"));
        }
        case TableType: {
            CORD c = "{";
            auto table = Match(t, TableType);
            c = CORD_cat(c, type_to_cord(table->key_type));
            c = CORD_cat(c, "=");
            c = CORD_cat(c, type_to_cord(table->value_type));
            c = CORD_cat(c, "}");
            return c;
        }
        case FunctionType: {
            CORD c = "(";
            auto fn = Match(t, FunctionType);
            for (int64_t i = 0; i < LIST_LEN(fn->arg_types); i++) {
                if (i > 0) c = CORD_cat(c, ",");
                c = CORD_cat(c, type_to_cord(LIST_ITEM(fn->arg_types, i)));
            }
            c = CORD_cat(c, ")=>");
            c = CORD_cat(c, type_to_cord(fn->ret));
            return c;
        }
        case StructType: {
            auto struct_ = Match(t, StructType);
            if (struct_->name)
                return struct_->name;
            CORD c = CORD_cat(NULL, "{");
            for (int64_t i = 0; i < LIST_LEN(struct_->field_types); i++) {
                bl_type_t *ft = LIST_ITEM(struct_->field_types, i);
                istr_t fname = LIST_ITEM(struct_->field_names, i);
                if (i > 0)
                    c = CORD_cat(c, ",");

                if (fname)
                    c = CORD_cat(CORD_cat(c, fname), "=");

                CORD fstr = type_to_cord(ft);
                c = CORD_cat(c, fstr);
            }
            c = CORD_cat(c, "}");
            return c;
        }
        case OptionalType: {
            auto opt = Match(t, OptionalType);
            return CORD_cat(type_to_cord(opt->nonnil), "?");
        }
        case TagType: {
            auto tag = Match(t, TagType);
            return CORD_cat(tag->name, ".Tag");
        }
        case TaggedUnionType: {
            auto tagged = Match(t, TaggedUnionType);
            return tagged->name;
        }
        case UnionType: return "Union";
        default: {
            CORD c;
            CORD_sprintf(&c, "Unknown type: %d", t->tag);
            return c;
        }
    }
}

istr_t type_to_string(bl_type_t *t) {
    return intern_str(CORD_to_char_star(type_to_cord(t)));
}

bool type_is_a(bl_type_t *t, bl_type_t *req)
{
    if (t == req) return true;
    if (req->tag == OptionalType && type_is_a(t, Match(req, OptionalType)->nonnil))
        return true;
    return false;
}

bl_type_t *type_or_type(bl_type_t *a, bl_type_t *b)
{
    if (!a) return b;
    if (!b) return a;
    if (type_is_a(b, a)) return a;
    if (type_is_a(a, b)) return b;
    if (a->tag == AbortType && b->tag == OptionalType) return Match(b, OptionalType)->nonnil;
    if (b->tag == AbortType && a->tag == OptionalType) return Match(a, OptionalType)->nonnil;
    if (a->tag == AbortType) return b;
    if (b->tag == AbortType) return a;
    return NULL;
}

bool is_integral(bl_type_t *t)
{
    switch (t->tag) {
    case IntType: case Int32Type: case Int16Type: case Int8Type:
        return true;
    default:
        return false;
    }
}

bool is_numeric(bl_type_t *t)
{
    switch (t->tag) {
    case IntType: case Int32Type: case Int16Type: case Int8Type:
    case NumType: case Num32Type:
        return true;
    default:
        return false;
    }
}

int numtype_priority(bl_type_t *t)
{
    switch (t->tag) {
    case BoolType: case Int8Type: return 1;
    case Int16Type: return 2;
    case Int32Type: return 3;
    case IntType: return 4;
    case Num32Type: return 5;
    case NumType: return 6;
    default: return 0;
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
