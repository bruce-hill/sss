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
        case CharType: return "Char";
        case NumType: return "Num";
        case Num32Type: return "Num32";
        case NamedType: {
            auto named = Match(t, NamedType);
            return named->name;
        }
        case TypeType: return "Type";
        case RangeType: return "Range";
        case ArrayType: {
            auto list = Match(t, ArrayType);
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
        case PointerType: {
            auto ptr = Match(t, PointerType);
            return CORD_cat(ptr->is_optional ? "@?" : "@", type_to_cord(ptr->pointed));
        }
        case GeneratorType: {
            auto gen = Match(t, GeneratorType);
            return CORD_cat(type_to_cord(gen->generated), "(generator)");
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
    if (t->tag == PointerType && req->tag == PointerType) {
        auto t_ptr = Match(t, PointerType);
        auto req_ptr = Match(req, PointerType);
        if (t_ptr->pointed == req_ptr->pointed && req_ptr->is_optional)
            return true;
    }
    return false;
}

static bl_type_t *non_optional(bl_type_t *t)
{
    if (t->tag != PointerType) return t;
    auto ptr = Match(t, PointerType);
    return ptr->is_optional ? Type(PointerType, .is_optional=false, .pointed=ptr->pointed) : t;
}

bl_type_t *type_or_type(bl_type_t *a, bl_type_t *b)
{
    if (!a) return b;
    if (!b) return a;
    if (type_is_a(b, a)) return a;
    if (type_is_a(a, b)) return b;
    if (a->tag == AbortType) return non_optional(b);
    if (b->tag == AbortType) return non_optional(a);
    return NULL;
}

bool is_integral(bl_type_t *t)
{
    switch (t->tag) {
    case IntType: case Int32Type: case Int16Type: case Int8Type: case CharType:
        return true;
    default:
        return false;
    }
}

bool is_numeric(bl_type_t *t)
{
    switch (t->tag) {
    case IntType: case Int32Type: case Int16Type: case Int8Type: case CharType:
    case NumType: case Num32Type:
        return true;
    default:
        return false;
    }
}

int numtype_priority(bl_type_t *t)
{
    switch (t->tag) {
    case BoolType: case CharType: case Int8Type: return 1;
    case Int16Type: return 2;
    case Int32Type: return 3;
    case IntType: return 4;
    case Num32Type: return 5;
    case NumType: return 6;
    default: return 0;
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
