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
        case IntType: {
            auto int_ = Match(t, IntType);
            if (int_->bits == 64)
                return int_->units ? intern_strf("Int<%s>", int_->units) : "Int";
            else
                return int_->units ? intern_strf("Int%d<%s>", int_->bits, int_->units) : intern_strf("Int%d", int_->bits);
        }
        case CharType: return "Char";
        case NumType: {
            auto num = Match(t, NumType);
            if (num->bits == 64)
                return num->units ? intern_strf("Num<%s>", num->units) : "Num";
            else
                return num->units ? intern_strf("Num32<%s>", num->units) : "Num32";
        }
        case TypeType: {
            CORD ret;
            CORD_sprintf(&ret, "Type(%r)", type_to_cord(Match(t, TypeType)->type));
            return ret;
        }
        case RangeType: return "Range";
        case ArrayType: {
            auto list = Match(t, ArrayType);
            if (list->item_type == Type(CharType)) {
                if (list->dsl)
                    return CORD_cat("$", list->dsl);
                return "String";
            }
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
            if (struct_->name) {
                if (!struct_->units)
                    return struct_->name;
                CORD c;
                CORD_sprintf(&c, "%s<%s>", struct_->name, struct_->units);
                return c;
            }
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
            if (struct_->units)
                CORD_sprintf(&c, "%r<%s>", c, struct_->units);
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
    if (a->tag == GeneratorType) return Type(GeneratorType, .generated=type_or_type(Match(a, GeneratorType)->generated, b));
    if (b->tag == GeneratorType) return Type(GeneratorType, .generated=type_or_type(a, Match(b, GeneratorType)->generated));
    if (is_numeric(a) && is_numeric(b))
        return numtype_priority(a) >= numtype_priority(b) ? a : b;
    return NULL;
}

istr_t type_units(bl_type_t *t)
{
    switch (t->tag) {
    case IntType: return Match(t, IntType)->units;
    case NumType: return Match(t, NumType)->units;
    case StructType: return Match(t, StructType)->units;
    case PointerType: return type_units(Match(t, PointerType)->pointed);
    default: return NULL;
    }
}

bl_type_t *with_units(bl_type_t *t, istr_t units)
{
    switch (t->tag) {
    case IntType: return Type(IntType, .units=units, .bits=Match(t, IntType)->bits);
    case NumType: return Type(NumType, .units=units, .bits=Match(t, NumType)->bits);
    case StructType: {
        auto s = Match(t, StructType);
        return Type(StructType, .name=s->name, .field_names=s->field_names, .field_types=s->field_types, .units=units);
    }
    case PointerType: {
        return Type(PointerType, .pointed=with_units(Match(t, PointerType)->pointed, units));
    }
    default: return t;
    }
}

bool is_integral(bl_type_t *t)
{
    switch (t->tag) {
    case IntType: case CharType:
        return true;
    default:
        return false;
    }
}

bool is_numeric(bl_type_t *t)
{
    switch (t->tag) {
    case IntType: case NumType:
        return true;
    case CharType: return false;
    default:
        return false;
    }
}

int numtype_priority(bl_type_t *t)
{
    switch (t->tag) {
    case BoolType: return 1;
    case CharType: return 2;
    case IntType:
        switch (Match(t, IntType)->bits) {
        case 8: return 3;
        case 16: return 4;
        case 32: return 5;
        case 64: return 6;
        default: return 0;
        }
    case NumType:
        return Match(t, NumType)->bits == 32 ? 7 : 8;
    default: return 0;
    }
}

bool is_comparable(bl_type_t *t)
{
    switch (t->tag) {
    case ArrayType: return is_comparable(Match(t, ArrayType)->item_type);
    case PointerType: case FunctionType: return false;
    case StructType: case UnionType: {
        auto subtypes = t->tag == StructType ? Match(t, StructType)->field_types : Match(t, UnionType)->field_types;
        for (int64_t i = 0; i < LIST_LEN(subtypes); i++) {
            if (!is_comparable(LIST_ITEM(subtypes, i)))
                return false;
        }
        return true;
    }
    case TaggedUnionType: return is_comparable(Match(t, TaggedUnionType)->data);
    default: return true;
    }
}

bool has_heap_memory(bl_type_t *t)
{
    switch (t->tag) {
    case ArrayType: return has_heap_memory(Match(t, ArrayType)->item_type);
    case TableType: return true;
    case PointerType: return true;
    case GeneratorType: return has_heap_memory(Match(t, GeneratorType)->generated);
    case StructType: case UnionType: {
        auto field_types = t->tag == StructType ? Match(t, StructType)->field_types
            : Match(t, UnionType)->field_types;
        for (int64_t i = 0; i < LIST_LEN(field_types); i++) {
            if (has_heap_memory(LIST_ITEM(field_types, i)))
                return true;
        }
        return false;
    }
    case TaggedUnionType: return has_heap_memory(Match(t, TaggedUnionType)->data);
    default: return false;
    }
}

bool can_promote(bl_type_t *actual, bl_type_t *needed)
{
    // No promotion necessary:
    if (actual == needed)
        return true;

    // Numeric promotion:
    if (is_numeric(actual) && is_numeric(needed) && numtype_priority(actual) <= numtype_priority(needed))
        return type_units(actual) == type_units(needed);

    // Optional promotion:
    if (needed->tag == PointerType && actual->tag == PointerType) {
        auto needed_ptr = Match(needed, PointerType);
        auto actual_ptr = Match(actual, PointerType);
        return needed_ptr->pointed == actual_ptr->pointed && needed_ptr->is_optional;
    }

    // Function promotion:
    if (needed->tag == FunctionType && actual->tag == FunctionType) {
        auto needed_fn = Match(needed, FunctionType);
        auto actual_fn = Match(actual, FunctionType);
        if (LIST_LEN(needed_fn->arg_types) != LIST_LEN(actual_fn->arg_types) || needed_fn->ret != actual_fn->ret)
            return false;
        for (int64_t i = 0, len = LIST_LEN(needed_fn->arg_types); i < len; i++) {
            if (LIST_ITEM(actual_fn->arg_types, i) != LIST_ITEM(needed_fn->arg_types, i))
                return false;
        }
        return true;
    }

    // TODO: Struct promotion?

    return false;
}

bool can_leave_uninitialized(bl_type_t *t)
{
    switch (t->tag) {
    case PointerType: return Match(t, PointerType)->is_optional;
    case ArrayType: case IntType: case NumType: case CharType: case BoolType: case RangeType:
        return true;
    case StructType: {
        auto struct_ = Match(t, StructType);
        for (int64_t i = 0; i < LIST_LEN(struct_->field_types); i++)
            if (!can_leave_uninitialized(LIST_ITEM(struct_->field_types, i)))
                return false;
        return true;
    }
    case TaggedUnionType: {
        auto tu = Match(t, TaggedUnionType);
        auto tag = Match(tu->tag_type, TagType);
        auto union_ = Match(tu->data, UnionType);
        for (int64_t i = 0; i < LIST_LEN(tag->values); i++) {
            if (LIST_ITEM(tag->values, i) != 0)
                continue;

            istr_t name = LIST_ITEM(tag->names, i);
            for (int64_t j = 0; j < LIST_LEN(union_->field_names); j++) {
                if (LIST_ITEM(union_->field_names, j) == name)
                    return can_leave_uninitialized(LIST_ITEM(union_->field_types, j));
            }
            return true;
        }
        return false;
    }
    default: return false;
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
