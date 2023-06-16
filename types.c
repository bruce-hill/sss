#include <gc/cord.h>

#include "libsss/hashmap.h"
#include "types.h"
#include "util.h"

static CORD type_to_cord(sss_type_t *t, sss_hashmap_t *expanded) {
    switch (t->tag) {
        case UnknownType: return "???";
        case AbortType: return "Abort";
        case VoidType: return "Void";
        case BoolType: return "Bool";
        case IntType: {
            auto int_ = Match(t, IntType);
            CORD name = int_->is_unsigned ? "UInt" : "Int";
            CORD_sprintf(&name, "%r%d", name, int_->bits);
            if (int_->units)
                CORD_sprintf(&name, "%r<%s>", name, int_->units);
            return name;
        }
        case CharType: return "Char";
        case CStringCharType: return "CStringChar";
        case NumType: {
            auto num = Match(t, NumType);
            if (num->bits == 64)
                return num->units ? heap_strf("Num<%s>", num->units) : "Num";
            else
                return num->units ? heap_strf("Num32<%s>", num->units) : "Num32";
        }
        case TypeType: {
            sss_type_t *inner = Match(t, TypeType)->type;
            if (!inner) return "Type(...)";
            CORD ret;
            CORD_sprintf(&ret, "Type(%r)", type_to_cord(inner, expanded));
            return ret;
        }
        case RangeType: return "Range";
        case ArrayType: {
            auto array = Match(t, ArrayType);
            if (array->item_type->tag == CharType) {
                if (array->dsl)
                    return CORD_cat("$", array->dsl);
                return "Str";
            }
            return CORD_cat("[", CORD_cat(type_to_cord(array->item_type, expanded), "]"));
        }
        case TableType: {
            CORD c = "{";
            auto table = Match(t, TableType);
            c = CORD_cat(c, type_to_cord(table->key_type, expanded));
            c = CORD_cat(c, "=>");
            c = CORD_cat(c, type_to_cord(table->value_type, expanded));
            c = CORD_cat(c, "}");
            return c;
        }
        case FunctionType: {
            CORD c = "(";
            auto fn = Match(t, FunctionType);
            for (int64_t i = 0; i < LIST_LEN(fn->arg_types); i++) {
                if (i > 0) c = CORD_cat(c, ",");
                c = CORD_cat(c, type_to_cord(LIST_ITEM(fn->arg_types, i), expanded));
            }
            c = CORD_cat(c, ")->");
            c = CORD_cat(c, type_to_cord(fn->ret, expanded));
            return c;
        }
        case StructType: {
            auto struct_ = Match(t, StructType);
            if (struct_->name && (!expanded || hget(expanded, struct_->name, char*))) {
                if (!struct_->units)
                    return struct_->name;
                CORD c;
                CORD_sprintf(&c, "%s<%s>", struct_->name, struct_->units);
                return c;
            }
            if (struct_->name && expanded)
                hset(expanded, struct_->name, struct_->name);
            CORD c = NULL;
            if (struct_->name)
                c = CORD_cat(c, struct_->name);
            c = CORD_cat(c, "{");
            for (int64_t i = 0; i < LIST_LEN(struct_->field_types); i++) {
                sss_type_t *ft = LIST_ITEM(struct_->field_types, i);
                const char* fname = LIST_ITEM(struct_->field_names, i);
                if (i > 0)
                    c = CORD_cat(c, ",");

                if (fname && !streq(fname, heap_strf("_%lu", i+1)))
                    c = CORD_cat(CORD_cat(c, fname), ":");

                CORD fstr = type_to_cord(ft, expanded);
                c = CORD_cat(c, fstr);
            }
            c = CORD_cat(c, "}");
            if (struct_->units)
                CORD_sprintf(&c, "%r<%s>", c, struct_->units);
            return c;
        }
        case PointerType: {
            auto ptr = Match(t, PointerType);
            if (ptr->is_stack)
                return CORD_cat("&", type_to_cord(ptr->pointed, expanded));
            else
                return CORD_cat(ptr->is_optional ? "?" : "@", type_to_cord(ptr->pointed, expanded));
        }
        case GeneratorType: {
            auto gen = Match(t, GeneratorType);
            CORD ret;
            CORD_sprintf(&ret, "Generator(%r)", type_to_cord(gen->generated, expanded));
            return ret;
        }
        case TaggedUnionType: {
            auto tagged = Match(t, TaggedUnionType);
            if (!expanded || hget(expanded, tagged->name, char*))
                return tagged->name;

            if (tagged->name && expanded)
                hset(expanded, tagged->name, tagged->name);

            CORD c = CORD_cat(tagged->name, "{|");

            for (int64_t i = 0, len = LIST_LEN(tagged->members); i < len; i++) {
                if (i > 0)
                    c = CORD_cat(c, "|");
                auto member = LIST_ITEM(tagged->members, i);
                if (i == 0 ? member.tag_value == 0 : member.tag_value == 1 + LIST_ITEM(tagged->members, i-1).tag_value)
                    c = CORD_cat(c, member.name);
                else
                    CORD_sprintf(&c, "%r%s=%ld", c, member.name, member.tag_value);

                if (member.type)
                    CORD_sprintf(&c, "%r:%r", c, type_to_cord(member.type, expanded));
            }
            c = CORD_cat(c, "|}");
            return c;
        }
        case ModuleType: {
            CORD c;
            CORD_sprintf(&c, "Module(\"%s\")", Match(t, ModuleType)->path);
            return c;
        }
        default: {
            CORD c;
            CORD_sprintf(&c, "Unknown type: %d", t->tag);
            return c;
        }
    }
}

int printf_type_size(const struct printf_info *info, size_t n, int argtypes[n], int sizes[n])
{
    if (n < 1) return -1;
    (void)info;
    argtypes[0] = PA_POINTER;
    sizes[0] = sizeof(sss_type_t*);
    return 1;
}

int printf_type(FILE *stream, const struct printf_info *info, const void *const args[])
{
    sss_type_t *t = *(sss_type_t**)args[0];
    (void)info;
    sss_hashmap_t expanded = {0};
    CORD c = type_to_cord(t, &expanded);
    if (CORD_len(c) > 30)
        c = type_to_cord(t, NULL);
    return CORD_put(c, stream);
}

const char* type_to_string_concise(sss_type_t *t) {
    return CORD_to_char_star(type_to_cord(t, NULL));
}

const char* type_to_string(sss_type_t *t) {
    sss_hashmap_t expanded = {0};
    return CORD_to_char_star(type_to_cord(t, &expanded));
}

bool type_eq(sss_type_t *a, sss_type_t *b)
{
    if (a == b) return true;
    if (a->tag != b->tag) return false;
    return streq(type_to_string(a), type_to_string(b));
}

bool type_is_a(sss_type_t *t, sss_type_t *req)
{
    if (type_eq(t, req)) return true;
    if (t->tag == PointerType && req->tag == PointerType) {
        auto t_ptr = Match(t, PointerType);
        auto req_ptr = Match(req, PointerType);
        if (type_eq(t_ptr->pointed, req_ptr->pointed))
            return (!t_ptr->is_stack && !t_ptr->is_optional && req_ptr->is_stack)
                || (!t_ptr->is_stack && req_ptr->is_optional);
    }
    return false;
}

static sss_type_t *non_optional(sss_type_t *t)
{
    if (t->tag != PointerType) return t;
    auto ptr = Match(t, PointerType);
    return ptr->is_optional ? Type(PointerType, .is_optional=false, .pointed=ptr->pointed) : t;
}

sss_type_t *value_type(sss_type_t *t)
{
    while (t->tag == PointerType)
        t = Match(t, PointerType)->pointed;
    return t;
}

sss_type_t *type_or_type(sss_type_t *a, sss_type_t *b)
{
    if (!a) return b;
    if (!b) return a;
    if (type_is_a(b, a)) return a;
    if (type_is_a(a, b)) return b;
    if (a->tag == AbortType) return non_optional(b);
    if (b->tag == AbortType) return non_optional(a);
    if (a->tag == GeneratorType) return Type(GeneratorType, .generated=type_or_type(Match(a, GeneratorType)->generated, b));
    if (b->tag == GeneratorType) return Type(GeneratorType, .generated=type_or_type(a, Match(b, GeneratorType)->generated));
    if (is_numeric(a) && is_numeric(b)) {
        switch (compare_precision(a, b)) {
        case NUM_PRECISION_EQUAL: case NUM_PRECISION_MORE: return a;
        case NUM_PRECISION_LESS: return b;
        case NUM_PRECISION_INCOMPARABLE: {
            if (a->tag == IntType && b->tag == IntType && Match(a, IntType)->bits < 64)
                return Type(IntType, .bits=Match(a, IntType)->bits * 2, .is_unsigned=false);
            return NULL;
        }
        }
        return NULL;
    }
    return NULL;
}

const char* type_units(sss_type_t *t)
{
    assert(t);
    switch (t->tag) {
    case IntType: return Match(t, IntType)->units;
    case NumType: return Match(t, NumType)->units;
    case StructType: return Match(t, StructType)->units;
    case PointerType: return type_units(Match(t, PointerType)->pointed);
    case ArrayType: return type_units(Match(t, ArrayType)->item_type);
    default: return NULL;
    }
}

sss_type_t *with_units(sss_type_t *t, const char* units)
{
    switch (t->tag) {
    case IntType: return Type(IntType, .units=units, .bits=Match(t, IntType)->bits, .is_unsigned=Match(t, IntType)->is_unsigned);
    case NumType: return Type(NumType, .units=units, .bits=Match(t, NumType)->bits);
    case StructType: {
        auto s = Match(t, StructType);
        return Type(StructType, .name=s->name, .field_names=s->field_names, .field_types=s->field_types, .units=units);
    }
    case PointerType: {
        auto ptr = Match(t, PointerType);
        return Type(PointerType, .pointed=with_units(ptr->pointed, units), .is_optional=ptr->is_optional);
    }
    case ArrayType: {
        auto array = Match(t, ArrayType);
        return Type(ArrayType, .dsl=array->dsl, .item_type=with_units(array->item_type, units));
    }
    default: return t;
    }
}

bool is_integral(sss_type_t *t)
{
    return t->tag == IntType || t->tag == CharType || t->tag == CStringCharType;
}

bool is_numeric(sss_type_t *t)
{
    return t->tag == IntType || t->tag == NumType;
}

typedef enum {
    MAG_INAPPLICABLE = 0, MAG_ZERO, MAG1, MAG2, MAG3, MAG4, MAG5, MAG6, MAG7, MAG8, MAG9, MAG10, MAG11, MAG12,
} magnitude_e;

static inline magnitude_e type_min_magnitue(sss_type_t *t)
{
    switch (t->tag) {
    case BoolType: case CharType: case CStringCharType: return MAG_ZERO;
    case IntType: {
        if (Match(t, IntType)->is_unsigned) return MAG_ZERO;
        switch (Match(t, IntType)->bits) {
        case 8: return MAG1;
        case 16: return MAG2;
        case 32: return MAG3;
        case 64: return MAG4;
        default: return MAG_INAPPLICABLE;
        }
    }
    case NumType: return Match(t, NumType)->bits == 32 ? MAG5 : MAG6;
    default: return MAG_INAPPLICABLE;
    }
}

static inline int type_max_magnitue(sss_type_t *t)
{
    switch (t->tag) {
    case BoolType: return MAG1;
    case CharType: case CStringCharType: return MAG2;
    case IntType: {
        bool is_unsigned = Match(t, IntType)->is_unsigned;
        switch (Match(t, IntType)->bits) {
        case 8: return is_unsigned ? MAG4 : MAG3;
        case 16: return is_unsigned ? MAG6 : MAG5;
        case 32: return is_unsigned ? MAG8 : MAG7;
        case 64: return is_unsigned ? MAG10 : MAG9;
        default: return MAG_INAPPLICABLE;
        }
    }
    case NumType:
        return Match(t, NumType)->bits == 32 ? MAG11 : MAG12;
    default: return MAG_INAPPLICABLE;
    }
}

precision_cmp_e compare_precision(sss_type_t *a, sss_type_t *b)
{
    magnitude_e a_min = type_min_magnitue(a),
                b_min = type_min_magnitue(b),
                a_max = type_max_magnitue(a),
                b_max = type_max_magnitue(b);

    if (a_min == MAG_INAPPLICABLE || b_min == MAG_INAPPLICABLE || a_max == MAG_INAPPLICABLE || b_max == MAG_INAPPLICABLE)
        return NUM_PRECISION_INCOMPARABLE;
    else if (a_min == b_min && a_max == b_max) return NUM_PRECISION_EQUAL;
    else if (a_min <= b_min && a_max <= b_max) return NUM_PRECISION_LESS;
    else if (a_min >= b_min && a_max >= b_max) return NUM_PRECISION_MORE;
    else return NUM_PRECISION_INCOMPARABLE;
}

bool is_orderable(sss_type_t *t)
{
    switch (t->tag) {
    case ArrayType: return is_orderable(Match(t, ArrayType)->item_type);
    case PointerType: case FunctionType: case TableType: return false;
    case StructType: {
        auto subtypes = Match(t, StructType)->field_types;
        for (int64_t i = 0; i < LIST_LEN(subtypes); i++) {
            if (!is_orderable(LIST_ITEM(subtypes, i)))
                return false;
        }
        return true;
    }
    case TaggedUnionType: {
        auto members = Match(t, TaggedUnionType)->members;
        for (int64_t i = 0; i < LIST_LEN(members); i++) {
            auto member = LIST_ITEM(members, i);
            if (member.type && !is_orderable(member.type))
                return false;
        }
        return true;
    }
    default: return true;
    }
}

bool has_heap_memory(sss_type_t *t)
{
    switch (t->tag) {
    case ArrayType: return true;
    case TableType: return true;
    case PointerType: return true;
    case GeneratorType: return has_heap_memory(Match(t, GeneratorType)->generated);
    case StructType: {
        auto field_types = Match(t, StructType)->field_types;
        for (int64_t i = 0; i < LIST_LEN(field_types); i++) {
            if (has_heap_memory(LIST_ITEM(field_types, i)))
                return true;
        }
        return false;
    }
    case TaggedUnionType: {
        auto members = Match(t, TaggedUnionType)->members;
        for (int64_t i = 0; i < LIST_LEN(members); i++) {
            auto member = LIST_ITEM(members, i);
            if (member.type && has_heap_memory(member.type))
                return true;
        }
        return false;
    }
    default: return false;
    }
}

bool has_stack_memory(sss_type_t *t)
{
    switch (t->tag) {
    case PointerType: return Match(t, PointerType)->is_stack;
    case GeneratorType: return has_stack_memory(Match(t, GeneratorType)->generated);
    default: return false;
    }
}

bool can_promote(sss_type_t *actual, sss_type_t *needed)
{
    // No promotion necessary:
    if (type_eq(actual, needed))
        return true;

    if (!streq(type_units(actual), type_units(needed)))
        return false;

    if (is_numeric(actual) && is_numeric(needed)) {
        auto cmp = compare_precision(actual, needed);
        return cmp == NUM_PRECISION_EQUAL || cmp == NUM_PRECISION_LESS;
    }

    // Optional promotion:
    if (needed->tag == PointerType && actual->tag == PointerType) {
        auto needed_ptr = Match(needed, PointerType);
        auto actual_ptr = Match(actual, PointerType);
        if (needed_ptr->pointed->tag != VoidType && !type_eq(needed_ptr->pointed, actual_ptr->pointed))
            // Can't use @Foo for a function that wants @Baz
            // But you *can* use @Foo for a function that wants @Void
            return false;
        else if (actual_ptr->is_stack && !needed_ptr->is_stack)
            // Can't use &x for a function that wants a @Foo or ?Foo
            return false;
        else if (actual_ptr->is_optional && !needed_ptr->is_optional)
            // Can't use !Foo for a function that wants @Foo
            return false;
        else
            return true;
    }

    // Function promotion:
    if (needed->tag == FunctionType && actual->tag == FunctionType) {
        auto needed_fn = Match(needed, FunctionType);
        auto actual_fn = Match(actual, FunctionType);
        if (LIST_LEN(needed_fn->arg_types) != LIST_LEN(actual_fn->arg_types) || !type_eq(needed_fn->ret, actual_fn->ret))
            return false;
        for (int64_t i = 0, len = LIST_LEN(needed_fn->arg_types); i < len; i++) {
            if (!type_eq(LIST_ITEM(actual_fn->arg_types, i), LIST_ITEM(needed_fn->arg_types, i)))
                return false;
        }
        return true;
    }

    // String <-> c string promotion
    if (type_eq(actual, Type(PointerType, .pointed=Type(CStringCharType))) && type_eq(needed, Type(ArrayType, .item_type=Type(CharType))))
        return true;
    else if (type_eq(actual, Type(ArrayType, .item_type=Type(CharType))) && type_eq(needed, Type(PointerType, .pointed=Type(CStringCharType))))
        return true;

    if (actual->tag == StructType && needed->tag == StructType) {
        auto actual_struct = Match(actual, StructType);
        auto needed_struct = Match(needed, StructType);
        // TODO: allow promoting with uninitialized or extraneous values?
        if (LIST_LEN(actual_struct->field_types) != LIST_LEN(needed_struct->field_types))
            return false;
        for (int64_t i = 0; i < LIST_LEN(actual_struct->field_types); i++) {
            // TODO: check field names??
            if (!can_promote(LIST_ITEM(actual_struct->field_types, i), LIST_ITEM(needed_struct->field_types, i)))
                return false;
        }
        return true;
    }

    return false;
}

bool can_leave_uninitialized(sss_type_t *t)
{
    switch (t->tag) {
    case PointerType: return Match(t, PointerType)->is_optional;
    case ArrayType: case IntType: case NumType: case CharType: case CStringCharType: case BoolType: case RangeType:
        return true;
    case StructType: {
        auto struct_ = Match(t, StructType);
        for (int64_t i = 0; i < LIST_LEN(struct_->field_types); i++)
            if (!can_leave_uninitialized(LIST_ITEM(struct_->field_types, i)))
                return false;
        return true;
    }
    case TaggedUnionType: {
        auto members = Match(t, TaggedUnionType)->members;
        LIST_FOR (members, member, _) {
            if (member->type && !can_leave_uninitialized(member->type))
                return false;
        }
        return true;
    }
    default: return false;
    }
}

sss_type_t *table_entry_type(sss_type_t *table_t)
{
    static sss_hashmap_t cache = {0};
    sss_type_t *t = Type(StructType, .field_names=LIST(const char*, "key", "value"),
                        .field_types=LIST(sss_type_t*, Match(table_t, TableType)->key_type,
                                          Match(table_t, TableType)->value_type));
    sss_type_t *cached = hget(&cache, type_to_string(t), sss_type_t*);
    if (cached) {
        return cached;
    } else {
        hset(&cache, type_to_string(t), t);
        return t;
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
