// Logic for handling sss_type_t types
#include <gc/cord.h>

#include "builtins/table.h"
#include "types.h"
#include "util.h"

typedef enum {
    DEFAULT = 0,
    FILENAMES = 1 << 0,
    ARG_NAMES = 1 << 1,
    EXPAND_ARGS = 1 << 2,
} stringify_flags_e;

static CORD type_to_cord(sss_type_t *t, table_t *expanded, stringify_flags_e flags) {
    switch (t->tag) {
        case UnknownType: return "???";
        case AbortType: return "Abort";
        case VoidType: return "Void";
        case MemoryType: return "Memory";
        case BoolType: return "Bool";
        case IntType: {
            auto int_ = Match(t, IntType);
            CORD name = int_->is_unsigned ? "UInt" : "Int";
            if (int_->bits != 64)
                CORD_sprintf(&name, "%r%d", name, int_->bits);
            if (int_->units)
                CORD_sprintf(&name, "%r<%s>", name, int_->units);
            return name;
        }
        case CharType: return "Char";
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
            CORD_sprintf(&ret, "Type(%r)", type_to_cord(inner, expanded, flags));
            return ret;
        }
        case RangeType: return "Range";
        case ArrayType: {
            auto array = Match(t, ArrayType);
            return CORD_cat("[", CORD_cat(type_to_cord(array->item_type, expanded, flags), "]"));
        }
        case TableType: {
            CORD c = "{";
            auto table = Match(t, TableType);
            c = CORD_cat(c, type_to_cord(table->key_type, expanded, flags));
            c = CORD_cat(c, "=>");
            c = CORD_cat(c, type_to_cord(table->value_type, expanded, flags));
            c = CORD_cat(c, "}");
            return c;
        }
        case FunctionType: {
            CORD c = "func(";
            auto fn = Match(t, FunctionType);
            if (!(flags & EXPAND_ARGS)) expanded = NULL;
            for (int64_t i = 0; i < LENGTH(fn->arg_types); i++) {
                if (i > 0) c = CORD_cat(c, ", ");
                if ((flags & ARG_NAMES) && fn->arg_names) c = CORD_cat(c, ith(fn->arg_names, i));
                if ((flags & ARG_NAMES) && fn->arg_defaults) {
                    ast_t *def = ith(fn->arg_defaults, i);
                    if (def) {
                        CORD_sprintf(&c, "%r=%#W", c, def);
                        continue;
                    }
                }
                if ((flags & ARG_NAMES) && fn->arg_names) c = CORD_cat(c, ":");
                c = CORD_cat(c, type_to_cord(ith(fn->arg_types, i), expanded, flags));
            }
            c = CORD_cat(c, ")->");
            c = CORD_cat(c, type_to_cord(fn->ret, expanded, flags));
            return c;
        }
        case StructType: {
            auto struct_ = Match(t, StructType);
            CORD c = "struct(";
            for (int64_t i = 0; i < LENGTH(struct_->field_types); i++) {
                sss_type_t *ft = ith(struct_->field_types, i);
                const char *fname = struct_->field_names ? ith(struct_->field_names, i) : heap_strf("_%lu", i+1);
                if (i > 0)
                    c = CORD_cat(c, ",");

                if (fname && !streq(fname, heap_strf("_%lu", i+1)))
                    c = CORD_cat(CORD_cat(c, fname), ":");
                else
                    c = CORD_cat(c, ":");

                CORD fstr = type_to_cord(ft, expanded, flags);
                c = CORD_cat(c, fstr);
            }
            c = CORD_cat(c, ")");
            if (struct_->units)
                CORD_sprintf(&c, "%r<%s>", c, struct_->units);
            return c;
        }
        case PointerType: {
            auto ptr = Match(t, PointerType);
            CORD sigil = ptr->is_stack ? "&" : (ptr->is_optional ? "?" : "@");
            if (ptr->is_readonly) sigil = CORD_cat(sigil, "(read-only)");
            return CORD_cat(sigil, type_to_cord(ptr->pointed, expanded, flags));
        }
        case GeneratorType: {
            auto gen = Match(t, GeneratorType);
            CORD ret;
            CORD_sprintf(&ret, "Generator(%r)", type_to_cord(gen->generated, expanded, flags));
            return ret;
        }
        case TaggedUnionType: {
            auto tagged = Match(t, TaggedUnionType);

            if (!(flags & EXPAND_ARGS)) expanded = NULL;

            CORD c = "enum(";
            for (int64_t i = 0, len = LENGTH(tagged->members); i < len; i++) {
                if (i > 0)
                    c = CORD_cat(c, "|");
                auto member = ith(tagged->members, i);
                // name, tag_value, type
                c = CORD_cat(c, member.name);
                if (member.type) {
                    c = CORD_cat(c, "(");
                    auto struct_ = Match(member.type, StructType);
                    for (int64_t j = 0; j < LENGTH(struct_->field_types); j++) {
                        sss_type_t *ft = ith(struct_->field_types, j);
                        const char *fname = struct_->field_names ? ith(struct_->field_names, j) : NULL;
                        if (j > 0)
                            c = CORD_cat(c, ",");

                        if (fname && !streq(fname, heap_strf("_%lu", j+1)))
                            c = CORD_cat(CORD_cat(c, fname), ":");

                        CORD fstr = type_to_cord(ft, expanded, flags);
                        c = CORD_cat(c, fstr);
                    }
                    c = CORD_cat(c, ")");
                }

                if (!(i == 0 ? member.tag_value == 0 : member.tag_value == 1 + ith(tagged->members, i-1).tag_value))
                    CORD_sprintf(&c, "%r=%ld", c, member.tag_value);
            }
            c = CORD_cat(c, ")");
            return c;
        }
        case ModuleType: {
            CORD c;
            CORD_sprintf(&c, "Module(\"%s\")", Match(t, ModuleType)->path);
            return c;
        }
        case VariantType: {
            auto variant = Match(t, VariantType);
            const char *name = (flags & FILENAMES) ? heap_strf("%s:%s", variant->filename, variant->name) : variant->name;
            return name;
        }
        default: {
            CORD c;
            CORD_sprintf(&c, "Unknown type: %d", t->tag);
            return c;
        }
    }
}

int printf_pointer_size(const struct printf_info *info, size_t n, int argtypes[n], int sizes[n])
{
    if (n < 1) return -1;
    (void)info;
    argtypes[0] = PA_POINTER;
    sizes[0] = sizeof(void*);
    return 1;
}

int printf_type(FILE *stream, const struct printf_info *info, const void *const args[])
{
    sss_type_t *t = *(sss_type_t**)args[0];
    table_t expanded = {0};
    stringify_flags_e flags = info->alt ? ARG_NAMES : DEFAULT;
    CORD c = type_to_cord(t, &expanded, flags);
    if (!info->alt || (info->width > 0 && CORD_len(c) > (size_t)info->width))
        c = type_to_cord(t, NULL, flags);
    return CORD_put(c, stream);
}

int printf_ast(FILE *stream, const struct printf_info *info, const void *const args[])
{
    ast_t *ast = *(ast_t**)(args[0]);
    if (ast) {
        if (info->alt)
            return fprintf(stream, "%.*s", (int)(ast->end - ast->start), ast->start);
        else
            return fprintf(stream, "%s", ast_to_str(ast));
    } else {
        return fputs("(null)", stream);
    }
}

const char* type_to_string_concise(sss_type_t *t) {
    return CORD_to_char_star(type_to_cord(t, NULL, DEFAULT));
}

const char* type_to_string(sss_type_t *t) {
    table_t expanded = {0};
    return CORD_to_char_star(type_to_cord(t, &expanded, FILENAMES | EXPAND_ARGS));
}

const char* type_to_typeof_string(sss_type_t *t) {
    table_t expanded = {0};
    return CORD_to_char_star(type_to_cord(t, &expanded, DEFAULT));
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

sss_type_t *base_value_type(sss_type_t *t)
{
    for (;;) {
        if (t->tag == PointerType)
            t = Match(t, PointerType)->pointed;
        else if (t->tag == VariantType)
            t = Match(t, VariantType)->variant_of;
        else break;
    }
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
    if ((a->tag == IntType || a->tag == NumType) && (b->tag == IntType || b->tag == NumType)) {
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
    case VariantType: return type_units(Match(t, VariantType)->variant_of);
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
        return Type(StructType, .field_names=s->field_names, .field_types=s->field_types, .units=units);
    }
    case PointerType: {
        auto ptr = Match(t, PointerType);
        return Type(PointerType, .pointed=with_units(ptr->pointed, units), .is_optional=ptr->is_optional);
    }
    case ArrayType: {
        auto array = Match(t, ArrayType);
        return Type(ArrayType, .item_type=with_units(array->item_type, units));
    }
    case VariantType: {
        auto variant = Match(t, VariantType);
        return Type(VariantType, .filename=variant->filename, .name=variant->name, .variant_of=with_units(variant->variant_of, units));
    }
    default: return t;
    }
}

bool is_integral(sss_type_t *t)
{
    t = base_variant(t);
    return t->tag == IntType || t->tag == CharType;
}

bool is_floating_point(sss_type_t *t)
{
    t = base_variant(t);
    return t->tag == NumType;
}

bool is_numeric(sss_type_t *t)
{
    t = base_variant(t);
    return t->tag == IntType || t->tag == NumType;
}

typedef enum {
    MAG_INAPPLICABLE = 0, MAG_ZERO, MAG1, MAG2, MAG3, MAG4, MAG5, MAG6, MAG7, MAG8, MAG9, MAG10, MAG11, MAG12,
} magnitude_e;

static inline magnitude_e type_min_magnitude(sss_type_t *t)
{
    switch (t->tag) {
    case BoolType: case CharType: return MAG_ZERO;
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
    case VariantType: return type_min_magnitude(Match(t, VariantType)->variant_of);
    default: return MAG_INAPPLICABLE;
    }
}

static inline int type_max_magnitude(sss_type_t *t)
{
    switch (t->tag) {
    case BoolType: return MAG1;
    case CharType: return MAG2;
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
    case NumType: return Match(t, NumType)->bits == 32 ? MAG11 : MAG12;
    case VariantType: return type_max_magnitude(Match(t, VariantType)->variant_of);
    default: return MAG_INAPPLICABLE;
    }
}

precision_cmp_e compare_precision(sss_type_t *a, sss_type_t *b)
{
    magnitude_e a_min = type_min_magnitude(a),
                b_min = type_min_magnitude(b),
                a_max = type_max_magnitude(a),
                b_max = type_max_magnitude(b);

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
        for (int64_t i = 0; i < LENGTH(subtypes); i++) {
            if (!is_orderable(ith(subtypes, i)))
                return false;
        }
        return true;
    }
    case TaggedUnionType: {
        auto members = Match(t, TaggedUnionType)->members;
        for (int64_t i = 0; i < LENGTH(members); i++) {
            auto member = ith(members, i);
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
        for (int64_t i = 0; i < LENGTH(field_types); i++) {
            if (has_heap_memory(ith(field_types, i)))
                return true;
        }
        return false;
    }
    case TaggedUnionType: {
        auto members = Match(t, TaggedUnionType)->members;
        for (int64_t i = 0; i < LENGTH(members); i++) {
            auto member = ith(members, i);
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

    // Automatic dereferencing:
    if (actual->tag == PointerType && !Match(actual, PointerType)->is_optional
        && can_promote(Match(actual, PointerType)->pointed, needed))
        return true;

    // Optional promotion:
    if (needed->tag == PointerType && actual->tag == PointerType) {
        auto needed_ptr = Match(needed, PointerType);
        auto actual_ptr = Match(actual, PointerType);
        if (needed_ptr->pointed->tag != MemoryType && !type_eq(needed_ptr->pointed, actual_ptr->pointed))
            // Can't use @Foo for a function that wants @Baz
            // But you *can* use @Foo for a function that wants @Memory
            return false;
        else if (actual_ptr->is_stack && !needed_ptr->is_stack)
            // Can't use &x for a function that wants a @Foo or ?Foo
            return false;
        else if (actual_ptr->is_optional && !needed_ptr->is_optional)
            // Can't use !Foo for a function that wants @Foo
            return false;
        else if (actual_ptr->is_readonly && !needed_ptr->is_readonly)
            // Can't use pointer to readonly data when we need a pointer that can write to the data
            return false;
        else
            return true;
    }

    // Function promotion:
    if (needed->tag == FunctionType && actual->tag == FunctionType) {
        auto needed_fn = Match(needed, FunctionType);
        auto actual_fn = Match(actual, FunctionType);
        if (LENGTH(needed_fn->arg_types) != LENGTH(actual_fn->arg_types) || !type_eq(needed_fn->ret, actual_fn->ret))
            return false;
        for (int64_t i = 0, len = LENGTH(needed_fn->arg_types); i < len; i++) {
            if (!type_eq(ith(actual_fn->arg_types, i), ith(needed_fn->arg_types, i)))
                return false;
        }
        return true;
    }

    // If we have a DSL, it should be possible to use it as a Str
    if (is_variant_of(actual, needed))
        return true;

    if (actual->tag == StructType && base_variant(needed)->tag == StructType) {
        auto actual_struct = Match(actual, StructType);
        auto needed_struct = Match(base_variant(needed), StructType);
        // TODO: allow promoting with uninitialized or extraneous values?
        if (LENGTH(actual_struct->field_types) != LENGTH(needed_struct->field_types))
            return false;
        for (int64_t i = 0; i < LENGTH(actual_struct->field_types); i++) {
            // TODO: check field names??
            if (!can_promote(ith(actual_struct->field_types, i), ith(needed_struct->field_types, i)))
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
    case ArrayType: case IntType: case NumType: case CharType: case BoolType: case RangeType:
        return true;
    case StructType: {
        auto struct_ = Match(t, StructType);
        for (int64_t i = 0; i < LENGTH(struct_->field_types); i++)
            if (!can_leave_uninitialized(ith(struct_->field_types, i)))
                return false;
        return true;
    }
    case TaggedUnionType: {
        auto members = Match(t, TaggedUnionType)->members;
        foreach (members, member, _) {
            if (member->type && !can_leave_uninitialized(member->type))
                return false;
        }
        return true;
    }
    default: return false;
    }
}

static bool _can_have_cycles(sss_type_t *t, table_t *seen)
{
    switch (t->tag) {
        case ArrayType: return _can_have_cycles(Match(t, ArrayType)->item_type, seen);
        case TableType: {
            auto table = Match(t, TableType);
            return _can_have_cycles(table->key_type, seen) || _can_have_cycles(table->value_type, seen);
        }
        case StructType: {
            auto struct_ = Match(t, StructType);
            for (int64_t i = 0; i < LENGTH(struct_->field_types); i++) {
                sss_type_t *ft = ith(struct_->field_types, i);
                if (_can_have_cycles(ft, seen))
                    return true;
            }
            return false;
        }
        case PointerType: return _can_have_cycles(Match(t, PointerType)->pointed, seen);
        case GeneratorType: return _can_have_cycles(Match(t, GeneratorType)->generated, seen);
        case TaggedUnionType: {
            auto tagged = Match(t, TaggedUnionType);
            for (int64_t i = 0, len = LENGTH(tagged->members); i < len; i++) {
                auto member = ith(tagged->members, i);
                if (member.type && _can_have_cycles(member.type, seen))
                    return true;
            }
            return false;
        }
        case VariantType: {
            const char *name = Match(t, VariantType)->name;
            if (name && Table_str_get(seen, name))
                return true;
            Table_str_set(seen, name, t);
            return _can_have_cycles(Match(t, VariantType)->variant_of, seen);
        }
        default: return false;
    }
}

bool can_have_cycles(sss_type_t *t)
{
    table_t seen = {0};
    return _can_have_cycles(t, &seen);
}

sss_type_t *table_entry_type(sss_type_t *table_type)
{
    static table_t cache = {0};
    sss_type_t *t = Type(StructType, .field_names=ARRAY((const char*)"key", "value"),
                        .field_types=ARRAY((sss_type_t*)Match(table_type, TableType)->key_type,
                                           Match(table_type, TableType)->value_type));
    sss_type_t *cached = Table_str_get(&cache, type_to_string(t));
    if (cached) {
        return cached;
    } else {
        Table_str_set(&cache, type_to_string(t), t);
        return t;
    }
}

sss_type_t *base_variant(sss_type_t *t)
{
    while (t->tag == VariantType)
        t = Match(t, VariantType)->variant_of;
    return t;
}

bool is_variant_of(sss_type_t *t, sss_type_t *base)
{
    for (; t->tag == VariantType; t = Match(t, VariantType)->variant_of) {
        if (type_eq(Match(t, VariantType)->variant_of, base))
            return true;
    }
    return false;
}

sss_type_t *replace_type(sss_type_t *t, sss_type_t *target, sss_type_t *replacement)
{
    if (type_eq(t, target))
        return replacement;

#define COPY(t) memcpy(GC_MALLOC(sizeof(sss_type_t)), (t), sizeof(sss_type_t))
#define REPLACED_MEMBER(t, tag, member) ({ t = memcpy(GC_MALLOC(sizeof(sss_type_t)), (t), sizeof(sss_type_t)); Match((struct sss_type_s*)(t), tag)->member = replace_type(Match((t), tag)->member, target, replacement); t; })
    switch (t->tag) {
        case TypeType: return REPLACED_MEMBER(t, TypeType, type);
        case ArrayType: return REPLACED_MEMBER(t, ArrayType, item_type);
        case TableType: {
            t = REPLACED_MEMBER(t, TableType, key_type);
            t = REPLACED_MEMBER(t, TableType, value_type);
            return t;
        }
        case FunctionType: {
            auto fn = Match(t, FunctionType);
            auto arg_types = EMPTY_ARRAY(sss_type_t*);
            foreach (fn->arg_types, arg_t, _) {
                append(arg_types, replace_type(*arg_t, target, replacement));
            }
            t = COPY(t);
            Match((struct sss_type_s*)t, FunctionType)->ret = replace_type(Match(t, FunctionType)->ret, target, replacement);
            Match((struct sss_type_s*)t, FunctionType)->arg_types = arg_types;
            return t;
        }
        case StructType: {
            auto struct_ = Match(t, StructType);
            auto field_types = EMPTY_ARRAY(sss_type_t*);
            foreach (struct_->field_types, field_t, _) {
                append(field_types, replace_type(*field_t, target, replacement));
            }
            t = COPY(t);
            Match((struct sss_type_s*)t, StructType)->field_types = field_types;
            return t;
        }
        case PointerType: return REPLACED_MEMBER(t, PointerType, pointed);
        case GeneratorType: return REPLACED_MEMBER(t, GeneratorType, generated);
        case TaggedUnionType: {
            auto tagged = Match(t, TaggedUnionType);
            auto members = EMPTY_ARRAY(sss_tagged_union_member_t);
            foreach (tagged->members, member, _) {
                sss_tagged_union_member_t new_member = *member;
                new_member.type = replace_type(new_member.type, target, replacement);
                append(members, new_member);
            }
            t = COPY(t);
            Match((struct sss_type_s*)t, TaggedUnionType)->members = members;
            return t;
        }
        case VariantType: return REPLACED_MEMBER(t, VariantType, variant_of);
        default: return t;
    }
#undef COPY
#undef REPLACED_MEMBER
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
