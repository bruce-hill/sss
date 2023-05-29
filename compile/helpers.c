// Helper functions used for compiling
#include <assert.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <gc/cord.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>

#include "../ast.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../libsss/hashmap.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

static sss_hashmap_t opaque_structs = {0};

const char *fresh(const char *name)
{
    static sss_hashmap_t seen = {0};
    // static int id = 0;
    char *tmp = (char*)heap_str(name);
    for (size_t i = 0; i < strlen(tmp); i++) {
        if (!isalpha(name[i]) && !isdigit(name[i]) && name[i] != '_')
            tmp[i] = '_';
    }
    name = (const char*)tmp;
    int64_t count = hget(&seen, name, int64_t);
    const char *ret = heap_strf("%s__%ld", name, count++);
    hset(&seen, name, count+1);
    return ret;
}

// Kinda janky, but libgccjit doesn't have this function built in
ssize_t gcc_alignof(env_t *env, sss_type_t *sss_t)
{
    switch (sss_t->tag) {
    case ArrayType: return sizeof(void*);
    case TableType: return sizeof(void*);
    case StructType: {
        ssize_t align = 0;
        auto struct_type = Match(sss_t, StructType);
        foreach (struct_type->field_types, ftype, _) {
            if (type_eq(*ftype, sss_t))
                compiler_err(env, NULL, "The struct '%s' recursively contains itself, which would be infinitely large. If you want to reference other %s structs, use a pointer or an array.",
                             type_to_string(sss_t), type_to_string(sss_t));
            ssize_t field_align = gcc_alignof(env, *ftype);
            if (field_align > align) align = field_align;
        }
        return align;
    }
    case TaggedUnionType: {
        auto tagged = Match(sss_t, TaggedUnionType);
        ssize_t align = tagged->tag_bits/8;
        foreach (tagged->members, member, _) {
            if (!member->type) continue;
            ssize_t member_align = gcc_alignof(env, member->type);
            if (member_align > align) align = member_align;
        }
        return align;
    }
    case ModuleType: return 1;
    case VoidType: return 1;
    default:
        return gcc_sizeof(env, sss_t);
    }
}

// Kinda janky, but libgccjit doesn't have this function built in, except for integer types:
ssize_t gcc_sizeof(env_t *env, sss_type_t *sss_t)
{
    gcc_type_t *gcc_t = sss_type_to_gcc(env, sss_t);
    if (gcc_type_is_integral(gcc_t))
        return gcc_type_size(gcc_t);

    switch (sss_t->tag) {
    case ArrayType: return sizeof (struct {void* items; int32_t len; int16_t stride, free;});
    case TableType: return sizeof (sss_hashmap_t);
    case RangeType: return sizeof (struct {int64_t start,stop,step;});
    case BoolType: return sizeof(bool);
    case TypeType: return sizeof(char*);
    case NumType: return Match(sss_t, NumType)->bits / 8;
    case FunctionType:
    case PointerType: return sizeof(void*);
    case StructType: {
        ssize_t size = 0;
        ssize_t max_align = 0;
        auto struct_type = Match(sss_t, StructType);
        foreach (struct_type->field_types, ftype, _) {
            if (type_eq(*ftype, sss_t))
                compiler_err(env, NULL, "The struct '%s' recursively contains itself, which would be infinitely large. If you want to reference other %s structs, use a pointer or an array.",
                             type_to_string(sss_t), type_to_string(sss_t));
            ssize_t field_align = gcc_alignof(env, *ftype);
            if (field_align > 1 && size % field_align)
                size += field_align - (size % field_align); // padding
            size += gcc_sizeof(env, *ftype);
            if (field_align > max_align) max_align = field_align;
        }
        if (max_align > 1 && size % max_align)
            size += max_align - (size % max_align); // padding
        return size;
    }
    case TaggedUnionType: {
        auto tagged = Match(sss_t, TaggedUnionType);
        ssize_t size = tagged->tag_bits / 8;
        ssize_t union_align = size;
        ssize_t union_size = 0;
        foreach (tagged->members, member, _) {
            if (!member->type) continue;
            ssize_t member_align = gcc_alignof(env, member->type);
            if (member_align > union_align) union_align = member_align;
            ssize_t member_size = gcc_sizeof(env, member->type);
            if (member_size > union_size) union_size = member_size;
        }
        // Padding for union:
        if (size % union_align != 0) size = (size - (size % union_align)) + union_align;
        size += union_size;
        return size;
    }
    case ModuleType: return 0;
    case VoidType: return 0;
    default: compiler_err(env, NULL, "gcc_sizeof() isn't implemented for %s", type_to_string(sss_t));
    }
}

gcc_type_t *get_tag_type(env_t *env, sss_type_t *t)
{
    auto tagged = Match(t, TaggedUnionType);
    switch (tagged->tag_bits) {
    case 64: return gcc_type(env->ctx, INT64);
    case 32: return gcc_type(env->ctx, INT32);
    case 16: return gcc_type(env->ctx, INT16);
    case 8: return gcc_type(env->ctx, INT8);
    default: compiler_err(env, NULL, "Unsupported tagged enum size: %d\n", tagged->tag_bits); 
    }
}

gcc_type_t *get_union_type(env_t *env, sss_type_t *t)
{
    static sss_hashmap_t cache = {0};
    gcc_type_t *gcc_t = hget(&cache, type_to_string(t), gcc_type_t*);
    if (gcc_t) return gcc_t;
    auto tagged = Match(t, TaggedUnionType);
    auto fields = LIST(gcc_field_t*);
    foreach (tagged->members, member, _) {
        if (member->type && hget(&opaque_structs, type_to_string(member->type), gcc_type_t*))
            compiler_err(env, NULL, "The tagged union '%s' recursively contains itself, which could be infinitely large. If you want to reference other %s values, use a pointer or an array.",
                         type_to_string(t), type_to_string(t));
        gcc_type_t *gcc_ft = member->type ? sss_type_to_gcc(env, member->type)
            : gcc_struct_as_type(gcc_new_struct_type(env->ctx, NULL, member->name, 0, NULL)); // empty struct
        gcc_field_t *field = gcc_new_field(env->ctx, NULL, gcc_ft, member->name);
        append(fields, field);
    }
    gcc_type_t *union_gcc_t = gcc_union(env->ctx, NULL, "data_union", length(fields), fields[0]);
    hset(&cache, type_to_string(t), union_gcc_t);
    return union_gcc_t;
}

// This must be memoized because GCC JIT doesn't do structural equality
gcc_type_t *sss_type_to_gcc(env_t *env, sss_type_t *t)
{
    static sss_hashmap_t cache = {0};
    t = with_units(t, NULL);
    gcc_type_t *gcc_t = hget(&cache, type_to_string(t), gcc_type_t*);
    if (gcc_t) return gcc_t;

    switch (t->tag) {
    case IntType: {
        if (Match(t, IntType)->is_unsigned) {
            switch (Match(t, IntType)->bits) {
            case 64: gcc_t = gcc_type(env->ctx, UINT64); break;
            case 32: gcc_t = gcc_type(env->ctx, UINT32); break;
            case 16: gcc_t = gcc_type(env->ctx, UINT16); break;
            case 8: gcc_t = gcc_type(env->ctx, UINT8); break;
            case 0: gcc_t = gcc_type(env->ctx, UINT32); break;
            default: compiler_err(env, NULL, "I couldn't get a GCC type for an unsigned integer with %d bits", Match(t, IntType)->bits);
            }
        } else {
            switch (Match(t, IntType)->bits) {
            case 64: gcc_t = gcc_type(env->ctx, INT64); break;
            case 32: gcc_t = gcc_type(env->ctx, INT32); break;
            case 16: gcc_t = gcc_type(env->ctx, INT16); break;
            case 8: gcc_t = gcc_type(env->ctx, INT8); break;
            case 0: gcc_t = gcc_type(env->ctx, INT); break;
            default: compiler_err(env, NULL, "I couldn't get a GCC type for an integer with %d bits", Match(t, IntType)->bits);
            }
        }
        break;
    }
    case CharType: case CStringCharType: gcc_t = gcc_type(env->ctx, CHAR); break;
    case BoolType: gcc_t = gcc_type(env->ctx, BOOL); break;
    case NumType: gcc_t = Match(t, NumType)->bits == 32 ? gcc_type(env->ctx, FLOAT) : gcc_type(env->ctx, DOUBLE); break;
    case VoidType: case AbortType: gcc_t = gcc_type(env->ctx, VOID); break;
    case PointerType: {
        gcc_t = sss_type_to_gcc(env, Match(t, PointerType)->pointed);
        gcc_t = gcc_get_ptr_type(gcc_t);
        break;
    }
    case RangeType: {
        gcc_type_t *i64 = gcc_type(env->ctx, INT64);
        gcc_field_t *fields[3] = {
            gcc_new_field(env->ctx, NULL, i64, "first"),
            gcc_new_field(env->ctx, NULL, i64, "step"),
            gcc_new_field(env->ctx, NULL, i64, "last"),
        };
        gcc_struct_t *range = gcc_new_struct_type(env->ctx, NULL, "Range", 3, fields);
        gcc_t = gcc_struct_as_type(range);
        break;
    }
    case ArrayType: {
        sss_type_t *item_type = Match(t, ArrayType)->item_type;
        // GCC type is the same for DSL and non-DSLs:
        if (Match(t, ArrayType)->dsl) return sss_type_to_gcc(env, Type(ArrayType, .item_type=item_type));
        gcc_field_t *fields[] = {
            [ARRAY_DATA_FIELD]=gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(sss_type_to_gcc(env, item_type)), "items"),
            [ARRAY_LENGTH_FIELD]=gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT32), "length"),
            [ARRAY_STRIDE_FIELD]=gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT16), "stride"),
            [ARRAY_CAPACITY_FIELD]=gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT16), "free"),
        };
        gcc_struct_t *array = gcc_new_struct_type(env->ctx, NULL, fresh("Array"), sizeof(fields)/sizeof(fields[0]), fields);
        gcc_t = gcc_struct_as_type(array);
        break;
    }
    case TableType: {
        gcc_type_t *u32 = gcc_type(env->ctx, UINT32);
        auto table = Match(t, TableType);
        if (table->key_type->tag == VoidType || table->value_type->tag == VoidType)
            compiler_err(env, NULL, "Tables can't hold Void types");

        gcc_field_t *bucket_fields[] = {
            gcc_new_field(env->ctx, NULL, u32, "index1"),
            gcc_new_field(env->ctx, NULL, u32, "next1"),
        };
        gcc_struct_t *bucket = gcc_new_struct_type(env->ctx, NULL, "Bucket", 2, bucket_fields);

        gcc_struct_t *gcc_struct = gcc_opaque_struct(env->ctx, NULL, "Table");
        gcc_field_t *fields[] = {
            [TABLE_ENTRIES_FIELD]=gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, VOID_PTR), "entries"),
            [TABLE_BUCKETS_FIELD]=gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(gcc_struct_as_type(bucket)), "buckets"),
            [TABLE_FALLBACK_FIELD]=gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(gcc_struct_as_type(gcc_struct)), "fallback"),
            [TABLE_DEFAULT_FIELD]=gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(sss_type_to_gcc(env, table->value_type)), "default_value"),
            [TABLE_CAPACITY_FIELD]=gcc_new_field(env->ctx, NULL, u32, "capacity"),
            [TABLE_COUNT_FIELD]=gcc_new_field(env->ctx, NULL, u32, "count"),
            [TABLE_LASTFREE_FIELD]=gcc_new_field(env->ctx, NULL, u32, "lastfree_index1"),
            [TABLE_COW_FIELD]=gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, BOOL), "copy_on_write"),
        };
        gcc_set_fields(gcc_struct, NULL, sizeof(fields)/sizeof(fields[0]), fields);
        gcc_t = gcc_struct_as_type(gcc_struct);
        break;
    }
    case FunctionType: {
        NEW_LIST(gcc_type_t*, arg_types);
        auto fn = Match(t, FunctionType);
        foreach (fn->arg_types, arg_t, _)
            append(arg_types, sss_type_to_gcc(env, *arg_t));
        gcc_type_t *ret_type = sss_type_to_gcc(env, fn->ret);
        gcc_t = gcc_new_func_type(env->ctx, NULL, ret_type, length(arg_types), arg_types[0], 0);
        break;
    }
    case StructType: {
        auto struct_t = Match(t, StructType);
        const char *t_str = type_to_string(t);
        gcc_struct_t *gcc_struct = gcc_opaque_struct(env->ctx, NULL, struct_t->name ? struct_t->name : "Tuple");
        gcc_t = gcc_struct_as_type(gcc_struct);
        hset(&cache, t_str, gcc_t);
        hset(&opaque_structs, t_str, gcc_t);

        NEW_LIST(gcc_field_t*, fields);
        for (int64_t i = 0; i < length(struct_t->field_types); i++) {
            sss_type_t *sss_ft = ith(struct_t->field_types, i);
            if (hget(&opaque_structs, type_to_string(sss_ft), gcc_type_t*))
                compiler_err(env, NULL, "The struct '%s' recursively contains itself, which would be infinitely large. If you want to reference other %s structs, use a pointer or an array.",
                             type_to_string(t), type_to_string(t));
            gcc_type_t *gcc_ft = sss_type_to_gcc(env, sss_ft);
            assert(gcc_ft);
            gcc_field_t *field = gcc_new_field(env->ctx, NULL, gcc_ft, ith(struct_t->field_names, i));
            append(fields, field);
        }
        gcc_set_fields(gcc_struct, NULL, length(fields), fields[0]);
        gcc_t = gcc_struct_as_type(gcc_struct);
        hremove(&opaque_structs, t_str, gcc_type_t*);
        break;
    }
    case TaggedUnionType: {
        auto tagged = Match(t, TaggedUnionType);
        const char *t_str = type_to_string(t);
        gcc_struct_t *gcc_struct = gcc_opaque_struct(env->ctx, NULL, tagged->name);
        gcc_t = gcc_struct_as_type(gcc_struct);
        hset(&cache, type_to_string(t), gcc_t);
        hset(&opaque_structs, t_str, gcc_t);
        gcc_set_fields(gcc_struct, NULL, 2, (gcc_field_t*[]){
            gcc_new_field(env->ctx, NULL, get_tag_type(env, t), "tag"),
            gcc_new_field(env->ctx, NULL, get_union_type(env, t), "__data"),
        });
        hremove(&opaque_structs, t_str, gcc_type_t*);
        break;
    }
    case TypeType: {
        gcc_t = gcc_get_ptr_type(gcc_type(env->ctx, CHAR));
        break;
    }
    case GeneratorType: {
        auto generator = Match(t, GeneratorType);
        switch (generator->generated->tag) {
        case AbortType: case VoidType: return gcc_type(env->ctx, VOID);
        default: goto unknown_gcc_type;
        }
    }
    case ModuleType: {
        gcc_t = gcc_struct_as_type(gcc_new_struct_type(env->ctx, NULL, "Module", 0, NULL));
        break;
    }
    default: {
      unknown_gcc_type:
        compiler_err(env, NULL, "The following SSS type doesn't have a GCC type: %s", type_to_string(t));
    }
    }

    hset(&cache, type_to_string(t), gcc_t);
    return gcc_t;
}

bool demote_int_literals(ast_t **ast, sss_type_t *needed)
{
    if ((*ast)->tag != Int || needed->tag != IntType) return false;

    auto needed_int = Match(needed, IntType);
    auto int_ast = Match(*ast, Int);
    if (streq(needed_int->units, int_ast->units)) {
        *ast = WrapAST(*ast, Int, .i=int_ast->i, .precision=needed_int->bits, .is_unsigned=needed_int->is_unsigned, .units=needed_int->units);
        return true;
    } else {
        return false;
    }
}

bool promote(env_t *env, sss_type_t *actual, gcc_rvalue_t **val, sss_type_t *needed)
{
    if (!can_promote(actual, needed))
        return false;

    // String <-> c string promotion
    if (type_eq(actual, Type(PointerType, .pointed=Type(CStringCharType))) && type_eq(needed, Type(ArrayType, .item_type=Type(CharType)))) {
        binding_t *b = get_from_namespace(env, needed, "from_pointer");
        *val = gcc_callx(env->ctx, NULL, b->func, *val);
    } else if (type_eq(actual, Type(ArrayType, .item_type=Type(CharType))) && type_eq(needed, Type(PointerType, .pointed=Type(CStringCharType)))) {
        binding_t *b = get_from_namespace(env, actual, "c_string");
        *val = gcc_callx(env->ctx, NULL, b->func, *val);
    } else if (actual->tag == StructType && needed->tag == StructType) { // Struct promotion
        gcc_type_t *actual_gcc_t = sss_type_to_gcc(env, actual);
        gcc_type_t *needed_gcc_t = sss_type_to_gcc(env, needed);
        auto actual_field_types = Match(actual, StructType)->field_types;
        auto needed_field_types = Match(needed, StructType)->field_types;
        if (LIST_LEN(needed_field_types) == 0)
            return true;
        NEW_LIST(gcc_field_t*, needed_fields);
        NEW_LIST(gcc_rvalue_t*, field_vals);
        for (int64_t i = 0; i < LIST_LEN(actual_field_types); i++) {
            APPEND(needed_fields, gcc_get_field(gcc_type_if_struct(needed_gcc_t), i));
            gcc_rvalue_t *field_val = gcc_rvalue_access_field(*val, NULL, gcc_get_field(gcc_type_if_struct(actual_gcc_t), i));
            if (!promote(env, LIST_ITEM(actual_field_types, i), &field_val, LIST_ITEM(needed_field_types, i)))
                return false;
            APPEND(field_vals, field_val);
        }
        *val = gcc_struct_constructor(env->ctx, NULL, needed_gcc_t, LIST_LEN(needed_fields), needed_fields[0], field_vals[0]);
    } else if (!type_eq(actual, needed) || actual->tag == FunctionType) {
        *val = gcc_cast(env->ctx, NULL, *val, sss_type_to_gcc(env, needed));
    }
    return true;
}

void check_truthiness(env_t *env, gcc_block_t **block, ast_t *obj, gcc_block_t *if_truthy, gcc_block_t *if_falsey)
{
    sss_type_t *t = get_type(env, obj);
    gcc_rvalue_t *bool_val = compile_expr(env, block, obj); 
    if (t->tag == PointerType) {
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, bool_val, gcc_null(env->ctx, gcc_t));
    } else if (t->tag != BoolType) {
        compiler_err(env, obj, "This value can't be used for a boolean value, since it's a %s\n", type_to_string(t));
    }
    gcc_jump_condition(*block, NULL, bool_val, if_truthy, if_falsey);
    *block = NULL;
}

void maybe_print_str(env_t *env, gcc_block_t **block, gcc_rvalue_t *do_print, gcc_rvalue_t *file, const char *str)
{
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *print_block = gcc_new_block(func, fresh("do_print")),
                *done_block = gcc_new_block(func, fresh("no_print"));
    gcc_jump_condition(*block, NULL, do_print, print_block, done_block);

    gcc_func_t *fputs_fn = get_function(env, "fputs");
    gcc_eval(print_block, NULL, gcc_callx(env->ctx, NULL, fputs_fn, gcc_str(env->ctx, str), file));
    gcc_jump(print_block, NULL, done_block);
    *block = done_block;
}

gcc_func_t *get_print_func(env_t *env, sss_type_t *t)
{
    // Hash map for tracking recursion: {ptr => index, "__max_index" => max_index}
    // Create a function `int print(T obj, FILE* file, void* recursion)`
    // that prints an object to a given file

    // print() is the same for optional/non-optional pointers:
    if (t->tag == PointerType)
        t = Type(PointerType, .pointed=Match(t, PointerType)->pointed, .is_optional=true, .is_stack=Match(t, PointerType)->is_stack);

    // Memoize:
    binding_t *b = get_from_namespace(env, t, "__print");
    if (b) return b->func;

    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);

    gcc_type_t *void_ptr_t = sss_type_to_gcc(env, Type(PointerType, .pointed=Type(VoidType)));
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("obj")),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, FILE_PTR), fresh("file")),
        gcc_new_param(env->ctx, NULL, void_ptr_t, fresh("recursion")),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, BOOL), fresh("color")),
    };
    const char* sym_name = fresh("__print");
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, gcc_type(env->ctx, VOID), sym_name, 4, params, 0);
    sss_type_t *fn_t = Type(FunctionType,
                           .arg_types=LIST(sss_type_t*, t, Type(PointerType, .pointed=Type(VoidType)),
                                           Type(PointerType, .pointed=Type(VoidType)), Type(BoolType)),
                           .arg_names=LIST(const char*, "obj", "file", "recursion", "color"),
                           .arg_defaults=NULL, .ret=Type(IntType));
    sss_hashmap_t *ns = get_namespace(env, t);
    hset(ns, "__print", new(binding_t, .func=func, .rval=gcc_get_func_address(func, NULL), .type=fn_t, .sym_name=sym_name));

    gcc_block_t *block = gcc_new_block(func, fresh("print"));
    gcc_comment(block, NULL, CORD_to_char_star(CORD_cat("print() for type: ", type_to_string(t))));
    gcc_rvalue_t *obj = gcc_param_as_rvalue(params[0]);
    gcc_rvalue_t *file = gcc_param_as_rvalue(params[1]);
    gcc_rvalue_t *rec = gcc_param_as_rvalue(params[2]);
    gcc_rvalue_t *color = gcc_param_as_rvalue(params[3]);

    gcc_func_t *fputs_fn = get_function(env, "fputs");

#define WRITE_LITERAL(block, str) gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, fputs_fn, gcc_str(env->ctx, str), file))
#define COLOR_LITERAL(block, str) maybe_print_str(env, block, color, file, str)

    switch (t->tag) {
    case BoolType: {
        gcc_block_t *yes_block = gcc_new_block(func, fresh("yes"));
        gcc_block_t *no_block = gcc_new_block(func, fresh("no"));
        COLOR_LITERAL(&block, "\x1b[35m");
        assert(block);
        gcc_jump_condition(block, NULL, obj, yes_block, no_block);
        WRITE_LITERAL(yes_block, "yes");
        COLOR_LITERAL(&yes_block, "\x1b[m");
        gcc_return_void(yes_block, NULL);
        WRITE_LITERAL(no_block, "no");
        COLOR_LITERAL(&no_block, "\x1b[m");
        gcc_return_void(no_block, NULL);
        break;
    }
    case CharType: case CStringCharType: {
        char *escapes[128] = {['\a']="\\a",['\b']="\\b",['\x1b']="\\e",['\f']="\\f",['\n']="\\n",['\t']="\\t",['\r']="\\r",['\v']="\\v",['"']="\\\""};
        NEW_LIST(gcc_case_t*, cases);

        for (int i = 0; i < 128; i++) {
            char *escape_str = escapes[i];
            if (!escape_str) continue;
            gcc_rvalue_t *case_val = gcc_rvalue_from_long(env->ctx, gcc_t, i);
            gcc_block_t *case_block = gcc_new_block(func, fresh("char_escape"));
            gcc_case_t *case_ = gcc_new_case(env->ctx, case_val, case_val, case_block);
            COLOR_LITERAL(&case_block, "\x1b[1;34m");
            WRITE_LITERAL(case_block, escape_str);
            COLOR_LITERAL(&case_block, "\x1b[m");
            gcc_return_void(case_block, NULL);
            APPEND(cases, case_);
        }

        // Hex escape:
        gcc_block_t *hex_block = gcc_new_block(func, fresh("char_hex_escape"));
        int intervals[][2] = {{'\0','\x06'}, {'\x0E','\x1A'}, {'\x1C','\x1F'},{'\x7F','\x7F'},{CHAR_MIN,-1}};
        for (size_t i = 0; i < sizeof(intervals)/sizeof(intervals[0]); i++) {
            gcc_case_t *hex_case = gcc_new_case(
                env->ctx,
                gcc_rvalue_from_long(env->ctx, gcc_t, intervals[i][0]),
                gcc_rvalue_from_long(env->ctx, gcc_t, intervals[i][1]), hex_block);
            APPEND(cases, hex_case);
        }

        COLOR_LITERAL(&hex_block, "\x1b[1;34m");
        gcc_func_t *fprintf_fn = get_function(env, "fprintf");
        gcc_eval(hex_block, NULL, gcc_callx(env->ctx, NULL, fprintf_fn, file, gcc_str(env->ctx, "\\x%02X"), obj));
        COLOR_LITERAL(&hex_block, "\x1b[m");
        gcc_return_void(hex_block, NULL);

        gcc_block_t *default_block = gcc_new_block(func, fresh("default"));
        gcc_switch(block, NULL, obj, default_block, length(cases), cases[0]);

        COLOR_LITERAL(&default_block, "\x1b[35m");
        gcc_func_t *fputc_fn = get_function(env, "fputc");
        gcc_eval(default_block, NULL, gcc_callx(env->ctx, NULL, fputc_fn, obj, file));
        COLOR_LITERAL(&default_block, "\x1b[m");
        gcc_return_void(default_block, NULL);

        break;
    }
    case IntType: case NumType: {
        COLOR_LITERAL(&block, "\x1b[35m");
        const char *fmt;
        if (t->tag == IntType && (Match(t, IntType)->bits == 64 || Match(t, IntType)->bits == 0)) {
            fmt = Match(t, IntType)->is_unsigned ? "%lu" : "%ld";
        } else if (t->tag == IntType && Match(t, IntType)->bits == 32) {
            fmt = Match(t, IntType)->is_unsigned ? "%u_u32" : "%d_i32";
        } else if (t->tag == IntType && Match(t, IntType)->bits == 16) {
            // I'm not sure why, but printf() gets confused if you pass smaller ints to a "%d" format
            obj = gcc_cast(env->ctx, NULL, obj, gcc_type(env->ctx, INT));
            fmt = Match(t, IntType)->is_unsigned ? "%u_u16" : "%d_i16";
        } else if (t->tag == IntType && Match(t, IntType)->bits == 8) {
            obj = gcc_cast(env->ctx, NULL, obj, gcc_type(env->ctx, INT));
            fmt = Match(t, IntType)->is_unsigned ? "%u_u8" : "%d_i8";
        } else if (t->tag == NumType && (Match(t, NumType)->bits == 64 || Match(t, NumType)->bits == 0)) {
            fmt = "%g";
        } else if (t->tag == NumType && Match(t, NumType)->bits == 32) {
            // I'm not sure why, but printf() gets confused if you pass a 'float' here instead of a 'double'
            obj = gcc_cast(env->ctx, NULL, obj, gcc_type(env->ctx, DOUBLE));
            fmt = "%g_f32";
        } else {
            errx(1, "Unreachable");
        }
        const char* units = type_units(t);
        if (streq(units, "%")) {
            obj = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, gcc_t, obj, gcc_rvalue_from_long(env->ctx, gcc_t, 100));
        }
        gcc_func_t *fprintf_fn = get_function(env, "fprintf");
        gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, fprintf_fn, file, gcc_str(env->ctx, fmt), obj));

        if (streq(units, "%")) {
            COLOR_LITERAL(&block, "\x1b[33;2m");
            WRITE_LITERAL(block, "%");
            COLOR_LITERAL(&block, "\x1b[m");
        } else if (units && strlen(units) > 0) {
            COLOR_LITERAL(&block, "\x1b[33;2m");
            WRITE_LITERAL(block, heap_strf("<%s>", units));
            COLOR_LITERAL(&block, "\x1b[m");
        } else {
            COLOR_LITERAL(&block, "\x1b[m");
        }
        gcc_return_void(block, NULL);
        break;
    }
    case TaggedUnionType: {
        gcc_struct_t *tagged_struct = gcc_type_if_struct(gcc_t);
        gcc_field_t *tag_field = gcc_get_field(tagged_struct, 0);
        gcc_rvalue_t *tag = gcc_rvalue_access_field(obj, NULL, tag_field);
        auto tagged = Match(t, TaggedUnionType);
        COLOR_LITERAL(&block, "\x1b[0;1;36m");
        WRITE_LITERAL(block, tagged->name);
        WRITE_LITERAL(block, ".");
        gcc_block_t *done = gcc_new_block(func, fresh("done"));
        gcc_type_t *tag_gcc_t = get_tag_type(env, t);
        gcc_lvalue_t *tag_var = gcc_local(func, NULL, tag_gcc_t, "_tag");
        gcc_assign(block, NULL, tag_var, tag);
        tag = gcc_rval(tag_var);
        gcc_type_t *union_gcc_t = get_union_type(env, t);
        NEW_LIST(gcc_case_t*, cases);
        bool any_values = false;
        for (int64_t i = 0; i < length(tagged->members); i++) {
            auto member = ith(tagged->members, i);
            if (member.type) any_values = true;
            gcc_block_t *tag_block = gcc_new_block(func, fresh(member.name));
            gcc_block_t *rest_of_tag_block = tag_block;
            WRITE_LITERAL(rest_of_tag_block, member.name);
            if (member.type) {
                WRITE_LITERAL(rest_of_tag_block, "(");
                COLOR_LITERAL(&rest_of_tag_block, "\x1b[m");
                gcc_field_t *data_field = gcc_get_field(tagged_struct, 1);
                gcc_rvalue_t *data = gcc_rvalue_access_field(obj, NULL, data_field);
                gcc_field_t *union_field = gcc_get_union_field(union_gcc_t, i);
                gcc_func_t *tag_print = get_print_func(env, member.type);
                gcc_eval(rest_of_tag_block, NULL, gcc_callx(
                    env->ctx, NULL, tag_print,
                    gcc_rvalue_access_field(data, NULL, union_field),
                    file, rec, color));
                COLOR_LITERAL(&rest_of_tag_block, "\x1b[0;1;36m");
                WRITE_LITERAL(rest_of_tag_block, ")");
                COLOR_LITERAL(&rest_of_tag_block, "\x1b[m");
            }
            gcc_jump(rest_of_tag_block, NULL, done);
            gcc_rvalue_t *rval = gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value);
            gcc_case_t *case_ = gcc_new_case(env->ctx, rval, rval, tag_block);
            APPEND(cases, case_);
        }
        gcc_block_t *default_block = gcc_new_block(func, fresh("default"));
        gcc_block_t *rest_of_default = default_block;
        if (any_values) {
            COLOR_LITERAL(&rest_of_default, "\x1b[31;1m");
            WRITE_LITERAL(rest_of_default, "???");
            COLOR_LITERAL(&rest_of_default, "\x1b[m");
        } else {
            // for each tag, if val&tag, print "+Tag", then print "+???" if anything left over
            gcc_block_t *continue_loop = gcc_new_block(func, fresh("find_tags")),
                        *done = gcc_new_block(func, fresh("done"));
            WRITE_LITERAL(continue_loop, "+");
            gcc_jump(continue_loop, NULL, rest_of_default);

            // gcc_jump(rest_of_default, NULL, continue_loop);
            // rest_of_default = continue_loop;
            for (int64_t i = 0; i < length(tagged->members); i++) {
                // Pseudocode:
                //     if (tag & member_tag == member_tag) {
                //         print(member_tag);
                //         tag &= ~member_tag;
                //     }
                auto member = ith(tagged->members, i);
                if (member.tag_value == 0) continue;
                gcc_block_t *has_tag = gcc_new_block(func, fresh("has_tag")),
                            *done_with_tag = gcc_new_block(func, fresh("done_with_tag"));
                gcc_rvalue_t *member_tag = gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value);
                gcc_rvalue_t *bit_and = gcc_binary_op(env->ctx, NULL, GCC_BINOP_BITWISE_AND, tag_gcc_t, tag, member_tag);
                gcc_jump_condition(rest_of_default, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, bit_and, member_tag),
                                   has_tag, done_with_tag);
                WRITE_LITERAL(has_tag, member.name);
                gcc_update(has_tag, NULL, tag_var, GCC_BINOP_BITWISE_AND, gcc_unary_op(env->ctx, NULL, GCC_UNOP_BITWISE_NEGATE, tag_gcc_t, member_tag));
                gcc_jump_condition(has_tag, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, tag, gcc_zero(env->ctx, tag_gcc_t)),
                                   continue_loop, done);
                rest_of_default = done_with_tag;
            }

            gcc_block_t *has_leftovers = gcc_new_block(func, fresh("has_leftovers"));
            gcc_jump_condition(rest_of_default, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, tag, gcc_zero(env->ctx, tag_gcc_t)),
                               has_leftovers, done);
            COLOR_LITERAL(&has_leftovers, "\x1b[31;1m");
            WRITE_LITERAL(has_leftovers, "???");
            COLOR_LITERAL(&has_leftovers, "\x1b[m");
            gcc_jump(has_leftovers, NULL, done);
            rest_of_default = done;
        }
        gcc_jump(rest_of_default, NULL, done);

        gcc_switch(block, NULL, tag, default_block, length(cases), cases[0]);

        gcc_return_void(done, NULL);
        break;
    }
    case VoidType: {
        errx(1, "Can't define print functions with 'void' as an argument");
        break;
    }
    case RangeType: {
        errx(1, "This should be handled by an externally defined function.");
    }
    case PointerType: {
        gcc_block_t *nil_block = gcc_new_block(func, fresh("nil")),
                    *nonnil_block = gcc_new_block(func, fresh("nonnil"));

        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        gcc_rvalue_t *is_nil = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, obj, gcc_null(env->ctx, gcc_t));

        assert(block);
        gcc_jump_condition(block, NULL, is_nil, nil_block, nonnil_block);
        block = NULL;

        // If it's nil, print !Type:
        sss_type_t *pointed_type = Match(t, PointerType)->pointed;
        COLOR_LITERAL(&nil_block, "\x1b[0;34;1m");
        WRITE_LITERAL(nil_block, heap_strf("!%s", type_to_string(pointed_type)));
        COLOR_LITERAL(&nil_block, "\x1b[m");
        gcc_return_void(nil_block, NULL);

        if (pointed_type->tag == CStringCharType) {
            gcc_func_t *fputs_fn = get_function(env, "fputs");
            gcc_eval(nonnil_block, NULL, gcc_callx(env->ctx, NULL, fputs_fn, obj, file));
            gcc_return_void(nonnil_block, NULL);
            return func;
        }

        gcc_func_t *fprintf_fn = get_function(env, "fprintf");

        const char *sigil = Match(t, PointerType)->is_stack ? "&" : "@";

        if (pointed_type->tag == VoidType) {
            block = nonnil_block;
            COLOR_LITERAL(&block, "\x1b[0;34;1m");
            gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, fprintf_fn, file, gcc_str(env->ctx, heap_strf("%sVoid<%%p>", sigil)), obj));
            COLOR_LITERAL(&block, "\x1b[m");
            gcc_return_void(block, NULL);
            block = NULL;
            break;
        }

        { // If it's non-nil, check for cycles:
            // Summary of the approach:
            //     index = *hashmap_set(cycle_checker, &obj, NULL)
            //     if (index == *cycle_checker->default_value) (i.e. uninitialized, i.e. this is the first time we've seen this)
            //         ++cycle_checker->default_value;
            //         ...proceed...
            //     else
            //         print("@#%d", index)
            gcc_type_t *i64 = gcc_type(env->ctx, INT64);
            gcc_type_t *void_star = gcc_type(env->ctx, VOID_PTR);
            gcc_func_t *hash_set_func = get_function(env, "sss_hashmap_set");
            gcc_func_t *hash_func = get_function(env, "hash_64bits");
            gcc_func_t *cmp_func = get_function(env, "compare_64bits");

            // val = sss_hashmap_set(rec, &obj, NULL)
            block = nonnil_block;
            gcc_block_t *noncycle_block = gcc_new_block(func, fresh("noncycle"));
            gcc_block_t *cycle_block = gcc_new_block(func, fresh("cycle"));
            sss_type_t *rec_t = Type(TableType, .key_type=Type(PointerType, .pointed=Type(VoidType)), .value_type=Type(IntType, .bits=64));
            gcc_rvalue_t *index_ptr = gcc_callx(
                env->ctx, NULL, hash_set_func, rec,
                gcc_cast(env->ctx, NULL, gcc_get_func_address(hash_func, NULL), void_star),
                gcc_cast(env->ctx, NULL, gcc_get_func_address(cmp_func, NULL), void_star),
                gcc_rvalue_size(env->ctx, sizeof(struct{void* key; int64_t value;})), 
                gcc_cast(env->ctx, NULL, gcc_lvalue_address(gcc_param_as_lvalue(params[0]), NULL), void_star),
                gcc_rvalue_size(env->ctx, offsetof(struct{void* key; int64_t value;}, value)), 
                gcc_null(env->ctx, gcc_get_ptr_type(gcc_type(env->ctx, INT64))));
            gcc_lvalue_t *index_var = gcc_local(func, NULL, gcc_get_ptr_type(i64), "_index");
            gcc_assign(block, NULL, index_var, gcc_cast(env->ctx, NULL, index_ptr, gcc_get_ptr_type(i64)));

            gcc_type_t *rec_gcc_t = sss_type_to_gcc(env, rec_t);
            gcc_lvalue_t *rec_default = gcc_deref(gcc_rval(gcc_deref_field(
                gcc_cast(env->ctx, NULL, rec, gcc_get_ptr_type(rec_gcc_t)), NULL,
                gcc_get_field(gcc_type_if_struct(sss_type_to_gcc(env, rec_t)), TABLE_DEFAULT_FIELD))), NULL);

            // if (entry == NULL) goto cycle else goto noncycle
            gcc_jump_condition(block, NULL,
                               gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, gcc_rval(gcc_deref(gcc_rval(index_var), NULL)),
                                              gcc_rval(rec_default)),
                               cycle_block, noncycle_block);

            // If we're in a recursive cycle, print @T#index and return without recursing further
            block = cycle_block;
            const char* backref = heap_strf("%s%s#%%ld", sigil, type_to_string(pointed_type));
            gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, fprintf_fn, file, gcc_str(env->ctx, backref),
                                            gcc_rval(gcc_deref(gcc_rval(index_var), NULL))));
            gcc_return_void(block, NULL);

            // If this is a nonrecursive situation
            block = noncycle_block;
            gcc_update(block, NULL, rec_default, GCC_BINOP_PLUS, gcc_one(env->ctx, i64));
        }

        // Prepend "@"/"&"
        COLOR_LITERAL(&block, "\x1b[0;34;1m");
        WRITE_LITERAL(block, sigil);
        COLOR_LITERAL(&block, "\x1b[m");

        gcc_func_t *print_fn = get_print_func(env, pointed_type);
        gcc_eval(block, NULL, gcc_callx(
            env->ctx, NULL, print_fn,
            gcc_rval(gcc_rvalue_dereference(obj, NULL)),
            file, rec, color));
        gcc_return_void(block, NULL);

        break;
    }
    case StructType: {
        auto struct_t = Match(t, StructType);

        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);

#define ADD_INT(a, b) gcc_binary_op(env->ctx, NULL, GCC_BINOP_PLUS, int_t, a, b)

        if (struct_t->name) {
            COLOR_LITERAL(&block, "\x1b[0;1m");
            WRITE_LITERAL(block, struct_t->name);
        }
        COLOR_LITERAL(&block, "\x1b[m");
        WRITE_LITERAL(block, "{");
        
        size_t num_fields = gcc_field_count(gcc_struct);
        for (size_t i = 0; i < num_fields; i++) {
            if (i > 0) {
                COLOR_LITERAL(&block, "\x1b[m");
                WRITE_LITERAL(block, ", ");
            }

            const char* name = ith(struct_t->field_names, i);
            if (name) {
                COLOR_LITERAL(&block, "\x1b[m");
                WRITE_LITERAL(block, name);
                COLOR_LITERAL(&block, "\x1b[33m");
                WRITE_LITERAL(block, "=");
            }

            sss_type_t *member_t = ith(struct_t->field_types, i);
            gcc_func_t *print_fn = get_print_func(env, member_t);
            assert(print_fn);
            gcc_field_t *field = gcc_get_field(gcc_struct, i);
            gcc_eval(block, NULL, gcc_callx(
                    env->ctx, NULL, print_fn, 
                    gcc_rvalue_access_field(obj, NULL, field),
                    file, rec, color));
        }

        COLOR_LITERAL(&block, "\x1b[m");
        WRITE_LITERAL(block, "}");

        const char* units = type_units(t);
        if (units && strlen(units) > 0) {
            COLOR_LITERAL(&block, "\x1b[33;2m");
            WRITE_LITERAL(block, heap_strf("<%s>", units));
            COLOR_LITERAL(&block, "\x1b[m");
        }

        gcc_return_void(block, NULL);
        break;
    }
#undef ADD_INT
    case ArrayType: {
        compile_array_print_func(env, &block, obj, file, rec, color, t);
        break;
    }
    case TableType: {
        compile_table_print_func(env, &block, obj, file, rec, color, t);
        break;
    }
    case FunctionType: {
        COLOR_LITERAL(&block, "\x1b[36m");
        WRITE_LITERAL(block, type_to_string(t));
        COLOR_LITERAL(&block, "\x1b[m");
        gcc_return_void(block, NULL);
        break;
    }
    case TypeType: {
        COLOR_LITERAL(&block, "\x1b[36m");
        gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, fputs_fn,
                                        gcc_cast(env->ctx, NULL, obj, gcc_type(env->ctx, STRING)), file));
        COLOR_LITERAL(&block, "\x1b[m");
        gcc_return_void(block, NULL);
        break;
    }
    case ModuleType: {
        WRITE_LITERAL(block, type_to_string(t));
        COLOR_LITERAL(&block, "\x1b[m");
        gcc_return_void(block, NULL);
        break;
    }
    default: {
        fprintf(stderr, "\x1b[31;1mprint(%s) function is not yet implemented!\n", type_to_string(t));
        exit(1);
    }
    }
    return func;
#undef WRITE_LITERAL
#undef COLOR_LITERAL
}

void flatten_arrays(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *array_ptr)
{
    if (t->tag != ArrayType) return;
    // If necessary, flatten first:
    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
    gcc_func_t *func = gcc_block_func(*block);
    gcc_rvalue_t *stride_field = gcc_rval(gcc_rvalue_dereference_field(array_ptr, NULL, gcc_get_field(struct_t, ARRAY_STRIDE_FIELD)));
    sss_type_t *item_type = Match(t, ArrayType)->item_type;
    gcc_block_t *needs_flattening = gcc_new_block(func, fresh("needs_flattening")),
                *already_flat = gcc_new_block(func, fresh("already_flat"));
    gcc_jump_condition(*block, NULL,
                       gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, stride_field, gcc_one(env->ctx, gcc_type(env->ctx, INT16))),
                       needs_flattening, already_flat);
    *block = needs_flattening;
    gcc_func_t *flatten = get_function(env, "array_flatten");
    gcc_eval(*block, NULL, gcc_callx(
            env->ctx, NULL, flatten,
            gcc_cast(env->ctx, NULL, array_ptr, gcc_get_type(env->ctx, GCC_T_VOID_PTR)),
            gcc_rvalue_size(env->ctx, gcc_sizeof(env, item_type)),
            gcc_rvalue_bool(env->ctx, !has_heap_memory(item_type))));

    gcc_jump(*block, NULL, already_flat);
    *block = already_flat;
}

gcc_func_t *get_hash_func(env_t *env, sss_type_t *t)
{
    // Return a hash function for a given type.

    // Memoize:
    binding_t *b = get_from_namespace(env, t, "__hash");
    if (b) return b->func;

    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_type_t *u32 = gcc_type(env->ctx, UINT32);

    gcc_param_t *params[] = {gcc_new_param(env->ctx, NULL, gcc_get_ptr_type(gcc_t), fresh("obj"))};
    const char* sym_name = fresh("hash");
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, u32, sym_name, 1, params, 0);
    sss_type_t *fn_t = Type(FunctionType, .arg_types=LIST(sss_type_t*, Type(PointerType, .pointed=t)),
                           .arg_names=LIST(const char*, "obj"), .arg_defaults=NULL, .ret=Type(IntType, .bits=32, .is_unsigned=true));
    hset(get_namespace(env, t), "__hash",
         new(binding_t, .func=func, .rval=gcc_get_func_address(func, NULL), .type=fn_t));
    gcc_block_t *block = gcc_new_block(func, fresh("hash"));
    gcc_comment(block, NULL, heap_strf("Implementation of hash(%s)", type_to_string(t)));

    gcc_func_t *halfsiphash = get_function(env, "halfsiphash");

    gcc_type_t *t_void_ptr = gcc_get_type(env->ctx, GCC_T_VOID_PTR);
    gcc_rvalue_t *k = gcc_cast(env->ctx, NULL, gcc_str(env->ctx, "My secret key!!!"), t_void_ptr);

    gcc_rvalue_t *obj_ptr = gcc_param_as_rvalue(params[0]);
    gcc_lvalue_t *hashval = gcc_local(func, NULL, gcc_type(env->ctx, UINT32), "_hashval");

    switch (t->tag) {
    case StructType: {
        auto struct_type = Match(t, StructType);

        foreach (struct_type->field_types, ftype, _) {
            if ((*ftype)->tag == ArrayType || (*ftype)->tag == TableType)
                goto need_to_handle_arrays_or_tables;
        }
        goto memory_hash;

      need_to_handle_arrays_or_tables:;
        gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
        NEW_LIST(sss_type_t*, hash_members);
        NEW_LIST(gcc_rvalue_t*, values);
        for (int64_t i = 0; i < length(struct_type->field_types); i++) {
            sss_type_t *ftype = ith(struct_type->field_types, i);
            if (ftype->tag == ArrayType || ftype->tag == TableType) {
                append(hash_members, Type(IntType, .bits=32, .is_unsigned=true));
                gcc_func_t *item_hash = get_hash_func(env, ftype);
                gcc_rvalue_t *member = gcc_lvalue_address(gcc_rvalue_dereference_field(obj_ptr, NULL, gcc_get_field(struct_t, i)), NULL);
                append(values, gcc_callx(env->ctx, NULL, item_hash, member));
            } else {
                append(hash_members, ftype);
                gcc_rvalue_t *member = gcc_rval(gcc_rvalue_dereference_field(obj_ptr, NULL, gcc_get_field(struct_t, i)));
                append(values, member);
            }
        }

        sss_type_t *safe_tup_t = Type(StructType, .field_names=struct_type->field_names, .field_types=hash_members);
        gcc_type_t *safe_tup_gcc_t = sss_type_to_gcc(env, safe_tup_t);
        gcc_struct_t *safe_tup_struct = gcc_type_if_struct(safe_tup_gcc_t);
        gcc_lvalue_t *safe_tup = gcc_local(func, NULL, safe_tup_gcc_t, "_to_hash");
        gcc_assign(block, NULL, safe_tup, gcc_struct_constructor(env->ctx, NULL, safe_tup_gcc_t, 0, NULL, NULL));

        for (int64_t i = 0; i < length(values); i++)
            gcc_assign(block, NULL, gcc_lvalue_access_field(safe_tup, NULL, gcc_get_field(safe_tup_struct, i)),
                       ith(values, i));

        t = safe_tup_t;
        obj_ptr = gcc_lvalue_address(safe_tup, NULL);
        goto memory_hash;
    }
    case TaggedUnionType: {
        // TODO: properly implement hashes here: hash(tag, hash(value))
        auto tagged = Match(t, TaggedUnionType);
        for (int64_t i = 0, len = length(tagged->members); i < len; i++) {
            auto member = ith(tagged->members, i);
            if (member.type && has_heap_memory(member.type))
                compiler_err(env, NULL, "This program is attempting to use a '%s' tagged union value as a table key, but hashing isn't implemented for tagged union with members like '%s' that have complex values.",
                             type_to_string(t), member.name);
        }
        goto memory_hash;
    }
    case PointerType: case IntType: case NumType: case CharType: case CStringCharType: case BoolType:
    case RangeType: case FunctionType: case TypeType: {
      memory_hash:;
        gcc_rvalue_t *inlen = gcc_rvalue_size(env->ctx, gcc_sizeof(env, t));
        gcc_rvalue_t *outlen = gcc_rvalue_size(env->ctx, sizeof(uint32_t));
        gcc_rvalue_t *call = gcc_callx(
            env->ctx, NULL, halfsiphash, obj_ptr, inlen, k,
            gcc_cast(env->ctx, NULL, gcc_lvalue_address(hashval, NULL), t_void_ptr), outlen);
        gcc_eval(block, NULL, call);
        break;
    }
    case TableType: {
        gcc_func_t *hash_fn = get_function(env, "sss_hashmap_hash");
        sss_type_t *entry_t = table_entry_type(t);
        gcc_func_t *entry_hash = get_hash_func(env, entry_t);
        gcc_assign(block, NULL, hashval, gcc_callx(env->ctx, NULL, hash_fn, obj_ptr,
                                                   gcc_get_func_address(entry_hash, NULL),
                                                   gcc_rvalue_size(env->ctx, gcc_sizeof(env, entry_t))));
        break;
    }
    case ArrayType: {
        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        gcc_rvalue_t *array = obj_ptr;
        // If necessary, flatten first:
        flatten_arrays(env, &block, t, array);
        gcc_struct_t *struct_t = gcc_type_if_struct(gcc_t);
        gcc_rvalue_t *data_field = gcc_rval(gcc_rvalue_dereference_field(array, NULL, gcc_get_field(struct_t, ARRAY_DATA_FIELD)));
        gcc_rvalue_t *length_field = gcc_rval(gcc_rvalue_dereference_field(array, NULL, gcc_get_field(struct_t, ARRAY_LENGTH_FIELD)));
        sss_type_t *item_type = Match(t, ArrayType)->item_type;
        gcc_rvalue_t *inlen = gcc_rvalue_size(env->ctx, gcc_sizeof(env, item_type));
        gcc_type_t *t_size = gcc_type(env->ctx, SIZE);
        inlen = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, t_size, inlen, gcc_cast(env->ctx, NULL, length_field, t_size));
        gcc_rvalue_t *outlen = gcc_rvalue_size(env->ctx, sizeof(uint32_t));
        gcc_rvalue_t *call = gcc_callx(
            env->ctx, NULL, halfsiphash,
            gcc_cast(env->ctx, NULL, data_field, t_void_ptr), inlen, k,
            gcc_cast(env->ctx, NULL, gcc_lvalue_address(hashval, NULL), t_void_ptr), outlen);
        gcc_eval(block, NULL, call);
        break;
    }
    default:
        compiler_err(env, NULL, "Hash functions aren't yet implemented for %s", type_to_string(t));
    }

    gcc_return(block, NULL, gcc_rval(hashval));
    return func;
}

// Helper function to make value comparison return an int that is one of [-1,0,1]
gcc_rvalue_t *compare_values(env_t *env, sss_type_t *t, gcc_rvalue_t *a, gcc_rvalue_t *b)
{
    // (int)((a > b) - (a < b))
    if (is_numeric(t) || t->tag == PointerType || t->tag == CharType || t->tag == CStringCharType || t->tag == BoolType || t->tag == TypeType) {
        gcc_type_t *int_t = gcc_type(env->ctx, INT);
        return gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, int_t,
                             gcc_cast(env->ctx, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, a, b), int_t),
                             gcc_cast(env->ctx, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_LT, a, b), int_t));
    } else {
        gcc_func_t *cmp_fn = get_compare_func(env, t);
        return gcc_callx(env->ctx, NULL, cmp_fn, a, b);
    }
}

// Get a comparison function: -1 means lhs < rhs; 0 means lhs == rhs; 1 means lhs > rhs
gcc_func_t *get_compare_func(env_t *env, sss_type_t *t)
{
    // Memoize:
    binding_t *b = get_from_namespace(env, t, "__compare");
    if (b) return b->func;

    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
    gcc_type_t *int_t = gcc_type(env->ctx, INT);

    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("lhs")),
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("rhs")),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, int_t, fresh("compare"), 2, params, 0);
    sss_type_t *fn_t = Type(FunctionType, .arg_types=LIST(sss_type_t*, t, t), .arg_names=LIST(const char*, "lhs", "rhs"),
                           .arg_defaults=NULL, .ret=Type(IntType, .bits=32));
    hset(get_namespace(env, t), "__compare",
         new(binding_t, .func=func, .rval=gcc_get_func_address(func, NULL), .type=fn_t));

    gcc_block_t *block = gcc_new_block(func, fresh("compare"));
    gcc_comment(block, NULL, CORD_to_char_star(CORD_cat("compare(a,b) for type: ", type_to_string(t))));
    gcc_rvalue_t *lhs = gcc_param_as_rvalue(params[0]);
    gcc_rvalue_t *rhs = gcc_param_as_rvalue(params[1]);

    switch (t->tag) {
    case StructType: {
        auto struct_ = Match(t, StructType);
        gcc_struct_t *gcc_struct = gcc_type_if_struct(sss_type_to_gcc(env, t));
        gcc_block_t *done = gcc_new_block(func, fresh("done"));
        gcc_lvalue_t *cmp = gcc_local(func, NULL, gcc_type(env->ctx, INT), "_cmp");
        gcc_rvalue_t *zero = gcc_zero(env->ctx, gcc_type(env->ctx, INT));
        gcc_assign(block, NULL, cmp, zero);
        for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
            gcc_block_t *next_field = gcc_new_block(func, fresh("next_field"));
            sss_type_t *field_t = ith(struct_->field_types, i);

            gcc_rvalue_t *lhs_field = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(gcc_struct, i));
            gcc_rvalue_t *rhs_field = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(gcc_struct, i));
            gcc_assign(block, NULL, cmp, compare_values(env, field_t, lhs_field, rhs_field));
            gcc_jump_condition(block, NULL,
                               gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, gcc_rval(cmp), zero),
                               next_field, done);

            block = next_field;
        }
        gcc_jump(block, NULL, done);
        gcc_return(done, NULL, gcc_rval(cmp));
        break;
    }
    case TaggedUnionType: {
        // Compare tags, return difference if any
        // Otherwise, switch to a comparison of the union member
        gcc_struct_t *tagged_struct = gcc_type_if_struct(gcc_t);

        auto tagged = Match(t, TaggedUnionType);
        gcc_type_t *tag_gcc_t = get_tag_type(env, t);
        gcc_type_t *union_gcc_t = get_union_type(env, t);
        gcc_field_t *data_field = gcc_get_field(tagged_struct, 1);
        NEW_LIST(gcc_case_t*, cases);
        for (int64_t i = 0, len = length(tagged->members); i < len; i++) {
            auto member = ith(tagged->members, i);
            if (!member.type) continue;
            gcc_block_t *tag_block = gcc_new_block(func, fresh(member.name));
            gcc_rvalue_t *lhs_data = gcc_rvalue_access_field(lhs, NULL, data_field);
            gcc_rvalue_t *rhs_data = gcc_rvalue_access_field(rhs, NULL, data_field);
            gcc_field_t *union_field = gcc_get_union_field(union_gcc_t, i);
            gcc_rvalue_t *lhs_field = gcc_rvalue_access_field(lhs_data, NULL, union_field);
            gcc_rvalue_t *rhs_field = gcc_rvalue_access_field(rhs_data, NULL, union_field);
            gcc_return(tag_block, NULL, compare_values(env, member.type, lhs_field, rhs_field));

            gcc_rvalue_t *rval = gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value);
            gcc_case_t *case_ = gcc_new_case(env->ctx, rval, rval, tag_block);
            APPEND(cases, case_);
        }

        gcc_field_t *tag_field = gcc_get_field(tagged_struct, 0);
        gcc_rvalue_t *lhs_tag = gcc_rvalue_access_field(lhs, NULL, tag_field);
        gcc_rvalue_t *rhs_tag = gcc_rvalue_access_field(rhs, NULL, tag_field);

        if (length(cases) == 0) {
            gcc_return(block, NULL, compare_values(env, INT_TYPE, lhs_tag, rhs_tag));
            break;
        }

        gcc_block_t *tags_equal = gcc_new_block(func, fresh("tags_equal")),
                    *tags_differ = gcc_new_block(func, fresh("tags_differ"));
        gcc_jump_condition(block, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, lhs_tag, rhs_tag),
                           tags_equal, tags_differ);

        // tags_differ:
        gcc_return(tags_differ, NULL, compare_values(env, INT_TYPE, lhs_tag, rhs_tag));

        // tags_equal:
        gcc_switch(tags_equal, NULL, lhs_tag, tags_differ, length(cases), cases[0]);
        break;
    }
    case ArrayType: {
        // Logic is roughly as follows:
        // if (lhs.items != rhs.items || lhs.stride != rhs.stride) {
        //   for (i=0; i < lhs.len && i < rhs.len; i++) {
        //     int c = cmp(lhs.items[i*lhs.stride], rhs.items[i*rhs.stride])
        //     if (c != 0) return c;
        //   }
        // }
        // return lhs.len - rhs.len

        gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
        gcc_rvalue_t *lhs_data = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, ARRAY_DATA_FIELD)),
                     *rhs_data = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, ARRAY_DATA_FIELD));
        gcc_rvalue_t *lhs_stride = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, ARRAY_STRIDE_FIELD)),
                     *rhs_stride = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, ARRAY_STRIDE_FIELD));

        gcc_block_t *loop_condition = gcc_new_block(func, fresh("loop_condition")),
                    *loop_body = gcc_new_block(func, fresh("loop_body")),
                    *loop_end = gcc_new_block(func, fresh("loop_end"));

        gcc_type_t *int32 = gcc_type(env->ctx, INT32);
        gcc_lvalue_t *index_var = gcc_local(func, NULL, int32, "_i");
        gcc_rvalue_t *index_rval = gcc_rval(index_var);
        gcc_assign(block, NULL, index_var, gcc_zero(env->ctx, int32));

        gcc_jump_condition(block, NULL,
                           gcc_binary_op(env->ctx, NULL, GCC_BINOP_LOGICAL_AND, gcc_type(env->ctx, BOOL),
                                         gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, lhs_data, rhs_data),
                                         gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, lhs_stride, rhs_stride)),
                           loop_end, loop_condition);

        // loop_condition:
        gcc_rvalue_t *lhs_len = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, ARRAY_LENGTH_FIELD)),
                     *rhs_len = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, ARRAY_LENGTH_FIELD));
        gcc_rvalue_t *lhs_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GE, index_rval, lhs_len);
        gcc_rvalue_t *rhs_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GE, index_rval, rhs_len);
        gcc_rvalue_t *either_done = gcc_binary_op(env->ctx, NULL, GCC_BINOP_LOGICAL_OR, gcc_type(env->ctx, BOOL),
                                                  lhs_done, rhs_done);

        gcc_jump_condition(loop_condition, NULL, either_done, loop_end, loop_body);

        // loop_body:
        sss_type_t *item_t = Match(t, ArrayType)->item_type;

        gcc_func_t *cmp_fn = get_compare_func(env, item_t);
        assert(cmp_fn);
        gcc_rvalue_t *lhs_offset = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, int32, index_rval, gcc_cast(env->ctx, NULL, lhs_stride, int32));
        gcc_rvalue_t *rhs_offset = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, int32, index_rval, gcc_cast(env->ctx, NULL, rhs_stride, int32));
        gcc_rvalue_t *difference = gcc_callx(env->ctx, NULL, cmp_fn,
            // lhs.data[i*lhs.stride], rhs.data[i*rhs.stride]
            gcc_rval(gcc_array_access(env->ctx, NULL, lhs_data, lhs_offset)),
            gcc_rval(gcc_array_access(env->ctx, NULL, rhs_data, rhs_offset)));

        gcc_block_t *early_return = gcc_new_block(func, fresh("return_early")),
                    *keep_going = gcc_new_block(func, fresh("keep_going"));
        gcc_jump_condition(loop_body, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, difference, gcc_zero(env->ctx, int_t)),
                           early_return, keep_going);

        // early_return:
        gcc_return(early_return, NULL, difference);

        // keep_going:
        gcc_update(keep_going, NULL, index_var, GCC_BINOP_PLUS, gcc_one(env->ctx, int32));
        gcc_jump(keep_going, NULL, loop_condition);

        // loop_end:
        gcc_return(loop_end, NULL, gcc_cast(env->ctx, NULL, gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, int32, lhs_len, rhs_len), int_t));
        break;
    }
    case TableType: {
        gcc_func_t *compare_fn = get_function(env, "sss_hashmap_compare");
        sss_type_t *entry_t = table_entry_type(t);
        gcc_func_t *key_hash = get_hash_func(env, Match(t, TableType)->key_type);
        gcc_func_t *key_compare = get_indirect_compare_func(env, Match(t, TableType)->key_type);
        gcc_func_t *value_compare = get_indirect_compare_func(env, Match(t, TableType)->value_type);
        size_t key_size = gcc_sizeof(env, Match(t, TableType)->key_type);
        size_t value_align = gcc_alignof(env, Match(t, TableType)->value_type);
        size_t value_offset = key_size;
        if (value_offset % value_align != 0) value_offset = (value_offset - (value_offset % value_align) + value_align);
        gcc_return(block, NULL, gcc_callx(env->ctx, NULL, compare_fn,
                                          gcc_lvalue_address(gcc_param_as_lvalue(params[0]), NULL),
                                          gcc_lvalue_address(gcc_param_as_lvalue(params[1]), NULL),
                                          gcc_get_func_address(key_hash, NULL),
                                          gcc_get_func_address(key_compare, NULL),
                                          gcc_get_func_address(value_compare, NULL),
                                          gcc_rvalue_size(env->ctx, gcc_sizeof(env, entry_t)),
                                          gcc_rvalue_size(env->ctx, value_offset)));
        break;
    }
    default: {
        if (is_numeric(t) || t->tag == PointerType || t->tag == CharType || t->tag == CStringCharType || t->tag == BoolType || t->tag == TypeType) {
            gcc_return(block, NULL, compare_values(env, t, lhs, rhs));
            break;
        }
        fprintf(stderr, "\x1b[31;1mcompare(%s,%s) function is not implemented!\n", type_to_string(t), type_to_string(t));
        exit(1);
    }
    }
    return func;
}

// Get a comparison function for pointers to data (instead of the data itself)
// This is just a shim around the real comparison function
gcc_func_t *get_indirect_compare_func(env_t *env, sss_type_t *t)
{
    // Memoize:
    binding_t *b = get_from_namespace(env, t, "__compare_indirect");
    if (b) return b->func;

    gcc_type_t *gcc_t = gcc_get_ptr_type(sss_type_to_gcc(env, t));
    gcc_type_t *int_t = gcc_type(env->ctx, INT);

    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("lhs")),
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("rhs")),
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, int_t, fresh("compare_indirect"), 2, params, 0);
    sss_type_t *fn_t = Type(FunctionType, .arg_types=LIST(sss_type_t*, t, t), .arg_names=LIST(const char*, "lhs", "rhs"),
                           .arg_defaults=NULL, .ret=Type(IntType, .bits=32));
    hset(get_namespace(env, t), "__compare_indirect",
         new(binding_t, .func=func, .rval=gcc_get_func_address(func, NULL), .type=fn_t));

    gcc_block_t *block = gcc_new_block(func, fresh("compare_indirect"));
    gcc_comment(block, NULL, CORD_to_char_star(CORD_cat("compare_indirect(a,b) for type: ", type_to_string(t))));
    gcc_rvalue_t *lhs = gcc_rval(gcc_rvalue_dereference(gcc_param_as_rvalue(params[0]), NULL));
    gcc_rvalue_t *rhs = gcc_rval(gcc_rvalue_dereference(gcc_param_as_rvalue(params[1]), NULL));
    gcc_func_t *compare_func = get_compare_func(env, t);
    gcc_return(block, NULL, gcc_callx(env->ctx, NULL, compare_func, lhs, rhs));
    return func;
}

gcc_rvalue_t *ternary(gcc_block_t **block, gcc_rvalue_t *condition, gcc_type_t *gcc_t, gcc_rvalue_t *true_val, gcc_rvalue_t *false_val)
{
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *is_true = gcc_new_block(func, fresh("is_true")),
                *is_false = gcc_new_block(func, fresh("is_false")),
                *got_value = gcc_new_block(func, fresh("got_value"));

    gcc_lvalue_t *result = gcc_local(func, NULL, gcc_t, "_result");
    gcc_jump_condition(*block, NULL, condition, is_true, is_false);
    *block = NULL;

    gcc_assign(is_true, NULL, result, true_val);
    gcc_jump(is_true, NULL, got_value);

    gcc_assign(is_false, NULL, result, false_val);
    gcc_jump(is_false, NULL, got_value);

    *block = got_value;
    return gcc_rval(result);
}

gcc_lvalue_t *get_lvalue(env_t *env, gcc_block_t **block, ast_t *ast, bool allow_slices)
{
    (void)block;
    gcc_loc_t *loc = ast_loc(env, ast);
    switch (ast->tag) {
    case Var: {
        binding_t *binding = get_binding(env, Match(ast, Var)->name);
        if (binding) {
            if (!binding->lval)
                compiler_err(env, ast, "This variable can't be assigned to. You can try declaring a new variable with the same name, though.");
            return binding->lval;
        } else {
            compiler_err(env, ast, "I don't know what this variable is referring to."); 
        }
    }
    case Dereference: {
        (void)get_type(env, ast); // Check this is a pointer type
        ast_t *value = Match(ast, Dereference)->value;
        gcc_rvalue_t *rval = compile_expr(env, block, value);
        return gcc_rvalue_dereference(rval, loc);
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        sss_type_t *fielded_t = get_type(env, access->fielded);

        gcc_lvalue_t *fielded_lval;
        if (fielded_t->tag == PointerType) {
            gcc_rvalue_t *ptr_rval = compile_expr(env, block, access->fielded);
            auto ptr = Match(fielded_t, PointerType);
          dereference_again:
            if (ptr->is_optional)
                compiler_err(env, ast, "Accessing a field on this value could result in trying to dereference a nil value, since the type is optional");
            fielded_lval = gcc_rvalue_dereference(ptr_rval, loc);
            fielded_t = ptr->pointed;
            if (fielded_t->tag == PointerType) {
                ptr_rval = gcc_rval(fielded_lval);
                ptr = Match(fielded_t, PointerType);
                goto dereference_again;
            }
        } else {
            fielded_lval = get_lvalue(env, block, access->fielded, allow_slices);
        } 

        if (fielded_t->tag == StructType) {
            auto fielded_struct = Match(fielded_t, StructType);
            for (int64_t i = 0, len = length(fielded_struct->field_names); i < len; i++) {
                if (streq(ith(fielded_struct->field_names, i), access->field)) {
                    gcc_struct_t *gcc_struct = gcc_type_if_struct(sss_type_to_gcc(env, fielded_t));
                    gcc_field_t *field = gcc_get_field(gcc_struct, i);
                    return gcc_lvalue_access_field(fielded_lval, loc, field);
                }
            }
            compiler_err(env, ast, "The struct %s doesn't have a field called '%s'",
                  type_to_string(fielded_t), access->field);
        } else if (fielded_t->tag == ArrayType) { 
            // arr.x => [item.x for x in arr]
            if (!allow_slices)
                compiler_err(env, ast, "I can't use array slices as assignment targets");
            gcc_func_t *func = gcc_block_func(*block);
            gcc_rvalue_t *slice_val = array_field_slice(env, block, access->fielded, access->field, ACCESS_WRITE);
            if (!slice_val)
                compiler_err(env, ast, "This isn't a valid slice that I can assign to.");
            gcc_lvalue_t *slice = gcc_local(func, loc, sss_type_to_gcc(env, get_type(env, ast)), "_slice");
            gcc_assign(*block, loc, slice, slice_val);
            return slice;
        } else if (fielded_t->tag == TableType) {
            sss_type_t *table_t = fielded_t;
            gcc_func_t *func = gcc_block_func(*block);
            if (streq(access->field, "default")) {
                sss_type_t *key_t = Match(table_t, TableType)->key_type;
                gcc_func_t *alloc_func = get_function(env, has_heap_memory(key_t) ? "GC_malloc" : "GC_malloc_atomic");
                gcc_lvalue_t *def_ptr = gcc_local(func, loc, gcc_get_ptr_type(sss_type_to_gcc(env, key_t)), "_default_ptr");
                gcc_assign(*block, loc, def_ptr, gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, gcc_rvalue_size(env->ctx, gcc_sizeof(env, key_t))), 
                                                          gcc_get_ptr_type(sss_type_to_gcc(env, key_t))));
                gcc_struct_t *gcc_struct = gcc_type_if_struct(sss_type_to_gcc(env, table_t));
                gcc_field_t *field = gcc_get_field(gcc_struct, TABLE_DEFAULT_FIELD);
                gcc_assign(*block, loc, gcc_lvalue_access_field(fielded_lval, loc, field), gcc_rval(def_ptr));
                return gcc_rvalue_dereference(gcc_rval(def_ptr), loc);
            } else if (streq(access->field, "fallback")) {
                gcc_func_t *alloc_func = get_function(env, has_heap_memory(table_t) ? "GC_malloc" : "GC_malloc_atomic");
                gcc_lvalue_t *fallback_ptr = gcc_local(func, loc, gcc_get_ptr_type(sss_type_to_gcc(env, table_t)), "_fallback_ptr");
                gcc_assign(*block, loc, fallback_ptr, gcc_cast(env->ctx, loc, gcc_callx(env->ctx, loc, alloc_func, gcc_rvalue_size(env->ctx, gcc_sizeof(env, table_t))), 
                                                          gcc_get_ptr_type(sss_type_to_gcc(env, table_t))));
                gcc_struct_t *gcc_struct = gcc_type_if_struct(sss_type_to_gcc(env, table_t));
                gcc_field_t *field = gcc_get_field(gcc_struct, TABLE_FALLBACK_FIELD);
                gcc_assign(*block, loc, gcc_lvalue_access_field(fielded_lval, loc, field), gcc_rval(fallback_ptr));
                return gcc_rvalue_dereference(gcc_rval(fallback_ptr), loc);
            } else {
                compiler_err(env, ast, "The only fields that can be mutated on a table are '.default' and '.fallback', not '.%s'", access->field);
            }
        } else {
            // TODO: support using TaggedUnion field and Type fields as lvalues
            compiler_err(env, ast, "This value is a %s, and I don't know how to assign to fields on it.",
                  type_to_string(fielded_t));
        }
    }
    case Index: {
        auto indexing = Match(ast, Index);

        if (!allow_slices && get_type(env, indexing->index)->tag == RangeType)
            compiler_err(env, ast, "I can't assign to array slices");

        sss_type_t *indexed_t = get_type(env, indexing->indexed);
        if (indexed_t->tag == ArrayType) {
            if (!allow_slices)
                compiler_err(env, ast, "I can't assign to an array value (which is immutable), only to array pointers.");
        } else if (indexed_t->tag == TableType) {
            if (!allow_slices)
                compiler_err(env, ast, "I can't assign to a table value (which is immutable), only to table pointers.");
        }

        sss_type_t *pointed_type = indexed_t;
        while (pointed_type->tag == PointerType) {
            auto ptr = Match(pointed_type, PointerType);
            if (ptr->is_optional)
                compiler_err(env, ast, "Accessing an index on this value could result in trying to dereference a nil value, since the type is optional");
            pointed_type = ptr->pointed;
        }

        if (pointed_type->tag == ArrayType) {
            return array_index(env, block, indexing->indexed, indexing->index, indexing->unchecked, ACCESS_WRITE);
        } else if (pointed_type->tag == TableType) {
            gcc_func_t *func = gcc_block_func(*block);
            gcc_lvalue_t *table_ptr = gcc_local(func, loc, gcc_get_ptr_type(sss_type_to_gcc(env, pointed_type)), "_table");
            gcc_assign(*block, loc, table_ptr, compile_expr(env, block, indexing->indexed));
            mark_table_cow(env, block, gcc_rval(table_ptr));
            return table_lvalue(env, block, pointed_type, gcc_rval(table_ptr), indexing->index);
        } else {
            compiler_err(env, ast, "I only know how to index into Arrays and Tables for assigning");
        }
    }
    case StackReference:
        compiler_err(env, ast, "I can't assign to a stack reference. You don't need the '&', you can just assign to the value directly.");
    case HeapAllocate:
        compiler_err(env, ast, "I can't assign to a heap allocation. You don't need the '@', you can just assign to the value directly.");
    default:
        compiler_err(env, ast, "This is not a valid value for assignment");
    }
}

void insert_defers(env_t *env, gcc_block_t **block, defer_t *stop_at_defer)
{
    if (!*block) {
        // This can happen if the block ends due to a 'fail', in which case the defers won't run
        return;
    }
    for (; env->deferred && env->deferred != stop_at_defer; env->deferred = env->deferred->next) {
        compile_block_statement(env->deferred->environment, block, env->deferred->body);
    }
}

void insert_failure(env_t *env, gcc_block_t **block, span_t *span, const char *user_fmt, ...)
{
    char *info = NULL, *info_nocolor = NULL;
    {
        char *buf = NULL; size_t size = 0;
        FILE *f = open_memstream(&info, &size);
        if (span)
            fprint_span(f, *span, "\x1b[31;1m", 2, true);
        fflush(f);
        info = heap_strn(buf, size);
        fclose(f);
        free(buf);
    }
    {
        char *buf = NULL; size_t size = 0;
        FILE *f = open_memstream(&buf, &size);
        if (span)
            fprint_span(f, *span, NULL, 2, false);
        fflush(f);
        info_nocolor = heap_strn(buf, size);
        fclose(f);
        free(buf);
    }

    gcc_func_t *fail = get_function(env, "fail");
    gcc_func_t *func = gcc_block_func(*block);
    gcc_lvalue_t *fmt_var = gcc_local(func, NULL, gcc_type(env->ctx, STRING), "_fmt");
    gcc_block_t *use_color = gcc_new_block(func, fresh("use_color")),
                *no_color = gcc_new_block(func, fresh("no_color")),
                *carry_on = gcc_new_block(func, fresh("fmt_set"));
    gcc_jump_condition(*block, NULL, get_binding(env, "USE_COLOR")->rval, use_color, no_color);

    if (span) {
        gcc_assign(no_color, NULL, fmt_var, gcc_str(
            env->ctx, heap_strf("%s:%ld.%ld: %s\n\n%s",
                span->file->relative_filename,
                sss_get_line_number(span->file, span->start),
                sss_get_line_column(span->file, span->start),
                user_fmt,
                info_nocolor)));
        gcc_assign(use_color, NULL, fmt_var, gcc_str(
            env->ctx, heap_strf("\x1b[31;1;7m%s:%ld.%ld: %s\x1b[m\n\n%s",
                span->file->relative_filename,
                sss_get_line_number(span->file, span->start),
                sss_get_line_column(span->file, span->start),
                user_fmt,
                info)));
    } else {
        gcc_assign(use_color, NULL, fmt_var, gcc_str(env->ctx, heap_strf("\x1b[31;1;7m%s\x1b[m\n\n%s", user_fmt, info)));
        gcc_assign(no_color, NULL, fmt_var, gcc_str(env->ctx, heap_strf("%s\n\n%s", user_fmt, info_nocolor)));
    }
    gcc_jump(use_color, NULL, carry_on);
    gcc_jump(no_color, NULL, carry_on);
    *block = carry_on;

    gcc_rvalue_t *fmt_val = gcc_rval(fmt_var);

    NEW_LIST(gcc_rvalue_t*, args);
    append(args, fmt_val);

    va_list ap;
    va_start(ap, user_fmt);

    gcc_func_t *open_memstream_fn = hget(env->global_funcs, "open_memstream", gcc_func_t*);
    gcc_func_t *fflush_fn = hget(env->global_funcs, "fflush", gcc_func_t*);
    gcc_func_t *free_fn = hget(env->global_funcs, "free", gcc_func_t*);
    gcc_func_t *fclose_fn = hget(env->global_funcs, "fclose", gcc_func_t*);
    gcc_func_t *alloc_fn = hget(env->global_funcs, "GC_malloc_atomic", gcc_func_t*);
    gcc_func_t *memcpy_fn = hget(env->global_funcs, "memcpy", gcc_func_t*);

    for (const char *p = user_fmt; *p; p++) {
        if (*p != '%') continue;
        switch (*(++p)) {
        case '#': {
            assert(*(++p) == 's');
            sss_type_t *t = va_arg(ap, sss_type_t*);
            gcc_rvalue_t *rval = va_arg(ap, gcc_rvalue_t*);

            // Insert strings directly:
            if (type_eq(t, Type(ArrayType, .item_type=Type(CharType)))) {
                gcc_func_t *to_c_str = get_from_namespace(env, t, "c_string")->func;
                append(args, gcc_callx(env->ctx, NULL, to_c_str, rval));
                continue;
            }

            // char *buf; size_t size;
            // FILE *f = open_memstream(&buf, &size);
            gcc_lvalue_t *buf_var = gcc_local(func, NULL, gcc_type(env->ctx, STRING), "_buf");
            gcc_lvalue_t *size_var = gcc_local(func, NULL, gcc_type(env->ctx, SIZE), "_size");
            gcc_lvalue_t *file_var = gcc_local(func, NULL, gcc_type(env->ctx, FILE_PTR), "_file");
            gcc_assign(*block, NULL, file_var,
                       gcc_callx(env->ctx, NULL, open_memstream_fn, gcc_lvalue_address(buf_var, NULL), gcc_lvalue_address(size_var, NULL)));
            gcc_rvalue_t *file = gcc_rval(file_var);

            gcc_func_t *print_fn = get_print_func(env, t);
            // Do sss_hashmap_t rec = {0}; def = 0; rec->default = &def; print(obj, &rec)
            sss_type_t *cycle_checker_t = Type(TableType, .key_type=Type(PointerType, .pointed=Type(VoidType)), .value_type=Type(IntType, .bits=64));
            gcc_type_t *hashmap_gcc_t = sss_type_to_gcc(env, cycle_checker_t);
            gcc_lvalue_t *cycle_checker = gcc_local(func, NULL, hashmap_gcc_t, "_rec");
            gcc_assign(*block, NULL, cycle_checker, gcc_struct_constructor(env->ctx, NULL, hashmap_gcc_t, 0, NULL, NULL));
            gcc_lvalue_t *next_index = gcc_local(func, NULL, gcc_type(env->ctx, INT64), "_index");
            gcc_assign(*block, NULL, next_index, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
            gcc_assign(*block, NULL, gcc_lvalue_access_field(
                    cycle_checker, NULL, gcc_get_field(gcc_type_if_struct(hashmap_gcc_t), TABLE_DEFAULT_FIELD)),
                gcc_lvalue_address(next_index, NULL));

            gcc_type_t *void_star = gcc_type(env->ctx, VOID_PTR);
            gcc_rvalue_t *print_call = gcc_callx(
                env->ctx, NULL, print_fn, rval, file,
                gcc_cast(env->ctx, NULL, gcc_lvalue_address(cycle_checker, NULL), void_star),
                gcc_rvalue_bool(env->ctx, false));
            gcc_eval(*block, NULL, print_call);
            gcc_eval(*block, NULL, gcc_callx(env->ctx, NULL, fflush_fn, file));

            gcc_rvalue_t *size = gcc_binary_op(env->ctx, NULL, GCC_BINOP_PLUS, gcc_type(env->ctx, SIZE),
                                               gcc_rval(size_var), gcc_one(env->ctx, gcc_type(env->ctx, SIZE)));
            gcc_rvalue_t *str = gcc_callx(env->ctx, NULL, alloc_fn, size);
            str = gcc_callx(env->ctx, NULL, memcpy_fn, str, gcc_rval(buf_var), size);
            str = gcc_cast(env->ctx, NULL, str, gcc_type(env->ctx, STRING));
            gcc_lvalue_t *str_var = gcc_local(func, NULL, gcc_type(env->ctx, STRING), "_str");
            gcc_assign(*block, NULL, str_var, str);
            gcc_eval(*block, NULL, gcc_callx(env->ctx, NULL, fclose_fn, file));
            gcc_eval(*block, NULL, gcc_callx(env->ctx, NULL, free_fn, gcc_rval(buf_var)));
            append(args, gcc_rval(str_var));
            break;
        }
        case 's': {
            const char *str = va_arg(ap, const char*);
            append(args, gcc_str(env->ctx, str));
            break;
        }
        default: compiler_err(env, NULL, "String format option not supported: %c", p[-1]);
        }
    }
    va_end(ap);

    gcc_rvalue_t *failure = gcc_jit_context_new_call(env->ctx, NULL, fail, LIST_LEN(args), args[0]);
    gcc_eval(*block, NULL, failure);
    gcc_jump(*block, NULL, *block);
    *block = NULL;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
