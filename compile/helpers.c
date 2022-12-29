// Helper functions used for compiling
#include <assert.h>
#include <bhash.h>
#include <libgccjit.h>
#include <bp/files.h>
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
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

istr_t fresh(istr_t name)
{
    static int id = 0;
    return intern_strf("%s__%d", name, id++);
}

// Kinda janky, but libgccjit doesn't have this function built in
ssize_t gcc_alignof(env_t *env, bl_type_t *bl_t)
{
    if (bl_t->tag == StructType) {
        ssize_t align = 0;
        auto struct_type = Match(bl_t, StructType);
        foreach (struct_type->field_types, ftype, _) {
            ssize_t field_align = gcc_alignof(env, *ftype);
            if (field_align > align) align = field_align;
        }
        return align;
    } else {
        return gcc_sizeof(env, bl_t);
    }
}

// Kinda janky, but libgccjit doesn't have this function built in, except for integer types:
ssize_t gcc_sizeof(env_t *env, bl_type_t *bl_t)
{
    gcc_type_t *gcc_t = bl_type_to_gcc(env, bl_t);
    if (gcc_type_is_integral(gcc_t))
        return gcc_type_size(gcc_t);

    switch (bl_t->tag) {
    case ArrayType: return sizeof (struct {void* items; int32_t len, stride;});
    case RangeType: return sizeof (struct {int64_t start,stop,step;});
    case PointerType: return sizeof(void*);
    case DSLType: case TypeType: return sizeof(char*);
    case StructType: {
        ssize_t size = 0;
        ssize_t max_align = 0;
        auto struct_type = Match(bl_t, StructType);
        foreach (struct_type->field_types, ftype, _) {
            ssize_t field_align = gcc_alignof(env, *ftype);
            if (size % field_align)
                size += (field_align - size) % field_align; // padding
            size += gcc_sizeof(env, *ftype);
            if (field_align > max_align) max_align = field_align;
        }
        if (size % max_align)
            size += (max_align - size) % max_align; // padding
        return size;
    }
    case UnionType: {
        ssize_t size = 0;
        ssize_t max_align = 0;
        auto union_type = Match(bl_t, UnionType);
        foreach (union_type->field_types, ftype, _) {
            ssize_t field_align = gcc_alignof(env, *ftype);
            if (size % field_align)
                size += (field_align - size) % field_align; // padding
            ssize_t field_size = gcc_sizeof(env, *ftype);
            if (field_size > size) size = field_size;
            if (field_align > max_align) max_align = field_align;
        }
        if (size % max_align)
            size += (max_align - size) % max_align; // padding
        return size;
    }
    default: return 8;
    }
}

// This must be memoized because GCC JIT doesn't do structural equality
gcc_type_t *bl_type_to_gcc(env_t *env, bl_type_t *t)
{
    gcc_type_t *gcc_t = hashmap_get(env->gcc_types, type_to_string(t));
    if (gcc_t) return gcc_t;

    switch (t->tag) {
    case IntType: gcc_t = gcc_type(env->ctx, INT64); break;
    case Int32Type: gcc_t = gcc_type(env->ctx, INT32); break;
    case Int16Type: gcc_t = gcc_type(env->ctx, INT16); break;
    case Int8Type: gcc_t = gcc_type(env->ctx, INT8); break;
    case CharType: gcc_t = gcc_type(env->ctx, CHAR); break;
    case BoolType: gcc_t = gcc_type(env->ctx, BOOL); break;
    case NumType: gcc_t = gcc_type(env->ctx, DOUBLE); break;
    case Num32Type: gcc_t = gcc_type(env->ctx, FLOAT); break;
    case VoidType: gcc_t = gcc_type(env->ctx, VOID); break;
    case PointerType: {
        gcc_t = bl_type_to_gcc(env, Match(t, PointerType)->pointed);
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
        gcc_field_t *fields[3] = {
            gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(bl_type_to_gcc(env, Match(t, ArrayType)->item_type)), "items"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT32), "length"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT32), "stride"),
        };
        gcc_struct_t *array = gcc_new_struct_type(env->ctx, NULL, "Array", 3, fields);
        gcc_t = gcc_struct_as_type(array);
        break;
    }
    case FunctionType: {
        NEW_LIST(gcc_type_t*, arg_types);
        auto fn = Match(t, FunctionType);
        foreach (fn->arg_types, arg_t, _)
            append(arg_types, bl_type_to_gcc(env, *arg_t));
        gcc_type_t *ret_type = bl_type_to_gcc(env, fn->ret);
        gcc_t = gcc_new_func_type(env->ctx, NULL, ret_type, length(arg_types), arg_types[0], 0);
        break;
    }
    case StructType: {
        auto struct_t = Match(t, StructType);
        gcc_struct_t *gcc_struct = gcc_opaque_struct(env->ctx, NULL, struct_t->name);
        gcc_t = gcc_struct_as_type(gcc_struct);
        hashmap_set(env->gcc_types, type_to_string(t), gcc_t);

        NEW_LIST(gcc_field_t*, fields);
        foreach (struct_t->field_types, bl_ft, _) {
            int i = (int)(bl_ft - *struct_t->field_types);
            gcc_type_t *gcc_ft = bl_type_to_gcc(env, *bl_ft);
            assert(gcc_ft);
            gcc_field_t *field = gcc_new_field(env->ctx, NULL, gcc_ft, ith(struct_t->field_names, i));
            append(fields, field);
        }
        gcc_set_fields(gcc_struct, NULL, length(fields), fields[0]);
        gcc_t = gcc_struct_as_type(gcc_struct);
        break;
    }
    case TagType: {
        auto tag = Match(t, TagType);
        int64_t max = 0;
        for (int64_t i = 0, len = length(tag->values); i < len; i++) {
            if (ith(tag->values, i) > max)
                max = ith(tag->values, i);
        }
        if (max > INT32_MAX)
            gcc_t = gcc_type(env->ctx, INT64);
        else if (max > INT16_MAX)
            gcc_t = gcc_type(env->ctx, INT32);
        else if (max > INT8_MAX)
            gcc_t = gcc_type(env->ctx, INT16);
        else
            gcc_t = gcc_type(env->ctx, INT8);
        break;
    }
    case TaggedUnionType: {
        auto tagged = Match(t, TaggedUnionType);
        gcc_struct_t *gcc_struct = gcc_opaque_struct(env->ctx, NULL, tagged->name);
        bl_type_t *tag_t = tagged->tag_type;
        gcc_field_t *tag_field = gcc_new_field(env->ctx, NULL, bl_type_to_gcc(env, tag_t), "tag");
        gcc_t = gcc_struct_as_type(gcc_struct);
        hashmap_set(env->gcc_types, type_to_string(t), gcc_t);

        gcc_type_t *gcc_data_t = bl_type_to_gcc(env, tagged->data);
        if (gcc_data_t) {
            gcc_field_t *data_field = gcc_new_field(env->ctx, NULL, gcc_data_t, "data");
            gcc_set_fields(gcc_struct, NULL, 2, (gcc_field_t*[]){tag_field, data_field});
        } else {
            gcc_set_fields(gcc_struct, NULL, 1, &tag_field);
        }
        break;
    }
    case UnionType: {
        // NEW_LIST(gcc_field_t*, union_types);
        // for (int64_t i = 0, len = length(t->union_.field_names); i < len; i++) {
        //     bl_type_t *ft = ith(t->union_.field_types, i);
        //     gcc_type_t *gcc_ft = bl_type_to_gcc(env, ft);
        //     gcc_field_t *field = gcc_new_field(env->ctx, NULL, gcc_ft, ith(t->union_.field_names, i));
        //     append(union_types, field);
        // }

        auto union_t = Match(t, UnionType);
        if (length(union_t->fields) > 0) {
            gcc_t = gcc_union(env->ctx, NULL, "data_u", length(union_t->fields), union_t->fields[0]);
        } else {
            gcc_t = NULL;
        }
        break;
    }
    case TypeType: {
        gcc_t = gcc_get_ptr_type(gcc_type(env->ctx, CHAR));
        break;
    }
    default: {
        errx(1, "The following BL type doesn't have a GCC type: %s", type_to_string(t));
    }
    }

    hashmap_set(env->gcc_types, type_to_string(t), gcc_t);
    return gcc_t;
}

gcc_rvalue_t *move_to_heap(env_t *env, gcc_block_t **block, bl_type_t *t, gcc_rvalue_t *val)
{
    gcc_func_t *gc_malloc_func = hashmap_gets(env->global_funcs, "GC_malloc");
    gcc_func_t *func = gcc_block_func(*block);
    gcc_rvalue_t *size = gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, SIZE), gcc_sizeof(env, t));
    gcc_type_t *gcc_t = gcc_get_ptr_type(bl_type_to_gcc(env, t));
    gcc_lvalue_t *tmp = gcc_local(func, NULL, gcc_t, fresh("tmp"));
    // TODO: use gc_malloc_atomic() when possible
    gcc_assign(*block, NULL, tmp, gcc_cast(env->ctx, NULL, gcc_call(env->ctx, NULL, gc_malloc_func, 1, &size), gcc_t));
    gcc_assign(*block, NULL, gcc_rvalue_dereference(gcc_lvalue_as_rvalue(tmp), NULL), val);
    return gcc_lvalue_as_rvalue(tmp);
}

bool promote(env_t *env, bl_type_t *actual, gcc_rvalue_t **val, bl_type_t *needed)
{
    // No promotion necessary:
    if (actual == needed)
        return true;

    // Numeric promotion:
    if (is_numeric(actual) && is_numeric(needed) && numtype_priority(actual) < numtype_priority(needed)) {
        *val = gcc_cast(env->ctx, NULL, *val, bl_type_to_gcc(env, needed));
        return true;
    }

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
        if (length(needed_fn->arg_types) != length(actual_fn->arg_types) || needed_fn->ret != actual_fn->ret)
            return false;
        for (int64_t i = 0, len = length(needed_fn->arg_types); i < len; i++) {
            if (ith(actual_fn->arg_types, i) != ith(needed_fn->arg_types, i))
                return false;
        }
        // GCC is dumb and needs this function to be cast, even though it has the same type:
        *val = gcc_cast(env->ctx, NULL, *val, bl_type_to_gcc(env, needed));
        return true;
    }

    // TODO: Struct promotion?

    return false;
}

hashmap_t *global_bindings(hashmap_t *bindings)
{
    hashmap_t *globals = hashmap_new();
    for (hashmap_t *h = bindings; h; h = h->fallback) {
        for (istr_t key = NULL; (key = hashmap_next(h, key)); ) {
            binding_t *val = hashmap_get_raw(h, key);
            assert(val);
            if (val->is_global)
                hashmap_set(globals, key, val);
        }
    }
    return globals;
}

void check_truthiness(env_t *env, gcc_block_t **block, ast_t *obj, gcc_block_t *if_truthy, gcc_block_t *if_falsey)
{
    bl_type_t *t = get_type(env->file, env->bindings, obj);
    gcc_rvalue_t *bool_val = compile_expr(env, block, obj); 
    switch (t->tag) {
    case BoolType: break;
    case StructType: {
        ERROR(env, obj, "This value is a struct and can't be used as a conditional.");
        break;
    }
    default: {
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        if (gcc_type_if_pointer(gcc_t))
            bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, bool_val, gcc_null(env->ctx, gcc_t));
        else
            bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, bool_val, gcc_zero(env->ctx, gcc_t));
    }
    }
    gcc_jump_condition(*block, NULL, bool_val, if_truthy, if_falsey);
    *block = NULL;
}

gcc_func_t *get_print_func(env_t *env, bl_type_t *t)
{
    // Create a function `int print(T obj, FILE* file, void* stack)`
    // that prints an object to a given file

    // print() is the same for optional/non-optional pointers:
    if (t->tag == PointerType)
        t = Type(PointerType, .pointed=Match(t, PointerType)->pointed, .is_optional=true);

    // Memoize:
    gcc_func_t *func = hashmap_get(env->print_funcs, t);
    if (func) return func;

    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
    gcc_type_t *int_t = gcc_type(env->ctx, INT);

    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("obj")),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, FILE_PTR), fresh("file")),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, VOID_PTR), fresh("stack")),
    };
    func = gcc_new_func(
        env->ctx, NULL, GCC_FUNCTION_INTERNAL, int_t,
        fresh("print"), 3, params, 0);
    hashmap_set(env->print_funcs, t, func);

    gcc_block_t *block = gcc_new_block(func, fresh("print"));
    gcc_comment(block, NULL, CORD_to_char_star(CORD_cat("print() for type: ", type_to_string(t))));
    gcc_rvalue_t *obj = gcc_param_as_rvalue(params[0]);
    gcc_rvalue_t *f = gcc_param_as_rvalue(params[1]);

    gcc_func_t *fputs_fn = hashmap_gets(env->global_funcs, "fputs");

#define WRITE_LITERAL(str) gcc_call(env->ctx, NULL, fputs_fn, 2, (gcc_rvalue_t*[]){gcc_new_string(env->ctx, str), f})

    switch (t->tag) {
    case BoolType: {
        gcc_block_t *yes_block = gcc_new_block(func, fresh("yes"));
        gcc_block_t *no_block = gcc_new_block(func, fresh("no"));
        gcc_jump_condition(block, NULL, obj, yes_block, no_block);
        gcc_return(yes_block, NULL, WRITE_LITERAL("yes"));
        gcc_return(no_block, NULL, WRITE_LITERAL("no"));
        break;
    }
    case CharType: {
        gcc_func_t *fputc_fn = hashmap_gets(env->global_funcs, "fputc");
        gcc_return(block, NULL, gcc_call(env->ctx, NULL, fputc_fn, 2, (gcc_rvalue_t*[]){obj, f}));
        break;
    }
    case IntType: case Int32Type: case Int16Type: case Int8Type: case NumType: case Num32Type: {
        const char *fmt;
        switch (t->tag) {
        case Int32Type: case Int16Type: case Int8Type: fmt = "%d"; break;
        case NumType: case Num32Type: fmt = "%g"; break;
        default: fmt = "%ld"; break;
        }

        gcc_rvalue_t *args[] = {f, gcc_new_string(env->ctx, fmt), obj};
        gcc_func_t *fprintf_fn = hashmap_gets(env->global_funcs, "fprintf");
        gcc_return(block, NULL, gcc_call(env->ctx, NULL, fprintf_fn, 3, args));
        break;
    }
    case TagType: {
        gcc_rvalue_t *tag = obj;
        auto tags = Match(t, TagType);
        gcc_type_t *tag_gcc_t = bl_type_to_gcc(env, t);
        NEW_LIST(gcc_case_t*, cases);
        for (int64_t i = 0, len = length(tags->names); i < len; i++) {
            istr_t tag_name = ith(tags->names, i);
            gcc_block_t *tag_block = gcc_new_block(func, fresh(tag_name));
            gcc_return(tag_block, NULL, WRITE_LITERAL(intern_strf("%s.Tag.%s", tags->name, tag_name)));
            int64_t tag_value = ith(tags->values, i);
            gcc_rvalue_t *rval = gcc_rvalue_from_long(env->ctx, tag_gcc_t, tag_value);
            gcc_case_t *case_ = gcc_new_case(env->ctx, rval, rval, tag_block);
            APPEND(cases, case_);
        }
        gcc_block_t *default_block = gcc_new_block(func, fresh("default"));
        gcc_return(default_block, NULL, WRITE_LITERAL(intern_strf("<Unknown %s value>", tags->name)));
        gcc_switch(block, NULL, tag, default_block, length(cases), cases[0]);
        break;
    }
    case TaggedUnionType: {
        gcc_struct_t *tagged_struct = gcc_type_if_struct(gcc_t);
        gcc_field_t *tag_field = gcc_get_field(tagged_struct, 0);
        gcc_rvalue_t *tag = gcc_rvalue_access_field(obj, NULL, tag_field);
        auto tagged_t = Match(t, TaggedUnionType);
        auto tags = Match(tagged_t->tag_type, TagType);
        gcc_type_t *tag_gcc_t = bl_type_to_gcc(env, tagged_t->tag_type);
        istr_t type_prefix = intern_strf("%s.", tagged_t->name);
        NEW_LIST(gcc_case_t*, cases);
        for (int64_t i = 0, len = length(tags->names); i < len; i++) {
            istr_t tag_name = ith(tags->names, i);
            gcc_block_t *tag_block = gcc_new_block(func, fresh(tag_name));

            auto union_t = Match(tagged_t->data, UnionType);
            for (int64_t u = 0, len = length(union_t->field_names); u < len; u++) {
                if (ith(union_t->field_names, u) == tag_name) {
                    bl_type_t *tag_data_type = ith(union_t->field_types, u);
                    gcc_field_t *data_field = gcc_get_field(tagged_struct, 1);
                    gcc_rvalue_t *data = gcc_rvalue_access_field(obj, NULL, data_field);
                    gcc_field_t *union_field = ith(union_t->fields, u);
                    gcc_rvalue_t *args[] = {
                        gcc_rvalue_access_field(data, NULL, union_field),
                        f,
                        gcc_param_as_rvalue(params[2]),
                    };
                    gcc_func_t *tag_print = get_print_func(env, tag_data_type);
                    gcc_rvalue_t *prefix_len = WRITE_LITERAL(type_prefix);
                    gcc_rvalue_t *suffix_len = gcc_call(env->ctx, NULL, tag_print, 3, args);
                    gcc_return(tag_block, NULL,
                               gcc_binary_op(env->ctx, NULL, GCC_BINOP_PLUS, int_t, prefix_len, suffix_len));
                    goto found;
                }
            }
            gcc_return(tag_block, NULL, WRITE_LITERAL(intern_strf("%s.%s", tagged_t->name, tag_name)));
          found:;
            int64_t tag_value = ith(tags->values, i);
            gcc_rvalue_t *rval = gcc_rvalue_from_long(env->ctx, tag_gcc_t, tag_value);
            gcc_case_t *case_ = gcc_new_case(env->ctx, rval, rval, tag_block);
            APPEND(cases, case_);
        }
        gcc_block_t *default_block = gcc_new_block(func, fresh("default"));
        gcc_return(default_block, NULL, WRITE_LITERAL(intern_strf("<Unknown %s value>", tagged_t->name)));
        gcc_switch(block, NULL, tag, default_block, length(cases), cases[0]);
        break;
    }
    case VoidType: {
        gcc_return(block, NULL, WRITE_LITERAL("Void"));
        break;
    }
    case RangeType: {
        errx(1, "This should be handled by an externally defined function.");
    }
    case PointerType: {
        gcc_block_t *nil_block = gcc_new_block(func, fresh("nil"));
        gcc_block_t *nonnil_block = gcc_new_block(func, fresh("nonnil"));

        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_rvalue_t *is_nil = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, obj, gcc_null(env->ctx, gcc_t));

        gcc_jump_condition(block, NULL, is_nil, nil_block, nonnil_block);
        block = NULL;

        bl_type_t *pointed_type = Match(t, PointerType)->pointed;
        gcc_return(nil_block, NULL, WRITE_LITERAL(intern_strf("(nil @%s)", type_to_string(pointed_type))));

        // Prepend "@"
        gcc_rvalue_t *at = WRITE_LITERAL("@");

        gcc_rvalue_t *args[] = {
            gcc_lvalue_as_rvalue(gcc_rvalue_dereference(obj, NULL)),
            gcc_param_as_rvalue(params[1]),
            gcc_param_as_rvalue(params[2]),
        };
        gcc_func_t *print_fn = get_print_func(env, pointed_type);
        assert(print_fn);
        gcc_rvalue_t *printed = gcc_call(env->ctx, NULL, print_fn, 3, args);

        gcc_return(nonnil_block, NULL,
                   gcc_binary_op(env->ctx, NULL, GCC_BINOP_PLUS, int_t, at, printed));
        break;
    }
    case StructType: {
        auto struct_t = Match(t, StructType);

        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);

#define ADD_INT(a, b) gcc_binary_op(env->ctx, NULL, GCC_BINOP_PLUS, int_t, a, b)
        gcc_rvalue_t *written = gcc_zero(env->ctx, int_t);
        if (struct_t->name) {
            written = ADD_INT(written, WRITE_LITERAL(struct_t->name));
        }
        written = ADD_INT(written, WRITE_LITERAL("{"));
        
        size_t num_fields = gcc_field_count(gcc_struct);
        for (size_t i = 0; i < num_fields; i++) {
            if (i > 0)
                written = ADD_INT(written, WRITE_LITERAL(", "));

            istr_t name = ith(struct_t->field_names, i);
            if (name)
                written = ADD_INT(written, WRITE_LITERAL(intern_strf("%s=", name)));

            bl_type_t *member_t = ith(struct_t->field_types, i);
            gcc_func_t *print_fn = get_print_func(env, member_t);
            assert(print_fn);
            gcc_field_t *field = gcc_get_field(gcc_struct, i);
            gcc_rvalue_t *args[] = {
                gcc_rvalue_access_field(obj, NULL, field),
                gcc_param_as_rvalue(params[1]),
                gcc_param_as_rvalue(params[2]), // TODO: fix infinite recursion
            };
            written = ADD_INT(written, gcc_call(env->ctx, NULL, print_fn, 3, args));
        }

        written = ADD_INT(written, WRITE_LITERAL("}"));
        gcc_return(block, NULL, written);
        break;
    }
#undef ADD_INT
    case ArrayType: {
        compile_array_print_func(env, &block, obj, f, t);
        break;
    }
    case FunctionType: {
        gcc_return(block, NULL, WRITE_LITERAL(type_to_string(t)));
        break;
    }
    case TypeType: {
        fprintf(stderr, "\x1b[31;1mprint(%s) function is not yet implemented!\n", type_to_string(t));
        exit(1);
    }
    default: {
        fprintf(stderr, "\x1b[31;1mprint(%s) function is not yet implemented!\n", type_to_string(t));
        exit(1);
    }
    }
    return func;
#undef WRITE_LITERAL
}

// Get a comparison function: -1 means lhs < rhs; 0 means lhs == rhs; 1 means lhs > rhs
gcc_func_t *get_compare_func(env_t *env, bl_type_t *t)
{
    // Memoize:
    gcc_func_t *func = hashmap_get(env->cmp_funcs, t);
    if (func) return func;

    gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
    gcc_type_t *int_t = gcc_type(env->ctx, INT);

    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("lhs")),
        gcc_new_param(env->ctx, NULL, gcc_t, fresh("rhs")),
    };
    func = gcc_new_func(
        env->ctx, NULL, GCC_FUNCTION_INTERNAL, int_t,
        fresh("compare"), 2, params, 0);
    hashmap_set(env->cmp_funcs, t, func);

    gcc_block_t *block = gcc_new_block(func, fresh("compare"));
    gcc_comment(block, NULL, CORD_to_char_star(CORD_cat("compare(a,b) for type: ", type_to_string(t))));
    gcc_rvalue_t *lhs = gcc_param_as_rvalue(params[0]);
    gcc_rvalue_t *rhs = gcc_param_as_rvalue(params[1]);

    switch (t->tag) {
    case StructType: {
        auto struct_ = Match(t, StructType);
        gcc_struct_t *gcc_struct = gcc_type_if_struct(bl_type_to_gcc(env, t));
        gcc_block_t *done = gcc_new_block(func, fresh("done"));
        gcc_lvalue_t *cmp = gcc_local(func, NULL, gcc_type(env->ctx, INT), fresh("cmp"));
        gcc_rvalue_t *zero = gcc_zero(env->ctx, gcc_type(env->ctx, INT));
        gcc_assign(block, NULL, cmp, zero);
        for (int64_t i = 0, len = length(struct_->field_types); i < len; i++) {
            gcc_block_t *next_field = gcc_new_block(func, fresh("next_field"));
            bl_type_t *field_t = ith(struct_->field_types, i);
            gcc_func_t *cmp_fn = get_compare_func(env, field_t);
            gcc_assign(block, NULL, cmp,
                       gcc_call(env->ctx, NULL, cmp_fn, 2, (gcc_rvalue_t*[]){
                                    gcc_rvalue_access_field(lhs, NULL, gcc_get_field(gcc_struct, i)),
                                    gcc_rvalue_access_field(rhs, NULL, gcc_get_field(gcc_struct, i)),
                                }));
            gcc_jump_condition(block, NULL,
                               gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, gcc_lvalue_as_rvalue(cmp), zero),
                               next_field, done);

            block = next_field;
        }
        gcc_jump(block, NULL, done);
        gcc_return(done, NULL, gcc_lvalue_as_rvalue(cmp));
        break;
    }
    // case TaggedUnionType: {
    //     // if (lhs.tag != rhs.tag) return (lhs.tag - rhs.tag)
    //     // switch (lhs.tag) { ... case Foo: return cmp(lhs.__data.Foo, rhs.__data.Foo); ... }
    //     gcc_struct_t *tagged_struct = gcc_type_if_struct(gcc_t);
    //     gcc_field_t *tag_field = gcc_get_field(tagged_struct, 0);
    //     gcc_rvalue_t *tag = gcc_rvalue_access_field(obj, NULL, tag_field);
    //     auto tagged_t = Match(t, TaggedUnionType);
    //     auto tags = Match(tagged_t->tag_type, TagType);
    //     gcc_type_t *tag_gcc_t = bl_type_to_gcc(env, tagged_t->tag_type);
    //     istr_t type_prefix = intern_strf("%s.", tagged_t->name);
    //     NEW_LIST(gcc_case_t*, cases);
    //     for (int64_t i = 0, len = length(tags->names); i < len; i++) {
    //         istr_t tag_name = ith(tags->names, i);
    //         gcc_block_t *tag_block = gcc_new_block(func, fresh(tag_name));

    //         auto union_t = Match(tagged_t->data, UnionType);
    //         for (int64_t u = 0, len = length(union_t->field_names); u < len; u++) {
    //             if (ith(union_t->field_names, u) == tag_name) {
    //                 bl_type_t *tag_data_type = ith(union_t->field_types, u);
    //                 gcc_field_t *data_field = gcc_get_field(tagged_struct, 1);
    //                 gcc_rvalue_t *data = gcc_rvalue_access_field(obj, NULL, data_field);
    //                 gcc_field_t *union_field = ith(union_t->fields, u);
    //                 gcc_rvalue_t *args[] = {
    //                     gcc_rvalue_access_field(data, NULL, union_field),
    //                     f,
    //                     gcc_param_as_rvalue(params[2]),
    //                 };
    //                 gcc_func_t *tag_print = get_print_func(env, tag_data_type);
    //                 gcc_rvalue_t *prefix_len = WRITE_LITERAL(type_prefix);
    //                 gcc_rvalue_t *suffix_len = gcc_call(env->ctx, NULL, tag_print, 3, args);
    //                 gcc_return(tag_block, NULL,
    //                            gcc_binary_op(env->ctx, NULL, GCC_BINOP_PLUS, int_t, prefix_len, suffix_len));
    //                 goto found;
    //             }
    //         }
    //         gcc_return(tag_block, NULL, WRITE_LITERAL(intern_strf("%s.%s", tagged_t->name, tag_name)));
    //       found:;
    //         int64_t tag_value = ith(tags->values, i);
    //         gcc_rvalue_t *rval = gcc_rvalue_from_long(env->ctx, tag_gcc_t, tag_value);
    //         gcc_case_t *case_ = gcc_new_case(env->ctx, rval, rval, tag_block);
    //         APPEND(cases, case_);
    //     }
    //     gcc_block_t *default_block = gcc_new_block(func, fresh("default"));
    //     gcc_return(default_block, NULL, WRITE_LITERAL(intern_strf("<Unknown %s value>", tagged_t->name)));
    //     gcc_switch(block, NULL, tag, default_block, length(cases), cases[0]);
    //     break;
    // }
    case ArrayType: {
        // for (i=0; i < lhs.len && i < rhs.len; i++) {
        //    int c = cmp(lhs[i], rhs[i])
        //    if (c != 0) return c;
        // }
        // return lhs.len - rhs.len

        gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
        gcc_rvalue_t *lhs_len = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, 1));
        gcc_rvalue_t *rhs_len = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, 1));

        gcc_type_t *int32 = gcc_type(env->ctx, INT32);
        gcc_lvalue_t *index_var = gcc_local(func, NULL, int32, fresh("i"));
        gcc_rvalue_t *index_rval = gcc_lvalue_as_rvalue(index_var);
        gcc_assign(block, NULL, index_var, gcc_zero(env->ctx, int32));

        gcc_block_t *loop_condition = gcc_new_block(func, fresh("loop_condition")),
                    *loop_body = gcc_new_block(func, fresh("loop_body")),
                    *loop_end = gcc_new_block(func, fresh("loop_end"));

        gcc_jump(block, NULL, loop_condition);

        // loop_condition:
        gcc_rvalue_t *lhs_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GE, index_rval, lhs_len);
        gcc_rvalue_t *rhs_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GE, index_rval, rhs_len);
        gcc_rvalue_t *either_done = gcc_binary_op(env->ctx, NULL, GCC_BINOP_LOGICAL_OR, gcc_type(env->ctx, BOOL),
                                                  lhs_done, rhs_done);

        gcc_jump_condition(loop_condition, NULL, either_done, loop_end, loop_body);

        // loop_body:
        bl_type_t *item_t = Match(t, ArrayType)->item_type;

        gcc_func_t *cmp_fn = get_compare_func(env, item_t);
        assert(cmp_fn);
        gcc_rvalue_t *lhs_data = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, 0));
        gcc_rvalue_t *rhs_data = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, 0));
        gcc_rvalue_t *lhs_offset = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, int32, index_rval,
                                           gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, 2)));
        gcc_rvalue_t *rhs_offset = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, int32, index_rval,
                                           gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, 2)));
        gcc_rvalue_t *args[] = { // lhs.data[i*lhs.stride], rhs.data[i*rhs.stride]
            gcc_lvalue_as_rvalue(gcc_array_access(env->ctx, NULL, lhs_data, lhs_offset)),
            gcc_lvalue_as_rvalue(gcc_array_access(env->ctx, NULL, rhs_data, rhs_offset)),
        };
        gcc_rvalue_t *difference = gcc_call(env->ctx, NULL, cmp_fn, 2, args);

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
    default: {
        if (is_numeric(t) || t->tag == PointerType) {
            gcc_type_t *size = gcc_type(env->ctx, SIZE);
            gcc_return(block, NULL,
                       gcc_cast(env->ctx, NULL,
                           gcc_binary_op(env->ctx, NULL, GCC_BINOP_MINUS, size,
                                         gcc_cast(env->ctx, NULL, lhs, size),
                                         gcc_cast(env->ctx, NULL, rhs, size)),
                           int_t));
            break;
        }
        fprintf(stderr, "\x1b[31;1mcompare(%s,%s) function is not yet implemented!\n", type_to_string(t), type_to_string(t));
        exit(1);
    }
    }
    return func;
}

void coerce_numbers(env_t *env, bl_type_t **lhs_type, gcc_rvalue_t **lhs, bl_type_t **rhs_type, gcc_rvalue_t **rhs)
{
    if (numtype_priority(*lhs_type) < numtype_priority(*rhs_type)) {
        *lhs = gcc_cast(env->ctx, NULL, *lhs, bl_type_to_gcc(env, *rhs_type));
        *lhs_type = *rhs_type;
    } else if (numtype_priority(*lhs_type) > numtype_priority(*rhs_type)) {
        *rhs = gcc_cast(env->ctx, NULL, *rhs, bl_type_to_gcc(env, *lhs_type));
        *rhs_type = *lhs_type;
    }
}

gcc_lvalue_t *get_lvalue(env_t *env, gcc_block_t **block, ast_t *ast)
{
    (void)block;
    switch (ast->tag) {
    case Var: {
        binding_t *binding = hashmap_get(env->bindings, Match(ast, Var)->name);
        if (binding) {
            if (!binding->lval)
                ERROR(env, ast, "This variable can't be assigned to. You can try declaring a new variable with the same name, though.");
            return binding->lval;
        } else {
            ERROR(env, ast, "I don't know what this variable is referring to."); 
        }
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        gcc_lvalue_t *fielded_lval = get_lvalue(env, block, access->fielded);
        bl_type_t *fielded_t = get_type(env->file, env->bindings, access->fielded);
      keep_going:
        switch (fielded_t->tag) { 
        case StructType: {
            auto fielded_struct = Match(fielded_t, StructType);
            for (int64_t i = 0, len = length(fielded_struct->field_names); i < len; i++) {
                if (ith(fielded_struct->field_names, i) == access->field) {
                    gcc_struct_t *gcc_struct = gcc_type_if_struct(bl_type_to_gcc(env, fielded_t));
                    gcc_field_t *field = gcc_get_field(gcc_struct, i);
                    return gcc_lvalue_access_field(fielded_lval, NULL, field);
                }
            }
            ERROR(env, ast, "The struct %s doesn't have a mutable field called '%s'",
                  type_to_string(fielded_t), access->field);
        }
        case PointerType: {
            auto fielded_ptr = Match(fielded_t, PointerType);
            if (fielded_ptr->is_optional)
                ERROR(env, ast, "Accessing a field on this value could result in trying to dereference a nil value, since the type is optional");
            fielded_lval = gcc_rvalue_dereference(gcc_lvalue_as_rvalue(fielded_lval), NULL);
            fielded_t = fielded_ptr->pointed;
            goto keep_going;
        }
        default: {
            ERROR(env, ast, "This value is a %s, and I don't know how to assign to fields on it.",
                  type_to_string(fielded_t));
        }
        }
    }
    // case Index: {
    // }
    default:
        ERROR(env, ast, "This is not a valid Lvalue");
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
