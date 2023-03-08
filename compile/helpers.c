// Helper functions used for compiling
#include <assert.h>
#include <bhash.h>
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
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

istr_t fresh(istr_t name)
{
    static hashmap_t *seen = NULL;
    if (!seen) seen = hashmap_new();
    // static int id = 0;
    int64_t count = (int64_t)hashmap_get(seen, name);
    istr_t ret = intern_strf("%s__%ld", name, count++);
    hashmap_set(seen, name, (void*)(count+1));
    return ret;
}

// Kinda janky, but libgccjit doesn't have this function built in
ssize_t gcc_alignof(env_t *env, bl_type_t *bl_t)
{
    switch (bl_t->tag) {
    case StructType: {
        ssize_t align = 0;
        auto struct_type = Match(bl_t, StructType);
        foreach (struct_type->field_types, ftype, _) {
            ssize_t field_align = gcc_alignof(env, *ftype);
            if (field_align > align) align = field_align;
        }
        return align;
    }
    case UnionType: {
        ssize_t align = 0;
        auto union_type = Match(bl_t, UnionType);
        foreach (union_type->field_types, ftype, _) {
            ssize_t field_align = gcc_alignof(env, *ftype);
            if (field_align > align) align = field_align;
        }
        return align;
    }
    default:
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
    case BoolType: return sizeof(bool);
    case NumType: return Match(bl_t, NumType)->bits / 8;
    case FunctionType:
    case PointerType: return sizeof(void*);
    case StructType: {
        ssize_t size = 0;
        ssize_t max_align = 0;
        auto struct_type = Match(bl_t, StructType);
        foreach (struct_type->field_types, ftype, _) {
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
    case UnionType: {
        ssize_t size = 0;
        ssize_t max_align = 0;
        auto union_type = Match(bl_t, UnionType);
        foreach (union_type->field_types, ftype, _) {
            ssize_t field_align = gcc_alignof(env, *ftype);
            if (field_align > 1 && size % field_align)
                size += (field_align - size) % field_align; // padding
            ssize_t field_size = gcc_sizeof(env, *ftype);
            if (field_size > size) size = field_size;
            if (field_align > max_align) max_align = field_align;
        }
        if (max_align > 1 && size % max_align)
            size += (max_align - size) % max_align; // padding
        return size;
    }
    case TagType: {
        auto tag = Match(bl_t, TagType);
        int64_t max = 0;
        for (int64_t i = 0, len = length(tag->values); i < len; i++) {
            if (ith(tag->values, i) > max)
                max = ith(tag->values, i);
        }
        if (max > INT32_MAX)
            return 8;
        else if (max > INT16_MAX)
            return 4;
        else if (max > INT8_MAX)
            return 2;
        else
            return 1;
    }
    case TaggedUnionType: {
        auto tagged = Match(bl_t, TaggedUnionType);
        ssize_t size = gcc_sizeof(env, tagged->tag_type);
        ssize_t data_align = gcc_alignof(env, tagged->data);
        if (data_align > 1 && size % data_align)
            size += (data_align - size) % data_align; // padding
        size += gcc_sizeof(env, tagged->data);
        return size;
    }
    default: compile_err(env, NULL, "gcc_sizeof() isn't implemented for %s", type_to_string(bl_t));
    }
}

List(gcc_field_t*) get_union_fields(env_t *env, bl_type_t *t)
{
    // This is memoized to preserve GCC equality:
    List(gcc_field_t*) fields = hashmap_get(env->union_fields, type_to_string(t));
    if (fields) return fields;

    fields = LIST(gcc_field_t*);
    auto union_ = Match(t, UnionType);
    for (int64_t i = 0, len = length(union_->field_names); i < len; i++) {
        bl_type_t *ft = ith(union_->field_types, i);
        gcc_type_t *gcc_ft = bl_type_to_gcc(env, ft);
        gcc_field_t *field = gcc_new_field(env->ctx, NULL, gcc_ft, ith(union_->field_names, i));
        append(fields, field);
    }
    hashmap_set(env->union_fields, type_to_string(t), fields);
    return fields;
}

// This must be memoized because GCC JIT doesn't do structural equality
gcc_type_t *bl_type_to_gcc(env_t *env, bl_type_t *t)
{
    t = with_units(t, NULL);
    gcc_type_t *gcc_t = hashmap_get(env->gcc_types, type_to_string(t));
    if (gcc_t) return gcc_t;

    switch (t->tag) {
    case IntType: {
        switch (Match(t, IntType)->bits) {
        case 64: gcc_t = gcc_type(env->ctx, INT64); break;
        case 32: gcc_t = gcc_type(env->ctx, INT32); break;
        case 16: gcc_t = gcc_type(env->ctx, INT16); break;
        case 8: gcc_t = gcc_type(env->ctx, INT8); break;
        default: compile_err(env, NULL, "I couldn't get a GCC type for an integer with %d bits", Match(t, IntType)->bits);
        }
        break;
    }
    case CharType: gcc_t = gcc_type(env->ctx, CHAR); break;
    case BoolType: gcc_t = gcc_type(env->ctx, BOOL); break;
    case NumType: gcc_t = Match(t, NumType)->bits == 32 ? gcc_type(env->ctx, FLOAT) : gcc_type(env->ctx, DOUBLE); break;
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
        gcc_struct_t *array = gcc_new_struct_type(env->ctx, NULL, fresh("Array"), 3, fields);
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
        gcc_struct_t *gcc_struct = gcc_opaque_struct(env->ctx, NULL, struct_t->name ? struct_t->name : "Tuple");
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
        List(gcc_field_t*) fields = get_union_fields(env, t);
        if (length(fields) > 0) {
            gcc_t = gcc_union(env->ctx, NULL, "data_u", length(fields), fields[0]);
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

bool promote(env_t *env, bl_type_t *actual, gcc_rvalue_t **val, bl_type_t *needed)
{
    if (!can_promote(actual, needed))
        return false;
    
    if (actual != needed)
        *val = gcc_cast(env->ctx, NULL, *val, bl_type_to_gcc(env, needed));
    return true;
}

void check_truthiness(env_t *env, gcc_block_t **block, ast_t *obj, gcc_block_t *if_truthy, gcc_block_t *if_falsey)
{
    if (obj->tag == Declare) {
        // Special case for `if x := foo()`
        auto decl = Match(obj, Declare);
        bl_type_t *t = get_type(env, decl->value);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);

        if (t->tag == GeneratorType)
            compile_err(env, decl->value, "This expression isn't guaranteed to have a single value, so you can't use it to initialize a variable."); 
        else if (t->tag == VoidType)
            compile_err(env, decl->value, "This expression doesn't have a value (it has a Void type), so you can't store it in a variable."); 

        gcc_rvalue_t *rval = compile_expr(env, block, decl->value);
        gcc_func_t *func = gcc_block_func(*block);
        istr_t name = Match(decl->var, Var)->name;
        gcc_lvalue_t *lval = gcc_local(func, ast_loc(env, obj), gcc_t, fresh(name));
        binding_t *clobbered = hashmap_get_raw(env->bindings, name);
        if (clobbered && clobbered->type->tag == TypeType)
            compile_err(env, obj, "This name is already being used for the name of a type (struct or enum) in the same block, "
                  "and I get confused if you try to redeclare the name of a namespace.");
        hashmap_set(env->bindings, name,
                    new(binding_t, .lval=lval, .rval=gcc_rval(lval), .type=t));
        assert(rval);
        gcc_assign(*block, ast_loc(env, obj), lval, rval);

        gcc_rvalue_t *bool_val = gcc_rval(lval);
        if (gcc_type_if_pointer(gcc_t))
            bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, bool_val, gcc_null(env->ctx, gcc_t));
        else
            bool_val = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, bool_val, gcc_zero(env->ctx, gcc_t));
        gcc_jump_condition(*block, NULL, bool_val, if_truthy, if_falsey);
        *block = NULL;
        return;
    }

    bl_type_t *t = get_type(env, obj);
    gcc_rvalue_t *bool_val = compile_expr(env, block, obj); 
    switch (t->tag) {
    case BoolType: break;
    case StructType: {
        compile_err(env, obj, "This value is a struct and can't be used as a conditional.");
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

gcc_rvalue_t *quote_string(env_t *env, bl_type_t *t, gcc_rvalue_t *val)
{
    if (t->tag != ArrayType || Match(t, ArrayType)->item_type->tag != CharType)
        return val;

    istr_t dsl = Match(t, ArrayType)->dsl;
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, bl_type_to_gcc(env, t), "str"),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, STRING), "dsl"),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, BOOL), "use_color")
    };
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, bl_type_to_gcc(env, t),
                                    "bl_string_quoted", 3, params, 0);
    return gcc_callx(env->ctx, NULL, func, val, gcc_str(env->ctx, dsl ? dsl : ""), gcc_zero(env->ctx, gcc_type(env->ctx, BOOL)));
}

gcc_func_t *get_print_func(env_t *env, bl_type_t *t)
{
    // Hash map for tracking recursion: {ptr => index, "__max_index" => max_index}
    // Create a function `int print(T obj, FILE* file, void* recursion)`
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
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, VOID_PTR), fresh("recursion")),
    };
    func = gcc_new_func(
        env->ctx, NULL, GCC_FUNCTION_INTERNAL, int_t,
        fresh("print"), 3, params, 0);
    hashmap_set(env->print_funcs, t, func);

    gcc_block_t *block = gcc_new_block(func, fresh("print"));
    gcc_comment(block, NULL, CORD_to_char_star(CORD_cat("print() for type: ", type_to_string(t))));
    gcc_rvalue_t *obj = gcc_param_as_rvalue(params[0]);
    gcc_rvalue_t *f = gcc_param_as_rvalue(params[1]);
    gcc_rvalue_t *rec = gcc_param_as_rvalue(params[2]);

    gcc_func_t *fputs_fn = hashmap_gets(env->global_funcs, "fputs");

#define WRITE_LITERAL(str) gcc_callx(env->ctx, NULL, fputs_fn, gcc_str(env->ctx, str), f)

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
        gcc_return(block, NULL, gcc_callx(env->ctx, NULL, fputc_fn, obj, f));
        break;
    }
    case IntType: case NumType: {
        const char *fmt;
        if (t->tag == IntType && Match(t, IntType)->bits == 64) {
            fmt = "%ld";
        } else if (t->tag == IntType && Match(t, IntType)->bits == 32) {
            fmt = "%d_i32";
        } else if (t->tag == IntType && Match(t, IntType)->bits == 16) {
            // I'm not sure why, but printf() gets confused if you pass smaller ints to a "%d" format
            obj = gcc_cast(env->ctx, NULL, obj, gcc_type(env->ctx, INT));
            fmt = "%d_i16";
        } else if (t->tag == IntType && Match(t, IntType)->bits == 8) {
            obj = gcc_cast(env->ctx, NULL, obj, gcc_type(env->ctx, INT));
            fmt = "%d_i8";
        } else if (t->tag == NumType && Match(t, NumType)->bits == 64) {
            fmt = "%g";
        } else if (t->tag == NumType && Match(t, NumType)->bits == 32) {
            // I'm not sure why, but printf() gets confused if you pass a 'float' here instead of a 'double'
            obj = gcc_cast(env->ctx, NULL, obj, gcc_type(env->ctx, DOUBLE));
            fmt = "%g_f32";
        } else {
            errx(1, "Unreachable");
        }
        istr_t units = type_units(t);
        if (units == intern_str("%")) {
            fmt = intern_strf("%s%%%%", fmt);
            obj = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, gcc_t, obj, gcc_rvalue_from_long(env->ctx, gcc_t, 100));
        } else if (units && strlen(units) > 0) {
            fmt = intern_strf("%s<%s>", fmt, units);
        }
        gcc_func_t *fprintf_fn = hashmap_gets(env->global_funcs, "fprintf");
        gcc_return(block, NULL, gcc_callx(env->ctx, NULL, fprintf_fn, f, gcc_str(env->ctx, fmt), obj));
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
        gcc_lvalue_t *printed_var = gcc_local(func, NULL, int_t, "printed");
        gcc_assign(block, NULL, printed_var, WRITE_LITERAL(type_prefix));
        gcc_block_t *done = gcc_new_block(func, fresh("done"));
        NEW_LIST(gcc_case_t*, cases);
        for (int64_t i = 0, len = length(tags->names); i < len; i++) {
            istr_t tag_name = ith(tags->names, i);
            gcc_block_t *tag_block = gcc_new_block(func, fresh(tag_name));

            auto union_t = Match(tagged_t->data, UnionType);
            List(gcc_field_t*) union_fields = get_union_fields(env, tagged_t->data);
            for (int64_t u = 0, len = length(union_t->field_names); u < len; u++) {
                if (ith(union_t->field_names, u) == tag_name) {
                    bl_type_t *tag_data_type = ith(union_t->field_types, u);
                    gcc_field_t *data_field = gcc_get_field(tagged_struct, 1);
                    gcc_rvalue_t *data = gcc_rvalue_access_field(obj, NULL, data_field);
                    gcc_field_t *union_field = ith(union_fields, u);
                    gcc_func_t *tag_print = get_print_func(env, tag_data_type);
                    if (tag_data_type->tag != StructType)
                        gcc_update(tag_block, NULL, printed_var, GCC_BINOP_PLUS, WRITE_LITERAL(intern_strf("%s{", tag_name)));
                    else if (!Match(tag_data_type, StructType)->name)
                        gcc_update(tag_block, NULL, printed_var, GCC_BINOP_PLUS, WRITE_LITERAL(tag_name));
                    gcc_rvalue_t *suffix_len = gcc_callx(
                        env->ctx, NULL, tag_print,
                        quote_string(env, tag_data_type, gcc_rvalue_access_field(data, NULL, union_field)),
                        f,
                        gcc_param_as_rvalue(params[2]));
                    gcc_update(tag_block, NULL, printed_var, GCC_BINOP_PLUS, suffix_len);
                    if (tag_data_type->tag != StructType)
                        gcc_update(tag_block, NULL, printed_var, GCC_BINOP_PLUS, WRITE_LITERAL("}"));
                    goto found_struct;
                }
            }
            gcc_update(tag_block, NULL, printed_var, GCC_BINOP_PLUS, WRITE_LITERAL(tag_name));
          found_struct:;
            gcc_jump(tag_block, NULL, done);
            int64_t tag_value = ith(tags->values, i);
            gcc_rvalue_t *rval = gcc_rvalue_from_long(env->ctx, tag_gcc_t, tag_value);
            gcc_case_t *case_ = gcc_new_case(env->ctx, rval, rval, tag_block);
            APPEND(cases, case_);
        }
        gcc_block_t *default_block = gcc_new_block(func, fresh("default"));
        gcc_update(default_block, NULL, printed_var, GCC_BINOP_PLUS, WRITE_LITERAL("<Unknown value>"));
        gcc_jump(default_block, NULL, done);

        gcc_switch(block, NULL, tag, default_block, length(cases), cases[0]);

        gcc_return(done, NULL, gcc_rval(printed_var));
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
        gcc_block_t *nil_block = gcc_new_block(func, fresh("nil"));
        gcc_block_t *nonnil_block = gcc_new_block(func, fresh("nonnil"));

        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);
        gcc_rvalue_t *is_nil = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, obj, gcc_null(env->ctx, gcc_t));

        gcc_jump_condition(block, NULL, is_nil, nil_block, nonnil_block);
        block = NULL;

        // If it's nil, print !Type:
        bl_type_t *pointed_type = Match(t, PointerType)->pointed;
        gcc_return(nil_block, NULL, WRITE_LITERAL(intern_strf("!%s", type_to_string(pointed_type))));

        if (pointed_type->tag == CharType) {
            gcc_func_t *fputs_fn = hashmap_gets(env->global_funcs, "fputs");
            gcc_return(nonnil_block, NULL, gcc_callx(env->ctx, NULL, fputs_fn, obj, f));
            return func;
        }

        // If it's non-nil, check for cycles:
        gcc_type_t *i64 = gcc_type(env->ctx, INT64);
        gcc_type_t *void_star = gcc_type(env->ctx, VOID_PTR);
        gcc_param_t *gcc_hash_get_params[] = {
            gcc_new_param(env->ctx, NULL, void_star, "h"),
            gcc_new_param(env->ctx, NULL, void_star, "key"),
        };
        gcc_func_t *hash_get_func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, i64, "hashmap_get", 2, gcc_hash_get_params, 0);

        // index = hashmap_get(rec, obj)
        block = nonnil_block;
        gcc_block_t *noncycle_block = gcc_new_block(func, fresh("noncycle"));
        gcc_block_t *cycle_block = gcc_new_block(func, fresh("cycle"));
        gcc_lvalue_t *index = gcc_local(func, NULL, gcc_type(env->ctx, INT64), fresh("index"));
        gcc_assign(block, NULL, index, gcc_callx(
                env->ctx, NULL, hash_get_func, rec, gcc_cast(env->ctx, NULL, obj, void_star)));

        // if (index > 0) goto cycle else goto noncycle
        gcc_jump_condition(block, NULL,
                           gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GT, gcc_rval(index), gcc_zero(env->ctx, i64)),
                           cycle_block, noncycle_block);

        // If we're in a recursive cycle, print @T#index and return without recursing further
        block = cycle_block;
        gcc_func_t *fprintf_fn = hashmap_gets(env->global_funcs, "fprintf");
        istr_t backref = intern_strf("@%s#%%ld", type_to_string(pointed_type));
        gcc_return(block, NULL, gcc_callx(env->ctx, NULL, fprintf_fn, f, gcc_str(env->ctx, backref), gcc_rval(index)));

        // If this is a nonrecursive situation
        block = noncycle_block;
        gcc_assign(block, NULL, index, gcc_callx(
                env->ctx, NULL, hash_get_func, rec, gcc_cast(env->ctx, NULL, gcc_str(env->ctx, "max_index"), void_star)));
        gcc_update(block, NULL, index, GCC_BINOP_PLUS, gcc_one(env->ctx, i64));

        gcc_param_t *gcc_hash_set_params[] = {
            gcc_new_param(env->ctx, NULL, void_star, "h"),
            gcc_new_param(env->ctx, NULL, void_star, "key"),
            gcc_new_param(env->ctx, NULL, i64, "value"),
        };
        gcc_func_t *hash_set_func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_IMPORTED, gcc_type(env->ctx, VOID), "hashmap_set", 3, gcc_hash_set_params, 0);
        gcc_eval(block, NULL, gcc_callx(
                env->ctx, NULL, hash_set_func, rec, gcc_cast(env->ctx, NULL, gcc_str(env->ctx, "max_index"), void_star), gcc_rval(index)));
        gcc_eval(block, NULL, gcc_callx(
                env->ctx, NULL, hash_set_func, rec, gcc_cast(env->ctx, NULL, obj, void_star), gcc_rval(index)));

        if (pointed_type->tag == VoidType) {
            gcc_func_t *fprintf_fn = hashmap_gets(env->global_funcs, "fprintf");
            gcc_return(block, NULL, gcc_callx(env->ctx, NULL, fprintf_fn, f, gcc_str(env->ctx, "@Void<0x%X>"), obj));
            // gcc_return(noncycle_block, NULL, WRITE_LITERAL("@Void"));
        } else {
            // Prepend "@"
            gcc_rvalue_t *at = WRITE_LITERAL("@");

            gcc_func_t *print_fn = get_print_func(env, pointed_type);
            assert(print_fn);
            gcc_rvalue_t *printed = gcc_callx(
                env->ctx, NULL, print_fn,
                quote_string(env, pointed_type, gcc_rval(gcc_rvalue_dereference(obj, NULL))),
                gcc_param_as_rvalue(params[1]),
                rec);
            gcc_return(noncycle_block, NULL,
                       gcc_binary_op(env->ctx, NULL, GCC_BINOP_PLUS, int_t, at, printed));
        }

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
            written = ADD_INT(written,
                              gcc_callx(env->ctx, NULL, print_fn, 
                                        quote_string(env, member_t, gcc_rvalue_access_field(obj, NULL, field)),
                                        gcc_param_as_rvalue(params[1]),
                                        gcc_param_as_rvalue(params[2])
                              ));
        }

        written = ADD_INT(written, WRITE_LITERAL("}"));

        istr_t units = type_units(t);
        if (units && strlen(units) > 0)
            written = ADD_INT(written, WRITE_LITERAL(intern_strf("<%s>", units)));

        gcc_return(block, NULL, written);
        break;
    }
#undef ADD_INT
    case ArrayType: {
        compile_array_print_func(env, &block, obj, rec, f, t);
        break;
    }
    case FunctionType: {
        gcc_return(block, NULL, WRITE_LITERAL(type_to_string(t)));
        break;
    }
    case TypeType: {
        gcc_return(block, NULL,
                   gcc_callx(env->ctx, NULL, fputs_fn,
                             gcc_cast(env->ctx, NULL, obj, gcc_type(env->ctx, STRING)), f));
        break;
    }
    default: {
        fprintf(stderr, "\x1b[31;1mprint(%s) function is not yet implemented!\n", type_to_string(t));
        exit(1);
    }
    }
    return func;
#undef WRITE_LITERAL
}

// Helper function to make value comparison return an int that is one of [-1,0,1]
gcc_rvalue_t *compare_values(env_t *env, bl_type_t *t, gcc_rvalue_t *a, gcc_rvalue_t *b)
{
    // (int)((a > b) - (a < b))
    if (is_numeric(t) || t->tag == PointerType || t->tag == CharType || t->tag == BoolType) {
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

        auto tagged_t = Match(t, TaggedUnionType);
        auto tags = Match(tagged_t->tag_type, TagType);
        gcc_type_t *tag_gcc_t = bl_type_to_gcc(env, tagged_t->tag_type);
        NEW_LIST(gcc_case_t*, cases);
        for (int64_t i = 0, len = length(tags->names); i < len; i++) {
            istr_t tag_name = ith(tags->names, i);

            auto union_t = Match(tagged_t->data, UnionType);
            List(gcc_field_t*) union_fields = get_union_fields(env, tagged_t->data);
            for (int64_t u = 0, len = length(union_t->field_names); u < len; u++) {
                if (ith(union_t->field_names, u) == tag_name) {
                    gcc_block_t *tag_block = gcc_new_block(func, fresh(tag_name));
                    bl_type_t *tag_data_type = ith(union_t->field_types, u);
                    gcc_field_t *data_field = gcc_get_field(tagged_struct, 1);
                    gcc_rvalue_t *lhs_data = gcc_rvalue_access_field(lhs, NULL, data_field);
                    gcc_rvalue_t *rhs_data = gcc_rvalue_access_field(rhs, NULL, data_field);
                    gcc_field_t *union_field = ith(union_fields, u);
                    gcc_rvalue_t *lhs_field = gcc_rvalue_access_field(lhs_data, NULL, union_field);
                    gcc_rvalue_t *rhs_field = gcc_rvalue_access_field(rhs_data, NULL, union_field);
                    gcc_return(tag_block, NULL, compare_values(env, tag_data_type, lhs_field, rhs_field));

                    int64_t tag_value = ith(tags->values, i);
                    gcc_rvalue_t *rval = gcc_rvalue_from_long(env->ctx, tag_gcc_t, tag_value);
                    gcc_case_t *case_ = gcc_new_case(env->ctx, rval, rval, tag_block);
                    APPEND(cases, case_);
                    break;
                }
            }
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
        gcc_rvalue_t *lhs_data = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, 0)),
                     *rhs_data = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, 0));
        gcc_rvalue_t *lhs_stride = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, 2)),
                     *rhs_stride = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, 2));

        gcc_block_t *loop_condition = gcc_new_block(func, fresh("loop_condition")),
                    *loop_body = gcc_new_block(func, fresh("loop_body")),
                    *loop_end = gcc_new_block(func, fresh("loop_end"));

        gcc_type_t *int32 = gcc_type(env->ctx, INT32);
        gcc_lvalue_t *index_var = gcc_local(func, NULL, int32, fresh("i"));
        gcc_rvalue_t *index_rval = gcc_rval(index_var);
        gcc_assign(block, NULL, index_var, gcc_zero(env->ctx, int32));

        gcc_jump_condition(block, NULL,
                           gcc_binary_op(env->ctx, NULL, GCC_BINOP_LOGICAL_AND, gcc_type(env->ctx, BOOL),
                                         gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, lhs_data, rhs_data),
                                         gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, lhs_stride, rhs_stride)),
                           loop_end, loop_condition);

        // loop_condition:
        gcc_rvalue_t *lhs_len = gcc_rvalue_access_field(lhs, NULL, gcc_get_field(array_struct, 1)),
                     *rhs_len = gcc_rvalue_access_field(rhs, NULL, gcc_get_field(array_struct, 1));
        gcc_rvalue_t *lhs_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GE, index_rval, lhs_len);
        gcc_rvalue_t *rhs_done = gcc_comparison(env->ctx, NULL, GCC_COMPARISON_GE, index_rval, rhs_len);
        gcc_rvalue_t *either_done = gcc_binary_op(env->ctx, NULL, GCC_BINOP_LOGICAL_OR, gcc_type(env->ctx, BOOL),
                                                  lhs_done, rhs_done);

        gcc_jump_condition(loop_condition, NULL, either_done, loop_end, loop_body);

        // loop_body:
        bl_type_t *item_t = Match(t, ArrayType)->item_type;

        gcc_func_t *cmp_fn = get_compare_func(env, item_t);
        assert(cmp_fn);
        gcc_rvalue_t *lhs_offset = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, int32, index_rval, lhs_stride);
        gcc_rvalue_t *rhs_offset = gcc_binary_op(env->ctx, NULL, GCC_BINOP_MULT, int32, index_rval, rhs_stride);
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
    default: {
        if (is_numeric(t) || t->tag == PointerType || t->tag == CharType || t->tag == BoolType) {
            gcc_return(block, NULL, compare_values(env, t, lhs, rhs));
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

gcc_rvalue_t *ternary(gcc_block_t **block, gcc_rvalue_t *condition, gcc_type_t *gcc_t, gcc_rvalue_t *true_val, gcc_rvalue_t *false_val)
{
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *is_true = gcc_new_block(func, fresh("is_true")),
                *is_false = gcc_new_block(func, fresh("is_false")),
                *got_value = gcc_new_block(func, fresh("got_value"));

    gcc_lvalue_t *result = gcc_local(func, NULL, gcc_t, fresh("result"));
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
    switch (ast->tag) {
    case Var: {
        binding_t *binding = hashmap_get(env->bindings, Match(ast, Var)->name);
        if (binding) {
            if (!binding->lval)
                compile_err(env, ast, "This variable can't be assigned to. You can try declaring a new variable with the same name, though.");
            return binding->lval;
        } else {
            compile_err(env, ast, "I don't know what this variable is referring to."); 
        }
    }
    case Dereference: {
        (void)get_type(env, ast); // Check this is a pointer type
        ast_t *value = Match(ast, Dereference)->value;
        gcc_rvalue_t *rval = compile_expr(env, block, value);
        return gcc_rvalue_dereference(rval, ast_loc(env, ast));
    }
    case FieldAccess: {
        auto access = Match(ast, FieldAccess);
        bl_type_t *fielded_t = get_type(env, access->fielded);
        if (fielded_t->tag == ArrayType) {
            if (!allow_slices)
                compile_err(env, ast, "I can't assign to array slices");
            gcc_func_t *func = gcc_block_func(*block);
            gcc_lvalue_t *slice = gcc_local(func, NULL, bl_type_to_gcc(env, get_type(env, ast)), fresh("slice"));
            gcc_assign(*block, NULL, slice, compile_expr(env, block, ast));
            return slice;
        }
        gcc_lvalue_t *fielded_lval = get_lvalue(env, block, access->fielded, true);
      keep_going:
        switch (fielded_t->tag) { 
        case PointerType: {
            auto fielded_ptr = Match(fielded_t, PointerType);
            if (fielded_ptr->is_optional)
                compile_err(env, ast, "Accessing a field on this value could result in trying to dereference a nil value, since the type is optional");
            fielded_lval = gcc_rvalue_dereference(gcc_rval(fielded_lval), NULL);
            fielded_t = fielded_ptr->pointed;
            goto keep_going;
        }
        case StructType: {
            auto fielded_struct = Match(fielded_t, StructType);
            for (int64_t i = 0, len = length(fielded_struct->field_names); i < len; i++) {
                if (ith(fielded_struct->field_names, i) == access->field) {
                    gcc_struct_t *gcc_struct = gcc_type_if_struct(bl_type_to_gcc(env, fielded_t));
                    gcc_field_t *field = gcc_get_field(gcc_struct, i);
                    return gcc_lvalue_access_field(fielded_lval, NULL, field);
                }
            }
            compile_err(env, ast, "The struct %s doesn't have a field called '%s'",
                  type_to_string(fielded_t), access->field);
        }
        // TODO: support using TaggedUnion field and Type fields as lvalues
        default: {
            compile_err(env, ast, "This value is a %s, and I don't know how to assign to fields on it.",
                  type_to_string(fielded_t));
        }
        }
    }
    case Index: {
        auto indexing = Match(ast, Index);
        bl_type_t *indexed_t = get_type(env, indexing->indexed);
        gcc_type_t *gcc_t = bl_type_to_gcc(env, indexed_t);
        gcc_rvalue_t *obj = compile_expr(env, block, indexing->indexed);
        if (indexed_t->tag != ArrayType)
            compile_err(env, ast, "I only know how to index into lists, but this is a %s", type_to_string(indexed_t));

        bl_type_t *index_t = get_type(env, indexing->index);
        if (index_t->tag == RangeType) {
            if (!allow_slices)
                compile_err(env, ast, "I can't assign to array slices");
            gcc_func_t *func = gcc_block_func(*block);
            gcc_lvalue_t *slice = gcc_local(func, NULL, bl_type_to_gcc(env, get_type(env, ast)), fresh("slice"));
            gcc_assign(*block, NULL, slice, compile_expr(env, block, ast));
            return slice;
        } else if (!is_integral(index_t)) {
            compile_err(env, indexing->index, "I only support indexing arrays by integers, not %s", type_to_string(index_t));
        }
        
        gcc_type_t *i64_t = gcc_type(env->ctx, INT64);
        gcc_struct_t *array_struct = gcc_type_if_struct(gcc_t);
        gcc_loc_t *loc = ast_loc(env, ast);
        gcc_rvalue_t *items = gcc_rvalue_access_field(obj, loc, gcc_get_field(array_struct, 0));
        gcc_rvalue_t *index = gcc_cast(env->ctx, loc, compile_expr(env, block, indexing->index), i64_t);
        gcc_rvalue_t *stride64 = gcc_cast(env->ctx, loc, gcc_rvalue_access_field(obj, loc, gcc_get_field(array_struct, 2)), i64_t);

        if (!indexing->unchecked) {
            // Bounds check:
            gcc_rvalue_t *big_enough = gcc_comparison(env->ctx, loc, GCC_COMPARISON_GE, index, gcc_one(env->ctx, i64_t));
            gcc_rvalue_t *len64 = gcc_cast(env->ctx, loc, gcc_rvalue_access_field(obj, loc, gcc_get_field(array_struct, 1)), i64_t);
            gcc_rvalue_t *small_enough = gcc_comparison(env->ctx, loc, GCC_COMPARISON_LE, index, len64);
            gcc_rvalue_t *ok = gcc_binary_op(env->ctx, loc, GCC_BINOP_LOGICAL_AND, gcc_type(env->ctx, BOOL), big_enough, small_enough);

            gcc_func_t *func = gcc_block_func(*block);
            gcc_block_t *bounds_safe = gcc_new_block(func, fresh("bounds_safe")),
                        *bounds_unsafe = gcc_new_block(func, fresh("bounds_unsafe"));
            gcc_jump_condition(*block, loc, ok, bounds_safe, bounds_unsafe);

            // Bounds check failure:
            gcc_rvalue_t *fmt = gcc_str(env->ctx, "\x1b[31;1;7mError: index %ld is not inside the array (1..%ld)\x1b[m\n\n%s");
            char *info = NULL;
            size_t size = 0;
            FILE *f = open_memstream(&info, &size);
            fprint_span(f, ast->span, "\x1b[31;1m", 2, true);
            fputc('\0', f);
            fflush(f);
            gcc_rvalue_t *callstack = gcc_str(env->ctx, info);
            gcc_func_t *fail = hashmap_gets(env->global_funcs, "fail");
            gcc_eval(bounds_unsafe, loc, gcc_callx(env->ctx, loc, fail, fmt, index, len64, callstack));
            fclose(f);
            free(info);
            gcc_jump(bounds_unsafe, loc, bounds_unsafe);

            // Bounds check success:
            *block = bounds_safe;
        }
        gcc_rvalue_t *index0 = gcc_binary_op(env->ctx, loc, GCC_BINOP_MINUS, i64_t, index, gcc_one(env->ctx, i64_t));
        index0 = gcc_binary_op(env->ctx, loc, GCC_BINOP_MULT, i64_t, index0, stride64);
        return gcc_array_access(env->ctx, loc, items, index0);
    }
    default:
        compile_err(env, ast, "This is not a valid Lvalue");
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

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
