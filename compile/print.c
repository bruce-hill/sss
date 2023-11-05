// Logic for printing different SSS values as strings
#include <assert.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <gc/cord.h>
#include <limits.h>
#include <stdint.h>

#include "../ast.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../builtins/table.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

void maybe_append_cord(env_t *env, gcc_block_t **block, gcc_lvalue_t *cord, gcc_rvalue_t *do_append, const char *str)
{
    gcc_func_t *func = gcc_block_func(*block);
    gcc_block_t *append_block = gcc_new_block(func, fresh("do_append")),
                *done_block = gcc_new_block(func, fresh("done"));
    gcc_jump_condition(*block, NULL, do_append, append_block, done_block);

    gcc_func_t *cord_cat_fn = get_function(env, "CORD_cat");
    gcc_assign(append_block, NULL, cord, gcc_callx(env->ctx, NULL, cord_cat_fn, gcc_rval(cord), gcc_str(env->ctx, str)));
    gcc_jump(append_block, NULL, done_block);
    *block = done_block;
}

gcc_func_t *get_cord_func(env_t *env, sss_type_t *t)
{
    // Create a function `CORD __cord(T obj, void* recursion, bool use_color)`
    // that converts an object to a CORD

    // __cord() is the same for optional/non-optional pointers:
    if (t->tag == PointerType)
        t = Type(PointerType, .pointed=Match(t, PointerType)->pointed, .is_optional=true, .is_stack=Match(t, PointerType)->is_stack);

    // Memoize:
    const char *units = type_units(t);
    const char *func_name = units && units[0] ? heap_strf("__cord<%s>", units) : "__cord";
    binding_t *b = get_from_namespace(env, t, func_name);
    if (b) return b->func;

    // Reuse same function for all Type types:
    if (t->tag == TypeType && Match(t, TypeType)->type) {
        gcc_func_t *func = get_cord_func(env, Type(TypeType));
        Table_sets(get_namespace(env, t), func_name, get_from_namespace(env, Type(TypeType), func_name));
        return func;
    }

    gcc_type_t *gcc_t = sss_type_to_gcc(env, t);

    gcc_type_t *void_ptr_t = sss_type_to_gcc(env, Type(PointerType, .pointed=Type(MemoryType)));
    gcc_param_t *params[] = {
        gcc_new_param(env->ctx, NULL, gcc_t, "obj"),
        gcc_new_param(env->ctx, NULL, void_ptr_t, "recursion"),
        gcc_new_param(env->ctx, NULL, gcc_type(env->ctx, BOOL), "color"),
    };
    const char *sym_name = fresh( func_name);
    gcc_func_t *func = gcc_new_func(env->ctx, NULL, GCC_FUNCTION_INTERNAL, gcc_type(env->ctx, STRING), sym_name, 3, params, 0);
    sss_type_t *fn_t = Type(FunctionType,
                           .arg_types=ARRAY(t, Type(PointerType, .pointed=Type(MemoryType)), Type(BoolType)),
                           .arg_names=ARRAY((const char*)"obj", "recursion", "color"),
                           .arg_defaults=NULL, .ret=Type(PointerType, .pointed=Type(MemoryType)));
    table_t *ns = get_namespace(env, t);
    Table_sets(ns, func_name, new(binding_t, .func=func, .rval=gcc_get_func_address(func, NULL), .type=fn_t, .sym_name=sym_name));

    gcc_block_t *block = gcc_new_block(func, fresh("to_cord"));
    gcc_comment(block, NULL, CORD_to_char_star(CORD_cat("to_cord() for type: ", type_to_typeof_string(t))));
    gcc_rvalue_t *obj = gcc_param_as_rvalue(params[0]);
    gcc_rvalue_t *rec = gcc_param_as_rvalue(params[1]);
    gcc_rvalue_t *color = gcc_param_as_rvalue(params[2]);
    gcc_lvalue_t *cord = gcc_local(func, NULL, gcc_type(env->ctx, STRING), "cord");
    gcc_assign(block, NULL, cord, gcc_null(env->ctx, gcc_type(env->ctx, STRING)));

    gcc_func_t *cord_cat_fn = get_function(env, "CORD_cat");

#define APPEND_CORD(block, to_append) gcc_assign(block, NULL, cord, gcc_callx(env->ctx, NULL, cord_cat_fn, gcc_rval(cord), to_append))
#define APPEND_LITERAL(block, str) gcc_assign(block, NULL, cord, gcc_callx(env->ctx, NULL, cord_cat_fn, gcc_rval(cord), gcc_str(env->ctx, str)))
#define APPEND_COLOR_LITERAL(block, str) maybe_append_cord(env, block, cord, color, str)

    switch (t->tag) {
    case BoolType: {
        gcc_block_t *yes_block = gcc_new_block(func, fresh("yes"));
        gcc_block_t *no_block = gcc_new_block(func, fresh("no"));
        APPEND_COLOR_LITERAL(&block, "\x1b[35m");
        assert(block);
        gcc_jump_condition(block, NULL, obj, yes_block, no_block);
        APPEND_LITERAL(yes_block, "yes");
        APPEND_COLOR_LITERAL(&yes_block, "\x1b[m");
        gcc_return(yes_block, NULL, gcc_rval(cord));
        APPEND_LITERAL(no_block, "no");
        APPEND_COLOR_LITERAL(&no_block, "\x1b[m");
        gcc_return(no_block, NULL, gcc_rval(cord));
        break;
    }
    case CharType: case CStringCharType: {
        char *escapes[128] = {['\a']="\\a",['\b']="\\b",['\x1b']="\\e",['\f']="\\f",['\n']="\\n",['\t']="\\t",
            ['\r']="\\r",['\v']="\\v",['"']="\\\"",['\\']="\\\\"};
        auto cases = EMPTY_ARRAY(gcc_case_t*);

        for (int i = 0; i < 128; i++) {
            char *escape_str = escapes[i];
            if (!escape_str) continue;
            gcc_rvalue_t *case_val = gcc_rvalue_from_long(env->ctx, gcc_t, i);
            gcc_block_t *case_block = gcc_new_block(func, fresh("char_escape"));
            gcc_case_t *case_ = gcc_new_case(env->ctx, case_val, case_val, case_block);
            APPEND_COLOR_LITERAL(&case_block, "\x1b[1;34m");
            APPEND_LITERAL(case_block, escape_str);
            APPEND_COLOR_LITERAL(&case_block, "\x1b[m");
            gcc_return(case_block, NULL, gcc_rval(cord));
            append(cases, case_);
        }

        // Hex escape:
        gcc_block_t *hex_block = gcc_new_block(func, fresh("char_hex_escape"));
        int intervals[][2] = {{'\0','\x06'}, {'\x0E','\x1A'}, {'\x1C','\x1F'},{'\x7F','\x7F'},{CHAR_MIN,-1}};
        for (size_t i = 0; i < sizeof(intervals)/sizeof(intervals[0]); i++) {
            gcc_case_t *hex_case = gcc_new_case(
                env->ctx,
                gcc_rvalue_from_long(env->ctx, gcc_t, intervals[i][0]),
                gcc_rvalue_from_long(env->ctx, gcc_t, intervals[i][1]), hex_block);
            append(cases, hex_case);
        }

        APPEND_COLOR_LITERAL(&hex_block, "\x1b[1;34m");
        gcc_func_t *cord_sprintf_fn = get_function(env, "CORD_sprintf");
        gcc_eval(hex_block, NULL, gcc_callx(env->ctx, NULL, cord_sprintf_fn, gcc_lvalue_address(cord, NULL),
                                            gcc_str(env->ctx, "%r\\x%02X"), gcc_rval(cord), obj));
        APPEND_COLOR_LITERAL(&hex_block, "\x1b[m");
        gcc_return(hex_block, NULL, gcc_rval(cord));

        gcc_block_t *default_block = gcc_new_block(func, fresh("default"));
        gcc_switch(block, NULL, obj, default_block, LENGTH(cases), cases[0]);

        APPEND_COLOR_LITERAL(&default_block, "\x1b[35m");
        gcc_func_t *cord_cat_char_fn = get_function(env, "CORD_cat_char");
        gcc_assign(default_block, NULL, cord, gcc_callx(env->ctx, NULL, cord_cat_char_fn, gcc_rval(cord), obj));
        APPEND_COLOR_LITERAL(&default_block, "\x1b[m");
        gcc_return(default_block, NULL, gcc_rval(cord));

        break;
    }
    case IntType: case NumType: {
        APPEND_COLOR_LITERAL(&block, "\x1b[35m");
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
        gcc_func_t *cord_sprintf_fn = get_function(env, "CORD_sprintf");
        gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, cord_sprintf_fn, gcc_lvalue_address(cord, NULL),
                                        gcc_str(env->ctx, heap_strf("%%r%s", fmt)), gcc_rval(cord), obj));

        if (streq(units, "%")) {
            APPEND_COLOR_LITERAL(&block, "\x1b[33;2m");
            APPEND_LITERAL(block, "%");
            APPEND_COLOR_LITERAL(&block, "\x1b[m");
        } else if (units && strlen(units) > 0) {
            APPEND_COLOR_LITERAL(&block, "\x1b[33;2m");
            APPEND_LITERAL(block, heap_strf("<%s>", units));
            APPEND_COLOR_LITERAL(&block, "\x1b[m");
        } else {
            APPEND_COLOR_LITERAL(&block, "\x1b[m");
        }
        gcc_return(block, NULL, gcc_rval(cord));
        break;
    }
    case TaggedUnionType: {
        gcc_struct_t *tagged_struct = gcc_type_if_struct(gcc_t);
        gcc_field_t *tag_field = gcc_get_field(tagged_struct, 0);
        gcc_rvalue_t *tag = gcc_rvalue_access_field(obj, NULL, tag_field);
        auto tagged = Match(t, TaggedUnionType);
        APPEND_COLOR_LITERAL(&block, "\x1b[0;1;36m");
        gcc_block_t *done = gcc_new_block(func, fresh("done"));
        gcc_type_t *tag_gcc_t = get_tag_type(env, t);
        gcc_lvalue_t *tag_var = gcc_local(func, NULL, tag_gcc_t, "_tag");
        gcc_assign(block, NULL, tag_var, tag);
        tag = gcc_rval(tag_var);
        gcc_type_t *union_gcc_t = get_union_type(env, t);
        auto cases = EMPTY_ARRAY(gcc_case_t*);
        bool any_values = false;
        for (int64_t i = 0; i < LENGTH(tagged->members); i++) {
            auto member = ith(tagged->members, i);
            gcc_block_t *tag_block = gcc_new_block(func, fresh(member.name));
            gcc_block_t *rest_of_tag_block = tag_block;
            APPEND_LITERAL(rest_of_tag_block, member.name);
            if (member.type && member.type->tag == StructType && LENGTH(Match(member.type, StructType)->field_types) > 0) {
                any_values = true;
                APPEND_LITERAL(rest_of_tag_block, "(");
                APPEND_COLOR_LITERAL(&rest_of_tag_block, "\x1b[m");
                // TODO: don't print curly braces
                gcc_field_t *data_field = gcc_get_field(tagged_struct, 1);
                gcc_rvalue_t *data = gcc_rvalue_access_field(obj, NULL, data_field);
                gcc_field_t *union_field = gcc_get_union_field(union_gcc_t, i);

                gcc_rvalue_t *val = gcc_rvalue_access_field(data, NULL, union_field);

                auto field_names = Match(member.type, StructType)->field_names;
                auto field_types = Match(member.type, StructType)->field_types;
                gcc_struct_t *gcc_struct = gcc_type_if_struct(sss_type_to_gcc(env, member.type));
                for (int64_t i = 0; i < LENGTH(field_names); i++) {
                    if (i > 0) {
                        APPEND_COLOR_LITERAL(&rest_of_tag_block, "\x1b[m");
                        APPEND_LITERAL(rest_of_tag_block, ", ");
                    }

                    const char *name = field_names ? ith(field_names, i) : NULL;
                    if (name && !streq(name, heap_strf("_%lu", i+1))) {
                        APPEND_COLOR_LITERAL(&rest_of_tag_block, "\x1b[m");
                        APPEND_LITERAL(rest_of_tag_block, name);
                        APPEND_COLOR_LITERAL(&rest_of_tag_block, "\x1b[33m");
                        APPEND_LITERAL(rest_of_tag_block, "=");
                    }

                    sss_type_t *member_t = ith(field_types, i);
                    gcc_func_t *cord_fn = get_cord_func(env, member_t);
                    assert(cord_fn);
                    gcc_field_t *field = gcc_get_field(gcc_struct, i);
                    APPEND_CORD(rest_of_tag_block, gcc_callx(
                            env->ctx, NULL, cord_fn, 
                            gcc_rvalue_access_field(val, NULL, field),
                            rec, color));
                }

                APPEND_COLOR_LITERAL(&rest_of_tag_block, "\x1b[0;1;36m");
                APPEND_LITERAL(rest_of_tag_block, ")");
                APPEND_COLOR_LITERAL(&rest_of_tag_block, "\x1b[m");
            } else {
                APPEND_COLOR_LITERAL(&rest_of_tag_block, "\x1b[m");
            }
            gcc_jump(rest_of_tag_block, NULL, done);
            gcc_rvalue_t *rval = gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value);
            gcc_case_t *case_ = gcc_new_case(env->ctx, rval, rval, tag_block);
            append(cases, case_);
        }
        gcc_block_t *default_block = gcc_new_block(func, fresh("default"));
        gcc_block_t *rest_of_default = default_block;
        if (any_values) {
            APPEND_COLOR_LITERAL(&rest_of_default, "\x1b[31;1m");
            APPEND_LITERAL(rest_of_default, "???");
            APPEND_COLOR_LITERAL(&rest_of_default, "\x1b[m");
        } else {
            // for each tag, if val&tag, print "+Tag", then print "+???" if anything left over
            gcc_block_t *continue_loop = gcc_new_block(func, fresh("find_tags")),
                        *done = gcc_new_block(func, fresh("done"));
            APPEND_LITERAL(continue_loop, "+");
            gcc_jump(continue_loop, NULL, rest_of_default);

            // gcc_jump(rest_of_default, NULL, continue_loop);
            // rest_of_default = continue_loop;
            for (int64_t i = 0; i < LENGTH(tagged->members); i++) {
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
                APPEND_LITERAL(has_tag, member.name);
                gcc_update(has_tag, NULL, tag_var, GCC_BINOP_BITWISE_AND, gcc_unary_op(env->ctx, NULL, GCC_UNOP_BITWISE_NEGATE, tag_gcc_t, member_tag));
                gcc_jump_condition(has_tag, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, tag, gcc_zero(env->ctx, tag_gcc_t)),
                                   continue_loop, done);
                rest_of_default = done_with_tag;
            }

            gcc_block_t *has_leftovers = gcc_new_block(func, fresh("has_leftovers"));
            gcc_jump_condition(rest_of_default, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_NE, tag, gcc_zero(env->ctx, tag_gcc_t)),
                               has_leftovers, done);
            APPEND_COLOR_LITERAL(&has_leftovers, "\x1b[31;1m");
            APPEND_LITERAL(has_leftovers, "???");
            APPEND_COLOR_LITERAL(&has_leftovers, "\x1b[m");
            gcc_jump(has_leftovers, NULL, done);
            rest_of_default = done;
        }
        gcc_jump(rest_of_default, NULL, done);

        gcc_switch(block, NULL, tag, default_block, LENGTH(cases), cases[0]);

        gcc_return(done, NULL, gcc_rval(cord));
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
        APPEND_COLOR_LITERAL(&nil_block, "\x1b[0;34;1m");
        APPEND_LITERAL(nil_block, heap_strf("!%s", type_to_string_concise(pointed_type)));
        APPEND_COLOR_LITERAL(&nil_block, "\x1b[m");
        gcc_return(nil_block, NULL, gcc_rval(cord));

        if (pointed_type->tag == CStringCharType) {
            gcc_return(nonnil_block, NULL, gcc_cast(env->ctx, NULL, obj, gcc_type(env->ctx, STRING)));
            break;
        }

        gcc_func_t *cord_sprintf_fn = get_function(env, "CORD_sprintf");

        const char *sigil = Match(t, PointerType)->is_stack ? "&" : "@";

        block = nonnil_block;
        if (pointed_type->tag == MemoryType) {
            APPEND_COLOR_LITERAL(&block, "\x1b[0;34;1m");
            gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, cord_sprintf_fn, gcc_lvalue_address(cord, NULL),
                                            gcc_str(env->ctx, "%r%sMemory<%p>"), gcc_rval(cord), gcc_str(env->ctx, sigil), obj));
            APPEND_COLOR_LITERAL(&block, "\x1b[m");
            gcc_return(block, NULL, gcc_rval(cord));
            block = NULL;
            break;
        }

        if (can_have_cycles(t)) {
            // If it's non-nil and the type can have cycles, check for cycles:
            // Summary of the approach:
            //     if (!cycle_checker) cycle_checker = &hashmap{...};
            //     index = *hashmap_set(cycle_checker, &obj, NULL)
            //     if (index == *cycle_checker->default_value) (i.e. uninitialized, i.e. this is the first time we've seen this)
            //         ++cycle_checker->default_value;
            //         ...proceed...
            //     else
            //         print("@#%d", index)

            gcc_block_t *needs_cycle_checker = gcc_new_block(func, fresh("needs_cycle_checker")),
                        *has_cycle_checker = gcc_new_block(func, fresh("has_cycle_checker"));

            gcc_jump_condition(block, NULL, gcc_comparison(env->ctx, NULL, GCC_COMPARISON_EQ, rec,
                                                           gcc_null(env->ctx, void_ptr_t)),
                               needs_cycle_checker, has_cycle_checker);

            // If the cycle checker is null, stack allocate one here and initialize it:
            block = needs_cycle_checker;
            sss_type_t *cycle_checker_t = Type(TableType, .key_type=Type(PointerType, .pointed=Type(MemoryType)), .value_type=Type(IntType, .bits=64));
            gcc_type_t *hashmap_gcc_t = sss_type_to_gcc(env, cycle_checker_t);
            gcc_func_t *func = gcc_block_func(block);
            gcc_lvalue_t *cycle_checker = gcc_local(func, NULL, hashmap_gcc_t, "_rec");
            gcc_assign(block, NULL, cycle_checker, gcc_struct_constructor(env->ctx, NULL, hashmap_gcc_t, 0, NULL, NULL));
            gcc_lvalue_t *next_index = gcc_local(func, NULL, gcc_type(env->ctx, INT64), "_index");
            gcc_assign(block, NULL, next_index, gcc_one(env->ctx, gcc_type(env->ctx, INT64)));
            gcc_assign(block, NULL, gcc_lvalue_access_field(
                    cycle_checker, NULL, gcc_get_field(gcc_type_if_struct(hashmap_gcc_t), TABLE_DEFAULT_FIELD)),
                gcc_lvalue_address(next_index, NULL));
            gcc_assign(block, NULL, gcc_param_as_lvalue(params[1]),
                       gcc_cast(env->ctx, NULL, gcc_lvalue_address(cycle_checker, NULL), gcc_type(env->ctx, VOID_PTR)));
            gcc_jump(block, NULL, has_cycle_checker);
            block = has_cycle_checker;

            gcc_type_t *i64 = gcc_type(env->ctx, INT64);
            gcc_func_t *hash_set_func = get_function(env, "sss_hashmap_set");
            gcc_func_t *hash_func = get_function(env, "hash_64bits");
            gcc_func_t *cmp_func = get_function(env, "compare_64bits");

            // val = Table_set(rec, &obj, NULL)
            gcc_block_t *noncycle_block = gcc_new_block(func, fresh("noncycle"));
            gcc_block_t *cycle_block = gcc_new_block(func, fresh("cycle"));
            sss_type_t *rec_t = Type(TableType, .key_type=Type(PointerType, .pointed=Type(MemoryType)), .value_type=Type(IntType, .bits=64));
            gcc_rvalue_t *index_ptr = gcc_callx(
                env->ctx, NULL, hash_set_func, rec,
                gcc_cast(env->ctx, NULL, gcc_get_func_address(hash_func, NULL), void_ptr_t),
                gcc_cast(env->ctx, NULL, gcc_get_func_address(cmp_func, NULL), void_ptr_t),
                gcc_rvalue_size(env->ctx, sizeof(struct{void* key; int64_t value;})), 
                gcc_cast(env->ctx, NULL, gcc_lvalue_address(gcc_param_as_lvalue(params[0]), NULL), void_ptr_t),
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
            APPEND_COLOR_LITERAL(&block, "\x1b[34;1m");
            gcc_eval(block, NULL, gcc_callx(env->ctx, NULL, cord_sprintf_fn, gcc_lvalue_address(cord, NULL),
                                            gcc_str(env->ctx, heap_strf("%r%s%%s#%%ld", sigil)),
                                            gcc_rval(cord),
                                            gcc_str(env->ctx, type_to_string_concise(pointed_type)),
                                            gcc_rval(gcc_deref(gcc_rval(index_var), NULL))));
            APPEND_COLOR_LITERAL(&block, "\x1b[m");
            gcc_return(block, NULL, gcc_rval(cord));

            // If this is a nonrecursive situation
            block = noncycle_block;
            gcc_update(block, NULL, rec_default, GCC_BINOP_PLUS, gcc_one(env->ctx, i64));
        }

        // Prepend "@"/"&"
        APPEND_COLOR_LITERAL(&block, "\x1b[0;34;1m");
        APPEND_LITERAL(block, sigil);
        APPEND_COLOR_LITERAL(&block, "\x1b[m");

        gcc_func_t *cord_fn = get_cord_func(env, pointed_type);
        APPEND_CORD(block, gcc_callx(
            env->ctx, NULL, cord_fn,
            gcc_rval(gcc_rvalue_dereference(obj, NULL)),
            rec, color));
        gcc_return(block, NULL, gcc_rval(cord));

        break;
    }
    case StructType: {
        auto struct_t = Match(t, StructType);

        gcc_type_t *gcc_t = sss_type_to_gcc(env, t);
        gcc_struct_t *gcc_struct = gcc_type_if_struct(gcc_t);

#define ADD_INT(a, b) gcc_binary_op(env->ctx, NULL, GCC_BINOP_PLUS, int_t, a, b)

        APPEND_COLOR_LITERAL(&block, "\x1b[m");
        APPEND_LITERAL(block, "{");
        
        size_t num_fields = gcc_field_count(gcc_struct);
        for (size_t i = 0; i < num_fields; i++) {
            if (i > 0) {
                APPEND_COLOR_LITERAL(&block, "\x1b[m");
                APPEND_LITERAL(block, ", ");
            }

            const char *name = struct_t->field_names ? ith(struct_t->field_names, i) : NULL;
            if (name && !streq(name, heap_strf("_%lu", i+1))) {
                APPEND_COLOR_LITERAL(&block, "\x1b[m");
                APPEND_LITERAL(block, name);
                APPEND_COLOR_LITERAL(&block, "\x1b[33m");
                APPEND_LITERAL(block, "=");
            }

            sss_type_t *member_t = ith(struct_t->field_types, i);
            gcc_func_t *cord_fn = get_cord_func(env, member_t);
            assert(cord_fn);
            gcc_field_t *field = gcc_get_field(gcc_struct, i);
            APPEND_CORD(block, gcc_callx(
                    env->ctx, NULL, cord_fn, 
                    gcc_rvalue_access_field(obj, NULL, field),
                    rec, color));
        }

        APPEND_COLOR_LITERAL(&block, "\x1b[m");
        APPEND_LITERAL(block, "}");

        const char* units = type_units(t);
        if (units && strlen(units) > 0) {
            APPEND_COLOR_LITERAL(&block, "\x1b[33;2m");
            APPEND_LITERAL(block, heap_strf("<%s>", units));
            APPEND_COLOR_LITERAL(&block, "\x1b[m");
        }

        gcc_return(block, NULL, gcc_rval(cord));
        break;
    }
#undef ADD_INT
    case ArrayType: {
        compile_array_cord_func(env, &block, obj, rec, color, t);
        break;
    }
    case TableType: {
        compile_table_cord_func(env, &block, obj, rec, color, t);
        break;
    }
    case FunctionType: {
        APPEND_COLOR_LITERAL(&block, "\x1b[36m");
        APPEND_LITERAL(block, type_to_string_concise(t));
        APPEND_COLOR_LITERAL(&block, "\x1b[m");
        gcc_return(block, NULL, gcc_rval(cord));
        break;
    }
    case VariantType: {
        auto variant = Match(t, VariantType);
        APPEND_COLOR_LITERAL(&block, "\x1b[36m");
        if (variant->variant_of->tag == StructType)
            APPEND_LITERAL(block, variant->name);
        else if (variant->variant_of->tag != TaggedUnionType)
            APPEND_LITERAL(block, heap_strf("%s::", variant->name));
        APPEND_COLOR_LITERAL(&block, "\x1b[m");
        gcc_func_t *cord_fn = get_cord_func(env, variant->variant_of);
        assert(cord_fn);
        APPEND_CORD(block, gcc_callx(
                env->ctx, NULL, cord_fn, 
                gcc_bitcast(env->ctx, NULL, obj, sss_type_to_gcc(env, variant->variant_of)),
                rec, color));
        gcc_return(block, NULL, gcc_rval(cord));
        break;
    }
    case TypeType: {
        APPEND_COLOR_LITERAL(&block, "\x1b[36m");
        APPEND_CORD(block, gcc_cast(env->ctx, NULL, obj, gcc_type(env->ctx, STRING)));
        APPEND_COLOR_LITERAL(&block, "\x1b[m");
        gcc_return(block, NULL, gcc_rval(cord));
        break;
    }
    case ModuleType: {
        APPEND_LITERAL(block, type_to_string(t));
        APPEND_COLOR_LITERAL(&block, "\x1b[m");
        gcc_return(block, NULL, gcc_rval(cord));
        break;
    }
    default: {
        fprintf(stderr, "\x1b[31;1mprint(%s) function is not yet implemented!\n", type_to_string(t));
        exit(1);
    }
    }
    return func;
#undef APPEND_LITERAL
#undef APPEND_COLOR_LITERAL
}

