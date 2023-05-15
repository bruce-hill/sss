// Compilation logic for expressions
#include <assert.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>

#include "../ast.h"
#include "../parse.h"
#include "../span.h"
#include "../typecheck.h"
#include "../types.h"
#include "../units.h"
#include "../util.h"
#include "../SipHash/halfsiphash.h"
#include "compile.h"
#include "libgccjit_abbrev.h"

match_outcomes_t perform_conditional_match(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *val, ast_t *pattern)
{
    gcc_loc_t *loc = ast_loc(env, pattern);
    gcc_func_t *func = gcc_block_func(*block);

    match_outcomes_t outcomes = {
        .match_block = gcc_new_block(func, "match_success"),
        .no_match_block = gcc_new_block(func, "match_failure"),
        .match_env = fresh_scope(env),
    };

    switch (pattern->tag) {
    case Var: {
        const char *name = Match(pattern, Var)->name;
        if (t->tag == TaggedUnionType) {
            auto tu_t = Match(t, TaggedUnionType);
            foreach (tu_t->members, member, _) {
                if (streq(member->name, name)) {
                    gcc_struct_t *tagged_struct = gcc_type_if_struct(sss_type_to_gcc(env, t));
                    gcc_rvalue_t *is_match = gcc_comparison(
                        env->ctx, NULL, GCC_COMPARISON_EQ,
                        gcc_rvalue_access_field(val, loc, gcc_get_field(tagged_struct, 0)),
                        gcc_rvalue_from_long(env->ctx, get_tag_type(env, t), member->tag_value));
                    gcc_jump_condition(*block, loc, is_match, outcomes.match_block, outcomes.no_match_block);
                    *block = NULL;
                    return outcomes;
                }
            }
        }

        binding_t *b = get_binding(env, name);
        if (!b) {
            hset(outcomes.match_env->bindings, name, new(binding_t, .type=t, .rval=val));
            gcc_jump(*block, loc, outcomes.match_block);
            *block = NULL;
            return outcomes;
        }
        goto compare_values;
    }
    case HeapAllocate: {
        if (t->tag != PointerType)
            compiler_err(env, pattern, "This is a pointer pattern, but you're attempting to match it against a non-pointer value with type %s",
                         type_to_string(t));

        if (Match(t, PointerType)->is_optional) {
            gcc_rvalue_t *is_nonnull = gcc_comparison(
                env->ctx, NULL, GCC_COMPARISON_NE, val, gcc_null(env->ctx, sss_type_to_gcc(env, t)));

            gcc_jump_condition(*block, loc, is_nonnull, outcomes.match_block, outcomes.no_match_block);
            *block = NULL;
        } else {
            gcc_jump(*block, loc, outcomes.match_block);
        }
        gcc_rvalue_t *data_val = gcc_rval(gcc_rvalue_dereference(val, loc));
        ast_t *pointed = Match(pattern, HeapAllocate)->value;
        auto submatch_outcomes = perform_conditional_match(env, &outcomes.match_block, Match(t, PointerType)->pointed, data_val, pointed);
        gcc_jump(submatch_outcomes.no_match_block, loc, outcomes.no_match_block);
        return (match_outcomes_t){
            submatch_outcomes.match_block,
            outcomes.no_match_block,
            submatch_outcomes.match_env,
        };
    }
    case Struct: {
        auto pat_struct = Match(pattern, Struct);
        if (t->tag != StructType) {
            compiler_err(env, pattern, "This is a struct pattern, but you're attempting to match it against a non-struct value with type %s",
                         type_to_string(t));
        } else if (pat_struct->type) {
            sss_type_t *pat_t = get_type(env, pat_struct->type);
            if (!type_eq(t, pat_t))
                compiler_err(env, pattern, "This pattern is a %s, but you're attempting to match it against a value with type %s",
                             type_to_string(pat_t), type_to_string(t));
        } else if (Match(t, StructType)->name) {
            compiler_err(env, pattern, "This pattern is a nameless tuple, but you're attempting to match it against a value with type %s",
                         type_to_string(t));
        } else if (!streq(Match(t, StructType)->units, pat_struct->units)) {
            compiler_err(env, pattern, "The units of this pattern: <%s> don't match the units of the value being matched: <%s>",
                         Match(t, StructType)->units, pat_struct->units);
        }

        auto struct_info = Match(t, StructType);
        gcc_struct_t *gcc_struct = gcc_type_if_struct(sss_type_to_gcc(env, t));

        gcc_jump(*block, loc, outcomes.match_block);
        *block = NULL;

        sss_hashmap_t checked = {0};
        for (int64_t i = 0; i < LIST_LEN(pat_struct->members); i++) {
            ast_t *field_ast = ith(pat_struct->members, i);
            const char *name = Match(field_ast, StructField)->name;
            if (hget(&checked, name, ast_t*))
                compiler_err(env, field_ast, "This struct member is a duplicate of an earlier member.");

            ast_t *pat_member = Match(field_ast, StructField)->value;
            hset(&checked, name, pat_member);
            for (int64_t j = 0; j < LIST_LEN(struct_info->field_names); j++) {
                if (!streq(ith(struct_info->field_names, j), name)) continue;
                gcc_rvalue_t *member_val = gcc_rvalue_access_field(val, loc, gcc_get_field(gcc_struct, j));
                auto submatch_outcomes = perform_conditional_match(outcomes.match_env, &outcomes.match_block, ith(struct_info->field_types, j), member_val, pat_member);
                outcomes.match_block = submatch_outcomes.match_block;
                outcomes.match_env = submatch_outcomes.match_env;
                gcc_jump(submatch_outcomes.no_match_block, loc, outcomes.no_match_block);
                goto found_field_name;
            }
            compiler_err(env, field_ast, "There is no field called '%s' on the struct %s", name, type_to_string(t));

          found_field_name: continue;
        }
        return outcomes;
    }
    case FunctionCall: {
        auto call = Match(pattern, FunctionCall);
        if (call->fn->tag != Var)
            goto compare_values;

        const char *fn_name = Match(call->fn, Var)->name;
        if (t->tag != TaggedUnionType)
            goto compare_values;

        // Tagged Union Constructor:
        auto tu_t = Match(t, TaggedUnionType);
        int64_t tag_index = -1;
        for (int64_t i = 0; i < LIST_LEN(tu_t->members); i++) {
            if (streq(ith(tu_t->members, i).name, fn_name)) {
                tag_index = i;
                goto found_tag;
            }
        }
        // Failed to find the tag, this is just a function call:
        goto compare_values;

      found_tag:;
        auto member = ith(tu_t->members, tag_index);
        if (!member.type)
            compiler_err(env, pattern, "This tagged union member doesn't have any value");
        else if (LIST_LEN(call->args) != 1)
            compiler_err(env, pattern, "This tagged union constructor needs to have exactly 1 argument");

        gcc_struct_t *tagged_struct = gcc_type_if_struct(sss_type_to_gcc(env, t));

        gcc_type_t *tag_gcc_t = get_tag_type(env, t);
        gcc_type_t *union_gcc_t = get_union_type(env, t);
        gcc_field_t *tag_field = gcc_get_field(tagged_struct, 0);
        gcc_field_t *data_field = gcc_get_field(tagged_struct, 1);

        gcc_rvalue_t *is_match = gcc_comparison(
            env->ctx, NULL, GCC_COMPARISON_EQ,
            gcc_rvalue_access_field(val, loc, tag_field),
            gcc_rvalue_from_long(env->ctx, tag_gcc_t, member.tag_value));

        gcc_jump_condition(*block, loc, is_match, outcomes.match_block, outcomes.no_match_block);
        *block = NULL;

        gcc_rvalue_t *data_val = gcc_rvalue_access_field(
            gcc_rvalue_access_field(val, loc, data_field), loc, gcc_get_union_field(union_gcc_t, tag_index));
        auto submatch_outcomes = perform_conditional_match(env, &outcomes.match_block, member.type, data_val, ith(call->args, 0));
        gcc_jump(submatch_outcomes.no_match_block, loc, outcomes.no_match_block);
        return (match_outcomes_t){
            submatch_outcomes.match_block,
            outcomes.no_match_block,
            submatch_outcomes.match_env,
        };
    }
    default: {
      compare_values:;
        sss_type_t *pattern_t = get_type(env, pattern);
        gcc_rvalue_t *pattern_val = compile_expr(env, block, pattern);
        if (!promote(env, pattern_t, &pattern_val, t))
            compiler_err(env, pattern, "This pattern has type %s, but you're attempting to match it against a value with type %s",
                         type_to_string(pattern_t), type_to_string(t));
        gcc_rvalue_t *is_match = gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, compare_values(env, t, val, pattern_val),
                                  gcc_zero(env->ctx, gcc_type(env->ctx, INT)));
        gcc_jump_condition(*block, loc, is_match, outcomes.match_block, outcomes.no_match_block);
        *block = NULL;
        return outcomes;
    }
    }
}

const char *get_missing_pattern(env_t *env, sss_type_t *t, List(ast_t*) patterns)
{
    // Wildcard pattern:
    if (t->tag != TaggedUnionType) {
        foreach (patterns, pat, _) {
            if ((*pat)->tag == Var && !get_binding(env, Match(*pat, Var)->name))
                return NULL;
        }
    }

    if (t->tag == TaggedUnionType) {
        sss_hashmap_t member_handlers = {0};
        auto members = Match(t, TaggedUnionType)->members;
        for (int64_t i = 0; i < LIST_LEN(members); i++) {
            auto member = ith(members, i);
            NEW_LIST(ast_t*, list);
            hset(&member_handlers, member.name, list);
        }

        // Wildcard pattern (but not counting tagged union field names)
        foreach (patterns, pat, _) {
            if ((*pat)->tag != Var) continue;
            const char *name = Match(*pat, Var)->name;
            if (!get_binding(env, name) && !hget(&member_handlers, name, List(ast_t*)))
                return NULL;
        }

        foreach (patterns, pat, _) {
            if ((*pat)->tag == FunctionCall) {
                ast_t *fn = Match((*pat), FunctionCall)->fn;
                if (fn->tag == Var) {
                    const char *name = Match(fn, Var)->name;
                    List(ast_t*) handlers = hget(&member_handlers, name, List(ast_t*));
                    if (handlers) {
                        auto args = Match(*pat, FunctionCall)->args;
                        if (LIST_LEN(args) != 1)
                            compiler_err(env, *pat, "This constructor expected exactly one argument");
                        append(handlers, ith(args, 0));
                    }
                }
            } else if ((*pat)->tag == Var) {
                const char *name = Match(*pat, Var)->name;
                List(ast_t*) handlers = hget(&member_handlers, name, List(ast_t*));
                if (handlers)
                    append(handlers, *pat);
            }
        }

        const char *unhandled = NULL;
        for (int64_t i = 0; i < LIST_LEN(members); i++) {
            auto member = ith(members, i);
            List(ast_t*) handlers = hget(&member_handlers, member.name, List(ast_t*));
            if (LIST_LEN(handlers) == 0) {
                if (unhandled)
                    unhandled = heap_strf("%s, nor is %s.%s",
                                          unhandled, type_to_string(t), member.name);
                else
                    unhandled = heap_strf("The tagged union member %s.%s is not handled",
                                          type_to_string(t), member.name);
            } else if (member.type) {
                const char *missing = get_missing_pattern(env, member.type, handlers);
                if (!missing) continue;
                if (unhandled)
                    unhandled = heap_strf("%s, also for %s.%s(...): %s",
                                          unhandled, type_to_string(t), member.name, missing);
                else
                    unhandled = heap_strf("Among the patterns for %s.%s(...): %s",
                                          type_to_string(t), member.name, missing);
            }
        }
        return unhandled;
    } else if (t->tag == PointerType) {
        auto ptr = Match(t, PointerType);
        if (ptr->is_optional) {
            bool handled_null = false;
            foreach (patterns, pat, _) {
                if ((*pat)->tag == Nil
                    || ((*pat)->tag == Var && !get_binding(env, Match(*pat, Var)->name))) {
                    handled_null = true;
                    break;
                }
            }
            if (!handled_null) return "The null value is not handled";
        }

        NEW_LIST(ast_t*, value_handlers);
        foreach (patterns, pat, _) {
            if ((*pat)->tag == HeapAllocate)
                append(value_handlers, Match(*pat, HeapAllocate)->value);
            else if ((*pat)->tag == StackReference)
                append(value_handlers, Match(*pat, StackReference)->value);
        }
        return get_missing_pattern(env, ptr->pointed, value_handlers);
    } else if (t->tag == BoolType) {
        bool cases_handled[2] = {false, false};
        foreach (patterns, pat, _) {
            if ((*pat)->tag != Bool) continue;
            cases_handled[(int)Match(*pat, Bool)->b] = true;
        }
        if (!cases_handled[0])
            return "'no' is not handled";
        else if (!cases_handled[1])
            return "'yes' is not handled";
    } else if (t->tag == StructType) {
        // TODO: allow exhaustive struct matches, e.g. {yes}|{no}
    }
    return "I can't prove that every case in this 'when' block is handled by an 'is' clause. Please add a wildcard clause like: 'is _ then ...'";
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
