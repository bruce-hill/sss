// Compilation logic for expressions
#include <assert.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>

#include "../args.h"
#include "../ast.h"
#include "../parse.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"
#include "../SipHash/halfsiphash.h"
#include "compile.h"
#include "libgccjit_abbrev.h"

match_outcomes_t perform_conditional_match(env_t *env, gcc_block_t **block, sss_type_t *t, gcc_rvalue_t *val, ast_t *pattern)
{
    gcc_loc_t *loc = ast_loc(env, pattern);
    gcc_func_t *func = gcc_block_func(*block);

    match_outcomes_t outcomes = {
        .match_block = gcc_new_block(func, fresh("match_success")),
        .no_match_block = gcc_new_block(func, fresh("match_failure")),
        .match_env = fresh_scope(env),
    };

    switch (pattern->tag) {
    case Wildcard: {
        const char *name = Match(pattern, Wildcard)->name;
        if (name)
            Table_str_set(outcomes.match_env->bindings, name, new(binding_t, .type=t, .rval=val));
        gcc_jump(*block, loc, outcomes.match_block);
        *block = NULL;
        return outcomes;
    }
    case Var: {
        const char *name = Match(pattern, Var)->name;
        if (base_variant(t)->tag == TaggedUnionType) {
            auto tu_t = Match(base_variant(t), TaggedUnionType);
            foreach (tu_t->members, member, _) {
                if (streq(member->name, name)) {
                    gcc_struct_t *tagged_struct = gcc_type_as_struct(sss_type_to_gcc(env, t));
                    gcc_rvalue_t *is_match = gcc_comparison(
                        env->ctx, NULL, GCC_COMPARISON_EQ,
                        gcc_rvalue_access_field(val, loc, gcc_get_field(tagged_struct, 0)),
                        gcc_rvalue_from_long(env->ctx, gcc_type(env->ctx, INT32), member->tag_value));
                    gcc_jump_condition(*block, loc, is_match, outcomes.match_block, outcomes.no_match_block);
                    *block = NULL;
                    return outcomes;
                }
            }
        }
        goto compare_values;
    }
    case HeapAllocate: case StackReference: {
        if (t->tag != PointerType)
            compiler_err(env, pattern, "This is a pointer pattern, but you're attempting to match it against a non-pointer value with type %T", t);

        if (pattern->tag == HeapAllocate && Match(t, PointerType)->is_stack)
            compiler_err(env, pattern, "This is a pointer to heap memory, but you're attempting to match it against a stack pointer. Use '&' instead.");
        else if (pattern->tag == StackReference && !Match(t, PointerType)->is_stack)
            compiler_err(env, pattern, "This is a pointer to stack memory, but you're attempting to match it against a heap pointer. Use '@' instead.");

        if (Match(t, PointerType)->is_optional) {
            gcc_rvalue_t *is_nonnull = gcc_comparison(
                env->ctx, NULL, GCC_COMPARISON_NE, val, gcc_null(env->ctx, sss_type_to_gcc(env, t)));

            gcc_jump_condition(*block, loc, is_nonnull, outcomes.match_block, outcomes.no_match_block);
        } else {
            gcc_jump(*block, loc, outcomes.match_block);
        }
        *block = NULL;
        gcc_rvalue_t *data_val = gcc_rval(gcc_rvalue_dereference(val, loc));
        ast_t *pointed = pattern->tag == HeapAllocate ? Match(pattern, HeapAllocate)->value : Match(pattern, StackReference)->value;
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
        if (base_variant(t)->tag != StructType) {
            compiler_err(env, pattern, "This is a struct pattern, but you're attempting to match it against a non-struct value with type %T", t);
        } else if (pat_struct->type) {
            sss_type_t *pat_t = parse_type_ast(env, pat_struct->type);
            if (!type_eq(t, pat_t))
                compiler_err(env, pattern, "This pattern is a %T, but you're attempting to match it against a value with type %T", pat_t, t);
        }

        auto struct_info = Match(base_variant(t), StructType);
        gcc_struct_t *gcc_struct = gcc_type_as_struct(sss_type_to_gcc(env, t));

        gcc_jump(*block, loc, outcomes.match_block);
        *block = NULL;

        auto arg_infos = bind_arguments(
            env, pat_struct->members, struct_info->field_names, struct_info->field_types, struct_info->field_defaults);

        foreach (arg_infos, arg_info, _) {
            ast_t *arg = arg_info->ast;
            if (!arg) continue;
            if (arg->tag == KeywordArg) arg = Match(arg, KeywordArg)->arg;
            gcc_rvalue_t *member_val = gcc_rvalue_access_field(val, loc, gcc_get_field(gcc_struct, arg_info->position));
            auto submatch_outcomes = perform_conditional_match(outcomes.match_env, &outcomes.match_block, arg_info->type, member_val, arg);
            outcomes.match_block = submatch_outcomes.match_block;
            outcomes.match_env = submatch_outcomes.match_env;
            gcc_jump(submatch_outcomes.no_match_block, loc, outcomes.no_match_block);
        }
        return outcomes;
    }
    case FunctionCall: {
        auto call = Match(pattern, FunctionCall);
        if (call->fn->tag != Var)
            goto compare_values;

        const char *fn_name = Match(call->fn, Var)->name;
        sss_type_t *base_t = base_variant(t);
        if (base_t->tag != TaggedUnionType)
            goto compare_values;

        // Tagged Union Constructor:
        auto tu_t = Match(base_t, TaggedUnionType);
        int64_t tag_index = -1;
        for (int64_t i = 0; i < LENGTH(tu_t->members); i++) {
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

        gcc_struct_t *tagged_struct = gcc_type_as_struct(sss_type_to_gcc(env, base_t));

        gcc_type_t *tag_gcc_t = gcc_type(env->ctx, INT32);
        gcc_type_t *union_gcc_t = get_union_type(env, base_t);
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
        ast_t *m_pat = WrapAST(pattern, Struct, .members=call->args);
        auto submatch_outcomes = perform_conditional_match(env, &outcomes.match_block, member.type, data_val, m_pat);
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
            compiler_err(env, pattern, "This pattern has type %T, but you're attempting to match it against a value with type %T", pattern_t, t);
        gcc_rvalue_t *is_match;
        if (is_numeric(t) || t->tag == BoolType || t->tag == CharType || t->tag == PointerType) {
            is_match = gcc_comparison(env->ctx, loc, GCC_COMPARISON_EQ, val, pattern_val);
        } else {
            gcc_lvalue_t *val_var = gcc_local(func, loc, sss_type_to_gcc(env, t), "value");
            gcc_assign(*block, loc, val_var, val);
            gcc_lvalue_t *pat_var = gcc_local(func, loc, sss_type_to_gcc(env, t), "pattern");
            gcc_assign(*block, loc, pat_var, pattern_val);

            is_match = gcc_callx(env->ctx, loc, get_function(env, "generic_equal"),
                                 gcc_cast(env->ctx, loc, gcc_lvalue_address(val_var, loc), gcc_type(env->ctx, VOID_PTR)),
                                 gcc_cast(env->ctx, loc, gcc_lvalue_address(pat_var, loc), gcc_type(env->ctx, VOID_PTR)),
                                 get_typeinfo_pointer(env, t));
        }

        gcc_jump_condition(*block, loc, is_match, outcomes.match_block, outcomes.no_match_block);
        *block = NULL;
        return outcomes;
    }
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
