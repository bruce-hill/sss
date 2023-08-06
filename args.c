// Common logic used in typechecking and compiling for parsing a list of
// arguments (or keyword arguments) into their appropriate bindings, given a
// specification

#include <stdint.h>

#include "args.h"
#include "ast.h"
#include "environment.h"
#include "libsss/hashmap.h"
#include "libsss/list.h"
#include "util.h"

// Return an in-evaluation-order list of relevant information about each argument
List(arg_info_t) bind_arguments(env_t *env, List(ast_t*) args, List(const char*) arg_names, List(sss_type_t*) arg_types, List(ast_t*) arg_defaults)
{
    int64_t num_args = LIST_LEN(arg_types);
    arg_info_t arg_infos[num_args] = {};
    int64_t next_arg = 0;

    sss_hashmap_t arg_positions = {}; // name -> int64_t
    if (arg_names) {
        for (int64_t i = 0; i < LIST_LEN(arg_names); i++) {
            if (LIST_ITEM(arg_names, i))
                hset(&arg_positions, LIST_ITEM(arg_names, i), i);
        }
    }

    sss_hashmap_t kwargs = {}; // name -> ast_t*
    for (int64_t i = 0; i < LIST_LEN(args); i++) {
        ast_t *arg = LIST_ITEM(args, i);
        if (arg->tag == KeywordArg && Match(arg, KeywordArg)->name)
            hset(&kwargs, Match(arg, KeywordArg)->name, arg);
    }

    sss_hashmap_t populated = {}; // int64_t -> arg_info_t*
    int64_t next_spec_arg = 0;
    for (int64_t i = 0; i < LIST_LEN(args); i++) {
        ast_t *arg = LIST_ITEM(args, i);
        arg_info_t *info = &arg_infos[next_arg++];
        if (arg->tag == KeywordArg && Match(arg, KeywordArg)->name) {
            const char *name = Match(arg, KeywordArg)->name;
            int64_t *pos = hget_opt(&arg_positions, name, int64_t);
            if (!pos)
                compiler_err(env, arg, "'%s' is not a valid argument name", name);
            info->position = *pos;
        } else {
            if (arg->tag == KeywordArg) arg = Match(arg, KeywordArg)->arg;
            while (hget(&kwargs, LIST_ITEM(arg_names, next_spec_arg), ast_t*)) {
                if (next_spec_arg > num_args)
                    compiler_err(env, arg, "Too many arguments provided");
                ++next_spec_arg;
            }
            info->position = next_spec_arg++;
        }
        info->ast = arg;
        info->name = LIST_ITEM(arg_names, info->position);
        info->type = LIST_ITEM(arg_types, info->position);
        hset(&populated, info->position, info);
    }

    // Default arguments:
    for (int64_t i = 0; i < num_args; i++) {
        if (hget(&populated, i, arg_info_t*))
            continue;
        arg_info_t *info = &arg_infos[next_arg++];
        ast_t *def = arg_defaults ? LIST_ITEM(arg_defaults, i) : NULL;
        info->ast = def;
        info->position = i;
        info->name = LIST_ITEM(arg_names, i);
        info->type = LIST_ITEM(arg_types, i);
        info->is_default = true;
    }

    return (List(arg_info_t))list_new_items(sizeof(arg_info_t), num_args, arg_infos);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
