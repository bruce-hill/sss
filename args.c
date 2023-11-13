// Common logic used in typechecking and compiling for parsing a list of
// arguments (or keyword arguments) into their appropriate bindings, given a
// specification

#include <stdint.h>

#include "args.h"
#include "ast.h"
#include "environment.h"
#include "builtins/array.h"
#include "builtins/table.h"
#include "util.h"

extern Type CString_type, Int_type;

// Return an in-evaluation-order list of relevant information about each argument
ARRAY_OF(arg_info_t) bind_arguments(env_t *env, ARRAY_OF(ast_t*) args, ARRAY_OF(const char*) arg_names, ARRAY_OF(sss_type_t*) arg_types, ARRAY_OF(ast_t*) arg_defaults)
{
    int64_t num_args = LENGTH(arg_types);
    arg_info_t *arg_infos = GC_MALLOC(num_args * sizeof(arg_info_t));
    int64_t next_arg = 0;

    static Type c_str_to_int_type = {
        .name="{CString=>Int}", .size=sizeof(table_t), .align=sizeof(table_t),
        .tag=TableInfo, .TableInfo={&CString_type, &Int_type, sizeof(char*) + sizeof(int64_t), sizeof(char*)}
    };
    table_t arg_positions = {};
    if (arg_names) {
        for (int64_t i = 0; i < LENGTH(arg_names); i++) {
            const char *name = ith(arg_names, i);
            if (name)
                Table_set(&arg_positions, &name, &i, &c_str_to_int_type);
        }
    }

    table_t kwargs = {}; // name -> ast_t*
    for (int64_t i = 0; i < LENGTH(args); i++) {
        ast_t *arg = ith(args, i);
        if (arg->tag == KeywordArg && Match(arg, KeywordArg)->name)
            Table_str_set(&kwargs, Match(arg, KeywordArg)->name, arg);
    }

    arg_info_t *populated[num_args] = {};
    int64_t next_spec_arg = 0;
    for (int64_t i = 0; i < LENGTH(args); i++) {
        ast_t *arg = ith(args, i);
        arg_info_t *info = &arg_infos[next_arg++];
        if (arg->tag == KeywordArg && Match(arg, KeywordArg)->name) {
            const char *name = Match(arg, KeywordArg)->name;
            int64_t *pos = Table_get(&arg_positions, &name, &c_str_to_int_type);
            if (!pos)
                compiler_err(env, arg, "'%s' is not a valid argument name", name);
            info->position = *pos;
        } else {
            if (arg->tag == KeywordArg) arg = Match(arg, KeywordArg)->arg;
            while (Table_str_get(&kwargs, ith(arg_names, next_spec_arg))) {
                if (next_spec_arg > num_args)
                    compiler_err(env, arg, "Too many arguments provided");
                ++next_spec_arg;
            }
            info->position = next_spec_arg++;
        }
        info->ast = arg;
        info->name = ith(arg_names, info->position);
        info->type = ith(arg_types, info->position);
        populated[info->position] = info;
    }

    // Default arguments:
    for (int64_t i = 0; i < num_args; i++) {
        if (populated[i])
            continue;
        arg_info_t *info = &arg_infos[next_arg++];
        ast_t *def = arg_defaults ? ith(arg_defaults, i) : NULL;
        info->ast = def;
        info->position = i;
        info->name = ith(arg_names, i);
        info->type = ith(arg_types, i);
        info->is_default = true;
    }

    auto arg_info_array = (ARRAY_OF(arg_info_t))new(array_t, .data=arg_infos, .length=num_args, .stride=sizeof(arg_info_t));
    return arg_info_array;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
