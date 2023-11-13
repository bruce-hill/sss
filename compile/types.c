// Compilation logic for Arrays in this file
#include <assert.h>
#include <gc/cord.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
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

gcc_lvalue_t *get_type_lvalue(env_t *env, sss_type_t *t)
{
    const char *key = type_to_string_concise(t);
    gcc_lvalue_t *lval = Table_str_get(&env->global->type_lvals, key);
    if (lval) return lval;

    binding_t *b = get_binding(env, key);
    if (b && b->type->tag == TypeType)
        return b->lval;
    else if (b)
        compiler_err(env, NULL, "WTF: %s : %T", key, b->type);

    switch (t->tag) {
        case ArrayType: {
            sss_type_t *item_type = Match(t, ArrayType)->item_type;
            gcc_type_t *type_gcc_t = sss_type_to_gcc(env, Type(TypeType));
            gcc_field_t *fields[] = {
                gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, STRING), "name"),
                gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, SIZE), "size"),
                gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, SIZE), "align"),
                gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT32), "tag"),
                gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(type_gcc_t), "item_type"),
            };
            gcc_struct_t *gcc_struct = gcc_new_struct_type(env->ctx, NULL, "ArrayType", sizeof(fields)/sizeof(fields[0]), fields);

            gcc_rvalue_t *rvalues[] = {
                gcc_str(env->ctx, key),
                gcc_rvalue_size(env->ctx, sizeof(array_t)),
                gcc_rvalue_size(env->ctx, alignof(array_t)),
                gcc_rvalue_int32(env->ctx, ArrayInfo),
                get_type_pointer(env, item_type),
            };

            gcc_lvalue_t *global = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, gcc_struct_as_type(gcc_struct), fresh("ArrayType"));
            gcc_global_set_initializer_rvalue(
                global,
                gcc_struct_constructor(env->ctx, NULL, gcc_struct_as_type(gcc_struct), sizeof(fields)/sizeof(fields[0]), fields, rvalues));

            lval = gcc_rvalue_dereference(gcc_cast(env->ctx, NULL, gcc_lvalue_address(global, NULL), gcc_get_ptr_type(type_gcc_t)), NULL);
            break;
        }
        case TableType: {
            sss_type_t *key_type = Match(t, TableType)->key_type;
            sss_type_t *value_type = Match(t, TableType)->value_type;
            gcc_type_t *type_gcc_t = sss_type_to_gcc(env, Type(TypeType));
            gcc_field_t *fields[] = {
                gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, STRING), "name"),
                gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, SIZE), "size"),
                gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, SIZE), "align"),
                gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT32), "tag"),
                gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(type_gcc_t), "key_type"),
                gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(type_gcc_t), "value_type"),
                gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, SIZE), "entry_size"),
                gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, SIZE), "value_offset"),
            };
            gcc_struct_t *gcc_struct = gcc_new_struct_type(env->ctx, NULL, "TableType", sizeof(fields)/sizeof(fields[0]), fields);

            gcc_rvalue_t *rvalues[] = {
                gcc_str(env->ctx, key),
                gcc_rvalue_size(env->ctx, sizeof(table_t)),
                gcc_rvalue_size(env->ctx, alignof(table_t)),
                gcc_rvalue_int32(env->ctx, TableInfo),
                get_type_pointer(env, key_type),
                get_type_pointer(env, value_type),
                gcc_rvalue_size(env->ctx, gcc_sizeof(env, table_entry_type(t))),
                table_entry_value_offset(env, t),
            };

            gcc_lvalue_t *global = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, gcc_struct_as_type(gcc_struct), fresh("TableType"));
            gcc_global_set_initializer_rvalue(
                global,
                gcc_struct_constructor(env->ctx, NULL, gcc_struct_as_type(gcc_struct), sizeof(fields)/sizeof(fields[0]), fields, rvalues));

            lval = gcc_rvalue_dereference(gcc_cast(env->ctx, NULL, gcc_lvalue_address(global, NULL), gcc_get_ptr_type(type_gcc_t)), NULL);
            break;
        }
        case FunctionType:
        default: {
            compiler_err(env, NULL, "Type lvalue not implemented for: %T", t);
        }
    }

    Table_str_set(&env->global->type_lvals, key, lval);
    return lval;
}

gcc_rvalue_t *get_type_pointer(env_t *env, sss_type_t *t)
{
    return gcc_lvalue_address(get_type_lvalue(env, t), NULL);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
