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
    if (b && b->lval && b->type->tag == TypeType)
        return b->lval;
    else if (b)
        compiler_err(env, NULL, "Missing lvalue or wrong type: %s : %T", key, b->type);

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
    case PointerType: {
        auto ptr = Match(t, PointerType);
        gcc_type_t *type_gcc_t = sss_type_to_gcc(env, Type(TypeType));
        gcc_field_t *fields[] = {
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, STRING), "name"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, SIZE), "size"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, SIZE), "align"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT32), "tag"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, STRING), "sigil"),
            gcc_new_field(env->ctx, NULL, gcc_get_ptr_type(type_gcc_t), "pointed"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, BOOL), "cyclic"),
        };
        gcc_struct_t *gcc_struct = gcc_new_struct_type(env->ctx, NULL, "TableType", sizeof(fields)/sizeof(fields[0]), fields);

        CORD sigil = ptr->is_stack ? (ptr->is_optional ? "&(optional)" : "&") : (ptr->is_optional ? "?" : "@");
        if (ptr->is_readonly) sigil = CORD_cat(sigil, "(read-only)");

        gcc_rvalue_t *rvalues[] = {
            gcc_str(env->ctx, key),
            gcc_rvalue_size(env->ctx, sizeof(table_t)),
            gcc_rvalue_size(env->ctx, alignof(table_t)),
            gcc_rvalue_int32(env->ctx, PointerInfo),
            gcc_str(env->ctx, CORD_to_const_char_star(sigil)),
            get_type_pointer(env, ptr->pointed),
            gcc_rvalue_bool(env->ctx, can_have_cycles(t)),
        };

        gcc_lvalue_t *global = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, gcc_struct_as_type(gcc_struct), fresh("PointerType"));
        gcc_global_set_initializer_rvalue(
            global,
            gcc_struct_constructor(env->ctx, NULL, gcc_struct_as_type(gcc_struct), sizeof(fields)/sizeof(fields[0]), fields, rvalues));

        lval = gcc_rvalue_dereference(gcc_cast(env->ctx, NULL, gcc_lvalue_address(global, NULL), gcc_get_ptr_type(type_gcc_t)), NULL);
        break;
    }
    case StructType: {
        auto struct_info = Match(t, StructType);
        int64_t num_fields = LENGTH(struct_info->field_types);

        gcc_type_t *type_gcc_t = sss_type_to_gcc(env, Type(TypeType));
        sss_type_t *member_t = Type(
            StructType, .field_names=ARRAY((const char*)"name", "type"),
            .field_types=ARRAY(get_type_by_name(env, "CString"), Type(PointerType, .pointed=Type(TypeType))));
        sss_type_t *members_t = Type(ArrayType, member_t);
        gcc_type_t *members_gcc_t = sss_type_to_gcc(env, members_t);
        gcc_field_t *fields[] = {
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, STRING), "name"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, SIZE), "size"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, SIZE), "align"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, INT32), "tag"),
            gcc_new_field(env->ctx, NULL, members_gcc_t, "members"),
            gcc_new_field(env->ctx, NULL, gcc_type(env->ctx, BOOL), "is_pure_data"),
        };
        gcc_struct_t *gcc_struct = gcc_new_struct_type(env->ctx, NULL, "StructType", sizeof(fields)/sizeof(fields[0]), fields);

        gcc_type_t *member_gcc_t = sss_type_to_gcc(env, member_t);

        gcc_rvalue_t *member_rvals[num_fields] = {};
        for (int64_t i = 0; i < num_fields; i++) {
            const char *name = ith(struct_info->field_names, i);
            if (!name) name = heap_strf("_%d", i+1);
            member_rvals[i] = gcc_struct_constructor(
                env->ctx, NULL, member_gcc_t, 2, (gcc_field_t*[]){
                    gcc_get_field(gcc_type_if_struct(member_gcc_t), 0),
                    gcc_get_field(gcc_type_if_struct(member_gcc_t), 1),
                }, (gcc_rvalue_t*[]){
                    gcc_str(env->ctx, name),
                    get_type_pointer(env, ith(struct_info->field_types, i)),
                });
        }

        gcc_type_t *member_data_gcc_t = gcc_jit_context_new_array_type(env->ctx, NULL, member_gcc_t, num_fields);
        gcc_rvalue_t *member_data = gcc_jit_context_new_array_constructor(
            env->ctx, NULL, member_data_gcc_t, num_fields, member_rvals);

        gcc_lvalue_t *fuckass = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, member_data_gcc_t, fresh("StructFields"));
        gcc_global_set_initializer_rvalue(fuckass, member_data);
        gcc_rvalue_t *members = gcc_struct_constructor(
            env->ctx, NULL, members_gcc_t, 3, (gcc_field_t*[]){
                gcc_get_field(gcc_type_if_struct(members_gcc_t), ARRAY_DATA_FIELD),
                gcc_get_field(gcc_type_if_struct(members_gcc_t), ARRAY_LENGTH_FIELD),
                gcc_get_field(gcc_type_if_struct(members_gcc_t), ARRAY_STRIDE_FIELD),
            }, (gcc_rvalue_t*[]){
                gcc_cast(env->ctx, NULL, gcc_lvalue_address(fuckass, NULL), gcc_get_ptr_type(member_gcc_t)),
                gcc_rvalue_int64(env->ctx, num_fields),
                gcc_rvalue_int16(env->ctx, sizeof(void*) + sizeof(void*)),
            });

        gcc_rvalue_t *rvalues[] = {
            gcc_str(env->ctx, key),
            gcc_rvalue_size(env->ctx, sizeof(table_t)),
            gcc_rvalue_size(env->ctx, alignof(table_t)),
            gcc_rvalue_int32(env->ctx, StructInfo),
            members,
            gcc_rvalue_bool(env->ctx, false), // TODO: determine if it is pure memory or not
        };

        gcc_lvalue_t *global = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, gcc_struct_as_type(gcc_struct), fresh("StructType"));
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

    assert(lval);
    Table_str_set(&env->global->type_lvals, key, lval);
    return lval;
}

gcc_rvalue_t *get_type_pointer(env_t *env, sss_type_t *t)
{
    gcc_lvalue_t *lval = get_type_lvalue(env, t);
    assert(lval);
    return gcc_lvalue_address(lval, NULL);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
