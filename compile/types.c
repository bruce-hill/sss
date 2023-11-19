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

// Values here are static so they can be referenced later
static gcc_field_t *type_struct_fields[5],
                   *type_union_fields[6],
                   // Per-type union members:
                   *custom_info_fields[4],
                   *pointer_info_fields[3],
                   *array_info_fields[1],
                   *table_info_fields[4],
                   *struct_info_fields[2],
                   *tagged_union_info_fields[2];
static gcc_type_t *type_gcc_type = NULL, *data_union = NULL,
                  *custom_info = NULL, *pointer_info = NULL,
                  *array_info = NULL, *table_info = NULL, *struct_info = NULL,
                  *tagged_union_info = NULL,
                  *struct_member_type = NULL, *struct_member_array_type = NULL,
                  *tu_member_type = NULL, *tu_member_array_type = NULL;

gcc_type_t *get_type_gcc_type(env_t *env)
{
    if (type_gcc_type != NULL)
        return type_gcc_type;

    gcc_struct_t *type_struct = gcc_opaque_struct(env->ctx, NULL, "Type");
    type_gcc_type = gcc_struct_as_type(type_struct);

#define FIELD(name, type) gcc_new_field(env->ctx, NULL, type, name)
#define STRUCT(name, fields) gcc_struct_as_type(gcc_new_struct_type(env->ctx, NULL, name, sizeof(fields)/sizeof(fields[0]), fields))

    custom_info_fields[0] = FIELD("equal", gcc_type(env->ctx, VOID_PTR));
    custom_info_fields[1] = FIELD("compare", gcc_type(env->ctx, VOID_PTR));
    custom_info_fields[2] = FIELD("hash", gcc_type(env->ctx, VOID_PTR));
    custom_info_fields[3] = FIELD("cord", gcc_type(env->ctx, VOID_PTR));
    custom_info = STRUCT("CustomInfo", custom_info_fields);

    pointer_info_fields[0] = FIELD("sigil", gcc_type(env->ctx, STRING));
    pointer_info_fields[1] = FIELD("pointed", gcc_get_ptr_type(type_gcc_type));
    pointer_info_fields[2] = FIELD("cyclic", gcc_type(env->ctx, BOOL));
    pointer_info = STRUCT("PointerInfo", pointer_info_fields);

    array_info_fields[0] = FIELD("item", gcc_get_ptr_type(type_gcc_type));
    array_info = STRUCT("ArrayInfo", array_info_fields);

    table_info_fields[0] = FIELD("key", gcc_get_ptr_type(type_gcc_type));
    table_info_fields[1] = FIELD("value", gcc_get_ptr_type(type_gcc_type));
    table_info_fields[2] = FIELD("entry_size", gcc_type(env->ctx, INT64));
    table_info_fields[3] = FIELD("value_offset", gcc_type(env->ctx, INT64));
    table_info = STRUCT("TableInfo", table_info_fields);

    gcc_field_t *sfields[] = {FIELD("name", gcc_type(env->ctx, STRING)), FIELD("type", gcc_get_ptr_type(type_gcc_type))};
    struct_member_type = STRUCT("StructField", sfields);
    struct_member_array_type = make_array_gcc_type(env, struct_member_type);
    struct_info_fields[0] = FIELD("fields", struct_member_array_type);
    struct_info_fields[1] = FIELD("is_pure_data", gcc_type(env->ctx, BOOL));
    struct_info = STRUCT("StructInfo", struct_info_fields);

    gcc_field_t *tufields[] = {FIELD("tag", gcc_type(env->ctx, INT32)), FIELD("name", gcc_type(env->ctx, STRING)), FIELD("type", gcc_get_ptr_type(type_gcc_type))};
    tu_member_type = STRUCT("TaggedUnionField", tufields);
    tu_member_array_type = make_array_gcc_type(env, tu_member_type);
    tagged_union_info_fields[0] = FIELD("members", tu_member_array_type);
    tagged_union_info_fields[1] = FIELD("is_pure_data", gcc_type(env->ctx, BOOL));
    tagged_union_info = STRUCT("TaggedUnionInfo", tagged_union_info_fields);

    type_union_fields[CustomInfo] = FIELD("CustomInfo", custom_info);
    type_union_fields[PointerInfo] = FIELD("PointerInfo", pointer_info);
    type_union_fields[ArrayInfo] = FIELD("ArrayInfo", array_info);
    type_union_fields[TableInfo] = FIELD("TableInfo", table_info);
    type_union_fields[StructInfo] = FIELD("StructInfo", struct_info);
    type_union_fields[TaggedUnionInfo] = FIELD("TaggedUnionInfo", tagged_union_info);
    data_union = gcc_union(env->ctx, NULL, "TypeInfoUnion", sizeof(type_union_fields)/sizeof(type_union_fields[0]), type_union_fields);

    type_struct_fields[0] = FIELD("name", gcc_type(env->ctx, STRING));
    type_struct_fields[1] = FIELD("size", gcc_type(env->ctx, INT64));
    type_struct_fields[2] = FIELD("align", gcc_type(env->ctx, INT64));
    type_struct_fields[3] = FIELD("tag", gcc_type(env->ctx, INT32));
    type_struct_fields[4] = FIELD("info", data_union);
    gcc_set_fields(type_struct, NULL, sizeof(type_struct_fields)/sizeof(type_struct_fields[0]), type_struct_fields);

#undef STRUCT
#undef FIELD

    return type_gcc_type;
}

gcc_lvalue_t *get_type_lvalue(env_t *env, sss_type_t *t)
{
    const char *key = type_to_string_concise(t);
    gcc_lvalue_t *lval = Table_str_get(&env->global->type_lvals, key);
    if (lval) return lval;
    gcc_type_t *type_gcc_t = sss_type_to_gcc(env, Type(TypeType));
    lval = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, type_gcc_t, fresh("Type"));
    Table_str_set(&env->global->type_lvals, key, lval);
    return lval;
}

gcc_rvalue_t *get_type_pointer(env_t *env, sss_type_t *t)
{
    const char *key = type_to_string_concise(t);
    gcc_lvalue_t *lval = Table_str_get(&env->global->type_lvals, key);
    if (!lval) {
        lval = get_type_lvalue(env, t);
        // Auto-initialize types when we get the pointer via this route
        initialize_type_lvalue(env, t);
    }
    return gcc_lvalue_address(lval, NULL);
}

static table_t initialized_type_lvals = {0};
void mark_type_lvalue_initialized(env_t *env, sss_type_t *t)
{
    (void)env;
    Table_str_set(&initialized_type_lvals, type_to_string_concise(t), t);
}

void initialize_type_lvalue(env_t *env, sss_type_t *t)
{
    if (Table_str_get(&initialized_type_lvals, type_to_string_concise(t)))
        return;
    mark_type_lvalue_initialized(env, t);

    gcc_lvalue_t *lval = get_type_lvalue(env, t);
#define TAG_RVAL 3
#define INFO_RVAL 4
    gcc_rvalue_t *type_rvalues[5] = {
        gcc_str(env->ctx, type_to_string_concise(t->tag == TypeType ? Match(t, TypeType)->type : t)),
        gcc_rvalue_int64(env->ctx, gcc_sizeof(env, t)),
        gcc_rvalue_int64(env->ctx, gcc_alignof(env, t)),
        NULL, // tag
        NULL, // info
    };

#define SET_INFO(tag, info_type, fields, ...) do { type_rvalues[TAG_RVAL] = gcc_rvalue_int32(env->ctx, tag); \
            type_rvalues[INFO_RVAL] = gcc_union_constructor(\
            env->ctx, NULL, data_union, type_union_fields[tag], \
            gcc_struct_constructor( \
                env->ctx, NULL, info_type, sizeof((gcc_rvalue_t*[]){__VA_ARGS__})/sizeof(gcc_rvalue_t*), fields, \
                (gcc_rvalue_t*[]){__VA_ARGS__})); } while (0)

    // From here out, all the initialization of variants is the same as the underlying type
    sss_type_t *str_t = get_type_by_name(env, "Str");
    while (t->tag == VariantType) {
        t = Match(t, VariantType)->variant_of;

        // Special case: if we're dealing with a string variant, use the string metamethods:
        if (t == str_t) {
#define FUNC_PTR(name) gcc_cast(env->ctx, NULL, gcc_get_func_address(get_function(env, name), NULL), gcc_type(env->ctx, VOID_PTR))
            SET_INFO(CustomInfo, custom_info, custom_info_fields,
                     FUNC_PTR("Str__equal"),
                     FUNC_PTR("Str__compare"),
                     FUNC_PTR("Str__hash"),
                     FUNC_PTR("Str__cord"),
            );
#undef FUNC_PTR
            gcc_global_set_initializer_rvalue(
                lval,
                gcc_struct_constructor(env->ctx, NULL, type_gcc_type, sizeof(type_struct_fields)/sizeof(type_struct_fields[0]), type_struct_fields, type_rvalues));
            return;
        }
    }

    switch (t->tag) {
    case ArrayType: {
        sss_type_t *item_type = Match(t, ArrayType)->item_type;
        SET_INFO(ArrayInfo, array_info, array_info_fields, get_type_pointer(env, item_type));
        break;
    }
    case TableType: {
        sss_type_t *key_type = Match(t, TableType)->key_type;
        sss_type_t *value_type = Match(t, TableType)->value_type;
        SET_INFO(TableInfo, table_info, table_info_fields,
                 get_type_pointer(env, key_type),
                 get_type_pointer(env, value_type),
                 gcc_rvalue_int64(env->ctx, gcc_sizeof(env, table_entry_type(t))),
                 table_entry_value_offset(env, t),
        );
        break;
    }
    case PointerType: {
        auto ptr = Match(t, PointerType);
        CORD sigil = ptr->is_stack ? (ptr->is_optional ? "&(optional)" : "&") : (ptr->is_optional ? "?" : "@");
        if (ptr->is_readonly) sigil = CORD_cat(sigil, "(read-only)");

        SET_INFO(PointerInfo, pointer_info, pointer_info_fields,
                 gcc_str(env->ctx, CORD_to_const_char_star(sigil)),
                 get_type_pointer(env, ptr->pointed),
                 gcc_rvalue_bool(env->ctx, can_have_cycles(t)),
        );
        break;
    }
    case StructType: {
        auto info = Match(t, StructType);
        int64_t num_fields = LENGTH(info->field_types);
        gcc_rvalue_t *member_rvals[num_fields] = {};
        for (int64_t i = 0; i < num_fields; i++) {
            const char *name = ith(info->field_names, i);
            if (!name) name = heap_strf("_%d", i+1);
            member_rvals[i] = gcc_struct_constructor(
                env->ctx, NULL, struct_member_type, 2, (gcc_field_t*[]){
                    gcc_get_field(gcc_type_as_struct(struct_member_type), 0),
                    gcc_get_field(gcc_type_as_struct(struct_member_type), 1),
                }, (gcc_rvalue_t*[]){
                    gcc_str(env->ctx, name),
                    get_type_pointer(env, ith(info->field_types, i)),
                });
        }

        gcc_type_t *raw_fields_data_type = gcc_jit_context_new_array_type(env->ctx, NULL, struct_member_type, num_fields);
        gcc_rvalue_t *field_data = gcc_jit_context_new_array_constructor(
            env->ctx, NULL, raw_fields_data_type, num_fields, member_rvals);

        gcc_lvalue_t *struct_fields_lval = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, raw_fields_data_type, fresh("_struct_fields"));
        gcc_global_set_initializer_rvalue(struct_fields_lval, field_data);
        gcc_rvalue_t *fields = gcc_struct_constructor(
            env->ctx, NULL, struct_member_array_type, 3, (gcc_field_t*[]){
                gcc_get_field(gcc_type_as_struct(struct_member_array_type), ARRAY_DATA_FIELD),
                gcc_get_field(gcc_type_as_struct(struct_member_array_type), ARRAY_LENGTH_FIELD),
                gcc_get_field(gcc_type_as_struct(struct_member_array_type), ARRAY_STRIDE_FIELD),
            }, (gcc_rvalue_t*[]){
                gcc_cast(env->ctx, NULL, gcc_lvalue_address(struct_fields_lval, NULL), gcc_get_ptr_type(struct_member_type)),
                gcc_rvalue_int64(env->ctx, num_fields),
                gcc_rvalue_int16(env->ctx, sizeof(void*) + sizeof(void*)),
            });

        SET_INFO(StructInfo, struct_info, struct_info_fields,
            fields,
            gcc_rvalue_bool(env->ctx, false), // TODO: determine if it is pure memory or not
        );
        break;
    }
    case TaggedUnionType: {
        auto info = Match(t, TaggedUnionType);
        int64_t num_members = LENGTH(info->members);
        gcc_rvalue_t *member_rvals[num_members] = {};
        for (int64_t i = 0; i < num_members; i++) {
            auto member = ith(info->members, i);
            member_rvals[i] = gcc_struct_constructor(
                env->ctx, NULL, tu_member_type, 3, (gcc_field_t*[]){
                    gcc_get_field(gcc_type_as_struct(tu_member_type), 0),
                    gcc_get_field(gcc_type_as_struct(tu_member_type), 1),
                    gcc_get_field(gcc_type_as_struct(tu_member_type), 2),
                }, (gcc_rvalue_t*[]){
                    gcc_rvalue_int32(env->ctx, member.tag_value),
                    gcc_str(env->ctx, member.name),
                    member.type ? get_type_pointer(env, member.type) : gcc_null(env->ctx, gcc_get_ptr_type(type_gcc_type)),
                });
        }

        gcc_type_t *raw_fields_data_type = gcc_jit_context_new_array_type(env->ctx, NULL, tu_member_type, num_members);
        gcc_rvalue_t *field_data = gcc_jit_context_new_array_constructor(
            env->ctx, NULL, raw_fields_data_type, num_members, member_rvals);

        gcc_lvalue_t *tu_fields_lval = gcc_global(env->ctx, NULL, GCC_GLOBAL_INTERNAL, raw_fields_data_type, fresh("_tagged_union_members"));
        gcc_global_set_initializer_rvalue(tu_fields_lval, field_data);
        gcc_rvalue_t *fields = gcc_struct_constructor(
            env->ctx, NULL, tu_member_array_type, 3, (gcc_field_t*[]){
                gcc_get_field(gcc_type_as_struct(tu_member_array_type), ARRAY_DATA_FIELD),
                gcc_get_field(gcc_type_as_struct(tu_member_array_type), ARRAY_LENGTH_FIELD),
                gcc_get_field(gcc_type_as_struct(tu_member_array_type), ARRAY_STRIDE_FIELD),
            }, (gcc_rvalue_t*[]){
                gcc_cast(env->ctx, NULL, gcc_lvalue_address(tu_fields_lval, NULL), gcc_get_ptr_type(tu_member_type)),
                gcc_rvalue_int64(env->ctx, num_members),
                gcc_rvalue_int16(env->ctx, 8*3),
            });

        SET_INFO(TaggedUnionInfo, tagged_union_info, tagged_union_info_fields,
            fields,
            gcc_rvalue_bool(env->ctx, false), // TODO: determine if it is pure memory or not
        );
        break;
    }
    case VariantType:
    case FunctionType: {
        SET_INFO(CustomInfo, custom_info, custom_info_fields,
                 gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR)),
                 gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR)),
                 gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR)),
                 gcc_cast(env->ctx, NULL, gcc_get_func_address(get_function(env, "Func_cord"), NULL), gcc_type(env->ctx, VOID_PTR))
        );
        break;
    }
    case TypeType: {
        SET_INFO(CustomInfo, custom_info, custom_info_fields,
                 gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR)),
                 gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR)),
                 gcc_null(env->ctx, gcc_type(env->ctx, VOID_PTR)),
                 gcc_cast(env->ctx, NULL, gcc_get_func_address(get_function(env, "Type_cord"), NULL), gcc_type(env->ctx, VOID_PTR))
        );
        break;
    }
    default: {
        compiler_err(env, NULL, "Type lvalue not implemented for: %T", t);
    }
    }

    gcc_global_set_initializer_rvalue(
        lval,
        gcc_struct_constructor(env->ctx, NULL, type_gcc_type, sizeof(type_struct_fields)/sizeof(type_struct_fields[0]), type_struct_fields, type_rvalues));
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
