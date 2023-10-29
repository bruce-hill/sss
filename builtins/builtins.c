#include <gc.h>
#include <string.h>
#include <stdio.h>

#include "types.h"
#include "functions.h"
#include "table.h"

const char *SSS_HASH_VECTOR = "sss hash vector ----------------------------------------------";
extern Type Bool_type, Char_type,
       Int_type, Int32_type, Int16_type, Int8_type,
       UInt_type, UInt32_type, UInt16_type, UInt8_type,
       Num_type, Num32_type,
       Str_type, CString_type,
       Range_type, Memory_type;
extern Type (*make_array_type)(Type*);
extern Type (*make_table_type)(Type*, Type*, size_t, size_t);
extern void *table_get(Type *type, table_t *t, const void *key);
extern void *table_get_raw(Type *type, table_t *t, const void *key);
extern void *table_nth(Type *type, table_t *t, uint32_t n);
extern void *table_set(Type *type, table_t *t, const void *key, const void *value);

NamespaceBinding *load()
{
    NamespaceBinding builtins_namespace[] = {
        {"Bool", "Type", &Bool_type},
        {"Char", "Type", &Char_type},
        {"Int", "Type", &Int_type},
        {"Int32", "Type", &Int32_type},
        {"Int16", "Type", &Int16_type},
        {"Int8", "Type", &Int8_type},
        {"UInt", "Type", &UInt_type},
        {"UInt32", "Type", &UInt32_type},
        {"UInt16", "Type", &UInt16_type},
        {"UInt8", "Type", &UInt8_type},
        {"Num", "Type", &Num_type},
        {"Num32", "Type", &Num32_type},
        {"Str", "Type", &Str_type},
        {"CString", "Type", &CString_type},
        {"Range", "Type", &Range_type},
        {"Memory", "Type", &Memory_type},
        {"make_array_type", "func(item_type:Type) Type", make_array_type},

        {"make_table_type", "func(key_type:Type, value_type:Type, entry_size:UInt, value_offset:UInt) Type", make_table_type},
        {"table_get", "func(table_type:Type, table:@Memory, key:@Memory) ?Memory", table_get},
        {"table_get_raw", "func(table_type:Type, table:@Memory, key:@Memory) ?Memory", table_get_raw},
        {"table_nth", "func(table_type:Type, table:@Memory, index:UInt32) ?Memory", table_nth},
        {"table_set", "func(table_type:Type, table:@Memory, key:@Memory, value:?Memory) ?Memory", table_set},

        {"say", "func(text:Str, end=\"\\n\") Void", say},
        {"fail", "func(fmt:Str) Abort", say},
        {"last_error", "func() Str", last_err},
        {NULL, NULL, NULL},
    };
    NamespaceBinding *bindings = GC_MALLOC_ATOMIC(sizeof(builtins_namespace));
    memcpy(bindings, builtins_namespace, sizeof(builtins_namespace));
    printf("Loaded builtins!\n");
    return bindings;
}
