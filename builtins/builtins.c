#include <gc.h>
#include <string.h>
#include <stdio.h>

#include "array.h"
#include "types.h"
#include "functions.h"
#include "table.h"

__attribute__ ((visibility ("default")))
const char *SSS_HASH_VECTOR = "sss hash vector ----------------------------------------------";

extern Type Bool_type, Char_type,
       Int_type, Int32_type, Int16_type, Int8_type,
       UInt_type, UInt32_type, UInt16_type, UInt8_type,
       Num_type, Num32_type,
       Str_type, CString_type,
       Range_type, Memory_type;
extern Type *(make_array_type)(Type*);

__attribute__ ((visibility ("default"))) array_t load()
{
    static array_t builtins_namespace = {0};
    if (builtins_namespace.length == 0)
        builtins_namespace = STATIC_ARRAY(
            (void*)&Bool_type, &Char_type, &Int_type, &Int32_type, &Int16_type, &Int8_type,
            &UInt_type, &UInt32_type, &UInt16_type, &UInt8_type, &Num_type, &Num32_type,
            &Str_type, &CString_type, &Range_type, &Memory_type, &make_array_type, &make_table_type,
            builtin_say, builtin_fail, builtin_last_err,
        );
    return builtins_namespace;
}
