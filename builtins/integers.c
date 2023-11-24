#include <gc.h>
#include <gc/cord.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "../SipHash/halfsiphash.h"
#include "array.h"
#include "types.h"
#include "string.h"

extern const void *SSS_HASH_VECTOR;

#define xstr(a) str(a)
#define str(a) #a

#define DEFINE_INT_TYPE(c_type, KindOfInt, fmt, min_val, max_val)\
    public c_type KindOfInt##__min = min_val, KindOfInt##__max = max_val; \
    public CORD KindOfInt ## __cord(const c_type *i, bool colorize, const TypeInfo *type) { \
        const char *units = strchrnul(type->name, '<'); \
        CORD c; \
        if (colorize) CORD_sprintf(&c, "\x1b[35m%"fmt"\x1b[33;2m%s\x1b[m", *i, units); \
        else CORD_sprintf(&c, "%"fmt"%s", *i, units); \
        return c; \
    } \
    public int32_t KindOfInt ## __compare(const c_type *x, const c_type *y, const TypeInfo *type) { \
        (void)type; \
        return (*x > *y) - (*x < *y); \
    } \
    public Str_t KindOfInt ## __format(c_type i, int64_t digits) { \
        int len = snprintf(NULL, 0, "%0*" fmt, (int)digits, i); \
        char *str = GC_MALLOC_ATOMIC(len + 1); \
        snprintf(str, len+1, "%0*" fmt, (int)digits, i); \
        return (Str_t){.data=str, .length=len, .stride=1}; \
    } \
    public Str_t KindOfInt ## __hex(c_type i, int64_t digits, bool uppercase, bool prefix) { \
        const char *hex_fmt = uppercase ? (prefix ? "0x%0.*lX" : "%0.*lX") : (prefix ? "0x%0.*lx" : "%0.*lx"); \
        int len = snprintf(NULL, 0, hex_fmt, (int)digits, (uint64_t)i); \
        char *str = GC_MALLOC_ATOMIC(len + 1); \
        snprintf(str, len+1, hex_fmt, (int)digits, (uint64_t)i); \
        return (Str_t){.data=str, .length=len, .stride=1}; \
    } \
    public Str_t KindOfInt ## __octal(c_type i, int64_t digits, bool prefix) { \
        const char *octal_fmt = prefix ? "0o%0.*lo" : "%0.*lo"; \
        int len = snprintf(NULL, 0, octal_fmt, (int)digits, (uint64_t)i); \
        char *str = GC_MALLOC_ATOMIC(len + 1); \
        snprintf(str, len+1, octal_fmt, (int)digits, (uint64_t)i); \
        return (Str_t){.data=str, .length=len, .stride=1}; \
    } \
    public c_type KindOfInt ## __random(int64_t min, int64_t max) { \
        if (min > max) builtin_fail("Random min (%ld) is larger than max (%ld)", min, max); \
        if (min < (int64_t)KindOfInt##__min) builtin_fail("Random min (%ld) is smaller than the minimum "#KindOfInt" value", min); \
        if (max > (int64_t)KindOfInt##__max) builtin_fail("Random max (%ld) is smaller than the maximum "#KindOfInt" value", max); \
        int64_t range = max - min; \
        if (range > UINT32_MAX) builtin_fail("Random range (%ld) is larger than the maximum allowed (%ld)", range, UINT32_MAX); \
        uint32_t r = arc4random_uniform((uint32_t)range); \
        return min + (c_type)r; \
    } \
    public TypeInfo KindOfInt##_type = { \
        .name=#KindOfInt, \
        .size=sizeof(c_type), \
        .align=alignof(c_type), \
        .tag=CustomInfo, \
        .CustomInfo={.compare=(void*)KindOfInt##__compare, .cord=(void*)KindOfInt##__cord}, \
    };

DEFINE_INT_TYPE(int64_t,  Int,    "ld",     INT64_MIN, INT64_MAX);
DEFINE_INT_TYPE(int32_t,  Int32,  "d_i32",  INT32_MIN, INT32_MAX);
DEFINE_INT_TYPE(int16_t,  Int16,  "d_i16",  INT16_MIN, INT16_MAX);
DEFINE_INT_TYPE(int8_t,   Int8,   "d_i8",   INT8_MIN,  INT8_MAX);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
