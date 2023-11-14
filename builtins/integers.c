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
    public CORD KindOfInt ## __cord(const c_type *i, bool colorize, const Type *type) { \
        (void)type; \
        CORD c; \
        CORD_sprintf(&c, "%"fmt, *i); \
        if (colorize) CORD_sprintf(&c, "\x1b[35m%r\x1b[m", c); \
        return c; \
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
    public c_type KindOfInt ## __random(c_type min, c_type max) { \
        uint32_t r = arc4random_uniform((uint32_t)(max - min)); \
        return min + (c_type)r; \
    } \
    public c_type KindOfInt##__min = min_val, KindOfInt##__max = max_val; \
    public Type KindOfInt##_type = { \
        .name=#KindOfInt, \
        .size=sizeof(c_type), \
        .align=alignof(c_type), \
        .tag=CustomInfo, \
        .CustomInfo={.cord=(void*)KindOfInt ## __cord}, \
    };

DEFINE_INT_TYPE(int64_t,  Int,    "ld", INT64_MIN, INT64_MAX);
DEFINE_INT_TYPE(int32_t,  Int32,  "d",  INT32_MIN, INT32_MAX);
DEFINE_INT_TYPE(int16_t,  Int16,  "d",  INT16_MIN, INT16_MAX);
DEFINE_INT_TYPE(int8_t,   Int8,   "d",  INT8_MIN,  INT8_MAX);
DEFINE_INT_TYPE(uint64_t, UInt,   "lu", 0,         UINT64_MAX);
DEFINE_INT_TYPE(uint32_t, UInt32, "u",  0,         UINT32_MAX);
DEFINE_INT_TYPE(uint16_t, UInt16, "u",  0,         UINT16_MAX);
DEFINE_INT_TYPE(uint8_t,  UInt8,  "u",  0,         UINT8_MAX);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
