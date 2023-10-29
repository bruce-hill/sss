#include <gc.h>
#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "../SipHash/halfsiphash.h"
#include "types.h"
#include "string.h"
#include "cording.h"
#include "hashing.h"
#include "comparing.h"

extern const void *SSS_HASH_VECTOR;

#define xstr(a) str(a)
#define str(a) #a

#define DEFINE_INT_TYPE(c_type, KindOfInt, fmt, min_val, max_val, ...)\
    static CORD KindOfInt ## _cord(c_type *i, bool colorize) { \
        CORD c; \
        CORD_sprintf(&c, "%"fmt, *i); \
        if (colorize) CORD_sprintf(&c, "\x1b[35m%r\x1b[m", c); \
        return c; \
    } \
    static string_t KindOfInt ## _format(c_type i, int64_t digits) { \
        int len = snprintf(NULL, 0, "%0*" fmt, (int)digits, i); \
        char *str = GC_MALLOC_ATOMIC(len + 1); \
        snprintf(str, len+1, "%0*" fmt, (int)digits, i); \
        return (string_t){.data=str, .length=len, .stride=1}; \
    } \
    static string_t KindOfInt ## _hex(c_type i, int64_t digits, bool uppercase, bool prefix) { \
        const char *hex_fmt = uppercase ? (prefix ? "0x%0.*lX" : "%0.*lX") : (prefix ? "0x%0.*lx" : "%0.*lx"); \
        int len = snprintf(NULL, 0, hex_fmt, (int)digits, (uint64_t)i); \
        char *str = GC_MALLOC_ATOMIC(len + 1); \
        snprintf(str, len+1, hex_fmt, (int)digits, (uint64_t)i); \
        return (string_t){.data=str, .length=len, .stride=1}; \
    } \
    static string_t KindOfInt ## _octal(c_type i, int64_t digits, bool uppercase, bool prefix) { \
        const char *octal_fmt = uppercase ? (prefix ? "0x%0.*lO" : "%0.*lO") : (prefix ? "0x%0.*lo" : "%0.*lo"); \
        int len = snprintf(NULL, 0, octal_fmt, (int)digits, (uint64_t)i); \
        char *str = GC_MALLOC_ATOMIC(len + 1); \
        snprintf(str, len+1, octal_fmt, (int)digits, (uint64_t)i); \
        return (string_t){.data=str, .length=len, .stride=1}; \
    } \
    static c_type KindOfInt ## _random(c_type min, c_type max) { \
        uint32_t r = arc4random_uniform((uint32_t)(max - min)); \
        return min + (c_type)r; \
    } \
    static c_type KindOfInt##_min = min_val, KindOfInt##_max = max_val; \
    Type KindOfInt##_type = { \
        .name=STRING(#KindOfInt), \
        .cord=CordMethod(Function, (void*)KindOfInt ## _cord), \
        .compare=CompareMethod(Data, sizeof(c_type)), \
        .hash=HashMethod(Data, sizeof(c_type)), \
        .bindings=(NamespaceBinding[]){ \
            {"format", "func(i:"#KindOfInt", digits=1) Str", KindOfInt##_format}, \
            {"hex", "func(i:"#KindOfInt", digits=1, uppercase=yes, prefix=yes) Str", KindOfInt##_hex}, \
            {"octal", "func(i:"#KindOfInt", digits=1, uppercase=yes, prefix=yes) Str", KindOfInt##_octal}, \
            {"random", "func() "#KindOfInt, KindOfInt##_random}, \
            {"min", #KindOfInt, &KindOfInt##_min}, \
            {"max", #KindOfInt, &KindOfInt##_max}, \
            __VA_ARGS__ __VA_OPT__(,) \
            {NULL, NULL, NULL}, \
        }, \
    };

DEFINE_INT_TYPE(int64_t,  Int,    "ld", INT64_MIN, INT64_MAX,  {"abs", "func(i:Int) Int", labs});
DEFINE_INT_TYPE(int32_t,  Int32,  "d",  INT32_MIN, INT32_MAX,  {"abs", "func(i:Int32) Int", abs});
DEFINE_INT_TYPE(int16_t,  Int16,  "d",  INT16_MIN, INT16_MAX,  {"abs", "func(i:Int16) Int", abs});
DEFINE_INT_TYPE(int8_t,   Int8,   "d",  INT8_MIN,  INT8_MAX,   {"abs", "func(i:Int8) Int", abs});
DEFINE_INT_TYPE(uint64_t, UInt,   "lu", 0,         UINT64_MAX);
DEFINE_INT_TYPE(uint32_t, UInt32, "u",  0,         UINT32_MAX);
DEFINE_INT_TYPE(uint16_t, UInt16, "u",  0,         UINT16_MAX);
DEFINE_INT_TYPE(uint8_t,  UInt8,  "u",  0,         UINT8_MAX);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
