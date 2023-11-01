
#include <gc.h>
#include <gc/cord.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/param.h>
#include <err.h>

#include "string.h"
#include "types.h"

static CORD Char_cord(const Type *type, const char *c, bool colorize) {
    (void)type;
    CORD cord = 0;
    switch (*c) {
    case '\a': return "\\a";
    case '\b': return "\\b";
    case '\x1b': return "\\e";
    case '\f': return "\\f";
    case '\n': return "\\n";
    case '\t': return "\\t";
    case '\r': return "\\r";
    case '\v': return "\\v";
    case '\\': return "\\\\";
    case '"': return "\\\"";
    default: {
        if (!isprint(*c))
            CORD_sprintf(&cord, "\\x%02X", (int)*c);
        else
            cord = CORD_cat_char(0, *c);
    }
    }
    if (colorize) {
        if (CORD_len(cord) > 1)
            CORD_sprintf(&cord, "\x1b[34m%r\x1b[m", cord);
        else
            CORD_sprintf(&cord, "\x1b[35m%r\x1b[m", cord);
    }
    return cord;
}

// For some reason, the C functions from ctypes.h return integers instead of
// booleans, and what's worse, the integers are not limited to [0-1]. So,
// it's necessary to cast them to bools to clamp them to those values.
#define BOOLIFY(fn) bool Char_ ## fn(char c) { return (bool)fn(c); }
BOOLIFY(isalnum)
BOOLIFY(isalpha)
BOOLIFY(iscntrl)
BOOLIFY(isdigit)
BOOLIFY(isgraph)
BOOLIFY(islower)
BOOLIFY(isprint)
BOOLIFY(ispunct)
BOOLIFY(isspace)
BOOLIFY(isupper)
BOOLIFY(isxdigit)
BOOLIFY(isascii)
BOOLIFY(isblank)

typedef bool (*char_pred_t)(char);
Type Char_type = {
    .name=STRING("Char"),
    .size=sizeof(char),
    .align=alignof(char),
    .cord=CordMethod(Function, (void*)Char_cord),
    .order=OrderingMethod(Data, sizeof(char)),
    .hash=HashMethod(Data, sizeof(char)),
    .bindings=(NamespaceBinding[]){
        {"uppercased", "func(c:Char) Char", toupper},
        {"lowercased", "func(c:Char) Char", tolower},
        {"is_alphanumeric", "func(c:Char) Bool", Char_isalnum},
        {"is_alphabetic", "func(c:Char) Bool", Char_isalpha},
        {"is_control", "func(c:Char) Bool", Char_iscntrl},
        {"is_digit", "func(c:Char) Bool", Char_isdigit},
        {"is_graphic", "func(c:Char) Bool", Char_isgraph},
        {"is_lowercase", "func(c:Char) Bool", Char_islower},
        {"is_printable", "func(c:Char) Bool", Char_isprint},
        {"is_punctuation", "func(c:Char) Bool", Char_ispunct},
        {"is_space", "func(c:Char) Bool", Char_isspace},
        {"is_uppercase", "func(c:Char) Bool", Char_isupper},
        {"is_hex", "func(c:Char) Bool", Char_isxdigit},
        {"is_ascii", "func(c:Char) Bool", Char_isascii},
        {"is_blank", "func(c:Char) Bool", Char_isblank},
        {NULL}
    },
};

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
