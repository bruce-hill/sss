#include <gc.h>
#include <gc/cord.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/param.h>
#include <err.h>

#include "../SipHash/halfsiphash.h"
#include "types.h"
#include "array.h"
#include "string.h"

#define CLAMP(x, lo, hi) MIN(hi, MAX(x,lo))

extern const void *SSS_HASH_VECTOR;

typedef struct {
    uint8_t success;
    int32_t index;
} find_result_t;

static string_t compacted(string_t str)
{
    if (str.stride == 1) return str;
    char *buf = GC_MALLOC_ATOMIC(str.length + 1);
    for (unsigned long i = 0; i < str.length; i++)
        buf[i] = str.data[i*str.stride];
    return (string_t){.data=buf, .length=str.length, .stride=1};
}

static CORD Str_cord(const Type *type, const string_t *s, bool colorize)
{
    (void)type;
    char *data = s->data;
    // Note: it's important to have unicode strings not get broken up with
    // escapes, otherwise they won't print right.
    if (colorize) {
        CORD c = "\x1b[31m\"";
        for (uint32_t i = 0; i < s->length; i++) {
            switch (data[i]) {
#define BACKSLASHED(esc) "\x1b[35m\\\x1b[1m" esc "\x1b[0;34m"
            case '\a': c = CORD_cat(c, BACKSLASHED("a")); break;
            case '\b': c = CORD_cat(c, BACKSLASHED("b")); break;
            case '\x1b': c = CORD_cat(c, BACKSLASHED("e")); break;
            case '\f': c = CORD_cat(c, BACKSLASHED("f")); break;
            case '\n': c = CORD_cat(c, BACKSLASHED("n")); break;
            case '\r': c = CORD_cat(c, BACKSLASHED("r")); break;
            case '\t': c = CORD_cat(c, BACKSLASHED("t")); break;
            case '\v': c = CORD_cat(c, BACKSLASHED("v")); break;
            case '"': c = CORD_cat(c, BACKSLASHED("\"")); break;
            case '\\': c = CORD_cat(c, BACKSLASHED("\\")); break;
            case '\x00' ... '\x06': case '\x0E' ... '\x1A':
            case '\x1C' ... '\x1F': case '\x7F' ... '\x7F':
                CORD_sprintf(&c, "%r" BACKSLASHED("x%02X"), c, data[i]);
                break;
            default: c = CORD_cat_char(c, data[i]); break;
#undef BACKSLASHED
            }
        }
        c = CORD_cat(c, "\x1b[m");
        return c;
    } else {
        CORD c = "\"";
        for (uint32_t i = 0; i < s->length; i++) {
            switch (data[i]) {
            case '\a': c = CORD_cat(c, "\\a"); break;
            case '\b': c = CORD_cat(c, "\\b"); break;
            case '\x1b': c = CORD_cat(c, "\\e"); break;
            case '\f': c = CORD_cat(c, "\\f"); break;
            case '\n': c = CORD_cat(c, "\\n"); break;
            case '\r': c = CORD_cat(c, "\\r"); break;
            case '\t': c = CORD_cat(c, "\\t"); break;
            case '\v': c = CORD_cat(c, "\\v"); break;
            case '"': c = CORD_cat(c, "\\\""); break;
            case '\\': c = CORD_cat(c, "\\\\"); break;
            case '\x00' ... '\x06': case '\x0E' ... '\x1A':
            case '\x1C' ... '\x1F': case '\x7F' ... '\x7F':
                CORD_sprintf(&c, "%r\\x%02X", c, data[i]);
                break;
            default: c = CORD_cat_char(c, data[i]); break;
            }
        }
        c = CORD_cat_char(c, '"');
        return c;
    }
}

static int Str_compare(string_t *x, string_t *y)
{
    unsigned long length = x->length < y->length ? x->length : y->length;
    for (unsigned long i = 0; i < length; i++) {
        char xc = x->data[i], yc = y->data[i];
        if (xc != yc)
            return (xc > yc) ? 1 : -1;
    }
    return (x->length > y->length) - (x->length < y->length);
}

static int Str_hash(string_t *s)
{
    const char *data;
    if (s->stride == -1) {
        data = s->data - s->length + 1;
    } else if (s->stride == 1) {
        data = s->data;
    } else {
        char buf[s->length];
        for (unsigned long i = 0; i < s->length; i++)
            buf[i] = s->data[i*s->stride];
        data = buf;
    }

    uint32_t hash;
    halfsiphash(data, s->length, SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
    return hash;
}

static string_t uppercased(string_t s)
{
    char *s2 = GC_MALLOC_ATOMIC(s.length + 1);
    for (unsigned long i = 0; i < s.length; i++)
        s2[i] = toupper(s.data[i*s.stride]);
    return (string_t){.data=s2, .length=s.length, .stride=1};
}

static string_t lowercased(string_t s)
{
    char *s2 = GC_MALLOC_ATOMIC(s.length + 1);
    for (unsigned long i = 0; i < s.length; i++)
        s2[i] = tolower(s.data[i*s.stride]);
    return (string_t){.data=s2, .length=s.length, .stride=1};
}

static string_t capitalized(string_t s)
{
    char *s2 = GC_MALLOC_ATOMIC(s.length + 1);
    unsigned long i;
    for (i = 0; i < s.length; i++) {
        if (isalpha(s.data[i*s.stride])) {
            s2[i] = toupper(s.data[i*s.stride]);
            ++i;
            break;
        } else {
            s2[i] = s.data[i*s.stride];
        }
    }
    for (; i < s.length; i++)
        s2[i] = s.data[i*s.stride];
    return (string_t){.data=s2, .length=s.length, .stride=1};
}

static string_t titlecased(string_t s)
{
    char *s2 = GC_MALLOC_ATOMIC(s.length + 1);
    bool should_uppercase = true;
    for (unsigned long i = 0; i < s.length; i++) {
        if (isalpha(s.data[i*s.stride])) {
            if (should_uppercase) {
                s2[i] = toupper(s.data[i*s.stride]);
                should_uppercase = false;
            } else {
                s2[i] = tolower(s.data[i*s.stride]);
            }
        } else {
            should_uppercase = true;
            s2[i] = s.data[i*s.stride];
        }
    }
    return (string_t){.data=s2, .length=s.length, .stride=1};
}

static bool starts_with(string_t s, string_t prefix)
{
    if (s.length < prefix.length) return false;
    for (unsigned long i = 0; i < prefix.length; i++) {
        if (s.data[i*s.stride] != prefix.data[i*prefix.stride])
            return false;
    }
    return true;
}

static bool ends_with(string_t s, string_t suffix)
{
    if (s.length < suffix.length) return false;
    for (unsigned long i = 0; i < suffix.length; i++) {
        if (s.data[(s.length-suffix.length+i)*s.stride] != suffix.data[i*suffix.stride])
            return false;
    }
    return true;
}

static string_t without_prefix(string_t s, string_t prefix)
{
    if (s.length < prefix.length) return s;
    for (unsigned long i = 0; i < prefix.length; i++) {
        if (s.data[i*s.stride] != prefix.data[i*prefix.stride])
            return s;
    }
    return (string_t){
        .data=s.data + prefix.length*s.stride,
        .length=s.length - prefix.length,
        .stride=s.stride,
        .cow=0, .atomic=1,
    };
}

static string_t without_suffix(string_t s, string_t suffix)
{
    if (s.length < suffix.length) return s;
    for (unsigned long i = 0; i < suffix.length; i++) {
        if (s.data[(s.length - suffix.length + i)*s.stride] != suffix.data[i*suffix.stride])
            return s;
    }
    return (string_t){
        .data=s.data,
        .length=s.length - suffix.length,
        .stride=s.stride,
        .cow=0, .atomic=1,
    };
}

static string_t trimmed(string_t s, string_t trim_chars, bool trim_left, bool trim_right)
{
    unsigned long length = s.length;
    unsigned long start = 0;
    if (trim_left) {
        for (; start < s.length; start++) {
            for (unsigned long t = 0; t < trim_chars.length; t++) {
                if (s.data[start*s.stride] == trim_chars.data[t*trim_chars.stride])
                    goto found_ltrim;
            }
            goto done_trimming_left;
          found_ltrim:
            --length;
        }
    }
  done_trimming_left:;
    if (trim_right) {
        while (length > 0) {
            for (unsigned long t = 0; t < trim_chars.length; t++) {
                if (s.data[(start+length-1)*s.stride] == trim_chars.data[t*trim_chars.stride])
                    goto found_rtrim;
            }
            goto done_trimming_right;
          found_rtrim:
            --length;
        }
    }
  done_trimming_right:;
    return (string_t){.data=s.data+start*s.stride, .length=length, .stride=s.stride};
}

static string_t slice(string_t s, int64_t _first, int64_t _stride, int64_t length)
{
    if (_stride > INT16_MAX || _stride < INT16_MIN)
        errx(1, "Invalid string slice stride: %ld", _stride);
    int16_t stride = (int16_t)_stride;
    unsigned long first = (unsigned long)CLAMP(_first-1, 0, (int64_t)s.length-1),
                  last = (unsigned long)CLAMP(_first + length - 1, 0, (int64_t)s.length-1);
    unsigned long slice_len = (last - first)/stride;
    return (string_t){.data=&s.data[first*s.stride], .length=slice_len, .stride=stride};
}

static const char *c_string(string_t str)
{
    if (str.length == 0)
        return "";
    else if (str.stride == 1
        // Verify that the '\0' would be on the same page, so unlikely to segfault:
        && (((uint64_t)str.data + str.length + 1) & 0xFFF) != 0
        // Check for nul-termination
        && str.data[str.length+1] == '\0')
        return str.data;

    char *buf = GC_MALLOC_ATOMIC(str.length + 1);
    for (unsigned long i = 0; i < str.length; i++)
        buf[i] = str.data[i*str.stride];
    buf[str.length] = '\0';
    return buf;
}

static string_t from_c_string(const char *str)
{
    size_t length = str ? strlen(str) : 0;
    if (length == 0) return (string_t){.length=0, .stride=0};
    char *buf = GC_MALLOC_ATOMIC(length + 1);
    memcpy(buf, str, length+1);
    buf[length+1] = '\0';
    return (string_t){.data=buf, .length=length, .stride=1};
}

static find_result_t find(string_t str, string_t pat)
{
    if (str.length < pat.length) return (find_result_t){.success=0};
    if (pat.length == 0) return (find_result_t){.success=1, .index=1};

    // For short strings, do naive approach:
    // if (str.length*pat.length < UCHAR_MAX) {
    for (unsigned long s = 0; s < str.length; s++) {
        for (unsigned long p = 0; p < pat.length; p++) {
            if (str.data[s*str.stride] != pat.data[p*pat.stride])
                goto not_a_match;
        }
        return (find_result_t){.success=1, .index=s+1};
      not_a_match:;
    }
    return (find_result_t){.success=0};
    // }

    // // Boyer-Moore algorithm:
    // static int skip[UCHAR_MAX];
    // for (int32_t i = 0; i <= UCHAR_MAX; ++i)
    //     skip[i] = str.length;
    // for (int32_t i = 0; i < pat.length; ++i)
    //     skip[(unsigned char)pat.data[i*pat.stride]] = pat.length - i - 1;
    // char lastpatchar = pat.data[(pat.length - 1)*pat.stride];
    // int32_t min_skip = pat.length;
    // for (int32_t p = 0; p < pat.length - 1; ++p) {
    //     if (pat.data[p*pat.stride] == lastpatchar)
    //         min_skip = pat.length - p - 1;
    // }

    // for (int32_t i = pat.length - 1; i < str.length; ) {
    //     // Use skip table:
    //     int32_t can_skip = skip[(unsigned char)str.data[i*str.stride]];
    //     if (can_skip != 0) {
    //         i += can_skip;
    //         continue;
    //     }
    //     // Check for exact match:
    //     for (int32_t j = 0; j < pat.length; j++) {
    //         if (str.data[(i-pat.length+j)*str.stride] != pat.data[j*pat.stride]) {
    //             // Mismatch:
    //             i += min_skip;
    //             goto keep_going;
    //         }
    //     }
    //     return i - pat.length + 1;
    //   keep_going: continue;
    // }
    // return 0;
}

static string_t replace(string_t text, string_t pat, string_t replacement, int64_t limit)
{
    text = compacted(text);
    pat = compacted(pat);
    replacement = compacted(replacement);
    char *buf;
    size_t size;
    FILE *mem = open_memstream(&buf, &size);
    for (const char *pos = text.data; ; --limit) {
        const char *match = limit == 0 ? NULL : memmem(pos, (size_t)text.length - (size_t)(pos - text.data), pat.data, pat.length);
        if (match) {
            fwrite(pos, 1, (size_t)(match - pos), mem);
            fwrite(replacement.data, 1, replacement.length, mem);
            pos = match + pat.length;
        } else {
            fwrite(pos, 1, (size_t)text.length - (size_t)(pos - text.data), mem);
            break;
        }
    }
    fflush(mem);
    char *str = GC_MALLOC_ATOMIC(size + 1);
    memcpy(str, buf, size+1);
    fclose(mem);
    free(buf);
    return (string_t){.data=str, .length=size, .stride=1};
}

static string_t quoted(string_t text, const char *dsl, bool colorize)
{
    char *buf;
    size_t size;
    FILE *mem = open_memstream(&buf, &size);
    if (colorize) fputs("\x1b[35m", mem);
    if (dsl && dsl[0]) fprintf(mem, "$%s", dsl);
    const char *escape_color = colorize ? "\x1b[1;34m" : "";
    const char *reset_color = colorize ? "\x1b[0;35m" : "";
    fputc('"', mem);
    for (unsigned long i = 0; i < text.length; i++) {
        char c = text.data[i*text.stride];
        switch (c) {
        case '\\': fprintf(mem, "%s\\\\%s", escape_color, reset_color); break;
        case '"': fprintf(mem, "%s\\\"%s", escape_color, reset_color); break;
        case '\n': fprintf(mem, "%s\\n%s", escape_color, reset_color); break;
        case '\t': fprintf(mem, "%s\\t%s", escape_color, reset_color); break;
        case '\r': fprintf(mem, "%s\\r%s", escape_color, reset_color); break;
        case '\a': fprintf(mem, "%s\\a%s", escape_color, reset_color); break;
        case '\b': fprintf(mem, "%s\\b%s", escape_color, reset_color); break;
        case '\v': fprintf(mem, "%s\\v%s", escape_color, reset_color); break;
        default: {
            if (isprint(c))
                fputc(c, mem);
            else
                fprintf(mem, "%s\\x%02X%s", escape_color, (int)c, reset_color);
        }
        }
    }
    fputc('"', mem);
    if (colorize) fputs("\x1b[m", mem);
    fflush(mem);
    char *str = GC_MALLOC_ATOMIC(size + 1);
    memcpy(str, buf, size+1);
    fclose(mem);
    free(buf);
    return (string_t){.data=str, .length=size, .stride=1};
}

static string_array_t split(string_t str, string_t split_chars)
{
    if (str.length == 0) return (string_array_t){.stride=sizeof(string_t)};
    string_array_t strings = {.stride=sizeof(string_t)};
    size_t capacity = 0;
    bool separators[256] = {0};
    for (unsigned long i = 0; i < split_chars.length; i++)
        separators[(int)split_chars.data[split_chars.stride*i]] = true;

    for (unsigned long i = 0; i < str.length; i++) {
        if (separators[(int)str.data[str.stride*i]]) continue;
        unsigned long length = 0;
        while (i < str.length && !separators[(int)str.data[str.stride*i]]) {
            ++length;
            ++i;
        }
        strings.data = GC_REALLOC(strings.data, sizeof(string_t)*(capacity += 1));
        strings.data[strings.length++] = (string_t){
            .data=&str.data[str.stride*(i-length)],
            .length=length, 
            .stride=str.stride,
        };
    }
    return strings;
}

Type Str_type = {
    .name="Str",
    .cord=CordMethod(Function, (void*)Str_cord),
    .hash=HashMethod(Function, (void*)Str_hash),
    .order=OrderingMethod(Function, (void*)Str_compare),
    .bindings=(NamespaceBinding[]){
        {"uppercased", "func(str:Str) Str", uppercased},
        {"lowercased", "func(str:Str) Str", lowercased},
        {"capitalized", "func(str:Str) Str", capitalized},
        {"titlecased", "func(str:Str) Str", titlecased},
        {"starts_with", "func(str:Str, prefix:Str) Bool", starts_with},
        {"ends_with", "func(str:Str, suffix:Str) Bool", ends_with},
        {"without_prefix", "func(str:Str, prefix:Str) Str", without_prefix},
        {"without_suffix", "func(str:Str, suffix:Str) Str", without_suffix},
        {"trimmed", "func(str:Str, trim_chars:[Char], trim_left=yes, trim_right=yes) Str", trimmed},
        {"slice", "func(str:Str, start=1, stride=1, length=Int.max) Str", slice},
        {"c_string", "func(str:Str) CString", c_string},
        {"from_c_string", "func(c_str:CString) Str", from_c_string},
        {"find", "func(str:Str, target:Str) enum(Failure | Success(index:Int))", find},
        {"replace", "func(str:Str, target:Str, replacement:Str, limit=Int.max) Str", replace},
        {"quoted", "func(str:Str) Str", quoted},
        {"split", "func(str:Str, split_chars:[Char]) [Str]", split},

        // Array methods:
        {"compact", "func(str:@Str, item_size=1) Void", Array_compact},
        {"insert", "func(str:@Str, item:Char, index=#str, item_size=1) Void", Array_insert},
        {"insert_all", "func(str:@Str, items:[Str], index=#str, item_size=1) Void", Array_insert_all},
        {"remove", "func(str:@Str, index=#str, count=1, item_size=1) Void", Array_remove},
        {"sort", "func(str:@Str, item_compare=Char.compare, item_size=1) Void", Array_sort},
        {"shuffle", "func(str:@Str, item_size=1) Void", Array_shuffle},
        {"join", "func(pieces:[Str], glue:Str, item_size=1) Void", Array_join},
        {"clear", "func(str:@Str) Void", Array_clear},
        {NULL, NULL, NULL},
    },
};

static CORD CString_cord(const Type *type, const char **s, bool colorize)
{
    string_t str = {.data=(char*)*s, .length=strlen(*s), .stride=1};
    return Str_cord(type, &str, colorize);
}

static uint32_t CString_hash(const char **s)
{
    if (!*s) return 0;
    uint32_t hash;
    halfsiphash(*s, strlen(*s)+1, SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
    return hash;
}

static uint32_t CString_compare(const char **x, const char **y, const Type *type)
{
    (void)type;
    return strcmp(*x, *y);
}

Type CString_type = {
    .name="CString",
    .size=sizeof(string_t),
    .align=alignof(string_t),
    .cord=CordMethod(Function, (void*)CString_cord),
    .hash=HashMethod(Function, (void*)CString_hash),
    .order=OrderingMethod(Function, (void*)CString_compare),
    .bindings=(NamespaceBinding[]){
        {"string", "func(str:CString) Str", from_c_string},
        {"from_string", "func(str:Str) CString", c_string},
        {NULL, NULL, NULL},
    },
};

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
