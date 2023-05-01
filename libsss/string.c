#include <stdbool.h>
#include <assert.h>
#include <ctype.h>
#include <err.h>
#include <gc.h>
#include <gc/cord.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>

#include "string.h"

#define CLAMP(x, lo, hi) MIN(hi, MAX(x,lo))

string_t sss_string_uppercased(string_t s) {
    char *s2 = GC_MALLOC_ATOMIC(s.length + 1);
    for (int32_t i = 0; i < s.length; i++)
        s2[i] = toupper(s.data[i*s.stride]);
    return (string_t){.data=s2, .length=s.length, .stride=1};
}

string_t sss_string_lowercased(string_t s) {
    char *s2 = GC_MALLOC_ATOMIC(s.length + 1);
    for (int32_t i = 0; i < s.length; i++)
        s2[i] = tolower(s.data[i*s.stride]);
    return (string_t){.data=s2, .length=s.length, .stride=1};
}

string_t sss_string_capitalized(string_t s) {
    char *s2 = GC_MALLOC_ATOMIC(s.length + 1);
    int32_t i;
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

string_t sss_string_titlecased(string_t s) {
    char *s2 = GC_MALLOC_ATOMIC(s.length + 1);
    bool should_uppercase = true;
    for (int32_t i = 0; i < s.length; i++) {
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

bool sss_string_starts_with(string_t s, string_t prefix) {
    if (s.length < prefix.length) return false;
    for (int32_t i = 0; i < prefix.length; i++) {
        if (s.data[i*s.stride] != prefix.data[i*prefix.stride])
            return false;
    }
    return true;
}

bool sss_string_ends_with(string_t s, string_t suffix) {
    if (s.length < suffix.length) return false;
    for (int32_t i = 0; i < suffix.length; i++) {
        if (s.data[(s.length-suffix.length+i)*s.stride] != suffix.data[i*suffix.stride])
            return false;
    }
    return true;
}

string_t sss_string_trimmed(string_t s, string_t trim_chars, bool trim_left, bool trim_right)
{
    int32_t len = s.length;
    int32_t start = 0;
    if (trim_left) {
        for (; start < s.length; start++) {
            for (int32_t t = 0; t < trim_chars.length; t++) {
                if (s.data[start*s.stride] == trim_chars.data[t*trim_chars.stride])
                    goto found_ltrim;
            }
            goto done_trimming_left;
          found_ltrim:
            --len;
        }
    }
  done_trimming_left:;
    if (trim_right) {
        while (len > 0) {
            for (int32_t t = 0; t < trim_chars.length; t++) {
                if (s.data[(start+len-1)*s.stride] == trim_chars.data[t*trim_chars.stride])
                    goto found_rtrim;
            }
            goto done_trimming_right;
          found_rtrim:
            --len;
        }
    }
  done_trimming_right:;
    char *buf = GC_MALLOC_ATOMIC(len+1);
    char *ptr = buf;
    for (int32_t i = start; i < start+len; i++)
        *(ptr++) = s.data[i*s.stride];
    return (string_t){.data=buf, .length=len, .stride=1};
}

string_t sss_string_slice(string_t s, range_t *r) {
    int32_t stride = (int32_t)r->stride;
    int32_t first = (int32_t)CLAMP(r->first-1, 0, (int64_t)s.length-1),
            last = (int32_t)CLAMP(r->last-1, 0, (int64_t)s.length-1);
    int32_t slice_len = (last - first)/stride;
    return (string_t){.data=&s.data[first*s.stride], .length=slice_len, .stride=stride};
}

string_t flatten(string_t str)
{
    if (str.stride == 1) return str;
    char *buf = GC_MALLOC_ATOMIC(str.length + 1);
    for (int32_t i = 0; i < str.length; i++)
        buf[i] = str.data[i*str.stride];
    return (string_t){.data=buf, .length=str.length, .stride=1};
}

const char *c_string(string_t str)
{
    char *buf = GC_MALLOC_ATOMIC(str.length + 1);
    for (int32_t i = 0; i < str.length; i++)
        buf[i] = str.data[i*str.stride];
    buf[str.length] = '\0';
    return buf;
}

string_t from_c_string(const char *str)
{
    size_t len = str ? strlen(str) : 0;
    if (len == 0) return (string_t){.length=0, .stride=0};
    char *buf = GC_MALLOC_ATOMIC(len + 1);
    memcpy(buf, str, len+1);
    return (string_t){.data=buf, .length=len, .stride=1};
}

int32_t sss_string_find(string_t str, string_t pat)
{
    if (str.length < pat.length) return 0;
    if (pat.length == 0) return 1;

    // For short strings, do naive approach:
    // if (str.length*pat.length < UCHAR_MAX) {
    for (int32_t s = 0; s < str.length; s++) {
        for (int32_t p = 0; p < pat.length; p++) {
            if (str.data[s*str.stride] != pat.data[p*pat.stride])
                goto not_a_match;
        }
        return s+1;
      not_a_match:;
    }
    return 0;
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

string_t sss_string_replace(string_t text, string_t pat, string_t replacement, int64_t limit) {
    text = flatten(text);
    pat = flatten(pat);
    replacement = flatten(replacement);
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

string_t sss_string_quoted(string_t text, const char *dsl, bool colorize) {
    char *buf;
    size_t size;
    FILE *mem = open_memstream(&buf, &size);
    if (colorize) fputs("\x1b[35m", mem);
    if (dsl && dsl[0]) fprintf(mem, "$%s", dsl);
    const char *escape_color = colorize ? "\x1b[1;34m" : "";
    const char *reset_color = colorize ? "\x1b[0;35m" : "";
    fputc('"', mem);
    for (int i = 0; i < text.length; i++) {
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

string_t sss_string_number_format(double d, int64_t precision) {
    int len = snprintf(NULL, 0, "%.*f", (int)precision, d);
    char *str = GC_MALLOC_ATOMIC(len + 1);
    snprintf(str, len+1, "%.*f", (int)precision, d);
    return (string_t){.data=str, .length=len, .stride=1};
}
string_t sss_string_number_format32(float f, int64_t precision) { return sss_string_number_format((double)f, precision); }

string_t sss_string_scientific_notation(double d, int64_t precision) {
    int len = snprintf(NULL, 0, "%.*e", (int)precision, d);
    char *str = GC_MALLOC_ATOMIC(len + 1);
    snprintf(str, len+1, "%.*e", (int)precision, d);
    return (string_t){.data=str, .length=len, .stride=1};
}
string_t sss_string_scientific_notation32(float f, int64_t precision) { return sss_string_scientific_notation((double)f, precision); }

string_t sss_string_int_format(int64_t i, int64_t digits) {
    int len = snprintf(NULL, 0, "%0*ld", (int)digits, i);
    char *str = GC_MALLOC_ATOMIC(len + 1);
    snprintf(str, len+1, "%0*ld", (int)digits, i);
    return (string_t){.data=str, .length=len, .stride=1};
}

string_t sss_string_hex(int64_t i, int64_t digits, bool uppercase, bool prefix) {
    const char *fmt = uppercase ? (prefix ? "0x%0.*lX" : "%0.*lX") : (prefix ? "0x%0.*lx" : "%0.*lx");
    int len = snprintf(NULL, 0, fmt, (int)digits, (uint64_t)i);
    char *str = GC_MALLOC_ATOMIC(len + 1);
    snprintf(str, len+1, fmt, (int)digits, (uint64_t)i);
    return (string_t){.data=str, .length=len, .stride=1};
}

string_t sss_string_octal(int64_t i, int64_t digits, bool prefix) {
    const char *fmt = prefix ? "0o%0.*lo" : "%0.*lo";
    int len = snprintf(NULL, 0, fmt, (int)digits, (uint64_t)i);
    char *str = GC_MALLOC_ATOMIC(len + 1);
    snprintf(str, len+1, fmt, (int)digits, (uint64_t)i);
    return (string_t){.data=str, .length=len, .stride=1};
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
