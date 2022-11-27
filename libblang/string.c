#include <assert.h>
#include <bp/match.h>
#include <bp/pattern.h>
#include <bp/printmatch.h>
#include <ctype.h>
#include <err.h>
#include <gc.h>
#include <gc/cord.h>
#include <intern.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>

#include "string.h"

#define CLAMP(x, lo, hi) MIN(hi, MAX(x,lo))

istr_t bl_string_capitalized(istr_t s) {
    char *s2 = strdup(s);
    char *p = s2;
    *p = toupper(*p);
    for (++p; *p; ++p)
        *p = tolower(*p);
    istr_t ret = intern_str(s2);
    free(s2);
    return ret;
}

istr_t bl_string_titlecased(istr_t s) {
    char *s2 = strdup(s);
    for (char *p = s2; *p; ++p) {
        if (isalpha(*p)) {
            *p = toupper(*p);
            for (++p; *p && isalpha(*p); ++p)
                *p = tolower(*p);
        }
    }
    istr_t ret = intern_str(s2);
    free(s2);
    return ret;
}

istr_t bl_string_uppercased(istr_t s) {
    char *s2 = strdup(s);
    for (char *p = s2; *p; p++)
        *p = toupper(*p);
    istr_t ret = intern_str(s2);
    free(s2);
    return ret;
}

istr_t bl_string_lowercased(istr_t s) {
    char *s2 = strdup(s);
    for (char *p = s2; *p; p++)
        *p = tolower(*p);
    istr_t ret = intern_str(s2);
    free(s2);
    return ret;
}

bool bl_string_starts_with(istr_t s, istr_t prefix) {
    while (*prefix) {
        if (*prefix++ != *s++)
            return false;
    }
    return true;
}

bool bl_string_ends_with(istr_t s, istr_t suffix) {
    size_t s_len = strlen(s);
    size_t suffix_len = strlen(suffix);
    if (suffix_len > s_len) return false;
    return memcmp(s+(s_len-suffix_len), suffix, suffix_len) == 0;
}

istr_t bl_string_trimmed(istr_t s, istr_t trim_chars, bool trim_left, bool trim_right)
{
    if (trim_chars == NULL) trim_chars = " \n\r\t";
    const char *start = s;
    if (trim_left)
        start += strspn(s, trim_chars);
    size_t len = strlen(start);
    if (trim_right) {
        while (strspn(start + len - 1, trim_chars))
            --len;
    }
    const char *buf = strndupa(start, len);
    return intern_str(buf);
}

typedef struct {
    int64_t first,step,last;
} range_t;
istr_t bl_string_slice(istr_t s, range_t *r) {
    int64_t step = r->step;
    if (step == 0) return intern_str("");

    int64_t len = (int64_t)strlen(s);
    int64_t first = CLAMP(r->first-1, 0, len-1), last = CLAMP(r->last-1, 0, len-1);
    int64_t slice_len = 0;
    for (int64_t i = first; step > 0 ? i <= last : i >= last; i += step)
        ++slice_len;
    char *buf = calloc(slice_len+1, 1);
    assert(buf);
    for (int64_t i = first, b_i = 0; step > 0 ? i <= last : i >= last; i += step)
        buf[b_i++] = s[i];
    istr_t ret = intern_str(buf);
    free(buf);
    return ret;
}
