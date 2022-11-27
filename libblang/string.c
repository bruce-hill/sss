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

istr_t bl_string_replace(char *text, char *pat_text, char *rep_text, int64_t *limit_option) {
    maybe_pat_t maybe_pat = bp_stringpattern(pat_text, pat_text + strlen(pat_text));
    if (!maybe_pat.success) {
        return text;
    }
    pat_t *pat = maybe_pat.value.pat;

    maybe_pat_t maybe_replacement = bp_replacement(pat, rep_text, rep_text + strlen(rep_text));
    if (!maybe_replacement.success) {
        return text;
    }

    char *buf = NULL;
    size_t size = 0;
    FILE *out = open_memstream(&buf, &size);
    const char *prev = text;
    pat_t *rep_pat = maybe_replacement.value.pat;
    size_t textlen = strlen(text);
    int64_t limit = limit_option ? *limit_option : (int64_t)textlen + 1;
    if (limit > 0) {
        for (match_t *m = NULL; next_match(&m, text, &text[textlen], rep_pat, NULL, NULL, false); ) {
            fwrite(prev, sizeof(char), (size_t)(m->start - prev), out);
            fprint_match(out, text, m, NULL);
            prev = m->end;
            if (--limit == 0) {
                stop_matching(&m);
                break;
            }
        }
    }
    fwrite(prev, sizeof(char), (size_t)(&text[textlen] - prev) + 1, out);
    fflush(out);
    istr_t replaced = buf ? intern_str(buf) : intern_str("");
    fclose(out);
    return replaced;
}

// const char *bl_string_match(char *text, char *pat_text) {
//     maybe_pat_t maybe_pat = bp_pattern(pat_text, pat_text + strlen(pat_text));
//     if (!maybe_pat.success) {
//         return intern_str("");
//     }

//     char *buf = NULL;
//     size_t size = 0;
//     FILE *out = open_memstream(&buf, &size);
//     size_t textlen = strlen(text);
//     pat_t *pat = maybe_pat.value.pat;
//     for (match_t *m = NULL; next_match(&m, text, &text[textlen], pat, NULL, NULL, false); ) {
//         fprint_match(out, text, m, NULL);
//         stop_matching(&m);
//         break;
//     }
//     fflush(out);
//     const char *match = buf ? intern_str(buf) : intern_str("");
//     fclose(out);
//     return match;
// }

bool bl_string_matches(char *text, char *pat_text) {
    maybe_pat_t maybe_pat = bp_stringpattern(pat_text, pat_text + strlen(pat_text));
    if (!maybe_pat.success) {
        return intern_str("");
    }

    size_t textlen = strlen(text);
    pat_t *pat = maybe_pat.value.pat;
    match_t *m = NULL;
    if (next_match(&m, text, &text[textlen], pat, NULL, NULL, false)) {
        stop_matching(&m);
        return true;
    } else {
        return false;
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
