#include <stdbool.h>
#include <assert.h>
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

string_t bl_string_uppercased(string_t s) {
    char *s2 = GC_MALLOC_ATOMIC(s.length + 1);
    for (int32_t i = 0; i < s.length; i++)
        s2[i] = toupper(s.data[i*s.stride]);
    return (string_t){.data=s2, .length=s.length, .stride=1};
}

string_t bl_string_lowercased(string_t s) {
    char *s2 = GC_MALLOC_ATOMIC(s.length + 1);
    for (int32_t i = 0; i < s.length; i++)
        s2[i] = tolower(s.data[i*s.stride]);
    return (string_t){.data=s2, .length=s.length, .stride=1};
}

string_t bl_string_capitalized(string_t s) {
    char *s2 = GC_MALLOC_ATOMIC(s.length + 1);
    int32_t i;
    for (i = 0; i < s.length; i++) {
        if (isalpha(s.data[i*s.stride])) {
            s2[i] = toupper(s.data[i*s.stride]);
            break;
        }
    }
    for (; i < s.length; i++)
        s2[i] = tolower(s.data[i*s.stride]);
    return (string_t){.data=s2, .length=s.length, .stride=1};
}

string_t bl_string_titlecased(string_t s) {
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
        }
    }
    return (string_t){.data=s2, .length=s.length, .stride=1};
}

bool bl_string_starts_with(string_t s, string_t prefix) {
    if (s.length < prefix.length) return false;
    for (int32_t i = 0; i < prefix.length; i++) {
        if (s.data[i*s.stride] != prefix.data[i*prefix.stride])
            return false;
    }
    return true;
}

bool bl_string_ends_with(string_t s, string_t suffix) {
    if (s.length < suffix.length) return false;
    for (int32_t i = 0; i < suffix.length; i++) {
        if (s.data[(s.length-suffix.length+i)*s.stride] != suffix.data[i*suffix.stride])
            return false;
    }
    return true;
}

string_t bl_string_trimmed(string_t s, string_t trim_chars, bool trim_left, bool trim_right)
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

typedef struct {
    int64_t first,step,last;
} range_t;
string_t bl_string_slice(string_t s, range_t *r) {
    int32_t step = (int32_t)r->step;
    int32_t first = (int32_t)CLAMP(r->first-1, 0, (int64_t)s.length-1),
            last = (int32_t)CLAMP(r->last-1, 0, (int64_t)s.length-1);
    int32_t slice_len = (last - first)/step;
    return (string_t){.data=&s.data[first*s.stride], .length=slice_len, .stride=step};
}

string_t flatten(string_t str)
{
    if (str.stride == 1) return str;
    char *buf = GC_MALLOC_ATOMIC(str.length + 1);
    for (int32_t i = 0; i < str.length; i++)
        buf[i] = str.data[i*str.stride];
    return (string_t){.data=buf, .length=str.length, .stride=1};
}

int32_t bl_string_find(string_t str, string_t pat)
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

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
