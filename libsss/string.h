#pragma once
#include <stdbool.h>
#include <stdint.h>
#include "range.h"

typedef struct {
    const char *data;
    int32_t length;
    int16_t stride, free;
} string_t;

string_t sss_string_uppercased(string_t s);
string_t sss_string_lowercased(string_t s);
string_t sss_string_capitalized(string_t s);
string_t sss_string_titlecased(string_t s);
bool sss_string_starts_with(string_t s, string_t prefix);
bool sss_string_ends_with(string_t s, string_t suffix);
string_t sss_string_trimmed(string_t s, string_t trim_chars, bool trim_left, bool trim_right);
string_t sss_string_slice(string_t s, range_t *r);
string_t flatten(string_t str);
string_t from_c_string(const char *str);
const char *c_string(string_t str);
int32_t sss_string_find(string_t str, string_t pat);
string_t sss_string_replace(string_t text, string_t pat, string_t replacement, int64_t limit);
string_t sss_string_quoted(string_t text, const char *dsl, bool colorize);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
