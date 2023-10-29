#pragma once

#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>

#include "string.h"

void say(string_t str, string_t end);
void fail(const char *fmt, ...);
void fail_array(string_t fmt, ...);
string_t last_err();
void sss_doctest(const char *label, CORD expr, const char *type, bool use_color, const char *expected, const char *filename, int start, int end);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
