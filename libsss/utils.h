#pragma once

#include <stdbool.h>
#include "string.h"

typedef struct { string_t *items; int32_t length, stride; } str_array_t;
str_array_t arg_list(int argc, char *argv[]);
void say(string_t str, string_t end);
void fail(const char *fmt, ...);
void fail_array(string_t fmt, ...);
double sane_fmod(double num, double modulus);
string_t last_err();
