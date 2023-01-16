// Units of measure using interned strings:
#pragma once
#include <intern.h>

typedef struct conversion_s {
    istr_t derived;
    istr_t base;
    double ratio;
    struct conversion_s *next;
} derived_units_t;

istr_t unit_string(const char *str);
istr_t unit_string_mul(const char *a, const char *b);
istr_t unit_string_div(const char *a, const char *b);
istr_t unit_derive(istr_t unit, double *d, derived_units_t *conversions);
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
