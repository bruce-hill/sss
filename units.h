// Units of measure
#pragma once

typedef struct conversion_s {
    const char* derived;
    const char* base;
    double ratio;
    struct conversion_s *next;
} derived_units_t;

const char* unit_string(const char *str);
const char* unit_string_mul(const char *a, const char *b);
const char* unit_string_div(const char *a, const char *b);
const char* unit_derive(const char* unit, double *d, derived_units_t *conversions);
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
