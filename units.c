#include <ctype.h>
#include <gc.h>
#include <intern.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "libblang/list.h"

typedef struct {
    int exponent;
    istr_t name;
} component_t;

typedef struct {
    size_t len;
    component_t components[1];
} Unit;

static int cmp_component_exponents(const void *v1, const void *v2)
{
    component_t *a = (component_t*)v1, *b = (component_t*)v2;
    if (a->exponent != b->exponent)
        return (int)-(a->exponent - b->exponent);
    return strcmp(a->name, b->name);
}

static int cmp_component_names(const void *v1, const void *v2)
{
    component_t *a = (component_t*)v1, *b = (component_t*)v2;
    return strcmp(a->name, b->name);
}

static istr_t unit_to_string(Unit *u)
{
    if (u->len == 0) return NULL;
    char *buf;
    size_t len;
    FILE *f = open_memstream(&buf, &len);
    bool printed_numerator = false;
    bool printed_slash = false;
    bool printed_denominator = false;
    for (size_t i = 0; i < u->len; i++) {
        if (u->components[i].exponent == 0) continue;
        if (u->components[i].exponent > 0) {
            if (printed_numerator) fputs(" ", f);
            printed_numerator = true;
        } else {
            if (!printed_numerator) {
                fputs("1", f);
                printed_numerator = true;
            }
            if (!printed_slash) {
                fputs("/", f);
                printed_slash = true;
            }
            if (printed_denominator) fputs(" ", f);
            printed_denominator = true;
        }
        fputs(u->components[i].name, f);
        if (abs(u->components[i].exponent) > 1)
            fprintf(f, "^%d", abs(u->components[i].exponent));
    }
    fflush(f);
    istr_t ret = intern_strn(buf, len);
    fclose(f);
    free(buf);
    return ret;
}

static Unit *make_unit(size_t len, component_t components[len])
{
    component_t buf[len];
    memcpy(buf, components, len*sizeof(component_t));
    qsort(buf, len, sizeof(component_t), cmp_component_names);
    {
        Unit *u = GC_MALLOC(sizeof(Unit) - sizeof(component_t) + len*sizeof(component_t));
        u->len = len;
        memcpy(u->components, buf, sizeof(component_t)*len);
    }
    size_t merge_len = 0;
    for (size_t i = 0; i < len; ) {
        istr_t name = buf[i].name;
        int exponent = 0;
        for (; i < len && buf[i].name == name; ++i)
            exponent += buf[i].exponent;

        if (exponent != 0) {
            buf[merge_len] = (component_t){exponent, name};
            ++merge_len;
        }
    }
    qsort(buf, merge_len, sizeof(component_t), cmp_component_exponents);

    Unit *u = GC_MALLOC(sizeof(Unit) - sizeof(component_t) + merge_len*sizeof(component_t));
    u->len = merge_len;
    memcpy(u->components, buf, sizeof(component_t)*merge_len);
    return u;
}

static Unit *unit_from_string(const char *str)
{
    if (!str) str = "";
    NEW_LIST(component_t, components);
    bool in_denominator = false;
    for (const char *p = str; *p; ) {
        if (isalpha(*p)) {
            const char *name = p;
            while (isalpha(*p)) ++p;
            name = intern_strn(name, (size_t)(p - name));

            int exponent = 1;
            while (isspace(*p)) ++p;
            if (*p == '^') {
                ++p;
                while (isspace(*p)) ++p;
                exponent = (int)strtol(p, (char**)&p, 10);
            }
            if (in_denominator) exponent *= -1;

            component_t comp = {exponent, name};
            list_append((list_t*)components, sizeof(component_t), &comp);
        } else {
            if (*p == '/')
                in_denominator = true;
            ++p;
        }
    }
    size_t len = LIST_LEN(components);
    return make_unit(len, components[0]);
}

static Unit *unit_mul(Unit *a, Unit *b)
{
    size_t len = a->len + b->len;
    component_t buf[len];
    memcpy(buf, a->components, a->len * sizeof(component_t));
    memcpy(&buf[a->len], b->components, b->len * sizeof(component_t));
    return make_unit(len, buf);
}

static Unit *unit_div(Unit *a, Unit *b)
{
    size_t len = a->len + b->len;
    component_t buf[len];
    memcpy(buf, a->components, a->len * sizeof(component_t));
    memcpy(&buf[a->len], b->components, b->len * sizeof(component_t));
    for (size_t i = a->len; i < len; i++)
        buf[i].exponent *= -1;
    return make_unit(len, buf);
}

istr_t unit_string(const char *str)
{
    return unit_to_string(unit_from_string(str));
}

istr_t unit_string_mul(const char *a, const char *b)
{
    return unit_to_string(unit_mul(unit_from_string(a), unit_from_string(b)));
}

istr_t unit_string_div(const char *a, const char *b)
{
    return unit_to_string(unit_div(unit_from_string(a), unit_from_string(b)));
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
