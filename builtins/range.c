
#include <gc.h>
#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/param.h>
#include <err.h>

#include "range.h"
#include "types.h"
#include "../SipHash/halfsiphash.h"

extern const void *SSS_HASH_VECTOR;

static CORD Range_cord(const Type *type, const range_t *range, bool colorize) {
    (void)type;
    CORD c = NULL;
    if (colorize) c = "\x1b[0;35m";
    if (range->first != INT64_MIN)
        CORD_sprintf(&c, "%r%ld", c, range->first);

    if (colorize) c = CORD_cat(c, "\x1b[33m");
    c = CORD_cat(c, "..");

    if (range->last != INT64_MAX) {
        if (colorize) c = CORD_cat(c, "\x1b[0;35m");
        CORD_sprintf(&c, "%r%ld", c, range->last);
    }

    if (range->stride != 1) {
        if (colorize) c = CORD_cat(c, "\x1b[0;33m");
        c = CORD_cat(c, " by ");
        if (colorize) c = CORD_cat(c, "\x1b[35m");
        CORD_sprintf(&c, "%r%ld", c, range->stride);
    }

    if (colorize) c = CORD_cat(c, "\x1b[m");
    return c;
}

static int Range_compare(range_t *x, range_t *y) {
    if (x->first != y->first)
        return x->first > y->first ? 1 : -1;
    if (x->last != y->last)
        return x->last > y->last ? 1 : -1;
    return (x->stride > y->stride) - (x->stride < y->stride);
}

Type Range_type = {
    .name="Range",
    .cord=CordMethod(Function, (void*)Range_cord),
    .order=OrderingMethod(Function, (void*)Range_compare),
    .hash=HashMethod(Data, sizeof(range_t)),
};

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
