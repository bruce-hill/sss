
#include <gc.h>
#include <gc/cord.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/param.h>
#include <err.h>

#include "types.h"
#include "../SipHash/halfsiphash.h"

extern const void *SSS_HASH_VECTOR;

static CORD Memory_cord(const void *p, bool colorize, const Type *type) {
    (void)type;
    CORD cord;
    CORD_sprintf(&cord, colorize ? "\x1b[0;34;1mMemory<%p>\x1b[m" : "Memory<%p>", p);
    return cord;
}

static int Memory_compare(void *x, void *y) {
    return (x > y) - (x < y);
}

static int Memory_hash(void *p) {
    int hash;
    halfsiphash(&p, sizeof(void*), SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
    return hash;
}

Type Memory_type = {
    .name=STRING("Memory"),
    .size=sizeof(void*),
    .align=alignof(void*),
    .cord=CordMethod(Function, (void*)Memory_cord),
    .order=OrderingMethod(Function, (void*)Memory_compare),
    .hash=HashMethod(Function, (void*)Memory_hash),
};

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
