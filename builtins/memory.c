
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

Type Memory_type = {
    .name="Memory",
    .size=0,
    .align=0,
    .tag=VTableInfo,
    .__data.VTableInfo={.cord=(void*)Memory_cord},
};

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
