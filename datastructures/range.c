#include <stdint.h>
#include <gc.h>

#include "../util.h"
#include "range.h"

int64_t RANGE_MIN = -999999999999999999;
int64_t RANGE_MAX = +999999999999999999;

range_t *range_new(int64_t first, int64_t next, int64_t last) {
    if (next != first && last != first) {
        int64_t len = (last - first) / (next - first);
        last = first + len * (next - first);
    }
    return new(range_t, .first=first, .next=next, .last=last);
}

range_t *range_new_first_last(int64_t first, int64_t last) {
    return new(range_t, .first=first, .next=first <= last ? first+1 : first-1, .last=last);
}

range_t *range_new_first_next(int64_t first, int64_t next) {
    return new(range_t, .first=first, .next=next, .last=next >= first ? RANGE_MAX : RANGE_MIN);
}

int64_t range_len(range_t *r) {
    int64_t len = (r->last - r->first) / (r->next - r->first);
    return (len < 0) ? 0 : len + 1;
}

int64_t range_nth(range_t *r, int64_t n) {
    int64_t step = r->next - r->first;
    return r->first + (n-1)*step;
}

int64_t range_step(range_t *r) {
    return r->next - r->first;
}

range_t *range_backwards(range_t *src) {
    int64_t step = src->next - src->first;
    return range_new(src->last, src->last - step, src->first);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
