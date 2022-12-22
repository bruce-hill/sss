#include <err.h>
#include <gc.h>
#include <intern.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "../util.h"
#include "list.h"

list_t *list_new(size_t item_size, size_t min_items) {
    return new(list_t, .items=GC_MALLOC(item_size * min_items), .slack=(uint8_t)min_items);
}

list_t *list_new_items(size_t item_size, size_t len, void *items) {
    list_t *list = new(list_t, .items=GC_MALLOC(item_size * len), .len=len);
    memcpy(list->items, items, item_size * len);
    return list;
}

void list_append(list_t *list, size_t item_size, void *item) {
    if (list->slack > 0) {
        list->slack -= 1;
    } else {
        char *old_items = list->items;
        list->slack = 8;
        list->items = GC_MALLOC(item_size * (list->len + 1 + list->slack));
        memcpy(list->items, old_items, item_size*list->len);
    }
    memcpy(list->items + item_size * list->len, item, item_size);
    list->len += 1;
}

void list_insert(list_t *list, size_t item_size, int64_t index, void *item, const char *err_fmt) {
    if (index == 0) index = list->len + 1;
    else if (__builtin_expect((index < 1) | (index > list->len + 1), 0))
        errx(1, err_fmt, index);

    if (index == list->len + 1 && list->slack >= 1) {
        list->slack -= 1;
    } else {
        char *old_items = list->items;
        list->slack = 8;
        list->items = GC_MALLOC(item_size * (list->len + 1 + list->slack));
        memcpy(list->items, old_items, item_size*(index-1));
        memcpy(list->items+item_size*index, old_items+item_size*(index-1), item_size*(list->len - (index-1)));
    }
    memcpy(list->items + item_size * (index-1), item, item_size);
    list->len += 1;
}

void list_insert_all(list_t *list, size_t item_size, int64_t index, list_t *other, const char *err_fmt) {
    if (index == 0) index = list->len + 1;
    else if (__builtin_expect((index < 1) | (index > list->len + 1), 0))
        errx(1, err_fmt, index);

    if (index == list->len + 1 && list->slack >= other->len) {
        memcpy(list->items + (index-1)*item_size, other->items, other->len*item_size);
        list->slack -= other->len;
    } else {
        char *old_items = list->items;
        list->items = GC_MALLOC(item_size * (list->len + other->len));
        list->slack = 0;
        memcpy(list->items, old_items, item_size*(index-1));
        memcpy(list->items + (index-1)*item_size, other->items, other->len*item_size);
        memcpy(list->items + (index-1 + other->len)*item_size, old_items+item_size*(index-1), item_size*(list->len - (index-1)));
    }
    list->len += other->len;
}

void list_remove(list_t *list, size_t item_size, int64_t first, int64_t last, const char *err_fmt) {
    if (first == 0) first = list->len;
    if (last == 0) last = first;

    if (__builtin_expect((first < 1) | (last < first) | (last > list->len), 0))
        errx(1, err_fmt, first, last);

    int64_t to_remove = last - first + 1;
    if (to_remove <= 0) to_remove = 0;

    // Even if there is free memory, writing there would break invariants (like 
    // preserving slice/iteration content), so it's no longer slack
    list->slack = 0;

    if (last == list->len) {
        list->len -= to_remove;
        if (list->len == 0)
            list->items = NULL;
        return;
    }

    char *old_items = list->items;
    list->items = GC_MALLOC(item_size * (list->len - to_remove));
    if (first > 1)
        memcpy(list->items, old_items, item_size*(first-1));
    memcpy(list->items + (first-1)*item_size, old_items+item_size*last, item_size*(list->len - last));
    list->len -= to_remove;
}

bool list_equal(list_t *a, list_t *b, size_t item_size) {
    return a == b || ((a->len | b->len) == 0) || (a->len == b->len && memcmp(a->items, b->items, a->len*item_size) == 0);
}

list_t *list_copy(list_t *l, size_t item_size) {
    list_t *copy = new(list_t, .len=l->len);
    if (l->len > 0) {
        copy->items = GC_MALLOC(item_size*l->len);
        memcpy(copy->items, l->items, item_size*l->len);
    }
    return copy;
}

void list_clear(list_t *l) {
    l->len = 0;
    l->items = NULL;
}

list_t *list_slice(list_t *list, int64_t first, int64_t last, int64_t step, size_t item_size, bool allow_aliasing) {
    list_t *slice = new(list_t);
    if (step > 0) {
        if (first > list->len) return slice;
        if (first < 1) {
            first = first % step;
            if (first < 1) first += step;
        }
        if (last > list->len) last = list->len;
        if (last < first) return slice;
    } else {
        if (first < 1) return slice;
        if (first > list->len) {
            first = list->len - (list->len % -step) + (first % -step);
            if (first > list->len) first += step;
        }
        if (last < 1) last = 1;
        if (last > first) return slice;
    }
    int64_t len = ((last+step) - first) / step;
    if (len <= 0) len = 0;
    slice->len = len;

    if (step == 1 && allow_aliasing) {
        slice->items = list->items + (first-1)*item_size;
    } else if (len > 0) {
        void *p = GC_MALLOC(len * item_size);
        slice->items = p;
        void *src_items = list->items;
        if (step == 1) {
            memcpy(slice->items, list->items + (first-1)*item_size, len * item_size);
        } else {
            int64_t actual_len = 0;
            for (int64_t i = first; step > 0 ? (i <= last) : (i >= last); i += step) {
                actual_len += 1;
                p = mempcpy(p, src_items + (i - 1)*item_size, item_size);
            }
            slice->len = actual_len;
        }
    }
    return slice;
}

void list_sort(list_t *list, size_t item_size, int (*cmp)(const void*,const void*,void*), void *userdata)
{
    if (!cmp) {
        cmp = (int(*)(const void*,const void*,void*))(void*)memcmp;
        userdata = (void*)item_size;
    }

    qsort_r(list->items, list->len, item_size, cmp, userdata);
}

bool list_heap_pop(list_t *list, size_t item_size, void *ret, int (*cmp)(const void*,const void*,void*), void *userdata)
{
    if (list->len <= 0) return false;

    if (!cmp) {
        cmp = (int(*)(const void*,const void*,void*))(void*)memcmp;
        userdata = (void*)item_size;
    }

    char *items = (char*)list->items;
    memcpy(ret, items, item_size);
    char *last = items + item_size*(list->len-1);
    memcpy(items, last, item_size);
    list->len -= 1;
    bzero(items + list->len, item_size);
    for (int i = 0; i < (int)list->len; ) {
        int c1 = 2*i + 1,
            c2 = 2*i + 2;
        int swap_index = cmp(items+c1, items+c2, userdata) >= 0 ? c1 : c2;
        if (cmp(items+i, items+swap_index, userdata) >= 0)
            break;
        char tmp[item_size];
        memcpy(tmp, items+swap_index, item_size);
        memcpy(items+swap_index, items+i, item_size);
        memcpy(items+i, tmp, item_size);
        i = swap_index;
    }

    return true;
}

void list_heap_push(list_t *list, size_t item_size, int (*cmp)(const void*,const void*,void*), void *item, void *userdata)
{
    if (!cmp) {
        cmp = (int(*)(const void*,const void*,void*))(void*)memcmp;
        userdata = (void*)item_size;
    }

    list_insert(list, item_size, list->len, item, NULL);
    for (int64_t i = list->len-1; i > 0 && cmp(list->items+i/2,list->items+i,userdata) < 0; i /= 2) {
        char tmp[item_size];
        memcpy(tmp, list->items+i/2, item_size);
        memcpy(list->items+i/2, list->items+i, item_size);
        memcpy(list->items+i, tmp, item_size);
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
