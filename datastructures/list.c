#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <err.h>
#include <gc.h>
#include "list.h"

list_t *list_new(size_t item_size, size_t min_items) {
    list_t *list = GC_MALLOC(sizeof(list_t));
    list->items = GC_MALLOC(item_size * min_items);
    list->slack = (uint8_t)min_items;
    return list;
}

list_t *list_new_items(size_t item_size, size_t len, void *items) {
    list_t *list = GC_MALLOC(sizeof(list_t));
    list->items = GC_MALLOC(item_size * len);
    list->len = len;
    memcpy(list->items, items, item_size * len);
    return list;
}

void list_insert_all(list_t *list, size_t item_size, int64_t index, list_t *other, const char *err_fmt) {
    if (index == INT_NIL) index = list->len + 1;
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

void list_insert(list_t *list, size_t item_size, int64_t index, void *item, const char *err_fmt) {
    if (index == INT_NIL) index = list->len + 1;
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

void list_remove(list_t *list, size_t item_size, int64_t first, int64_t last, const char *err_fmt) {
    if (first == INT_NIL) first = list->len;
    if (last == INT_NIL) last = first;

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
    list_t *copy = GC_MALLOC(sizeof(list_t));
    copy->len = l->len;
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

list_t *list_slice(list_t *list, int64_t first, int64_t last, int64_t step, size_t list_item_size, bool allow_aliasing) {
    list_t *slice = GC_MALLOC(sizeof(list_t));
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
        slice->items = list->items + (first-1)*list_item_size;
    } else if (len > 0) {
        void *p = GC_MALLOC(len * list_item_size);
        slice->items = p;
        void *src_items = list->items;
        if (step == 1) {
            memcpy(slice->items, list->items + (first-1)*list_item_size, len * list_item_size);
        } else {
            int64_t actual_len = 0;
            for (int64_t i = first; step > 0 ? (i <= last) : (i >= last); i += step) {
                actual_len += 1;
                p = mempcpy(p, src_items + (i - 1)*list_item_size, list_item_size);
            }
            slice->len = actual_len;
        }
    }
    return slice;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
