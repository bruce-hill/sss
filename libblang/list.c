#include <err.h>
#include <gc.h>
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

void list_remove(list_t *list, size_t item_size, int64_t index, int64_t count, const char *err_fmt) {
    if (__builtin_expect((index < 0) | (count <= 0) | (index + count > list->len), 0))
        errx(1, err_fmt, index, count);

    list->slack += count;
    memmove((char*)list->items + item_size*index, (char*)list->items + item_size*(index+count),
            item_size*(list->len - (index+count)));
    list->len -= count;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
