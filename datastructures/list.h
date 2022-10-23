#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#define INT_NIL 0x7FFFFFFFFFFFFFFF

typedef struct {
    void *items;
    int64_t len;
    uint8_t slack;
} list_t;

list_t *list_new(size_t item_size, size_t min_items);
void list_insert_all(list_t *list, size_t item_size, int64_t index, list_t *other, const char *err_fmt);
void list_insert(list_t *list, size_t item_size, int64_t index, void *item, const char *err_fmt);
void list_remove(list_t *list, size_t item_size, int64_t first, int64_t last, const char *err_fmt);
bool list_equal(list_t *a, list_t *b, size_t item_size);
list_t *list_copy(list_t *l, size_t item_size);
void list_clear(list_t *l);
list_t *list_slice(list_t *list, int64_t first, int64_t last, int64_t step, size_t list_item_size, bool allow_aliasing);

#define List(t, x) t** x
#define NEW_LIST(t, x) t** x = (t**)list_new(sizeof(t), 8)
#define stringify(x) #x
#define APPEND(list, item) list_insert((list_t*)list, sizeof(list[0][0]), INT_NIL, &(typeof (list[0][0])){item}, "Invalid list index: %ld")
#define LIST_LEN(list) (((list_t*)(list))->len)
#define LIST_ITEM(list, i) ((list)[0][i])

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
