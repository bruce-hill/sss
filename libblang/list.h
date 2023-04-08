#pragma once
#include <assert.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

typedef struct {
    void *items;
    int64_t len;
    uint8_t slack;
} list_t;

list_t *list_new(size_t item_size, size_t min_items);
list_t *list_new_items(size_t item_size, size_t len, void *items);
void list_append(list_t *list, size_t item_size, void *item);
void list_insert(list_t *list, size_t item_size, int64_t index, void *item, const char *err_fmt);
void list_remove(list_t *list, size_t item_size, int64_t index, int64_t count, const char *err_fmt);

#define List(t) t**
#define EMPTY_LIST(t) ((t**)list_new(sizeof(t), 0))
#define LIST(t, ...) ((t**)list_new_items(sizeof(t), sizeof((t[]){__VA_ARGS__})/sizeof(t), (t[]){__VA_ARGS__}))
#define NEW_LIST(t, x) t** x = (t**)list_new(sizeof(t), 8)
#define stringify(x) #x
#define APPEND(list, item) list_append((list_t*)list, sizeof(list[0][0]), &(__typeof__ (list[0][0])){item})
#define APPEND_STRUCT(list, item) list_append((list_t*)list, sizeof(list[0][0]), &item)
#define LIST_LEN(list) (((list_t*)(list))->len)
#define LIST_ITEM(list, i) (assert(/* Check list index */ (int64_t)i >= 0 && (int64_t)i < LIST_LEN(list)), (list)[0][i])
#define LIST_FIRST(list) ((list)[0])
#define LIST_END(list) ((list)[0] + LIST_LEN(list) - 1)
#define LIST_FOR(list, item, last) for (__typeof__ (list[0]) item = LIST_FIRST(list), last = LIST_END(list); item <= last; item++)
#define LIST_REMOVE(list, i) list_remove((list_t*)list, sizeof(list[0][0]), i, 1, "Not a valid removal: %ld (%ld items)")

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
