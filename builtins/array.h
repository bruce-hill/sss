#pragma once
#include <stdbool.h>
#include <gc/cord.h>

#include "string.h"
#include "types.h"
#include "functions.h"

typedef struct {
    void *data;
    unsigned long int length:42;
    unsigned short int free:4, cow:1, atomic:1;
    short int stride:16;
} array_t;

void Array_insert(array_t *arr, void *item, int64_t index, size_t item_size);
void Array_insert_all(array_t *arr, array_t to_insert, int64_t index, size_t item_size);
void Array_remove(array_t *arr, int64_t index, int64_t count, size_t item_size);
void Array_sort(Type *type, array_t *arr, size_t item_size);
void Array_shuffle(array_t *arr, size_t item_size);
array_t Array_join(array_t pieces, array_t glue, size_t item_size);
void Array_clear(array_t *array);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
