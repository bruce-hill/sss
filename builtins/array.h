#pragma once
#include <stdbool.h>
#include <gc/cord.h>

#include "../util.h"
#include "datatypes.h"
#include "functions.h"
#include "range.h"
#include "types.h"

void Array_insert(array_t *arr, const void *item, int64_t index, size_t item_size);
void Array_insert_all(array_t *arr, array_t to_insert, int64_t index, size_t item_size);
void Array_remove(array_t *arr, int64_t index, int64_t count, size_t item_size);
void Array_sort(array_t *arr, const Type *type);
void Array_shuffle(array_t *arr, size_t item_size);
void Array_clear(array_t *array);
void Array_compact(array_t *arr, size_t item_size);
bool Array_contains(array_t array, void *item, const Type *type);
array_t Array_slice(array_t *array, range_t range, bool readonly, const Type *type);
uint32_t Array_hash(const array_t *arr, const Type *type);
int32_t Array_compare(const array_t *x, const array_t *y, const Type *type);
bool Array_equal(const array_t *x, const array_t *y, const Type *type);
CORD Array_cord(const array_t *arr, bool colorize, const Type *type);

Type *make_array_type(Type *item_type);

#define ARRAY_OF(t) t**
#define EMPTY_ARRAY(t) (t**)new(array_t)
#define LENGTH(arr) (((array_t*)(arr))->length)
#define ARRAY(x, ...) (__typeof(x)**)new(array_t, \
    .data=memcpy(GC_MALLOC(sizeof((__typeof(x)[]){x, __VA_ARGS__})), (__typeof(x)[]){x, __VA_ARGS__}, \
                 sizeof((__typeof(x)[]){x, __VA_ARGS__})), \
    .length=(sizeof((__typeof(x)[]){x, __VA_ARGS__})) / sizeof(x), \
    .stride=sizeof(x))
#define STATIC_ARRAY(x, ...) ((array_t){ \
    .data=(__typeof(x)[]){x, __VA_ARGS__}, \
    .length=(sizeof((__typeof(x)[]){x, __VA_ARGS__})) / sizeof(x), \
    .stride=sizeof(x)})
#define foreach(arr, var, end) for (__typeof(arr[0]) var = arr[0], end = ith_addr(arr, LENGTH(arr)); var != end; var = ((void*)var) + ((array_t*)(arr))->stride)
#define ith_addr(arr, i) ((__typeof(arr[0]))(((array_t*)(arr))->data + (i)*((array_t*)(arr))->stride))
#define ith(arr, i) (*ith_addr(arr,i))
#define append(arr, obj) Array_insert((array_t*)(arr), (__typeof(obj)[]){obj}, 0, sizeof(obj))
#define remove(arr, i) Array_remove((array_t*)(arr), (i)+1, 1, sizeof(arr[0][0]))

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
