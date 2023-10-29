#pragma once
#include <stdalign.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "types.h"

typedef struct {
    uint32_t index1, next1;
} hash_bucket_t;

#define CAPACITY(t) ((t)->buckets ? (t)->buckets[0].index1 : (uint32_t)0)
#define LAST_FREE(t) ((t)->buckets[0].next1)

typedef struct table_s {
    void *entries;
    hash_bucket_t *buckets;
    struct table_s *fallback;
    void *default_value;
    uint32_t count;
    bool copy_on_write;
} table_t;

void *table_get(Type *type, table_t *t, const void *key);
void *table_get_raw(Type *type, table_t *t, const void *key);
void *table_nth(Type *type, table_t *t, uint32_t n);
void *table_set(Type *type, table_t *t, const void *key, const void *value);
void table_remove(Type *type, table_t *t, const void *key);
void table_clear(table_t *t);
bool table_equals(Type *type, table_t *x, table_t *y);
