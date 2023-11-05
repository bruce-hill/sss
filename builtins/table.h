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

void *Table_get(const Type *type, const table_t *t, const void *key);
void *Table_get_raw(const Type *type, const table_t *t, const void *key);
void *Table_nth(const Type *type, const table_t *t, uint32_t n);
void *Table_set(const Type *type, table_t *t, const void *key, const void *value);
void Table_remove(const Type *type, table_t *t, const void *key);
void Table_clear(table_t *t);
bool Table_equals(const Type *type, const table_t *x, const table_t *y);

void *Table_nths(const table_t *t, uint32_t n);
void *Table_gets(const table_t *t, const char *key);
void *Table_gets_raw(const table_t *t, const char *key);
void *Table_sets(table_t *t, const char *key, const void *value);
void Table_removes(table_t *t, const char *key);

Type make_table_type(Type *key, Type *value);
