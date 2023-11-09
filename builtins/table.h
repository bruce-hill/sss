#pragma once
#include <stdalign.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "types.h"

typedef struct {
    uint32_t occupied:1, index:31;
    uint32_t next_bucket;
} hash_bucket_t;

typedef struct {
    uint32_t count:32, last_free:31;
    uint8_t copy_on_write:1;
    hash_bucket_t buckets[0];
} hash_buckets_t;

typedef struct {
    uint32_t count:32, space:31;
    uint8_t copy_on_write:1;
    char entries[0];
} entries_t;

typedef struct table_s {
    entries_t *entry_info;
    hash_buckets_t *bucket_info;
    struct table_s *fallback;
    void *default_value;
} table_t;

void *Table_get(const Type *type, const table_t *t, const void *key);
void *Table_get_raw(const Type *type, const table_t *t, const void *key);
void *Table_entry(const Type *type, const table_t *t, uint32_t n);
void *Table_set(const Type *type, table_t *t, const void *key, const void *value);
void Table_remove(const Type *type, table_t *t, const void *key);
void Table_clear(table_t *t);
void Table_mark_copy_on_write(table_t *t);
bool Table_equals(const Type *type, const table_t *x, const table_t *y);

void *Table_entrys(const table_t *t, uint32_t n);
void *Table_gets(const table_t *t, const char *key);
void *Table_gets_raw(const table_t *t, const char *key);
void *Table_sets(table_t *t, const char *key, const void *value);
void Table_removes(table_t *t, const char *key);

#define Table_length(t) ((t)->entry_info ? (t)->entry_info->count : 0)

Type make_table_type(Type *key, Type *value);
