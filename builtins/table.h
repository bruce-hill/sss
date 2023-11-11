#pragma once
#include <stdalign.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "types.h"
#include "array.h"

typedef struct {
    uint32_t occupied:1, index:31;
    uint32_t next_bucket;
} bucket_t;

typedef struct {
    uint32_t count:32, last_free:31;
    uint8_t copy_on_write:1;
    bucket_t buckets[0];
} bucket_info_t;

typedef struct table_s {
    array_t entries;
    bucket_info_t *bucket_info;
    struct table_s *fallback;
    void *default_value;
} table_t;

table_t Table_from_entries(const Type *type, array_t entries);
void *Table_get(const Type *type, const table_t *t, const void *key);
void *Table_get_raw(const Type *type, const table_t *t, const void *key);
void *Table_entry(const Type *type, const table_t *t, uint32_t n);
void *Table_set(const Type *type, table_t *t, const void *key, const void *value);
void Table_remove(const Type *type, table_t *t, const void *key);
void Table_clear(table_t *t);
void Table_mark_copy_on_write(table_t *t);
bool Table_equals(const table_t *x, const table_t *y, const Type *type);

void *Table_str_entry(const table_t *t, uint32_t n);
void *Table_str_get(const table_t *t, const char *key);
void *Table_str_get_raw(const table_t *t, const char *key);
void *Table_str_set(table_t *t, const char *key, const void *value);
void Table_str_remove(table_t *t, const char *key);

#define Table_length(t) ((t)->entries.length)

Type *make_table_type(Type *key, Type *value);
