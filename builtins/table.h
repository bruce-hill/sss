#pragma once
#include <stdalign.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "types.h"
#include "datatypes.h"
#include "array.h"

table_t Table_from_entries(array_t entries, const Type *type);
void *Table_get(const table_t *t, const void *key, const Type *type);
void *Table_get_raw(const table_t *t, const void *key, const Type *type);
void *Table_entry(const table_t *t, int64_t n);
void *Table_reserve(table_t *t, const void *key, const void *value, const Type *type);
void Table_set(table_t *t, const void *key, const void *value, const Type *type);
void Table_remove(table_t *t, const void *key, const Type *type);
void Table_clear(table_t *t);
void Table_mark_copy_on_write(table_t *t);
int32_t Table_compare(const table_t *x, const table_t *y, const Type *type);
bool Table_equal(const table_t *x, const table_t *y, const Type *type);
uint32_t Table_hash(const table_t *t, const Type *type);
CORD Table_cord(const table_t *t, bool colorize, const Type *type);

void *Table_str_entry(const table_t *t, int64_t n);
void *Table_str_get(const table_t *t, const char *key);
void *Table_str_get_raw(const table_t *t, const char *key);
void Table_str_set(table_t *t, const char *key, const void *value);
void *Table_str_reserve(table_t *t, const char *key, const void *value);
void Table_str_remove(table_t *t, const char *key);

#define Table_length(t) ((t)->entries.length)

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1
