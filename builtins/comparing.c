#include <gc.h>
#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/param.h>

#include "table.h"
#include "string.h"
#include "comparing.h"

#ifndef auto
#define auto __auto_type
#endif

int32_t generic_compare(const void *x, const void *y, Comparable *what)
{
    switch (what->tag) {
        case CompareFunction:
            return what->__data.CompareFunction.fn(x, y);
        case CompareData:
            return memcmp(x, y, what->__data.CompareData.size);
        case CompareArray: {
            string_t *sx = (string_t*)x, *sy = (string_t*)y;
            auto item = what->__data.CompareArray.item;
            if (item->tag == CompareData) {
                size_t item_size = item->__data.CompareData.size;
                for (int32_t i = 0, len = MIN(sx->length, sy->length); i < len; i++) {
                    int32_t cmp = (int32_t)memcmp(&sx->data[sx->stride*i], &sy->data[sy->stride*i], item_size);
                    if (cmp != 0) return cmp;
                }
            } else {
                for (int32_t i = 0, len = MIN(sx->length, sy->length); i < len; i++) {
                    int32_t cmp = generic_compare(&sx->data[sx->stride*i], &sy->data[sy->stride*i], item);
                    if (cmp != 0) return cmp;
                }
            }
            return (sx->length > sy->length) - (sx->length < sy->length);
        }
        case CompareTable: {
            auto table = what->__data.CompareTable;
            table_t *t_x = (table_t*)x, *t_y = (table_t*)y;

            size_t x_size = table.entry_size * t_x->count,
                   y_size = table.entry_size * t_y->count;
            void *x_keys, *y_keys;
            x_keys = x_size <= 128 ? alloca(x_size) : GC_MALLOC(x_size);
            y_keys = y_size <= 128 ? alloca(y_size) : GC_MALLOC(y_size);
            memcpy(x_keys, t_x->entries, table.entry_size * t_x->count);
            memcpy(y_keys, t_y->entries, table.entry_size * t_y->count);

            qsort_r(x_keys, t_x->count, table.entry_size, (void*)generic_compare, table.key);
            qsort_r(y_keys, t_y->count, table.entry_size, (void*)generic_compare, table.key);

            for (uint32_t i = 0; i < MIN(t_x->count, t_y->count); i++) {
                void *key_x = t_x->entries + i*table.entry_size;
                void *key_y = t_y->entries + i*table.entry_size;
                int cmp = generic_compare(key_x, key_y, table.key);
                if (cmp != 0) return cmp;

                void *value_x = key_x + table.value_offset;
                void *value_y = key_y + table.value_offset;
                cmp = generic_compare(value_x, value_y, table.key);
                if (cmp != 0) return cmp;
            }
            return (t_x->count > t_y->count) - (t_x->count < t_y->count);
        }
    }
    return 0;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
