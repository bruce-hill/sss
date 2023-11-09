
// table.c - C Hash table implementation for SSS
// Copyright 2023 Bruce Hill
// Provided under the MIT license with the Commons Clause
// See included LICENSE for details.

// Hash table (aka Dictionary) Implementation
// Hash keys and values are stored *by value*
// The hash insertion/lookup implementation is based on Lua's tables,
// which use a chained scatter with Brent's variation.

#include <assert.h>
#include <gc.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>

#include "../SipHash/halfsiphash.h"
#include "table.h"
#include "types.h"

#define DEBUG_TABLES

#ifdef DEBUG_TABLES
#define hdebug(fmt, ...) printf("\x1b[2m" fmt "\x1b[m" __VA_OPT__(,) __VA_ARGS__)
#else
#define hdebug(...) (void)0
#endif

// Helper accessors for type functions/values:
#define HASH(t, k) (generic_hash(type->info.__data.TableInfo.key, (k)) % ((t)->bucket_info->count))
#define EQUAL(x, y) (generic_equals(type->info.__data.TableInfo.key, (x), (y)))
#define ENTRY_SIZE (type->info.__data.TableInfo.entry_size)
#define VALUE_OFFSET (type->info.__data.TableInfo.value_offset)

#define GET_ENTRY(t, i) ((t)->entry_info->entries + ENTRY_SIZE*(i))

extern const void *SSS_HASH_VECTOR;

static inline void hshow(const table_t *t)
{
    hdebug("{");
    for (uint32_t i = 0; i < Table_length(t); i++) {
        if (i > 0) hdebug(" ");
        hdebug("[%d]=%d(%d)", i, t->bucket_info->buckets[i].index, t->bucket_info->buckets[i].next_offset);
    }
    hdebug("}\n");
}

typedef enum { COW_NONE = 0, COW_ENTRIES = 1, COW_BUCKETS = 2, COW_BOTH = 3 } cow_e;

static void maybe_copy_on_write(const Type *type, table_t *t, cow_e which_cow)
{
    if (t->entry_info && t->entry_info->copy_on_write && (which_cow & COW_ENTRIES)) {
        size_t size = sizeof(entries_t) + t->entry_info->count*ENTRY_SIZE;
        t->entry_info = memcpy(GC_MALLOC(size), t->entry_info, size);
        t->entry_info->copy_on_write = 0;
    }

    if (t->bucket_info && t->bucket_info->copy_on_write && (which_cow & COW_BUCKETS)) {
        size_t size = sizeof(hash_buckets_t) + t->bucket_info->count*sizeof(hash_bucket_t);
        t->bucket_info = memcpy(GC_MALLOC(size), t->bucket_info, size);
        t->bucket_info->copy_on_write = 0;
    }
}

void Table_mark_copy_on_write(table_t *t)
{
    if (t->entry_info) t->entry_info->copy_on_write = 1;
    if (t->bucket_info) t->bucket_info->copy_on_write = 1;
}

// Return address of value or NULL
void *Table_get_raw(const Type *type, const table_t *t, const void *key)
{
    if (!t || !key || !t->bucket_info) return NULL;

    uint32_t hash = HASH(t, key);
    hshow(t);
    hdebug("Getting value with initial probe at %u\n", hash);
    hash_bucket_t *buckets = t->bucket_info->buckets;
    for (uint32_t i = hash; buckets[i].occupied; i += buckets[i].next_offset) {
        hdebug("Checking against key in bucket %u\n", i);
        void *entry = GET_ENTRY(t, buckets[i].index);
        if (EQUAL(entry, key))
            return entry + VALUE_OFFSET;
        if (!buckets[i].next_offset)
            break;
    }
    return NULL;
}

void *Table_get(const Type *type, const table_t *t, const void *key)
{
    for (const table_t *iter = t; iter; iter = iter->fallback) {
        void *ret = Table_get_raw(type, iter, key);
        if (ret) return ret;
    }
    for (const table_t *iter = t; iter; iter = iter->fallback) {
        if (iter->default_value) return iter->default_value;
    }
    return NULL;
}

static void Table_set_bucket(const Type *type, table_t *t, const void *entry, int32_t index)
{
    assert(t->bucket_info && t->entry_info);
    hshow(t);
    const void *key = entry;
    hdebug("Set bucket '%s'\n", *(char**)key);
    hash_bucket_t *buckets = t->bucket_info->buckets;
    uint32_t hash = HASH(t, key);
    hdebug("Hash value (mod %u) = %u\n", t->bucket_info->count, hash);
    hash_bucket_t *bucket = &buckets[hash];
    if (!bucket->occupied) {
        hdebug("Got an empty space\n");
        // Empty space:
        bucket->index = index;
        bucket->occupied = 1;
        hshow(t);
        return;
    }

    while (buckets[t->bucket_info->last_free].occupied) {
        assert(t->bucket_info->last_free > 0);
        --t->bucket_info->last_free;
    }

    uint32_t collided_hash = HASH(t, GET_ENTRY(t, bucket->index));
    if (collided_hash != hash) { // Collided with a mid-chain entry
        hdebug("Hit a mid-chain entry\n");
        // Find chain predecessor
        uint32_t predecessor = collided_hash;
        while (predecessor + buckets[predecessor].next_offset != hash)
            predecessor += buckets[predecessor].next_offset;

        // Move mid-chain entry to free space and update predecessor
        buckets[predecessor].next_offset = (int32_t)(t->bucket_info->last_free - predecessor);
        buckets[t->bucket_info->last_free] = (hash_bucket_t){
            .occupied = 1,
            .index=bucket->index,
            .next_offset=bucket->next_offset ? (hash + bucket->next_offset) - t->bucket_info->last_free : 0,
        };
    } else { // Collided with the start of a chain
        hdebug("Hit start of a chain\n");
        int32_t end_of_chain = hash;
        while (buckets[end_of_chain].next_offset)
            end_of_chain += buckets[end_of_chain].next_offset;
        hdebug("Appending to chain\n");
        // Chain now ends on the free space:
        buckets[end_of_chain].next_offset = (int32_t)(t->bucket_info->last_free - end_of_chain);
        bucket = &buckets[t->bucket_info->last_free];
    }

    bucket->next_offset = 0;
    bucket->index = index;
    bucket->occupied = 1;
    hshow(t);
}

static void hashmap_resize_buckets(const Type *type, table_t *t, uint32_t new_capacity)
{
    hdebug("About to resize from %u to %u\n", t->bucket_info ? t->bucket_info->count : 0, new_capacity);
    hshow(t);
    size_t alloc_size = sizeof(hash_buckets_t) + (size_t)(new_capacity)*sizeof(hash_bucket_t);
    t->bucket_info = GC_MALLOC_ATOMIC(alloc_size);
    memset(t->bucket_info->buckets, 0, (size_t)new_capacity * sizeof(hash_bucket_t));
    t->bucket_info->count = new_capacity;
    t->bucket_info->last_free = new_capacity-1;
    // Rehash:
    for (uint32_t i = 0; i < Table_length(t); i++) {
        hdebug("Rehashing %u\n", i);
        Table_set_bucket(type, t, GET_ENTRY(t, i), i);
    }

    hshow(t);
    hdebug("Finished resizing\n");
}

static void hashmap_resize_entries(const Type *type, table_t *t, uint32_t new_capacity)
{
    entries_t *new_entry_info = GC_MALLOC(sizeof(entries_t) + ENTRY_SIZE*new_capacity);
    uint32_t length = Table_length(t);
    new_entry_info->count = length;
    new_entry_info->space = new_capacity - length;
    if (t->entry_info) {
        memcpy(new_entry_info->entries, t->entry_info->entries, ENTRY_SIZE*t->entry_info->count);
        memset(new_entry_info->entries + ENTRY_SIZE*t->entry_info->count, 0, ENTRY_SIZE*new_entry_info->space);
    }
    t->entry_info = new_entry_info;
}

// Return address of value
void *Table_set(const Type *type, table_t *t, const void *key, const void *value)
{
    if (!t || !key) return NULL;
    hshow(t);

    if (!t->bucket_info)
        hashmap_resize_buckets(type, t, 4);

    if (!t->entry_info)
        hashmap_resize_entries(type, t, 4);

    hdebug("Hash of key being set: %u\n", HASH(t, key));

    size_t value_size = ENTRY_SIZE - VALUE_OFFSET;
    void *value_home = Table_get_raw(type, t, key);
    if (value_home) { // Update existing slot
        // Ensure that `value_home` is still inside t->entries, even if COW occurs
        ptrdiff_t offset = value_home - (void*)t->entry_info->entries;
        maybe_copy_on_write(type, t, COW_BUCKETS);
        value_home = t->entry_info->entries + offset;

        if (value && value_size > 0)
            memcpy(value_home, value, value_size);

        return value_home;
    }
    // Otherwise add a new entry:

    // Grow entry capacity if needed:
    if (t->entry_info->space <= 0) {
        uint32_t newsize = t->entry_info->count + MIN(t->entry_info->count, 64);
        hashmap_resize_entries(type, t, newsize);
    }

    // Resize buckets if necessary
    if (t->entry_info->count >= t->bucket_info->count) {
        uint32_t newsize = t->bucket_info->count + MIN(t->bucket_info->count, 64);
        hashmap_resize_buckets(type, t, newsize);
    }

    if (!value && value_size > 0) {
        for (table_t *iter = t->fallback; iter; iter = iter->fallback) {
            value = Table_get_raw(type, iter, key);
            if (value) break;
        }
        for (table_t *iter = t; !value && iter; iter = iter->fallback) {
            if (iter->default_value) value = iter->default_value;
        }
    }

    maybe_copy_on_write(type, t, COW_BOTH);

    uint32_t entry_index = t->entry_info->count;
    ++t->entry_info->count;
    --t->entry_info->space;
    void *entry = GET_ENTRY(t, entry_index);
    memcpy(entry, key, VALUE_OFFSET);
    if (value && value_size > 0)
        memcpy(entry + VALUE_OFFSET, value, ENTRY_SIZE - VALUE_OFFSET);

    Table_set_bucket(type, t, entry, entry_index);

    return entry + VALUE_OFFSET;
}

void Table_remove(const Type *type, table_t *t, const void *key)
{
    if (!t || !t->bucket_info || !t->entry_info) return;

    // TODO: this work doesn't need to be done if the key is already missing
    maybe_copy_on_write(type, t, COW_BOTH);

    // If unspecified, pop a random key:
    if (!key) {
        uint32_t index = arc4random_uniform(t->entry_info->count);
        key = GET_ENTRY(t, index);
    }

    // Steps: look up the bucket for the removed key
    // If missing, then return immediately
    // Swap last key/value into the removed bucket's index1
    // Zero out the last key/value and decrement the count
    // Find the last key/value's bucket and update its index1
    // Look up the bucket for the removed key
    // If bucket is first in chain:
    //    Move bucket->next to bucket's spot
    //    zero out bucket->next's old spot
    //    maybe update lastfree_index1 to second-in-chain's index
    // Else:
    //    set prev->next = bucket->next
    //    zero out bucket
    //    maybe update lastfree_index1 to removed bucket's index

    uint32_t hash = HASH(t, key);
    uint32_t found_index = 0;
    hash_bucket_t *bucket, *prev = NULL;
    for (uint32_t i = hash; t->bucket_info->buckets[i].occupied; i += t->bucket_info->buckets[i].next_offset) {
        if (EQUAL(GET_ENTRY(t, t->bucket_info->buckets[i].index), key)) {
            bucket = &t->bucket_info->buckets[i];
            found_index = i;
            hdebug("Found key to delete\n");
            goto found_it;
        }
        if (t->bucket_info->buckets[i].next_offset == 0)
            return;
        prev = &t->bucket_info->buckets[i];
    }
    return;

  found_it:;
    assert(bucket->occupied);

    uint32_t last_index = t->entry_info->count-1;
    if (bucket->index == last_index) {
        hdebug("Popping last key/value\n");
        memset(GET_ENTRY(t, last_index), 0, ENTRY_SIZE);
    } else {
        hdebug("Removing key/value from middle\n");
        uint32_t i = HASH(t, GET_ENTRY(t, last_index));
        while (i + t->bucket_info->buckets[i].next_offset != last_index)
            i += t->bucket_info->buckets[i].next_offset;
        t->bucket_info->buckets[i].index = bucket->index;

        memcpy(GET_ENTRY(t, bucket->index), GET_ENTRY(t, last_index), ENTRY_SIZE);
        memset(GET_ENTRY(t, last_index), 0, ENTRY_SIZE);
    }

    --t->entry_info->count;
    ++t->entry_info->space;

    uint32_t bucket_to_clear;
    if (prev) { // Middle (or end) of a chain
        hdebug("Removing from middle of a chain\n");
        bucket_to_clear = (bucket - t->bucket_info->buckets);
        prev->next_offset = (bucket_to_clear + bucket->next_offset) - (prev - t->bucket_info->buckets);
    } else if (bucket->next_offset) { // Start of a chain
        hdebug("Removing from start of a chain\n");
        bucket_to_clear = (bucket - t->bucket_info->buckets) + bucket->next_offset;
        hash_bucket_t *to_move = &t->bucket_info->buckets[bucket_to_clear];
        *bucket = (hash_bucket_t){
            .occupied=1,
            .index=to_move->index,
            .next_offset=to_move->next_offset ? (bucket_to_clear + to_move->next_offset) - found_index : 0,
        };
    } else { // Empty chain
        hdebug("Removing from empty chain\n");
        bucket_to_clear = (bucket - t->bucket_info->buckets);
    }

    t->bucket_info->buckets[bucket_to_clear] = (hash_bucket_t){0};
    if (bucket_to_clear > t->bucket_info->last_free)
        t->bucket_info->last_free = bucket_to_clear;

    hshow(t);
}

void *Table_entry(const Type *type, const table_t *t, uint32_t n)
{
    assert(n >= 1 && n <= Table_length(t));
    return GET_ENTRY(t, n-1);
}

void Table_clear(table_t *t)
{
    memset(t, 0, sizeof(table_t));
}

bool Table_equals(const Type *type, const table_t *x, const table_t *y)
{
    if (Table_length(x) != Table_length(y))
        return false;
    
    if ((x->default_value != NULL) != (y->default_value != NULL))
        return false;
    
    if ((x->fallback != NULL) != (y->fallback != NULL))
        return false;

    const Type *value_type = type->info.__data.TableInfo.value;
    for (uint32_t i = 0, length = Table_length(x); i < length; i++) {
        void *x_key = GET_ENTRY(x, i);
        void *y_value = Table_get_raw(type, y, x_key);
        if (!y_value) return false;
        void *x_value = x_key + VALUE_OFFSET;
        if (!Table_equals(value_type, x_value, y_value))
            return false;
    }

    if (x->default_value && y->default_value
        && !generic_equals(value_type, x->default_value, y->default_value))
        return false;

    if (x->fallback && y->fallback
        && !generic_equals(type, x->fallback, y->fallback))
        return false;
    
    return true;
}

int32_t Table_compare(const table_t *x, const table_t *y, const Type *type)
{
    __auto_type table = type->info.__data.TableInfo;
    size_t x_size = table.entry_size * Table_length(x),
           y_size = table.entry_size * Table_length(y);
    void *x_keys, *y_keys;
    x_keys = x_size <= 128 ? alloca(x_size) : GC_MALLOC(x_size);
    y_keys = y_size <= 128 ? alloca(y_size) : GC_MALLOC(y_size);
    memcpy(x_keys, x->entry_info->entries, x_size);
    memcpy(y_keys, y->entry_info->entries, y_size);

    qsort_r(x_keys, Table_length(x), table.entry_size, (void*)generic_compare, table.key);
    qsort_r(y_keys, Table_length(y), table.entry_size, (void*)generic_compare, table.key);

    for (uint32_t i = 0, length = MIN(Table_length(x), Table_length(y)); i < length; i++) {
        void *key_x = x_keys + i*table.entry_size;
        void *key_y = y_keys + i*table.entry_size;
        int cmp = generic_compare(key_x, key_y, table.key);
        if (cmp != 0) return cmp;

        void *value_x = key_x + table.value_offset;
        void *value_y = key_y + table.value_offset;
        cmp = generic_compare(value_x, value_y, table.key);
        if (cmp != 0) return cmp;
    }
    return (Table_length(x) > Table_length(y)) - (Table_length(x) < Table_length(y));
}

uint32_t Table_hash(const Type *type, const table_t *t)
{
    // Table hashes are computed as:
    // hash(#t, xor(hash(k) for k in t.keys), xor(hash(v) for v in t.values), hash(t.fallback), hash(t.default))
    // Where fallback and default hash to zero if absent
    __auto_type table = type->info.__data.TableInfo;
    size_t value_offset = table.value_offset;

    uint32_t key_hashes = 0, value_hashes = 0, fallback_hash = 0, default_hash = 0;
    for (uint32_t i = 0, length = Table_length(t); i < length; i++) {
        void *entry = GET_ENTRY(t, i);
        key_hashes ^= generic_hash(table.key, entry);
        value_hashes ^= generic_hash(table.value, entry + value_offset);
    }

    if (t->fallback)
        fallback_hash = generic_hash(type, t->fallback);

    if (t->default_value)
        default_hash = generic_hash(table.value, t->default_value);

    uint32_t components[] = {
        Table_length(t),
        key_hashes,
        value_hashes,
        fallback_hash,
        default_hash,
    };
    uint32_t hash;
    halfsiphash(components, sizeof(components), SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
    return hash;
}

CORD Table_cord(const Type *type, const table_t *t, bool colorize)
{
    __auto_type table = type->info.__data.TableInfo;
    size_t value_offset = table.value_offset;
    CORD c = "{";
    for (uint32_t i = 1, length = Table_length(t); i <= length; i++) {
        if (i > 1)
            c = CORD_cat(c, ", ");
        void *entry = Table_entry(type, t, i);
        c = CORD_cat(c, generic_cord(table.key, entry, colorize));
        c = CORD_cat(c, "=>");
        c = CORD_cat(c, generic_cord(table.value, entry + value_offset, colorize));
    }

    if (t->fallback) {
        c = CORD_cat(c, "; fallback=");
        c = CORD_cat(c, generic_cord(type, t->fallback, colorize));
    }

    if (t->default_value) {
        c = CORD_cat(c, "; default=");
        c = CORD_cat(c, generic_cord(table.value, t->default_value, colorize));
    }

    c = CORD_cat(c, "}");
    return c;
}

static inline char *heap_strn(const char *str, size_t len)
{
    if (!str) return NULL;
    if (len == 0) return "";
    char *heaped = GC_MALLOC_ATOMIC(len + 1);
    memcpy(heaped, str, len);
    heaped[len] = '\0';
    return heaped;
}

static inline char *heap_strf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    char *tmp = NULL;
    int len = vasprintf(&tmp, fmt, args);
    if (len < 0) return NULL;
    va_end(args);
    char *ret = heap_strn(tmp, (size_t)len);
    free(tmp);
    return ret;
}

Type make_table_type(Type *key, Type *value)
{
    size_t entry_size = key->size;
    if (key->align > 1 && entry_size % key->align)
        entry_size += key->align - (entry_size % key->align); // padding
    size_t value_offset = entry_size;
    entry_size += key->size;
    size_t max_align = MAX(key->align, value->align);
    if (max_align > 1 && entry_size % max_align)
        entry_size += max_align - (entry_size % max_align); // padding

    const char *key_name = key->name.data,
          *value_name = value->name.data;
    return (Type){
        .name=STRING(heap_strf("{%s=>%s}", key_name, value_name)),
        .info={.tag=TableInfo, .__data.TableInfo={.key=key,.value=value, .entry_size=entry_size, .value_offset=value_offset}},
        .size=sizeof(table_t),
        .align=alignof(table_t),
        .order=OrderingMethod(Function, (void*)Table_compare),
        .equality=EqualityMethod(Function, (void*)Table_equals),
        .hash=HashMethod(Function, (void*)Table_hash),
        .cord=CordMethod(Function, (void*)Table_cord),
        .bindings=(NamespaceBinding[]){
            {"get", heap_strf("func(type:@Type, t:{%s=>%s}, key:@%s) ?(readonly)%s", key_name, value_name, key_name, key_name), Table_get},
            {"get_raw", heap_strf("func(type:@Type, t:{%s=>%s}, key:@%s) ?(readonly)%s", key_name, value_name, key_name, key_name), Table_get_raw},
            {"entry", heap_strf("func(type:@Type, t:{%s=>%s}, n:UInt32) {key:%s, value:%s}",
                                key_name, value_name, key_name, value_name), Table_entry},
            {"set", heap_strf("func(type:@Type, t:@{%s=>%s}, key:@%s, value:?%s) @%s",
                              key_name, value_name, key_name, value_name, value_name), Table_set},
            {"remove", heap_strf("func(type:@Type, t:@{%s=>%s}, key:?%s) Void",
                                 key_name, value_name, key_name), Table_remove},
            {"equals", heap_strf("func(type:@Type,x,y:@{%s=>%s}) Bool", key_name, value_name), Table_equals},
            {"clear", heap_strf("func(t:@{%s=>%s}) Void", key_name, value_name), Table_clear},
            {NULL, NULL, NULL},
        },
    };
}

Type CStringToVoidStarTable_type;

void *Table_gets(const table_t *t, const char *key)
{
    void **ret = Table_get(&CStringToVoidStarTable_type, t, &key);
    return ret ? *ret : NULL;
}

void *Table_gets_raw(const table_t *t, const char *key)
{
    void **ret = Table_get_raw(&CStringToVoidStarTable_type, t, &key);
    return ret ? *ret : NULL;
}

void *Table_sets(table_t *t, const char *key, const void *value)
{
    return Table_set(&CStringToVoidStarTable_type, t, &key, &value);
}

void Table_removes(table_t *t, const char *key)
{
    return Table_remove(&CStringToVoidStarTable_type, t, &key);
}

void *Table_entrys(const table_t *t, uint32_t n)
{
    return Table_entry(&CStringToVoidStarTable_type, t, n);
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1
