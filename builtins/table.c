
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
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>

#include "../SipHash/halfsiphash.h"
#include "table.h"
#include "types.h"

#ifdef DEBUG_HASHTABLE
#define hdebug(fmt, ...) printf("\x1b[2m" fmt "\x1b[m" __VA_OPT__(,) __VA_ARGS__)
#else
#define hdebug(...) (void)0
#endif

// Helper accessors for type functions/values:
#define HASH(k) (generic_hash(type->info.__data.TableInfo.key, (k)))
#define EQUAL(x, y) (generic_equals(type->info.__data.TableInfo.key, (x), (y)))
#define ENTRY_SIZE (type->info.__data.TableInfo.entry_size)
#define VALUE_OFFSET (type->info.__data.TableInfo.value_offset)

extern const void *SSS_HASH_VECTOR;

static inline void hshow(const table_t *t)
{
    hdebug("{");
    for (uint32_t i = 1; i <= CAPACITY(t); i++) {
        if (i > 0) hdebug(" ");
        hdebug("[%d]=%d(%d)", i, t->buckets[i].index1, t->buckets[i].next1);
    }
    hdebug("}\n");
}

static void copy_on_write(const Type *type, table_t *t, hash_bucket_t *original_buckets, char *original_entries)
{
    if (t->entries && t->entries == original_entries)
        t->entries = memcpy(GC_MALLOC((t->count+1)*ENTRY_SIZE), t->entries, (t->count+1)*ENTRY_SIZE);
    if (t->buckets && t->buckets == original_buckets)
        t->buckets = memcpy(GC_MALLOC(sizeof(hash_bucket_t)*CAPACITY(t)), t->buckets, sizeof(hash_bucket_t)*CAPACITY(t));
    t->copy_on_write = false;
}

// Return address of value or NULL
void *table_get_raw(const Type *type, const table_t *t, const void *key)
{
    if (!t || !key || !t->buckets) return NULL;

    uint32_t hash = HASH(key) % CAPACITY(t);
    hshow(t);
    hdebug("Getting with initial probe at %u\n", hash);
    for (uint32_t i = hash+1; t->buckets[i].index1; i = t->buckets[i].next1 - 1) {
        char *entry = t->entries + ENTRY_SIZE*(t->buckets[i].index1-1);
        if (!EQUAL(entry, key))
            return entry + VALUE_OFFSET;
        if (t->buckets[i].next1 == 0)
            break;
    }
    return NULL;
}

void *table_get(const Type *type, const table_t *t, const void *key)
{
    for (const table_t *iter = t; iter; iter = iter->fallback) {
        void *ret = table_get_raw(type, iter, key);
        if (ret) return ret;
    }
    for (const table_t *iter = t; iter; iter = iter->fallback) {
        if (iter->default_value) return iter->default_value;
    }
    return NULL;
}

static void table_set_bucket(const Type *type, table_t *t, const void *entry, int32_t index1)
{
    hshow(t);
    const void *key = entry;
    uint32_t hash = HASH(key) % CAPACITY(t);
    hdebug("Hash value = %u\n", hash);
    hash_bucket_t *bucket = &t->buckets[hash];
    if (bucket->index1 == 0) {
        hdebug("Got an empty space\n");
        // Empty space:
        bucket->index1 = index1;
        hshow(t);
        return;
    }

    while (t->buckets[LAST_FREE(t)].index1)
        --LAST_FREE(t);
    assert(LAST_FREE(t));

    uint32_t collided_hash = HASH(t->entries + ENTRY_SIZE*(bucket->index1-1)) % CAPACITY(t);
    if (collided_hash != hash) { // Collided with a mid-chain entry
        hdebug("Hit a mid-chain entry\n");
        // Find chain predecessor
        hash_bucket_t *prev = &t->buckets[collided_hash+1];
        while (prev->next1 != hash+1) {
            assert(HASH(t->entries + ENTRY_SIZE*(bucket->index1-1)) % CAPACITY(t) == collided_hash);
            prev = &t->buckets[prev->next1];
        }

        // Move mid-chain entry to free space and update predecessor
        prev->next1 = LAST_FREE(t)--;
        t->buckets[prev->next1] = *bucket;
    } else { // Collided with the start of a chain
        hdebug("Hit start of a chain\n");
        for (;;) {
            assert(EQUAL(t->entries + (bucket->index1-1)*ENTRY_SIZE, key));
            if (bucket->next1 == 0) {
                // End of chain
                break;
            } else {
                bucket = &t->buckets[bucket->next1];
            }
        }
        hdebug("Appending to chain\n");
        // Chain now ends on the free space:
        bucket->next1 = LAST_FREE(t)--;
        bucket = &t->buckets[bucket->next1];
    }

    bucket->next1 = 0;
    bucket->index1 = index1;
    hshow(t);
}

static void hashmap_resize(const Type *type, table_t *t, uint32_t new_capacity)
{
    hdebug("About to resize from %u to %u\n", t->capacity, new_capacity);
    hshow(t);
    // Add 1 capacity for storing metadata: capacity and nextfree
    t->buckets = GC_MALLOC_ATOMIC((size_t)(1+new_capacity)*sizeof(hash_bucket_t));
    memset(t->buckets, 0, (size_t)(1+new_capacity)*sizeof(hash_bucket_t));
    t->buckets[0].index1 = new_capacity;
    t->buckets[0].next1 = new_capacity;
    // Rehash:
    for (uint32_t i = 1; i <= t->count; i++) {
        hdebug("Rehashing %u\n", i);
        table_set_bucket(type, t, t->entries + ENTRY_SIZE*(i-1), i);
    }

    char *new_entries = GC_MALLOC(new_capacity*ENTRY_SIZE);
    if (t->entries) memcpy(new_entries, t->entries, t->count*ENTRY_SIZE);
    t->entries = new_entries;

    hshow(t);
    hdebug("Finished resizing\n");
}

// Return address of value
void *table_set(const Type *type, table_t *t, const void *key, const void *value)
{
    hdebug("Raw hash of key being set: %u\n", key_hash(key));
    if (!t || !key) return NULL;
    hshow(t);

    hash_bucket_t *original_buckets = t->buckets;
    char *original_entries = t->entries;

    if (!original_buckets)
        hashmap_resize(type, t, 4);

    size_t value_size = ENTRY_SIZE - VALUE_OFFSET;
    void *value_home = table_get_raw(type, t, key);
    if (value_home) { // Update existing slot
        if (t->copy_on_write) {
            // Ensure that `value_home` is still inside t->entries, even if COW occurs
            ptrdiff_t offset = value_home - (void*)t->entries;
            copy_on_write(type, t, original_buckets, original_entries);
            value_home = t->entries + offset;
        }

        if (value && value_size > 0)
            memcpy(value_home, value, value_size);

        return value_home;
    }
    // Otherwise add a new entry:

    // Resize buckets if necessary
    if (t->count >= CAPACITY(t)) {
        uint32_t newsize = CAPACITY(t);
        if (t->count + 1 > newsize) newsize *= 2;
        else if (t->count + 1 <= newsize/2) newsize /= 2;
        hashmap_resize(type, t, newsize);
    }

    if (!value && value_size > 0) {
        for (table_t *iter = t->fallback; iter; iter = iter->fallback) {
            value = table_get_raw(type, iter, key);
            if (value) break;
        }
        for (table_t *iter = t; !value && iter; iter = iter->fallback) {
            if (iter->default_value) value = iter->default_value;
        }
    }

    if (t->copy_on_write)
        copy_on_write(type, t, original_buckets, original_entries);

    int32_t index1 = ++t->count;
    void *entry = t->entries + (index1-1)*ENTRY_SIZE;
    memcpy(entry, key, VALUE_OFFSET);
    if (value && value_size > 0)
        memcpy(entry + VALUE_OFFSET, value, ENTRY_SIZE - VALUE_OFFSET);

    table_set_bucket(type, t, entry, index1);

    return entry + VALUE_OFFSET;
}

void table_remove(const Type *type, table_t *t, const void *key)
{
    if (!t || !t->buckets) return;

    if (t->copy_on_write)
        copy_on_write(type, t, NULL, NULL);

    // If unspecified, pop a random key:
    if (!key)
        key = t->entries + ENTRY_SIZE*arc4random_uniform(t->count);

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

    uint32_t hash = HASH(key) % CAPACITY(t);
    hash_bucket_t *bucket, *prev = NULL;
    for (uint32_t i = hash+1; t->buckets[i].index1; i = t->buckets[i].next1 - 1) {
        if (EQUAL(t->entries + ENTRY_SIZE*(t->buckets[i].index1-1), key)) {
            bucket = &t->buckets[i];
            hdebug("Found key to delete\n");
            goto found_it;
        }
        if (t->buckets[i].next1 == 0)
            return;
        prev = &t->buckets[i];
    }
    return;

  found_it:;
    assert(bucket->index1);

    if (bucket->index1 == t->count) {
        hdebug("Popping last key/value\n");
        memset(t->entries + (t->count-1)*ENTRY_SIZE, 0, ENTRY_SIZE);
    } else {
        hdebug("Removing key/value from middle\n");
        uint32_t last_index1 = t->count;
        uint32_t last_hash = HASH(t->entries + (last_index1-1)*ENTRY_SIZE) % CAPACITY(t);

        uint32_t i = last_hash + 1;
        while (t->buckets[i].index1 != last_index1) i = t->buckets[i].next1 - 1;
        t->buckets[i].index1 = bucket->index1;

        memcpy(t->entries + (bucket->index1-1)*ENTRY_SIZE, t->entries + (last_index1-1)*ENTRY_SIZE, ENTRY_SIZE);
        memset(t->entries + (last_index1-1)*ENTRY_SIZE, 0, ENTRY_SIZE);
    }

    --t->count;

    uint32_t to_clear_index1;
    if (prev) { // Middle (or end) of a chain
        hdebug("Removing from middle of a chain\n");
        to_clear_index1 = (bucket - t->buckets);
        prev->next1 = bucket->next1;
    } else if (bucket->next1) { // Start of a chain
        hdebug("Removing from start of a chain\n");
        to_clear_index1 = bucket->next1;
        *bucket = t->buckets[bucket->next1];
    } else { // Empty chain
        hdebug("Removing from empty chain\n");
        to_clear_index1 = (bucket - t->buckets);
    }

    t->buckets[to_clear_index1].next1 = 0;
    t->buckets[to_clear_index1].index1 = 0;
    if (to_clear_index1 > LAST_FREE(t))
        LAST_FREE(t) = to_clear_index1;

    hshow(t);
}

void *table_nth(const Type *type, const table_t *t, uint32_t n)
{
    assert(n >= 1 && n <= t->count);
    if (n < 1 || n > t->count) return NULL;
    return t->entries + (n-1)*ENTRY_SIZE;
}

void table_clear(table_t *t)
{
    memset(t, 0, sizeof(table_t));
}

bool table_equals(const Type *type, const table_t *x, const table_t *y)
{
    if (x->count != y->count)
        return false;
    
    if ((x->default_value != NULL) != (y->default_value != NULL))
        return false;
    
    if ((x->fallback != NULL) != (y->fallback != NULL))
        return false;

    const Type *value_type = type->info.__data.TableInfo.value;
    for (uint32_t i = 1; i <= x->count; i++) {
        void *x_key = x->entries + (i-1)*ENTRY_SIZE;
        void *y_value = table_get_raw(type, y, x_key);
        if (!y_value) return false;
        void *x_value = x_key + VALUE_OFFSET;
        if (!table_equals(value_type, x_value, y_value))
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

int32_t table_compare(const table_t *x, const table_t *y, const Type *type)
{
    __auto_type table = type->info.__data.TableInfo;
    size_t x_size = table.entry_size * x->count,
           y_size = table.entry_size * y->count;
    void *x_keys, *y_keys;
    x_keys = x_size <= 128 ? alloca(x_size) : GC_MALLOC(x_size);
    y_keys = y_size <= 128 ? alloca(y_size) : GC_MALLOC(y_size);
    memcpy(x_keys, x->entries, table.entry_size * x->count);
    memcpy(y_keys, y->entries, table.entry_size * y->count);

    qsort_r(x_keys, x->count, table.entry_size, (void*)generic_compare, table.key);
    qsort_r(y_keys, y->count, table.entry_size, (void*)generic_compare, table.key);

    for (uint32_t i = 0; i < MIN(x->count, y->count); i++) {
        void *key_x = x_keys + i*table.entry_size;
        void *key_y = y_keys + i*table.entry_size;
        int cmp = generic_compare(key_x, key_y, table.key);
        if (cmp != 0) return cmp;

        void *value_x = key_x + table.value_offset;
        void *value_y = key_y + table.value_offset;
        cmp = generic_compare(value_x, value_y, table.key);
        if (cmp != 0) return cmp;
    }
    return (x->count > y->count) - (x->count < y->count);
}

uint32_t table_hash(const Type *type, const table_t *t)
{
    // Table hashes are computed as:
    // hash(#t, xor(hash(k) for k in t.keys), xor(hash(v) for v in t.values), hash(t.fallback), hash(t.default))
    // Where fallback and default hash to zero if absent
    __auto_type table = type->info.__data.TableInfo;
    size_t entry_size = table.entry_size;
    size_t value_offset = table.value_offset;

    uint32_t key_hashes = 0, value_hashes = 0, fallback_hash = 0, default_hash = 0;
    for (uint32_t i = 0; i < t->count; i++) {
        void *entry = t->entries + i*entry_size;
        key_hashes ^= generic_hash(entry, table.key);
        value_hashes ^= generic_hash(entry + value_offset, table.value);
    }

    if (t->fallback)
        fallback_hash = generic_hash(type, t->fallback);

    if (t->default_value)
        default_hash = generic_hash(table.value, t->default_value);

    uint32_t components[] = {
        t->count,
        key_hashes,
        value_hashes,
        fallback_hash,
        default_hash,
    };
    uint32_t hash;
    halfsiphash(components, sizeof(components), SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
    return hash;
}

CORD table_cord(const Type *type, const table_t *t, bool colorize)
{
    __auto_type table = type->info.__data.TableInfo;
    size_t value_offset = table.value_offset;
    CORD c = "{";
    for (uint32_t i = 1; i <= t->count; i++) {
        if (i > 1)
            c = CORD_cat(c, ", ");
        void *entry = table_nth(type, t, i);
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

Type make_table_type(Type *key, Type *value, size_t entry_size, size_t value_offset)
{
    return (Type){
        .name=STRING(heap_strf("{%s=>%s}", key->name, value->name)),
        .info={.tag=TableInfo, .__data.TableInfo={.key=key,.value=value, .entry_size=entry_size, .value_offset=value_offset}},
        .order=OrderingMethod(Function, (void*)table_compare),
        .equality=EqualityMethod(Function, (void*)table_equals),
        .hash=HashMethod(Function, (void*)table_hash),
        .cord=CordMethod(Function, (void*)table_cord),
        .bindings=(NamespaceBinding[]){
            {"get", heap_strf("func(type:@Type, t:{%s=>%s}, key:@%s) ?(readonly)%s", key->name, value->name, key->name, key->name), table_get},
            {"get_raw", heap_strf("func(type:@Type, t:{%s=>%s}, key:@%s) ?(readonly)%s", key->name, value->name, key->name, key->name), table_get_raw},
            {"nth", heap_strf("func(type:@Type, t:{%s=>%s}, n:UInt32) {key:%s, value:%s}",
                              key->name, value->name, key->name, value->name), table_nth},
            {"set", heap_strf("func(type:@Type, t:@{%s=>%s}, key:@%s, value:?%s) @%s",
                              key->name, value->name, key->name, value->name, value->name), table_set},
            {"remove", heap_strf("func(type:@Type, t:@{%s=>%s}, key:?%s) Void",
                                 key->name, value->name, key->name), table_remove},
            {"equals", heap_strf("func(type:@Type,x,y:@{%s=>%s}) Bool", key->name, value->name), table_equals},
            {"clear", heap_strf("func(t:@{%s=>%s}) Void", key->name, value->name), table_clear},
            {NULL, NULL, NULL},
        },
    };
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1
