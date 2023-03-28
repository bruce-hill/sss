// hashmap.c - C Hash Map Library
// Copyright 2023 Bruce Hill
// Provided under the MIT license with the Commons Clause
// See included LICENSE for details.

// Hash Map (aka Dictionary) Implementation
// Hash keys and values are stored *by value*
// The hash insertion/lookup implementation is based on Lua's tables,
// which use a chained scatter with Brent's variation.

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <gc.h>
#include <stdio.h>

#include "hashmap.h"

typedef struct { const char *data; int32_t len, stride; } array_t;

// #define DEBUG_HASHTABLE 1

#ifdef DEBUG_HASHTABLE
#define hdebug(fmt, ...) printf("\x1b[2m" fmt "\x1b[m" __VA_OPT__(,) __VA_ARGS__)
#else
#define hdebug(...) (void)0
#endif

static inline void hshow(bl_hashmap_t *h)
{
    hdebug("{");
    for (uint32_t i = 0; i < h->capacity; i++) {
        if (i > 0) hdebug(" ");
        hdebug("%d", h->buckets[i].index1);
    }
    hdebug("}\n");
}

uint32_t bl_hashmap_len(bl_hashmap_t *h)
{
    return h->count;
}

static void copy_on_write(bl_hashmap_t *h, size_t entry_size_padded)
{
    h->entries = memcpy(GC_MALLOC((h->count+1)*entry_size_padded), h->entries, (h->count+1)*entry_size_padded);
    h->buckets = memcpy(GC_MALLOC(sizeof(bl_hash_bucket_t)*h->capacity), h->entries, sizeof(bl_hash_bucket_t)*h->capacity);
    h->copy_on_write = false;
}

void bl_hashmap_mark_cow(bl_hashmap_t *h)
{
    h->copy_on_write = true;
}

void *bl_hashmap_get(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key)
{
  recurse:
    if (!h || !key || h->capacity == 0) return NULL;

    uint32_t hash = key_hash(key) % (uint32_t)h->capacity;
    hshow(h);
    hdebug("Getting with initial probe at %u\n", hash);
    for (uint32_t i = hash; h->buckets[i].index1; i = h->buckets[i].next1 - 1) {
        char *entry = h->entries + entry_size_padded*(h->buckets[i].index1-1);
        if (key_cmp(entry, key) == 0)
            return entry;
        if (h->buckets[i].next1 == 0)
            break;
    }
    if (h->fallback) {
        h = h->fallback;
        goto recurse;
    }
    return NULL;
}

static void bl_hashmap_set_bucket(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, const void *entry, size_t entry_size_padded, int32_t index1)
{
    hshow(h);
    uint32_t hash = key_hash(entry) % (uint32_t)h->capacity;
    hdebug("Hash value = %u\n", hash);
    bl_hash_bucket_t *bucket = &h->buckets[hash];
    if (bucket->index1 == 0) {
        hdebug("Got an empty space\n");
        // Empty space:
        bucket->index1 = index1;
        hshow(h);
        return;
    }

    while (h->buckets[h->lastfree_index1-1].index1)
        --h->lastfree_index1;
    assert(h->lastfree_index1);

    uint32_t collided_hash = key_hash(h->entries + entry_size_padded*(bucket->index1-1)) % (uint32_t)h->capacity;
    if (collided_hash != hash) { // Collided with a mid-chain entry
        hdebug("Hit a mid-chain entry\n");
        // Find chain predecessor
        bl_hash_bucket_t *prev = &h->buckets[collided_hash];
        while (prev->next1 != hash+1) {
            assert(key_hash(h->entries + entry_size_padded*(bucket->index1-1)) % (uint32_t)h->capacity == collided_hash);
            prev = &h->buckets[prev->next1-1];
        }

        // Move mid-chain entry to free space and update predecessor
        prev->next1 = h->lastfree_index1--;
        h->buckets[prev->next1-1] = *bucket;
    } else { // Collided with the start of a chain
        hdebug("Hit start of a chain\n");
        for (;;) {
            assert(key_cmp(h->entries + (bucket->index1-1)*entry_size_padded, entry) != 0);
            if (bucket->next1 == 0) {
                // End of chain
                break;
            } else {
                bucket = &h->buckets[bucket->next1-1];
            }
        }
        hdebug("Appending to chain\n");
        // Chain now ends on the free space:
        bucket->next1 = h->lastfree_index1--;
        bucket = &h->buckets[bucket->next1-1];
    }

    bucket->next1 = 0;
    bucket->index1 = index1;
    hshow(h);
}

static void hashmap_resize(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, uint32_t new_capacity, size_t entry_size_padded)
{
    uint32_t old_count = h->count;
    hdebug("About to resize from %u to %u\n", h->capacity, new_capacity);
    hshow(h);
    h->buckets = GC_MALLOC_ATOMIC((size_t)new_capacity*sizeof(bl_hash_bucket_t));
    memset(h->buckets, 0, (size_t)new_capacity*sizeof(bl_hash_bucket_t));
    h->capacity = new_capacity;
    h->lastfree_index1 = new_capacity;
    // Rehash:
    for (uint32_t i = 1; i <= old_count; i++) {
        hdebug("Rehashing %u\n", i);
        bl_hashmap_set_bucket(h, key_hash, key_cmp, h->entries + entry_size_padded*(i-1), entry_size_padded, i);
    }
    hshow(h);
    hdebug("Finished resizing\n");
}

void *bl_hashmap_lvalue(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *entry)
{
    if (!h || !entry) return NULL;
    hshow(h);

    if (h->copy_on_write)
        copy_on_write(h, entry_size_padded);

    if (h->capacity == 0)
        hashmap_resize(h, key_hash, key_cmp, 4, entry_size_padded);

    bl_hashmap_t *fallback = h->fallback;
    h->fallback = NULL;

    void *entry_home = bl_hashmap_get(h, key_hash, key_cmp, entry_size_padded, entry);
    if (entry_home) {
        h->fallback = fallback;
        return entry_home;
    }

    if (h->count >= h->capacity) {
        uint32_t newsize = h->capacity;
        if (h->count + 1 > newsize) newsize *= 2;
        else if (h->count + 1 <= newsize/2) newsize /= 2;
        hashmap_resize(h, key_hash, key_cmp, newsize, entry_size_padded);
    }

    int32_t index1 = ++h->count;
    h->entries = GC_REALLOC(h->entries, h->count*entry_size_padded);
    entry_home = h->entries + (index1-1)*entry_size_padded;
    bl_hashmap_set_bucket(h, key_hash, key_cmp, entry, entry_size_padded, index1);

    if (fallback) {
        void *fallback_entry = bl_hashmap_get(fallback, key_hash, key_cmp, entry_size_padded, entry);
        if (fallback_entry)
            memcpy(entry_home, fallback_entry, entry_size_padded);
        h->fallback = fallback;
    }

    return entry_home;
}

void bl_hashmap_set(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *entry)
{
    void *dest = bl_hashmap_lvalue(h, key_hash, key_cmp, entry_size_padded, entry);
    if (dest)
        memcpy(dest, entry, entry_size_padded);
}

void bl_hashmap_remove(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key)
{
    if (!h || !key || h->capacity == 0) return;

    if (h->copy_on_write)
        copy_on_write(h, entry_size_padded);

    uint32_t hash = key_hash(key) % (uint32_t)h->capacity;
    bl_hash_bucket_t *bucket, *prev = NULL;
    for (uint32_t i = hash; h->buckets[i].index1; i = h->buckets[i].next1 - 1) {
        if (key_cmp(h->entries + entry_size_padded*(h->buckets[i].index1-1), key) == 0) {
            bucket = &h->buckets[i];
            hdebug("Found key to delete\n");
            goto found_it;
        }
        if (h->buckets[i].next1 == 0)
            return;
        prev = &h->buckets[i];
    }
    return;

  found_it:;
    assert(bucket->index1);

    if (bucket->index1 == h->count) {
        hdebug("Popping last key/value\n");
        memset(h->entries + (h->count-1)*entry_size_padded, 0, entry_size_padded);
    } else {
        hdebug("Removing key/value from middle\n");
        uint32_t last_index1 = h->count;
        uint32_t last_hash = key_hash(h->entries + (last_index1-1)*entry_size_padded) % (uint32_t)h->capacity;

        uint32_t i = last_hash;
        while (h->buckets[i].index1 != last_index1) i = h->buckets[i].next1 - 1;
        h->buckets[i].index1 = bucket->index1;

        memcpy(h->entries + (bucket->index1-1)*entry_size_padded, h->entries + (last_index1-1)*entry_size_padded, entry_size_padded);
        memset(h->entries + (last_index1-1)*entry_size_padded, 0, entry_size_padded);
    }

    --h->count;

    uint32_t to_clear_index1;
    if (prev) { // Middle (or end) of a chain
        hdebug("Removing from middle of a chain\n");
        to_clear_index1 = 1 + (bucket - h->buckets);
        prev->next1 = bucket->next1;
    } else if (bucket->next1) { // Start of a chain
        hdebug("Removing from start of a chain\n");
        to_clear_index1 = bucket->next1;
        *bucket = h->buckets[bucket->next1-1];
    } else { // Empty chain
        hdebug("Removing from empty chain\n");
        to_clear_index1 = 1 + (bucket - h->buckets);
    }

    h->buckets[to_clear_index1-1].next1 = 0;
    h->buckets[to_clear_index1-1].index1 = 0;
    if (to_clear_index1 > h->lastfree_index1)
        h->lastfree_index1 = to_clear_index1;

    hshow(h);

    // TODO: it should be possible to do a much faster hash removal
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
}

void *bl_hashmap_nth(bl_hashmap_t *h, int32_t n, size_t entry_size_padded)
{
    if (n < 1 || n > (int32_t)h->count) return NULL;
    return h->entries + (n-1)*entry_size_padded;
}

uint32_t bl_hashmap_hash(bl_hashmap_t *h, hash_fn_t entry_hash, size_t entry_size_padded)
{
    if (!h) return 0;

    uint32_t hash = 0x12345678;
    for (uint32_t i = 0; i < h->count; i++)
        hash ^= entry_hash(h->entries + i*entry_size_padded);
    return hash;
}

int32_t bl_hashmap_compare(bl_hashmap_t *h1, bl_hashmap_t *h2, hash_fn_t key_hash, cmp_fn_t key_cmp, cmp_fn_t entry_cmp, size_t entry_size_padded)
{
    if (h1->count != h2->count) return (int32_t)h1->count - (int32_t)h2->count;
    for (uint32_t i = 0; i < h1->count; i++) {
        void *entry = bl_hashmap_get(h2, key_hash, key_cmp, entry_size_padded, h1->entries + i*entry_size_padded);
        if (!entry) return 1;
        int32_t diff = entry_cmp(h1->entries + i*entry_size_padded, entry);
        if (diff) return diff;
    }
    return 0;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1
