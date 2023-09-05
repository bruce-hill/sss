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

#ifdef DEBUG_HASHTABLE
#define hdebug(fmt, ...) printf("\x1b[2m" fmt "\x1b[m" __VA_OPT__(,) __VA_ARGS__)
#else
#define hdebug(...) (void)0
#endif

#include "../SipHash/halfsiphash.h"
static uint8_t hash_random_vector[16] = {42,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
uint32_t hash_64bits(const void *x) {
    uint32_t hash;
    halfsiphash(*(char**)x, 8, hash_random_vector, (uint8_t*)&hash, sizeof(hash));
    return hash;
}
int compare_64bits(const void *x, const void *y) {
    return memcmp(*(void**)x, *(void**)y, 8);
}
uint32_t hash_64bit_value(const void *x) {
    uint32_t hash;
    halfsiphash((char*)x, 8, hash_random_vector, (uint8_t*)&hash, sizeof(hash));
    return hash;
}
int compare_64bit_value(const void *x, const void *y) {
    return memcmp((void*)x, (void*)y, 8);
}
uint32_t hash_str(const void *x) {
    uint32_t hash;
    halfsiphash(*(void**)x, strlen(*(char**)x), hash_random_vector, (uint8_t*)&hash, sizeof(hash));
    return hash;
}
int32_t compare_str(const void *x, const void *y) {
    return (int32_t)strcmp(*(char**)x, *(char**)y);
}

static inline void hshow(sss_hashmap_t *h)
{
    hdebug("{");
    for (uint32_t i = 0; i < h->capacity; i++) {
        if (i > 0) hdebug(" ");
        hdebug("[%d]=%d(%d)", i, h->buckets[i].index1, h->buckets[i].next1);
    }
    hdebug("}\n");
}

uint32_t sss_hashmap_len(sss_hashmap_t *h)
{
    return h->count;
}

static void copy_on_write(sss_hashmap_t *h, size_t entry_size_padded, sss_hash_bucket_t *original_buckets, char *original_entries)
{
    if (h->entries && h->entries == original_entries)
        h->entries = memcpy(GC_MALLOC((h->count+1)*entry_size_padded), h->entries, (h->count+1)*entry_size_padded);
    if (h->buckets && h->buckets == original_buckets)
        h->buckets = memcpy(GC_MALLOC(sizeof(sss_hash_bucket_t)*h->capacity), h->buckets, sizeof(sss_hash_bucket_t)*h->capacity);
    h->copy_on_write = false;
}

void sss_hashmap_mark_cow(sss_hashmap_t *h)
{
    h->copy_on_write = true;
}

// Return address of value or NULL
void *sss_hashmap_get_raw(sss_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key, size_t value_offset)
{
    if (!h || !key || h->capacity == 0) return NULL;

    uint32_t hash = key_hash(key) % (uint32_t)h->capacity;
    hshow(h);
    hdebug("Getting with initial probe at %u\n", hash);
    for (uint32_t i = hash; h->buckets[i].index1; i = h->buckets[i].next1 - 1) {
        char *entry = h->entries + entry_size_padded*(h->buckets[i].index1-1);
        if (key_cmp(entry, key) == 0)
            return entry + value_offset;
        if (h->buckets[i].next1 == 0)
            break;
    }
    return NULL;
}

void *sss_hashmap_get(sss_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key, size_t value_offset)
{
    for (sss_hashmap_t *iter = h; iter; iter = iter->fallback) {
        void *ret = sss_hashmap_get_raw(iter, key_hash, key_cmp, entry_size_padded, key, value_offset);
        if (ret) return ret;
    }
    for (sss_hashmap_t *iter = h; iter; iter = iter->fallback) {
        if (iter->default_value) return iter->default_value;
    }
    return NULL;
}

static void sss_hashmap_set_bucket(sss_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, const void *entry, size_t entry_size_padded, int32_t index1)
{
    hshow(h);
    uint32_t hash = key_hash(entry) % (uint32_t)h->capacity;
    hdebug("Hash value = %u\n", hash);
    sss_hash_bucket_t *bucket = &h->buckets[hash];
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
        sss_hash_bucket_t *prev = &h->buckets[collided_hash];
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

static void hashmap_resize(sss_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, uint32_t new_capacity, size_t entry_size_padded)
{
    hdebug("About to resize from %u to %u\n", h->capacity, new_capacity);
    hshow(h);
    h->buckets = GC_MALLOC_ATOMIC((size_t)new_capacity*sizeof(sss_hash_bucket_t));
    memset(h->buckets, 0, (size_t)new_capacity*sizeof(sss_hash_bucket_t));
    h->capacity = new_capacity;
    h->lastfree_index1 = new_capacity;
    // Rehash:
    for (uint32_t i = 1; i <= h->count; i++) {
        hdebug("Rehashing %u\n", i);
        sss_hashmap_set_bucket(h, key_hash, key_cmp, h->entries + entry_size_padded*(i-1), entry_size_padded, i);
    }

    char *new_entries = GC_MALLOC(new_capacity*entry_size_padded);
    if (h->entries) memcpy(new_entries, h->entries, h->count*entry_size_padded);
    h->entries = new_entries;

    hshow(h);
    hdebug("Finished resizing\n");
}

// Return address of value
void *sss_hashmap_set(sss_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key, size_t value_offset, const void *value)
{
    hdebug("Raw hash of key being set: %u\n", key_hash(key));
    if (!h || !key) return NULL;
    hshow(h);

    sss_hash_bucket_t *original_buckets = h->buckets;
    char *original_entries = h->entries;

    if (h->capacity == 0)
        hashmap_resize(h, key_hash, key_cmp, 4, entry_size_padded);

    size_t value_size = entry_size_padded - value_offset;
    void *value_home = sss_hashmap_get_raw(h, key_hash, key_cmp, entry_size_padded, key, value_offset);
    if (value_home) { // Update existing slot
        if (h->copy_on_write) {
            // Ensure that `value_home` is still inside h->entries, even if COW occurs
            ptrdiff_t offset = value_home - (void*)h->entries;
            copy_on_write(h, entry_size_padded, original_buckets, original_entries);
            value_home = h->entries + offset;
        }

        if (value && value_size > 0)
            memcpy(value_home, value, value_size);

        return value_home;
    }
    // Otherwise add a new entry:

    // Resize buckets if necessary
    if (h->count >= h->capacity) {
        uint32_t newsize = h->capacity;
        if (h->count + 1 > newsize) newsize *= 2;
        else if (h->count + 1 <= newsize/2) newsize /= 2;
        hashmap_resize(h, key_hash, key_cmp, newsize, entry_size_padded);
    }

    if (!value && value_size > 0) {
        for (sss_hashmap_t *iter = h->fallback; iter; iter = iter->fallback) {
            value = sss_hashmap_get_raw(iter, key_hash, key_cmp, entry_size_padded, key, value_offset);
            if (value) break;
        }
        for (sss_hashmap_t *iter = h; !value && iter; iter = iter->fallback) {
            if (iter->default_value) value = iter->default_value;
        }
    }

    if (h->copy_on_write)
        copy_on_write(h, entry_size_padded, original_buckets, original_entries);

    int32_t index1 = ++h->count;
    void *entry = h->entries + (index1-1)*entry_size_padded;
    memcpy(entry, key, value_offset);
    if (value && value_size > 0)
        memcpy(entry + value_offset, value, entry_size_padded - value_offset);

    sss_hashmap_set_bucket(h, key_hash, key_cmp, entry, entry_size_padded, index1);

    return entry + value_offset;
}

void sss_hashmap_remove(sss_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key)
{
    if (!h || h->capacity == 0) return;

    if (h->copy_on_write)
        copy_on_write(h, entry_size_padded, NULL, NULL);

    // If unspecified, pop a random key:
    if (!key)
        key = h->entries + entry_size_padded*arc4random_uniform(h->count);

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

    uint32_t hash = key_hash(key) % (uint32_t)h->capacity;
    sss_hash_bucket_t *bucket, *prev = NULL;
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
}

void *sss_hashmap_nth(sss_hashmap_t *h, int32_t n, size_t entry_size_padded)
{
    assert(n >= 1 && n <= (int32_t)h->count);
    if (n < 1 || n > (int32_t)h->count) return NULL;
    return h->entries + (n-1)*entry_size_padded;
}

uint32_t sss_hashmap_hash(sss_hashmap_t *h, hash_fn_t entry_hash, size_t entry_size_padded)
{
    if (!h) return 0;

    uint32_t hash = 0x12345678;
    for (uint32_t i = 0; i < h->count; i++)
        hash ^= entry_hash(h->entries + i*entry_size_padded);

    if (h->fallback)
        hash ^= sss_hashmap_hash(h->fallback, entry_hash, entry_size_padded);
    return hash;
}

int32_t sss_hashmap_compare(sss_hashmap_t *h1, sss_hashmap_t *h2, hash_fn_t key_hash, cmp_fn_t key_cmp, cmp_fn_t value_cmp, size_t entry_size_padded, size_t value_offset)
{
    if (h1->count != h2->count) return (int32_t)h1->count - (int32_t)h2->count;
    for (uint32_t i = 0; i < h1->count; i++) {
        void *val = sss_hashmap_get(h2, key_hash, key_cmp, entry_size_padded, h1->entries + i*entry_size_padded, value_offset);
        if (!val) return 1;
        int32_t diff = value_cmp(h1->entries + i*entry_size_padded + value_offset, val);
        if (diff) return diff;
    }
    if (h1->fallback != h2->fallback) {
        if (h1->fallback && h2->fallback) {
            int32_t diff = sss_hashmap_compare(h1->fallback, h2->fallback, key_hash, key_cmp, value_cmp, entry_size_padded, value_offset);
            if (diff) return diff;
        } else {
            return 1;
        }
    }
    if (h1->default_value != h2->default_value) {
        if (h1->default_value && h2->default_value) {
            int32_t diff = value_cmp(h1->default_value, h2->default_value);
            if (diff) return diff;
        } else {
            return 1;
        }
    }
    return 0;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1
