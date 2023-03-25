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

#define DEBUG_HASHTABLE 1

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

// static uint32_t array_item_size = 0;
// void set_array_item_size(uint32_t size) {
//     array_item_size = size;
// }

// uint32_t hash_array(const void *void_arr, size_t _)
// {
//     (void)_;
//     const array_t *arr = void_arr;
//     uint32_t hash = 0x1badD00d;
//     for (int32_t i = 0; i < arr->len; i++) {
//         uint32_t item_hash;
//         halfsiphash(arr->data + i*arr->stride*array_item_size, array_item_size, hash_random_vector, (uint8_t*)&item_hash, sizeof(item_hash));

//         if (i == 0) {
//             hash = item_hash;
//         } else {
//             uint32_t hashes[] = {hash, item_hash};
//             halfsiphash(hashes, sizeof(hashes), hash_random_vector, (uint8_t*)&hash, sizeof(hash));
//         }
//     }
//     return hash;
// }

// static uint32_t hashmap_key_size = 0, hashmap_key_padded_size = 0, hashmap_value_size = 0, hashmap_value_padded_size = 0;
// void set_hashmap_sizes(uint32_t key_size, uint32_t key_padded_size, uint32_t value_size, uint32_t value_padded_size) {
//     hashmap_key_size = key_size;
//     hashmap_key_padded_size = key_padded_size;
//     hashmap_value_size = value_size;
//     hashmap_value_padded_size = value_padded_size;
// }

// uint32_t hash_hashmap(const void *void_h, size_t _)
// {
//     (void)_;
//     const bl_hashmap_t *h = void_h;
//     uint32_t hash;
//     halfsiphash(&h->count, sizeof(h->count), hash_random_vector, (uint8_t*)&hash, sizeof(hash));
//     for (uint32_t i = 0; i < h->count; i++) {
//         union {
//             struct {uint16_t key_hash, value_hash;} parts;
//             uint32_t hash;
//         } entry_hash;
//         halfsiphash(h->keys + i*hashmap_key_padded_size, hashmap_key_size, hash_random_vector, (uint8_t*)&entry_hash.parts.key_hash, sizeof(entry_hash.parts.key_hash));
//         halfsiphash(h->values + i*hashmap_value_padded_size, hashmap_value_size, hash_random_vector, (uint8_t*)&entry_hash.parts.value_hash, sizeof(entry_hash.parts.value_hash));
//         hash = hash ^ entry_hash.hash;
//     }
//     return hash;
// }

// uint32_t hash_cmp(const void *void_h1, const void *void_h2, size_t _)
// {
//     (void)_;
//     const bl_hashmap_t *h = void_h;
//     uint32_t hash;
//     halfsiphash(&h->count, sizeof(h->count), hash_random_vector, (uint8_t*)&hash, sizeof(hash));
//     for (uint32_t i = 0; i < h->count; i++) {
//         union {
//             struct {uint16_t key_hash, value_hash;} parts;
//             uint32_t hash;
//         } entry_hash;
//         halfsiphash(h->keys + i*hashmap_key_padded_size, hashmap_key_size, hash_random_vector, (uint8_t*)&entry_hash.parts.key_hash, sizeof(entry_hash.parts.key_hash));
//         halfsiphash(h->values + i*hashmap_value_padded_size, hashmap_value_size, hash_random_vector, (uint8_t*)&entry_hash.parts.value_hash, sizeof(entry_hash.parts.value_hash));
//         hash = hash ^ entry_hash.hash;
//     }
//     return hash;
// }

// int32_t array_cmp(const void *void_a, const void *void_b, size_t _)
// {
//     (void)_;
//     const array_t *a = void_a, *b = void_b;
//     if (a == b) return 0;
//     if (a->len != b->len) return a->len - b->len;

//     if (a->stride == 1 && b->stride == 1) {
//         return memcmp(a->data, b->data, a->len*array_item_size);
//     } else {
//         for (int32_t i = 0; i < a->len; i++) {
//             int cmp = memcmp(a->data + i*a->stride*array_item_size, b->data + i*b->stride*array_item_size, array_item_size);
//             if (cmp != 0)
//                 return cmp;
//         }
//         return 0;
//     }
// }

// uint32_t hash_value(const void *mem, size_t len)
// {
//     uint32_t hash;
//     halfsiphash(mem, len, hash_random_vector, (uint8_t*)&hash, sizeof(hash));
//     return hash;
// }

uint32_t bl_hashmap_len(bl_hashmap_t *h)
{
    return h->count;
}

void *bl_hashmap_get(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, const void *key, size_t entry_size_padded)
{
    if (!h || !key || h->capacity == 0) return NULL;

    uint32_t hash = key_hash(key) % (uint32_t)h->capacity;
    for (uint32_t i = hash; h->buckets[i].index1; i = h->buckets[i].index1 - 1) {
        char *entry = h->entries + entry_size_padded*(h->buckets[i].index1-1);
        if (key_cmp(entry, key) == 0)
            return entry;
    }
    return NULL;
}

void bl_hashmap_set_internal(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, const void *entry, size_t entry_size_padded, int32_t index1)
{
    hshow(h);
    uint32_t hash = key_hash(entry) % (uint32_t)h->capacity;
    printf("Hash value = %u\n", hash);
    bl_hash_bucket_t *bucket = &h->buckets[hash];
    if (bucket->index1 == 0) {
        hdebug("Got an empty space\n");
        // Empty space:
        if (index1 == 0) {
            hdebug("Appended to list of keys/values\n");
            index1 = ++h->count;
            h->entries = GC_REALLOC(h->entries, h->count*entry_size_padded);
            memcpy(h->entries + entry_size_padded*(index1-1), entry, entry_size_padded);
        }
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
            if (key_cmp(h->entries + (bucket->index1-1)*entry_size_padded, entry) == 0) {
                // Found exact key, so clobber old value:
                hdebug("Clobbering key\n");
                memcpy(h->entries + entry_size_padded*(bucket->index1-1), entry, entry_size_padded);
                return;
            } else if (bucket->next1 == 0) {
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

    if (index1 == 0) {
        hdebug("Appending key/value\n");
        index1 = ++h->count;
        h->entries = GC_REALLOC(h->entries, h->count*entry_size_padded);
        memcpy(h->entries + entry_size_padded*(index1-1), entry, entry_size_padded);
    }

    bucket->next1 = 0;
    bucket->index1 = index1;
    hshow(h);
}

static void hashmap_resize(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, uint32_t new_capacity, size_t entry_size_padded)
{
    uint32_t old_count = h->count;
    h->buckets = GC_MALLOC_ATOMIC((size_t)new_capacity*sizeof(bl_hash_bucket_t));
    h->capacity = new_capacity;
    h->lastfree_index1 = new_capacity;
    // Rehash:
    for (uint32_t i = 1; i <= old_count; i++)
        bl_hashmap_set_internal(h, key_hash, key_cmp, h->entries + entry_size_padded*(i-1), entry_size_padded, i);
}

void bl_hashmap_set(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *entry)
{
    if (!h || !entry) return;

    // if (h->copy_on_write) {
    //     if (h->keys) h->keys = memcpy(GC_MALLOC(h->count * key_size_padded), h->keys, h->count * key_size_padded);
    //     if (h->values) h->values = memcpy(GC_MALLOC(h->count * value_size_padded), h->values, h->count * value_size_padded);
    //     if (h->buckets) h->buckets = memcpy(GC_MALLOC_ATOMIC(h->capacity * sizeof(bl_hash_bucket_t)), h->buckets, h->capacity * sizeof(bl_hash_bucket_t));
    // }

    if (h->capacity == 0) {
        hashmap_resize(h, key_hash, key_cmp, 4, entry_size_padded);
    } else if (h->count >= h->capacity) {
        uint32_t newsize = h->capacity;
        if (h->count + 1 > newsize) newsize *= 2;
        else if (h->count + 1 <= newsize/2) newsize /= 2;
        hashmap_resize(h, key_hash, key_cmp, newsize, entry_size_padded);
    }

    bl_hashmap_set_internal(h, key_hash, key_cmp, entry, entry_size_padded, 0);
}

void bl_hashmap_remove(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key)
{
    if (!h || !key || h->capacity == 0) return;

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

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1
