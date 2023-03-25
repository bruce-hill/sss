#pragma once
#include <stdalign.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

typedef struct {
    uint32_t index1, next1;
} bl_hash_bucket_t;

typedef uint32_t (hash_fn_t)(const void *data);
typedef int32_t (cmp_fn_t)(const void *a, const void *b);

typedef struct bl_hashmap_s {
    char *entries;
    bl_hash_bucket_t *buckets;
    struct bl_hashmap_s *fallback;
    uint32_t capacity, count, lastfree_index1;
    bool copy_on_write;
} bl_hashmap_t;

void *bl_hashmap_set(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *entry);
void *bl_hashmap_get(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key);
void bl_hashmap_remove(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key);
uint32_t bl_hashmap_len(bl_hashmap_t *h);
void *bl_hashmap_nth(bl_hashmap_t *h, int32_t n, size_t entry_size_padded);
uint32_t bl_hashmap_hash(bl_hashmap_t *h, hash_fn_t entry_hash, size_t entry_size_padded);
int32_t bl_hashmap_compare(bl_hashmap_t *h1, bl_hashmap_t *h2, hash_fn_t key_hash, cmp_fn_t key_cmp, cmp_fn_t entry_cmp, size_t entry_size_padded);
