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

// #define DEBUG_HASHTABLE 1

typedef struct bl_hashmap_s {
    char *entries;
    bl_hash_bucket_t *buckets;
    struct bl_hashmap_s *fallback;
    void *default_value;
    uint32_t capacity, count, lastfree_index1;
    bool copy_on_write;
} bl_hashmap_t;

uint32_t hash_64bits(const void *x);
int compare_64bits(const void *x, const void *y);
uint32_t hash_str(const void *x);
int compare_str(const void *x, const void *y);
#define FIX_STR(x) _Generic(x, char*:(char*)x, default:x)
#define HASH_FN(t) _Generic(t, char*:hash_str, const char*:hash_str, default: hash_64bits)
#define COMPARE_FN(t) _Generic(t, char*:compare_str, const char*:compare_str, default:compare_64bits)
#define _hset(h, key, val) ((__typeof__(val)*)bl_hashmap_set(h, HASH_FN(key), COMPARE_FN(key), sizeof(struct{ __typeof__ (key) _k; __typeof__ (val) _v; }), &(__typeof__(key)){key}, offsetof(struct{ __typeof__ (key) _k; __typeof__ (val) _v; }, _v), &(__typeof__(val)){val}))
#define hset(h, key, val) _hset(h, FIX_STR(key), FIX_STR(val))
#define _hget(h, key, val_t) ((val_t *)bl_hashmap_get(h, HASH_FN(key), COMPARE_FN(key), sizeof(struct{ __typeof__ (key) _k; val_t _v; }), &(__typeof__(key)){key}, offsetof(struct{ __typeof__ (key) _k; val_t _v; }, _v)))
#define hget_opt(h, key, val_t) _hget(h, FIX_STR(key), val_t)
#define hget(h, key, val_t) ({val_t *_raw = _hget(h, FIX_STR(key), val_t); _raw ? *_raw : 0;})
#define _hremove(h, key, val_t) bl_hashmap_remove(h, HASH_FN(key), COMPARE_FN(key), sizeof(struct{ __typeof__ (key) _k; val_t _v; }), &key)
#define hremove(h, key, val_t) _hremove(h, FIX_STR(key), val_t)
#define hnth(h, i, key_t, val_t) (struct{ key_t key; val_t value;}*)bl_hashmap_nth(h, i, sizeof(struct{ key_t _k; val_t _v; }))

void *bl_hashmap_set(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key, size_t value_offset, const void *value);
void *bl_hashmap_get(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key, size_t value_offset);
void bl_hashmap_remove(bl_hashmap_t *h, hash_fn_t key_hash, cmp_fn_t key_cmp, size_t entry_size_padded, const void *key);
uint32_t bl_hashmap_len(bl_hashmap_t *h);
void bl_hashmap_mark_cow(bl_hashmap_t *h);
void *bl_hashmap_nth(bl_hashmap_t *h, int32_t n, size_t entry_size_padded);
uint32_t bl_hashmap_hash(bl_hashmap_t *h, hash_fn_t entry_hash, size_t entry_size_padded);
int32_t bl_hashmap_compare(bl_hashmap_t *h1, bl_hashmap_t *h2, hash_fn_t key_hash, cmp_fn_t key_cmp, cmp_fn_t value_cmp, size_t entry_size_padded, size_t value_offset);
