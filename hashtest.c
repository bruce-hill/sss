#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/random.h>
#include <gc.h>
#include <assert.h>

#include "libblang/hashmap.h"
#include "SipHash/halfsiphash.h"

#include "SipHash/halfsiphash.c"

static inline size_t padded_size(size_t size, size_t align) {
    return size % align ? (size - (size % align)) + align : size;
}

#define padded_sizeof(x) padded_size(sizeof(x), __alignof__(x))

typedef struct {
    char *data;
    int32_t len, stride;
} char_array_t;

static char_array_t make_str(char *s, size_t len)
{
    return (char_array_t){.data=memcpy(GC_MALLOC_ATOMIC(len+1), s, len), .len=(int32_t)len, .stride=1};
}

static uint8_t hash_random_vector[16] = {42,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
static uint32_t hash_int(const void *x)
{
    uint32_t hash;
    halfsiphash(x, sizeof(int), hash_random_vector, (uint8_t*)&hash, sizeof(hash));
    return hash;
}

static int32_t cmp_ints(const void *a, const void *b)
{
    return *(int*)a - *(int*)b;
}

static uint32_t hash_char_array(const void *v)
{
    const char_array_t *arr = v;
    uint32_t hash;
    halfsiphash(arr->data, sizeof(char)*arr->len, hash_random_vector, (uint8_t*)&hash, sizeof(hash));
    return hash;
}

static int32_t cmp_char_arrays(const void *va, const void *vb)
{
    const char_array_t *a = va, *b = vb;
    if (a->len != b->len) return a->len - b->len;
    return memcmp(a->data, b->data, a->len);
}

int main(void) {
    getrandom(hash_random_vector, sizeof(hash_random_vector), 0);
    bl_hashmap_t h = {0};

    for (;;) {
        printf("Choose a type to test: (i)nt (s)tring (S)tring set\n");
        switch (getchar()) {
            case 'i': {
                typedef struct {
                    int key, value;
                } int_entry_t;
                char *line = NULL;
                size_t len = 0;
                ssize_t nread;
                while ((nread = getline(&line, &len, stdin)) > 0) {
                    if (*line == '?') {
                        int key = strtol(line+1, NULL, 10);
                        int *val = bl_hashmap_get(&h, hash_int, cmp_ints, padded_sizeof(int_entry_t), &key);
                        if (val)
                            printf("= %d\n", *val);
                        else
                            printf("= <missing>\n");
                    }
                    char *eq = strchr(line, '=');
                    if (eq) {
                        int_entry_t entry = {
                            .key = strtol(line, NULL, 10),
                            .value = strtol(eq+1, NULL, 10),
                        };
                        bl_hashmap_set(&h, hash_int, cmp_ints, padded_sizeof(int_entry_t), &entry);
                    } else {
                        int key = strtol(line, NULL, 10);
                        bl_hashmap_remove(&h, hash_int, cmp_ints, padded_sizeof(int_entry_t), &key);
                    }

                    printf("{");
                    for (int32_t i = 1; i <= (int32_t)h.count; i++) {
                        if (i > 1) printf(", ");
                        int_entry_t *entry = bl_hashmap_nth(&h, i, padded_sizeof(int_entry_t));
                        printf("[%d] = %d", entry->key, entry->value);
                    }
                    printf("}\n");
                }
                break;
            }
            case 's': {
                typedef struct {
                    char_array_t key, value;
                } str_entry_t;
                char *line = NULL;
                size_t len = 0;
                ssize_t nread;
                while ((nread = getline(&line, &len, stdin)) > 0) {
                    char *eq = strchr(line, '=');
                    if (eq) {
                        str_entry_t entry = {
                            .key = make_str(line, (size_t)(eq-line)),
                            .value = make_str(eq+1, strlen(eq+1)-1),
                        };
                        bl_hashmap_set(&h, hash_char_array, cmp_char_arrays, padded_sizeof(str_entry_t), &entry);
                    } else {
                        char_array_t key = make_str(line, strlen(line)-1);
                        bl_hashmap_remove(&h, hash_char_array, cmp_char_arrays, padded_sizeof(str_entry_t), &key);
                    }

                    printf("{");
                    for (int32_t i = 1; i <= (int32_t)h.count; i++) {
                        if (i > 1) printf(", ");
                        str_entry_t *entry = bl_hashmap_nth(&h, i, padded_sizeof(str_entry_t));
                        printf("%.*s = %.*s", entry->key.len, entry->key.data, entry->value.len, entry->value.data);
                    }
                    printf("}\n");
                }
                break;
            }
            case 'S': {
                typedef struct {
                    char_array_t key;
                    bool value;
                } str_set_entry_t;
                char *line = NULL;
                size_t len = 0;
                ssize_t nread;
                while ((nread = getline(&line, &len, stdin)) > 0) {
                    char *eq = strchr(line, '=');
                    if (eq) {
                        str_set_entry_t entry = {
                            .key = make_str(line, (size_t)(eq-line)),
                            .value = true,
                        };
                        bl_hashmap_set(&h, hash_char_array, cmp_char_arrays, padded_sizeof(str_set_entry_t), &entry);
                    } else {
                        char_array_t key = make_str(line, strlen(line)-1);
                        bl_hashmap_remove(&h, hash_char_array, cmp_char_arrays, padded_sizeof(str_set_entry_t), &key);
                    }

                    printf("{");
                    for (int32_t i = 1; i <= (int32_t)h.count; i++) {
                        if (i > 1) printf(", ");
                        str_set_entry_t *entry = bl_hashmap_nth(&h, i, padded_sizeof(str_set_entry_t));
                        printf("%.*s = %s", entry->key.len, entry->key.data, entry->value ? "yes" : "no");
                    }
                    printf("}\n");
                }
                break;
            }
            default: return 0;
        }
    }
    return 0;
}
