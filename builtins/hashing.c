#include <gc.h>
#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/param.h>

#include "../SipHash/halfsiphash.h"
#include "table.h"
#include "types.h"
#include "string.h"

#ifndef auto
#define auto __auto_type
#endif

extern const void *SSS_HASH_VECTOR;

uint32_t generic_hash(const Type *type, const void *obj)
{
    switch (type->hash.tag) {
    case HashNotImplemented:
        return 0;
    case HashFunction:
        return type->hash.__data.HashFunction.fn(obj);
    case HashData: {
        uint32_t hash;
        halfsiphash((void*)obj, type->hash.__data.HashData.size, SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
        return hash;
    }
    case HashArray: {
        // Array hash is calculated as a rolling, compacting hash of the length of the array, followed by
        // the hashes of its items (or the items themselves if they're small plain data)
        // In other words, it reads in a chunk of items or item hashes, then when it fills up the chunk,
        // hashes it down to a single item to start the next chunk. This repeats until the end, when it
        // hashes the last chunk down to a uint32_t.
        string_t *s = (string_t*)obj;
        auto item = type->info.__data.ArrayInfo.item;

        if (item->hash.tag == HashData) {
            size_t item_size = item->hash.__data.HashData.size;
            uint8_t hash_batch[4 + 8*item_size];
            uint8_t *p = hash_batch, *end = hash_batch + sizeof(hash_batch);
            unsigned long length = s->length;
            memcpy((p += sizeof(void*)), &length, sizeof(length));
            for (unsigned long i = 0; i < s->length; i++) {
                if (p >= end) {
                    uint32_t chunk_hash;
                    halfsiphash(&hash_batch, sizeof(hash_batch), SSS_HASH_VECTOR, (uint8_t*)&chunk_hash, sizeof(chunk_hash));
                    p = hash_batch;
                    *(uint32_t*)p = chunk_hash;
                    p += sizeof(uint32_t);
                }
                memcpy((p += item_size), &s->data[i*s->stride], item->hash.__data.HashData.size);
            }
            uint32_t hash;
            halfsiphash(&hash_batch, ((int64_t)p) - ((int64_t)hash_batch), SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
            return hash;
        } else {
            uint32_t hash_batch[16] = {(uint32_t)s->length};
            uint32_t *p = &hash_batch[1], *end = hash_batch + sizeof(hash_batch)/sizeof(hash_batch[0]);
            for (unsigned long i = 0; i < s->length; i++) {
                if (p >= end) {
                    uint64_t chunk_hash;
                    halfsiphash(&hash_batch, sizeof(hash_batch), SSS_HASH_VECTOR, (uint8_t*)&chunk_hash, sizeof(chunk_hash));
                    p = hash_batch;
                    *(p++) = chunk_hash;
                }
                *(p++) = generic_hash(item, &s->data[i*s->stride]);
            }
            uint32_t hash;
            halfsiphash(&hash_batch, ((int64_t)p) - ((int64_t)hash_batch), SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
            return hash;
        }
    }
    case HashTable: {
        // Table hashes are computed as:
        // hash(#t, xor(hash(k) for k in t.keys), xor(hash(v) for v in t.values), hash(t.fallback), hash(t.default))
        // Where fallback and default hash to zero if absent
        auto table = type->info.__data.TableInfo;
        size_t entry_size = table.entry_size;
        size_t value_offset = table.value_offset;
        table_t *t = (table_t*)obj;

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
    }
    return 0;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
