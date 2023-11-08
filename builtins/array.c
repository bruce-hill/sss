
#include <ctype.h>
#include <err.h>
#include <gc.h>
#include <gc/cord.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/param.h>

#include "array.h"
#include "types.h"
#include "functions.h"
#include "../SipHash/halfsiphash.h"

extern const void *SSS_HASH_VECTOR;

// Replace the array's .data pointer with a new pointer to a copy of the
// data that is compacted and has a stride of exactly `item_size`
void Array_compact(array_t *arr, size_t item_size)
{
    void *copy = NULL;
    if (arr->length > 0) {
        copy = arr->atomic ? GC_MALLOC_ATOMIC(arr->length * item_size) : GC_MALLOC(arr->length * item_size);
        if ((size_t)arr->stride == item_size) {
            memcpy(copy, arr->data, arr->length * item_size);
        } else {
            for (unsigned long i = 0; i < arr->length; i++)
                memcpy(copy + i*item_size, arr->data + arr->stride*i, item_size);
        }
    }
    *arr = (array_t){
        .data=copy,
        .length=arr->length,
        .stride=item_size,
        .free=0,
        .atomic=arr->atomic,
        .copy_on_write=0,
    };
}

void Array_insert(array_t *arr, const void *item, int64_t index, size_t item_size)
{
    if (index < 1) index = 1;
    else if (index > (int64_t)arr->length + 1) index = (int64_t)arr->length + 1;

    if (!arr->data) {
        arr->data = arr->atomic ? GC_MALLOC_ATOMIC(item_size) : GC_MALLOC(item_size);
        arr->free = 1;
    } else if (arr->free < 1 || (size_t)arr->stride != item_size) {
        arr->free = 6;
        void *copy = arr->atomic ? GC_MALLOC_ATOMIC((arr->length + arr->free) * item_size) : GC_MALLOC((arr->length + arr->free) * item_size);
        for (int64_t i = 0; i < index-1; i++)
            memcpy(copy + i*item_size, arr->data + arr->stride*i, item_size);
        for (int64_t i = index-1; i < (int64_t)arr->length; i++)
            memcpy(copy + (i+1)*item_size, arr->data + arr->stride*i, item_size);
        arr->data = copy;
        arr->copy_on_write = 0;
    } else {
        if (arr->copy_on_write)
            Array_compact(arr, item_size);

        if (index != arr->length+1)
            memmove((void*)arr->data + index*item_size, arr->data + (index-1)*item_size, (arr->length - index)*item_size);
    }
    --arr->free;
    ++arr->length;
    memcpy((void*)arr->data + (index-1)*item_size, item, item_size);
}

void Array_insert_all(array_t *arr, array_t to_insert, int64_t index, size_t item_size)
{
    if (index < 1) index = 1;
    else if (index > (int64_t)arr->length + 1) index = (int64_t)arr->length + 1;

    if (!arr->data) {
        arr->data = arr->atomic ? GC_MALLOC_ATOMIC(item_size*to_insert.length) : GC_MALLOC(item_size*to_insert.length);
        arr->free = to_insert.length;
    } else if ((int64_t)arr->free < (int64_t)to_insert.length || (size_t)arr->stride != item_size) {
        arr->free = to_insert.length;
        void *copy = arr->atomic ? GC_MALLOC_ATOMIC((arr->length + arr->free) * item_size) : GC_MALLOC((arr->length + arr->free) * item_size);
        for (int64_t i = 0; i < index-1; i++)
            memcpy(copy + i*item_size, arr->data + arr->stride*i, item_size);
        for (int64_t i = index-1; i < (int64_t)arr->length; i++)
            memcpy(copy + (i+to_insert.length)*item_size, arr->data + arr->stride*i, item_size);
        arr->data = copy;
        arr->copy_on_write = 0;
    } else {
        if (arr->copy_on_write)
            Array_compact(arr, item_size);

        if (index != arr->length+1)
            memmove((void*)arr->data + index*item_size, arr->data + (index-1)*item_size, (arr->length - index + to_insert.length-1)*item_size);
    }
    arr->free -= to_insert.length;
    arr->length += to_insert.length;
    for (unsigned long i = 0; i < to_insert.length; i++)
        memcpy((void*)arr->data + (index-1 + i)*item_size, to_insert.data + i*to_insert.stride, item_size);
}

void Array_remove(array_t *arr, int64_t index, int64_t count, size_t item_size)
{
    if (index < 1 || index > (int64_t)arr->length || count < 1) return;

    if (count > arr->length - index + 1)
        count = (arr->length - index) + 1;

    // TODO: optimize arr.remove(1) by just updating the .data and .length values

    if (index + count > arr->length) {
        if (arr->free >= 0)
            arr->free += count;
    } else if (arr->copy_on_write || (size_t)arr->stride != item_size) {
        void *copy = arr->atomic ? GC_MALLOC_ATOMIC((arr->length-1) * item_size) : GC_MALLOC((arr->length-1) * item_size);
        for (int64_t src = 1, dest = 1; src <= (int64_t)arr->length; src++) {
            if (src < index || src >= index + count) {
                memcpy(copy + (dest - 1)*item_size, arr->data + arr->stride*(src - 1), item_size);
                ++dest;
            }
        }
        arr->data = copy;
        arr->free = 0;
        arr->copy_on_write = 0;
    } else {
        memmove((void*)arr->data + (index-1)*item_size, arr->data + (index-1 + count)*item_size, (arr->length - index + count - 1)*item_size);
        arr->free += count;
    }
    arr->length -= count;
}

void Array_sort(const Type *type, array_t *arr)
{
    const Type *item_type = type->info.__data.ArrayInfo.item;
    size_t item_size = item_type->size;
    if (item_type->align > 1 && item_size % item_type->align)
        item_size += item_type->align - (item_size % item_type->align); // padding

    if (arr->copy_on_write || (size_t)arr->stride != item_size)
        Array_compact(arr, item_size);

    qsort_r(arr->data, arr->length, item_size, (void*)generic_compare, (void*)item_type);
}

void Array_shuffle(array_t *arr, size_t item_size)
{
    if (arr->copy_on_write || (size_t)arr->stride != item_size)
        Array_compact(arr, item_size);

    char tmp[item_size];
    for (unsigned long i = arr->length-1; i > 1; i--) {
        int32_t j = arc4random_uniform(i+1);
        memcpy(tmp, arr->data + i*item_size, item_size);
        memcpy((void*)arr->data + i*item_size, arr->data + j*item_size, item_size);
        memcpy((void*)arr->data + j*item_size, tmp, item_size);
    }
}

array_t Array_join(array_t pieces, array_t glue, size_t item_size)
{
    if (pieces.length == 0) return (array_t){.stride=item_size};

    unsigned long length = 0;
    for (unsigned long i = 0; i < pieces.length; i++) {
        if (i > 0) length += glue.length;
        length += ((array_t*)(pieces.data + i*pieces.stride))->length;
    }
    void *data = pieces.atomic ? GC_MALLOC_ATOMIC((size_t)length*item_size) : GC_MALLOC((size_t)length*item_size);
    void *ptr = data;
    for (unsigned long i = 0; i < pieces.length; i++) {
        if (i > 0) {
            for (unsigned long j = 0; j < glue.length; j++)
                ptr = mempcpy(ptr, (void*)glue.data + j*glue.stride, item_size);
        }
        array_t piece = *(array_t*)(pieces.data + i*pieces.stride);
        for (unsigned long j = 0; j < piece.length; j++)
            ptr = mempcpy(ptr, piece.data + j*piece.stride, item_size);
    }
    return (array_t){.data = data, .length = length, .stride = item_size};
}

void Array_clear(array_t *array)
{
    *array = (array_t){.data=0, .length=0};
}

int32_t Array_compare(const array_t *x, const array_t *y, const Type *type)
{
    // Early out for arrays with the same data, e.g. two copies of the same array:
    if (x->data == y->data && x->stride == y->stride)
        return (x->length > y->length) - (x->length < y->length);

    Type *item = type->info.__data.ArrayInfo.item;
    if (item->order.tag == OrderingData) {
        size_t item_size = item->order.__data.OrderingData.size;
        if (x->stride == (int32_t)item_size && y->stride == (int32_t)item_size) {
            int32_t cmp = (int32_t)memcmp(x->data, y->data, MIN(x->length, y->length)*item_size);
            if (cmp != 0) return cmp;
        } else {
            for (int32_t i = 0, len = MIN(x->length, y->length); i < len; i++) {
                int32_t cmp = (int32_t)memcmp(x->data+ x->stride*i, y->data + y->stride*i, item_size);
                if (cmp != 0) return cmp;
            }
        }
    } else {
        for (int32_t i = 0, len = MIN(x->length, y->length); i < len; i++) {
            int32_t cmp = generic_compare(x->data + x->stride*i, y->data + y->stride*i, item);
            if (cmp != 0) return cmp;
        }
    }
    return (x->length > y->length) - (x->length < y->length);
}

CORD Array_cord(const Type *type, const array_t *arr, bool colorize)
{
    Type *item_type = type->info.__data.ArrayInfo.item;
    CORD c = "[";
    for (unsigned long i = 0; i < arr->length; i++) {
        if (i > 0)
            c = CORD_cat(c, ", ");
        CORD item_cord = generic_cord(item_type, arr->data + i*arr->stride, colorize);
        c = CORD_cat(c, item_cord);
    }
    c = CORD_cat(c, "]");
    return c;
}

uint32_t Array_hash(const Type *type, const array_t *arr)
{
    // Array hash is calculated as a rolling, compacting hash of the length of the array, followed by
    // the hashes of its items (or the items themselves if they're small plain data)
    // In other words, it reads in a chunk of items or item hashes, then when it fills up the chunk,
    // hashes it down to a single item to start the next chunk. This repeats until the end, when it
    // hashes the last chunk down to a uint32_t.
    Type *item = type->info.__data.ArrayInfo.item;
    if (item->hash.tag == HashData) {
        size_t item_size = item->hash.__data.HashData.size;
        uint8_t hash_batch[4 + 8*item_size];
        uint8_t *p = hash_batch, *end = hash_batch + sizeof(hash_batch);
        unsigned long length = arr->length;
        memcpy((p += sizeof(void*)), &length, sizeof(length));
        for (unsigned long i = 0; i < arr->length; i++) {
            if (p >= end) {
                uint32_t chunk_hash;
                halfsiphash(&hash_batch, sizeof(hash_batch), SSS_HASH_VECTOR, (uint8_t*)&chunk_hash, sizeof(chunk_hash));
                p = hash_batch;
                *(uint32_t*)p = chunk_hash;
                p += sizeof(uint32_t);
            }
            memcpy((p += item_size), arr->data + i*arr->stride, item->hash.__data.HashData.size);
        }
        uint32_t hash;
        halfsiphash(&hash_batch, ((int64_t)p) - ((int64_t)hash_batch), SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
        return hash;
    } else {
        uint32_t hash_batch[16] = {(uint32_t)arr->length};
        uint32_t *p = &hash_batch[1], *end = hash_batch + sizeof(hash_batch)/sizeof(hash_batch[0]);
        for (unsigned long i = 0; i < arr->length; i++) {
            if (p >= end) {
                uint64_t chunk_hash;
                halfsiphash(&hash_batch, sizeof(hash_batch), SSS_HASH_VECTOR, (uint8_t*)&chunk_hash, sizeof(chunk_hash));
                p = hash_batch;
                *(p++) = chunk_hash;
            }
            *(p++) = generic_hash(item, arr->data + i*arr->stride);
        }
        uint32_t hash;
        halfsiphash(&hash_batch, ((int64_t)p) - ((int64_t)hash_batch), SSS_HASH_VECTOR, (uint8_t*)&hash, sizeof(hash));
        return hash;
    }
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

Type make_array_type(Type *item_type)
{
    const char *item_name = item_type->name.data;
    return (Type){
        .name=STRING(heap_strf("[%s]", item_name)),
        .info={.tag=ArrayInfo, .__data.ArrayInfo={item_type}},
        .size=sizeof(array_t),
        .align=alignof(array_t),
        .order=OrderingMethod(Function, (void*)Array_compare),
        .hash=HashMethod(Function, (void*)Array_hash),
        .cord=CordMethod(Function, (void*)Array_cord),
        .bindings=(NamespaceBinding[]){
            {"compact", heap_strf("func(arr:@[%s], item_size=sizeof(arr[1])) Void", item_name), Array_compact},
            {"insert", heap_strf("func(arr:@[%s], item:%s, index=#arr, item_size=sizeof(item)) Void", item_name, item_name), Array_insert},
            {"insert_all", heap_strf("func(arr:@[%s], items:[%s], index=#arr, item_size=sizeof(arr[1])) Void", item_name, item_name), Array_insert_all},
            {"remove", heap_strf("func(arr:@[%s], index=#arr, count=1, item_size=sizeof(arr[1])) Void", item_name), Array_remove},
            {"sort", heap_strf("func(arr:@[%s], item_size=sizeof(arr[1]), item_type=typeof(%s)) Void", item_name, item_name), Array_sort},
            {"shuffle", heap_strf("func(arr:@[%s], item_size=sizeof(arr[1])) Void", item_name), Array_shuffle},
            {"join", heap_strf("func(pieces:[[%s]], glue:[%s], item_size=sizeof(arr[1])) Void", item_name, item_name), Array_join},
            {"clear", heap_strf("func(arr:@[%s]) Void", item_name), Array_clear},
            {NULL, NULL, NULL},
        },
    };
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0