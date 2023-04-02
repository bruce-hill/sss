#include <dirent.h>
#include <errno.h>
#include <gc.h>
#include <gc/cord.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "utils.h"
#include "string.h"
#include "range.h"

const char *heap_strn(const char *str, size_t len)
{
    if (!str) return NULL;
    if (len == 0) return "";
    char *heaped = GC_MALLOC_ATOMIC(len + 1);
    memcpy(heaped, str, len);
    heaped[len] = '\0';
    return heaped;
}

const char *heap_str(const char *str)
{
    return heap_strn(str, strlen(str));
}

const char *heap_strf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    char *tmp = NULL;
    int len = vasprintf(&tmp, fmt, args);
    if (len < 0) return NULL;
    va_end(args);
    const char *ret = heap_strn(tmp, (size_t)len);
    free(tmp);
    return ret;
}

string_t first_arg(char *argv[]) {
    return (string_t){
        .data=heap_str(argv[0]),
        .length=(int32_t)strlen(argv[0]),
        .stride=1,
    };
}

str_array_t arg_list(int argc, char *argv[]) {
    // Skip program name:
    --argc;
    ++argv;
    str_array_t args = {.length=argc, .stride=1, .items=GC_MALLOC(sizeof(string_t)*argc)};
    for (int i = 0; i < argc; i++) {
        args.items[i] = (string_t){
            .data=heap_str(argv[i]),
            .length=(int32_t)strlen(argv[i]),
            .stride=1,
        };
    }
    return args;
}

void say(string_t str, string_t end)
{
    if (str.stride == 1) {
        write(STDOUT_FILENO, str.data, str.length);
    } else {
        for (int32_t i = 0; i < str.length; i++)
            write(STDOUT_FILENO, str.data + i*str.stride, 1);
    }

    if (end.stride == 1) {
        write(STDOUT_FILENO, end.data, end.length);
    } else {
        for (int32_t i = 0; i < end.length; i++)
            write(STDOUT_FILENO, end.data + i*end.stride, 1);
    }
}

void fail(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    raise(SIGABRT);
}


void fail_array(string_t fmt, ...)
{
    char buf[fmt.length+1];
    for (int32_t i = 0; i < fmt.length; i++)
        buf[i] = fmt.data[i*fmt.stride];
    buf[fmt.length] = '\0';

    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, buf, args);
    va_end(args);
    raise(SIGABRT);
}

double sane_fmod(double num, double modulus)
{
    double result = fmod(num, modulus);
    return (result < 0) != (modulus < 0) ? result + modulus : result;
}

const char *readdir_str(DIR* dir)
{
    struct dirent *ent = readdir(dir);
    return ent ? heap_str(ent->d_name) : NULL;
}

string_t last_err()
{
    const char *str = strerror(errno);
    return (string_t){.data=heap_str(str), .length=strlen(str), .stride=1};
}

typedef struct {
    int64_t seconds, nanoseconds;
} bl_time_t;

typedef struct {
    int64_t device, rdevice;
    int64_t inode, mode, links, user, group;
    int64_t size, block_size, block_count;
    bl_time_t *accessed, *modified, *moved;
} bl_fileinfo_t;

bl_fileinfo_t *bl_fstat(FILE* f)
{
    struct stat buf;
    if (fstat(fileno(f), &buf) != 0)
        return NULL;
    bl_fileinfo_t *ret = GC_malloc(sizeof(bl_fileinfo_t));
    ret->device = buf.st_dev;
    ret->rdevice = buf.st_rdev;
    ret->inode = buf.st_ino;
    ret->mode = buf.st_mode;
    ret->links = buf.st_nlink;
    ret->user = buf.st_uid;
    ret->group = buf.st_gid;
    ret->size = buf.st_size;
    ret->block_size = buf.st_blksize;
    ret->block_count = buf.st_blocks;

    ret->accessed = GC_malloc(sizeof(bl_time_t));
    ret->accessed->seconds = buf.st_atim.tv_sec;
    ret->accessed->nanoseconds = buf.st_atim.tv_nsec;

    ret->modified = GC_malloc(sizeof(bl_time_t));
    ret->modified->seconds = buf.st_mtim.tv_sec;
    ret->modified->nanoseconds = buf.st_mtim.tv_nsec;

    ret->moved = GC_malloc(sizeof(bl_time_t));
    ret->moved->seconds = buf.st_ctim.tv_sec;
    ret->moved->nanoseconds = buf.st_ctim.tv_nsec;

    return ret;
}

int range_print(range_t range, FILE *f, void *stack) {
    (void)stack;
    int printed = 0;
    if (range.first != INT64_MIN)
        printed += fprintf(f, "%ld", range.first);

    if (range.stride < 0)
        printed += fprintf(f, "..%ld", range.stride);
    else if (range.stride != 1)
        printed += fprintf(f, "..+%ld", range.stride);

    if (range.last != INT64_MAX)
        printed += fprintf(f, "..%ld", range.last);
    else
        printed += fprintf(f, "..");

    return printed;
}

string_t range_slice(string_t array, range_t range, int64_t item_size)
{
    if (range.stride == 0) {
        // printf("Zero stride\n");
        return (string_t){array.data, 0, 1};
    } else if (range.stride < 0) {
        if (range.first == INT64_MIN) range.first = array.length;
        if (range.last == INT64_MAX) range.last = 1;
        if (range.first > array.length) {
            // printf("Range starting after array\n");
            range.first = (array.length % (-array.stride)) + (range.first % (-array.stride));
        }
        if (range.first < 1 || range.first > array.length) {
            // printf("Range outside array\n");
            return (string_t){array.data, 0, 1};
        }
    } else {
        if (range.first == INT64_MIN) range.first = 1;
        if (range.last == INT64_MAX) range.last = array.length;
        if (range.first < 1) {
            // printf("Range starting before array\n");
            range.first = range.first % array.stride;
        }
        if (range.first < 1) range.first += array.stride;
        if (range.first > array.length) {
            // printf("Range outside array\n");
            return (string_t){array.data, 0, 1};
        }
    }

    int64_t len = (range.last - range.first) / range.stride + 1;
    // If less than zero, set to zero (without a conditional branch)
    len = len & ~(len >> 63);
    if (len > array.length/labs(range.stride) + 1) len = array.length/labs(range.stride) + 1;
    if (len < 0) len = -len;

    return (string_t){
        (char*)array.data + item_size*(range.first-1),
        (int32_t)len,
        (int32_t)(array.stride * range.stride),
    };
}

// Copy on write for arrays:
void array_cow(void *voidarr, size_t item_size, bool atomic)
{
    struct {char *data; int32_t len, stride, free;} *arr = voidarr;
    char *copy = atomic ? GC_MALLOC_ATOMIC(arr->len * item_size) : GC_MALLOC(arr->len * item_size);
    if (arr->stride == 1) {
        memcpy(copy, arr->data, arr->len * item_size);
    } else {
        for (int32_t i = 0; i < arr->len; i++)
            memcpy(copy + i*item_size, arr->data + arr->stride*i*item_size, item_size);
    }
    arr->data = copy;
    arr->free = 0;
}

void array_flatten(void *voidarr, size_t item_size, bool atomic)
{
    struct {char *data; int32_t len, stride, free;} *arr = voidarr;
    char *copy = atomic ? GC_MALLOC_ATOMIC(arr->len * item_size) : GC_MALLOC(arr->len * item_size);
    if (arr->stride == 1) {
        memcpy(copy, arr->data, arr->len * item_size);
    } else {
        for (int32_t i = 0; i < arr->len; i++)
            memcpy(copy + i*item_size, arr->data + arr->stride*i*item_size, item_size);
    }
    arr->stride = 1;
    arr->data = copy;
    arr->free = 0;
}

void array_insert(void *voidarr, char *item, int64_t index, size_t item_size, bool atomic)
{
    struct {char *data; int32_t len, stride, free;} *arr = voidarr;
    if (index < 1) index = 1;
    else if (index > (int64_t)arr->len + 1) index = (int64_t)arr->len + 1;

    if (arr->free < 1 || arr->stride != 1) {
        arr->free = 6;
        char *copy = atomic ? GC_MALLOC_ATOMIC((arr->len + arr->free) * item_size) : GC_MALLOC((arr->len + arr->free) * item_size);
        for (int32_t i = 0; i < index-1; i++)
            memcpy(copy + i*item_size, arr->data + arr->stride*i*item_size, item_size);
        for (int32_t i = index-1; i < arr->len; i++)
            memcpy(copy + (i+1)*item_size, arr->data + arr->stride*i*item_size, item_size);
        arr->data = copy;
    } else if (index != arr->len+1) {
        memmove(arr->data + index*item_size, arr->data + (index-1)*item_size, (arr->len - index)*item_size);
    }
    --arr->free;
    ++arr->len;
    memcpy(arr->data + (index-1)*item_size, item, item_size);
}

void array_insert_all(void *voidarr, void *voidarr2, int64_t index, size_t item_size, bool atomic)
{
    struct {char *data; int32_t len, stride, free;} *arr = voidarr, *arr2 = voidarr2;
    if (index < 1) index = 1;
    else if (index > (int64_t)arr->len + 1) index = (int64_t)arr->len + 1;

    if (arr->free < arr2->len || arr->stride != 1) {
        arr->free = arr2->len;
        char *copy = atomic ? GC_MALLOC_ATOMIC((arr->len + arr->free) * item_size) : GC_MALLOC((arr->len + arr->free) * item_size);
        for (int32_t i = 0; i < index-1; i++)
            memcpy(copy + i*item_size, arr->data + arr->stride*i*item_size, item_size);
        for (int32_t i = index-1; i < arr->len; i++)
            memcpy(copy + (i+arr2->len)*item_size, arr->data + arr->stride*i*item_size, item_size);
        arr->data = copy;
    } else if (index != arr->len+1) {
        memmove(arr->data + index*item_size, arr->data + (index-1)*item_size, (arr->len - index + arr2->len-1)*item_size);
    }
    arr->free -= arr2->len;
    arr->len += arr2->len;
    for (int64_t i = 0; i < arr2->len; i++)
        memcpy(arr->data + (index-1 + i)*item_size, arr2->data + i*item_size*arr2->stride, item_size);
}

void array_remove(void *voidarr, int64_t index, int64_t count, size_t item_size, bool atomic)
{
    struct {char *data; int32_t len, stride, free;} *arr = voidarr;
    if (index < 1 || index > (int64_t)arr->len || count < 1) return;

    if (count > arr->len - index + 1)
        count = (arr->len - index) + 1;

    if (index + count > arr->len) {
        if (arr->free >= 0)
            arr->free += count;
    } else if (arr->free < 0 || arr->stride != 1) { // Copy on write
        char *copy = atomic ? GC_MALLOC_ATOMIC((arr->len-1) * item_size) : GC_MALLOC((arr->len-1) * item_size);
        for (int32_t src = 1, dest = 1; src <= arr->len; src++) {
            if (src < index || src >= index + count) {
                memcpy(copy + (dest - 1)*item_size, arr->data + arr->stride*(src - 1)*item_size, item_size);
                ++dest;
            }
        }
        arr->data = copy;
        arr->free = 0;
    } else {
        memmove(arr->data + (index-1)*item_size, arr->data + (index-1 + count)*item_size, (arr->len - index + count - 1)*item_size);
        arr->free += count;
    }
    arr->len -= count;
}

typedef int (cmp_fn_t)(const void *a, const void *b);
void array_sort(void *voidarr, cmp_fn_t compare, size_t item_size, bool atomic)
{
    struct {char *data; int32_t len, stride, free;} *arr = voidarr;
    if (arr->free < 0 || arr->stride != 1)
        array_flatten(voidarr, item_size, atomic);
    qsort(arr->data, arr->len, item_size, compare);
}

void array_shuffle(void *voidarr, size_t item_size, bool atomic)
{
    struct {char *data; int32_t len, stride, free;} *arr = voidarr;
    if (arr->free < 0 || arr->stride != 1)
        array_flatten(voidarr, item_size, atomic);

    char tmp[item_size];
    for (int32_t i = arr->len-1; i > 1; i--) {
        int32_t j = arc4random_uniform(i+1);
        memcpy(tmp, arr->data + i*item_size, item_size);
        memcpy(arr->data + i*item_size, arr->data + j*item_size, item_size);
        memcpy(arr->data + j*item_size, tmp, item_size);
    }
}
