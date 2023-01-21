#include <bhash.h>
#include <dirent.h>
#include <errno.h>
#include <gc.h>
#include <gc/cord.h>
#include <intern.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#include "utils.h"
#include "string.h"
#include "range.h"

str_array_t arg_list(int argc, char *argv[]) {
    // Skip program name:
    --argc;
    ++argv;
    str_array_t args = {.length=argc, .stride=1, .items=GC_MALLOC(sizeof(string_t)*argc)};
    for (int i = 0; i < argc; i++) {
        args.items[i] = (string_t){
            .data=intern_str(argv[i]),
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
    return ent ? intern_str(ent->d_name) : NULL;
}

string_t last_err()
{
    const char *str = strerror(errno);
    return (string_t){.data=intern_str(str), .length=strlen(str), .stride=1};
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

    if (range.last != INT64_MIN)
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
        if (range.last == INT64_MIN) range.last = 1;
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
        if (range.last == INT64_MIN) range.last = array.length;
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
    if (len > array.length/range.stride) len = array.length/range.stride;
    if (len < 0) len = -len;

    return (string_t){
        (char*)array.data + item_size*(range.first-1),
        (int32_t)len,
        (int32_t)(array.stride * range.stride),
    };
}

#define toggle(val, nil) ((void*)((int64_t)(val) ^ (int64_t)(nil)))
void bl_table_set(hashmap_t *h, const void *key, const void *value, const void *key_nil, const void *value_nil) {
    hashmap_set(h, toggle(key,key_nil), toggle(value,value_nil));
}
void *bl_table_get(hashmap_t *h, const void *key, const void *key_nil, const void *value_nil) {
    return toggle(hashmap_get(h, toggle(key,key_nil)), value_nil);
}
const void *bl_table_next(hashmap_t *h, const void *key, const void *key_nil) {
    return toggle(hashmap_next(h, toggle(key,key_nil)), key_nil);
}
