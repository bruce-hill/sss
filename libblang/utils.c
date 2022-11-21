#include <bhash.h>
#include <dirent.h>
#include <errno.h>
#include <gc.h>
#include <gc/cord.h>
#include <intern.h>
#include <limits.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

void fail(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    raise(SIGABRT);
}

const char *readdir_str(DIR* dir)
{
    struct dirent *ent = readdir(dir);
    return ent ? intern_str(ent->d_name) : NULL;
}

const char *last_err()
{
    return intern_str(strerror(errno));
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

typedef struct {
    int64_t first,step,last;
} range_t;
const char *range_tostring(range_t range, void *stack) {
    (void)stack;
    CORD str = NULL;
    if (range.first != INT64_MIN)
        CORD_sprintf(&str, "%ld", range.first);
    if (range.step != 1)
        CORD_sprintf(&str, "%r,%ld", str, range.step);
    if (range.last != INT64_MAX)
        CORD_sprintf(&str, "%r..%ld", str, range.last);
    else
        str = CORD_cat(str, "..");
    return intern_str(CORD_to_char_star(str));
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
