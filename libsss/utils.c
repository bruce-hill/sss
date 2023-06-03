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
#include <time.h>
#include <unistd.h>
#include <poll.h>

#include "utils.h"
#include "string.h"
#include "range.h"

char *heap_strn(const char *str, size_t len)
{
    if (!str) return NULL;
    if (len == 0) return "";
    char *heaped = GC_MALLOC_ATOMIC(len + 1);
    memcpy(heaped, str, len);
    heaped[len] = '\0';
    return heaped;
}

char *heap_str(const char *str)
{
    return heap_strn(str, strlen(str));
}

char *heap_strf(const char *fmt, ...)
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
    str_array_t args = {.length=argc, .stride=sizeof(string_t), .items=GC_MALLOC(sizeof(string_t)*argc)};
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

void warn(string_t str, string_t end, bool colorize)
{
    if (colorize) write(STDERR_FILENO, "\x1b[33m", 5);
    if (str.stride == 1) {
        write(STDERR_FILENO, str.data, str.length);
    } else {
        for (int32_t i = 0; i < str.length; i++)
            write(STDERR_FILENO, str.data + i*str.stride, 1);
    }

    if (end.stride == 1) {
        write(STDERR_FILENO, end.data, end.length);
    } else {
        for (int32_t i = 0; i < end.length; i++)
            write(STDERR_FILENO, end.data + i*end.stride, 1);
    }
    if (colorize) write(STDERR_FILENO, "\x1b[m", 3);
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

string_t last_err()
{
    const char *str = strerror(errno);
    return (string_t){.data=heap_str(str), .length=strlen(str), .stride=1};
}

typedef struct {
    int64_t seconds, nanoseconds;
} sss_time_t;

string_t sss_time_format(sss_time_t sss_time, string_t fmt)
{
    static char buf[256];
    time_t time = (time_t)sss_time.seconds;
    struct tm my_time;
    localtime_r(&time, &my_time);
    size_t len = strftime(buf, sizeof(buf), c_string(fmt), &my_time);
    char *copy = GC_MALLOC_ATOMIC(len+1);
    memcpy(copy, buf, len);
    copy[len] = '\0';
    return (string_t){.data=copy, .length=(int32_t)len, .stride=1};
}

typedef struct {
    int64_t device, rdevice;
    int64_t inode, mode, links, user, group;
    int64_t size, block_size, block_count;
    sss_time_t accessed, modified, moved;
} sss_fileinfo_t;

sss_fileinfo_t sss_fstat(FILE* f)
{
    struct stat buf;
    if (fstat(fileno(f), &buf) != 0)
        return (sss_fileinfo_t){0};

    sss_fileinfo_t ret = {
        .device = buf.st_dev,
        .rdevice = buf.st_rdev,
        .inode = buf.st_ino,
        .mode = buf.st_mode,
        .links = buf.st_nlink,
        .user = buf.st_uid,
        .group = buf.st_gid,
        .size = buf.st_size,
        .block_size = buf.st_blksize,
        .block_count = buf.st_blocks,
        .accessed = {
            .seconds=buf.st_atim.tv_sec,
            .nanoseconds = buf.st_atim.tv_nsec,
        },
        .modified = {
            .seconds = buf.st_mtim.tv_sec,
            .nanoseconds = buf.st_mtim.tv_nsec,
        },
        .moved = {
            .seconds = buf.st_ctim.tv_sec,
            .nanoseconds = buf.st_ctim.tv_nsec,
        },
    };
    return ret;
}

void range_print(range_t range, FILE *f, void *stack, bool color) {
    (void)stack;
    if (color) fputs("\x1b[0;35m", f);
    if (range.first != INT64_MIN)
        fprintf(f, "%ld", range.first);

    if (color) fputs("\x1b[33m", f);
    fputs("..", f);

    if (range.last != INT64_MAX) {
        if (color) fputs("\x1b[0;35m", f);
        fprintf(f, "%ld", range.last);
    }

    if (range.stride != 1) {
        if (color) fputs("\x1b[0;33m", f);
        fputs(" by ", f);
        if (color) fputs("\x1b[35m", f);
        fprintf(f, "%ld", range.stride);
    }

    if (color) fputs("\x1b[m", f);
}

string_t range_slice(string_t array, range_t range, int64_t item_size)
{
    if (range.stride > INT16_MAX)
        range.stride = INT16_MAX;
    else if (range.stride < INT16_MIN)
        range.stride = INT16_MIN;

    if (range.stride == 0) {
        // printf("Zero stride\n");
        return (string_t){array.data, 0, 1, 0};
    } else if (range.stride < 0) {
        if (range.first == INT64_MIN) range.first = array.length;
        if (range.last == INT64_MAX) range.last = 1;
        if (range.first > array.length) {
            // printf("Range starting after array\n");
            int64_t residual = range.first % -range.stride;
            range.first = array.length - (array.length % -range.stride) + residual;
        }
        if (range.first > array.length) range.first += range.stride;
        if (range.first < 1) {
            // printf("Range outside array\n");
            return (string_t){array.data, 0, 1, 0};
        }
    } else {
        if (range.first == INT64_MIN) range.first = 1;
        if (range.last == INT64_MAX) range.last = array.length;
        if (range.first < 1) {
            // printf("Range starting before array\n");
            range.first = range.first % range.stride;
        }
        while (range.first < 1) range.first += range.stride;
        if (range.first > array.length) {
            // printf("Range outside array\n");
            return (string_t){array.data, 0, 1, 0};
        }
    }

    int64_t len = (range.last - range.first) / range.stride + 1;
    // If less than zero, set to zero (without a conditional branch)
    len = len & ~(len >> 63);
    if (len > array.length/labs(range.stride) + 1) len = array.length/labs(range.stride) + 1;
    if (len < 0) len = -len;

    return (string_t){
        (char*)array.data + item_size*(range.first-1),
        (int32_t)len, (int16_t)(array.stride * range.stride), -1,
    };
}

// Copy on write for arrays:
void array_cow(void *voidarr, size_t item_size, bool atomic)
{
    struct {char *data; int32_t len; int16_t stride, free;} *arr = voidarr;
    char *copy = atomic ? GC_MALLOC_ATOMIC(arr->len * item_size) : GC_MALLOC(arr->len * item_size);
    if ((size_t)arr->stride == item_size) {
        memcpy(copy, arr->data, arr->len * item_size);
    } else {
        for (int32_t i = 0; i < arr->len; i++)
            memcpy(copy + i*item_size, arr->data + arr->stride*i, item_size);
    }
    arr->stride = item_size;
    arr->data = copy;
    arr->free = 0;
}

void array_flatten(void *voidarr, size_t item_size, bool atomic)
{
    struct {char *data; int32_t len; int16_t stride, free;} *arr = voidarr;
    char *copy = atomic ? GC_MALLOC_ATOMIC(arr->len * item_size) : GC_MALLOC(arr->len * item_size);
    if ((size_t)arr->stride == item_size) {
        memcpy(copy, arr->data, arr->len * item_size);
    } else {
        for (int32_t i = 0; i < arr->len; i++)
            memcpy(copy + i*item_size, arr->data + arr->stride*i, item_size);
    }
    arr->stride = item_size;
    arr->data = copy;
    arr->free = 0;
}

void array_insert(void *voidarr, char *item, int64_t index, size_t item_size, bool atomic)
{
    string_t *arr = voidarr;
    if (index < 1) index = 1;
    else if (index > (int64_t)arr->length + 1) index = (int64_t)arr->length + 1;

    if (!arr->data) {
        arr->data = atomic ? GC_MALLOC_ATOMIC(item_size) : GC_MALLOC(item_size);
        arr->free = 1;
    } else if (arr->free < 1 || (size_t)arr->stride != item_size) {
        arr->free = 6;
        char *copy = atomic ? GC_MALLOC_ATOMIC((arr->length + arr->free) * item_size) : GC_MALLOC((arr->length + arr->free) * item_size);
        for (int32_t i = 0; i < index-1; i++)
            memcpy(copy + i*item_size, arr->data + arr->stride*i, item_size);
        for (int32_t i = index-1; i < arr->length; i++)
            memcpy(copy + (i+1)*item_size, arr->data + arr->stride*i, item_size);
        arr->data = copy;
    } else if (index != arr->length+1) {
        memmove((char*)arr->data + index*item_size, arr->data + (index-1)*item_size, (arr->length - index)*item_size);
    }
    --arr->free;
    ++arr->length;
    memcpy((char*)arr->data + (index-1)*item_size, item, item_size);
}

void array_insert_all(void *voidarr, void *voidarr2, int64_t index, size_t item_size, bool atomic)
{
    string_t *arr = voidarr, *arr2 = voidarr2;
    if (index < 1) index = 1;
    else if (index > (int64_t)arr->length + 1) index = (int64_t)arr->length + 1;

    if (!arr->data) {
        arr->data = atomic ? GC_MALLOC_ATOMIC(item_size*arr2->length) : GC_MALLOC(item_size*arr2->length);
        arr->free = arr2->length;
    } else if ((int64_t)arr->free < (int64_t)arr2->length || (size_t)arr->stride != item_size) {
        arr->free = arr2->length;
        char *copy = atomic ? GC_MALLOC_ATOMIC((arr->length + arr->free) * item_size) : GC_MALLOC((arr->length + arr->free) * item_size);
        for (int32_t i = 0; i < index-1; i++)
            memcpy(copy + i*item_size, arr->data + arr->stride*i, item_size);
        for (int32_t i = index-1; i < arr->length; i++)
            memcpy(copy + (i+arr2->length)*item_size, arr->data + arr->stride*i, item_size);
        arr->data = copy;
    } else if (index != arr->length+1) {
        memmove((char*)arr->data + index*item_size, arr->data + (index-1)*item_size, (arr->length - index + arr2->length-1)*item_size);
    }
    arr->free -= arr2->length;
    arr->length += arr2->length;
    for (int32_t i = 0; i < arr2->length; i++)
        memcpy((char*)arr->data + (index-1 + i)*item_size, arr2->data + i*arr2->stride, item_size);
}

void array_remove(void *voidarr, int64_t index, int64_t count, size_t item_size, bool atomic)
{
    string_t *arr = voidarr;
    if (index < 1 || index > (int64_t)arr->length || count < 1) return;

    if (count > arr->length - index + 1)
        count = (arr->length - index) + 1;

    if (index + count > arr->length) {
        if (arr->free >= 0)
            arr->free += count;
    } else if (arr->free < 0 || (size_t)arr->stride != item_size) { // Copy on write
        char *copy = atomic ? GC_MALLOC_ATOMIC((arr->length-1) * item_size) : GC_MALLOC((arr->length-1) * item_size);
        for (int32_t src = 1, dest = 1; src <= arr->length; src++) {
            if (src < index || src >= index + count) {
                memcpy(copy + (dest - 1)*item_size, arr->data + arr->stride*(src - 1), item_size);
                ++dest;
            }
        }
        arr->data = copy;
        arr->free = 0;
    } else {
        memmove((char*)arr->data + (index-1)*item_size, arr->data + (index-1 + count)*item_size, (arr->length - index + count - 1)*item_size);
        arr->free += count;
    }
    arr->length -= count;
}

typedef int (cmp_fn_t)(const void *a, const void *b);
void array_sort(void *voidarr, cmp_fn_t compare, size_t item_size, bool atomic)
{
    string_t *arr = voidarr;
    if (arr->free < 0 || (size_t)arr->stride != item_size)
        array_flatten(voidarr, item_size, atomic);
    qsort((char*)arr->data, arr->length, item_size, compare);
}

void array_shuffle(void *voidarr, size_t item_size, bool atomic)
{
    string_t *arr = voidarr;
    if (arr->free < 0 || (size_t)arr->stride != item_size)
        array_flatten(voidarr, item_size, atomic);

    char tmp[item_size];
    for (int32_t i = arr->length-1; i > 1; i--) {
        int32_t j = arc4random_uniform(i+1);
        memcpy(tmp, arr->data + i*item_size, item_size);
        memcpy((char*)arr->data + i*item_size, arr->data + j*item_size, item_size);
        memcpy((char*)arr->data + j*item_size, tmp, item_size);
    }
}

string_t array_join(void *voidarr, void *voidglue, size_t item_size, bool atomic)
{
    struct {string_t *data; int32_t length; int16_t stride, free;} *strings = voidarr;
    string_t *glue = voidglue;
    if (strings->length == 0) return (string_t){.stride=item_size};

    int32_t len = 0;
    for (int32_t i = 0; i < strings->length; i++) {
        if (i > 0) len += glue->length;
        len += ((string_t*)((void*)strings->data + i*strings->stride))->length;
    }
    char *data = atomic ? GC_MALLOC_ATOMIC((size_t)len*item_size) : GC_MALLOC((size_t)len*item_size);
    char *ptr = data;
    for (int32_t i = 0; i < strings->length; i++) {
        if (i > 0) {
            for (int32_t j = 0; j < glue->length; j++)
                ptr = mempcpy(ptr, (void*)glue->data + j*glue->stride, item_size);
        }
        string_t str = *(string_t*)((void*)strings->data + i*strings->stride);
        for (int32_t j = 0; j < str.length; j++)
            ptr = mempcpy(ptr, (void*)str.data + j*str.stride, item_size);
    }
    return (string_t){.data = data, .length = len, .stride = item_size};
}


typedef struct { FILE *file; } SSSFile;

typedef struct {
    enum { Failure, Success } tag;
    union {
        string_t Failure;
        SSSFile *Success;
    } __data;
} FileResult;

void sss_file_finalizer(void *obj, void *_)
{
    (void)_;
    SSSFile *f = (SSSFile*)obj;
    if (f->file) fclose(f->file);
    f->file = NULL;
}

#include <assert.h>
#define STR_LITERAL(s) (string_t){.data=s, .stride=1, .length=(int32_t)strlen(s)}
FileResult sss_fopen(string_t path, string_t mode)
{
    if (path.length > PATH_MAX)
        return (FileResult){.tag=Failure, .__data.Failure=STR_LITERAL("Path name is too long!")};
    FILE *f = fopen(c_string(path), c_string(mode));
    FileResult ret;
    if (f) {
        ret.tag = Success;
        ret.__data.Success = GC_MALLOC_ATOMIC(sizeof(SSSFile));
        ret.__data.Success->file = f;
        GC_REGISTER_FINALIZER(ret.__data.Success, sss_file_finalizer, NULL, NULL, NULL);
    } else {
        ret.tag = Failure;
        ret.__data.Failure = last_err();
    }
    return ret;
}

SSSFile *sss_tmpfile(void)
{
    SSSFile *bf = GC_MALLOC_ATOMIC(sizeof(SSSFile));
    bf->file = tmpfile();
    GC_REGISTER_FINALIZER(bf, sss_file_finalizer, NULL, NULL, NULL);
    return bf;
}

string_t sss_readfile(SSSFile *bf, int64_t bytes)
{
    const size_t chunk_size = 1000;
    if (!bf || !bf->file || bytes <= 0) return (string_t){.stride=1};
    size_t to_read = (size_t)bytes;
    size_t buf_len = 0, buf_cap = to_read < chunk_size ? to_read : chunk_size;
    char *buf = GC_MALLOC_ATOMIC(buf_cap);
    while (to_read > 0) {
        char chunk[chunk_size];
        size_t got = fread(chunk, 1, to_read < chunk_size ? to_read : chunk_size, bf->file);
        if (got == 0) break;
        if (buf_len + got > buf_cap)
            buf = GC_REALLOC(buf, (buf_cap += got));
        memcpy(buf + buf_len, chunk, got);
        buf_len += got;
        to_read -= got;
    }
    return (string_t){.data=buf, .stride=1, .length=(int32_t)buf_len};
}

string_t get_line(FILE *f)
{
    char *buf = NULL; size_t len = 0;
    ssize_t got = getline(&buf, &len, f);
    string_t ret = {.stride=1};
    if (got > 0) {
        if (buf[got-1] == '\n') --got;
        ret.length = (int32_t)got;
        ret.data = GC_MALLOC_ATOMIC(got + 1);
        memcpy((char*)ret.data, buf, got);
        *(char*)(ret.data+got) = '\0';

        // Trigger feof when there isn't some kind of waiting happening
        struct pollfd pfd = {
            .fd=fileno(f),
            .events=POLLIN,
        };
        if (poll(&pfd, 1, 0) > 0)
            ungetc(getc(f), f);
    }
    if (buf) free(buf);
    return ret;
}

// Conversion functions:
typedef struct {
    unsigned char tag;
    union {
        int64_t value;
        struct {
            int64_t value;
            string_t remaining;
        } partial;
    } data;
} int_conversion_t;

static const unsigned char FAILURE = 0, INVALID_RANGE = 1, PARTIAL_SUCCESS = 2, SUCCESS = 3, INVALID_BASE = 4;

int_conversion_t sss_string_to_int(string_t str, int64_t base)
{
    str = flatten(str);
    char *endptr = (char*)&str.data[str.length];
    errno = 0;
    int64_t n = strtol(str.data, &endptr, base);
    switch (errno) {
    case EINVAL: return (int_conversion_t){.tag=INVALID_BASE, .data.value=n};
    case ERANGE: return (int_conversion_t){.tag=INVALID_RANGE, .data.value=n};
    default:
        if (endptr == str.data)
            return (int_conversion_t){.tag=FAILURE, .data.value=n};
        else if (endptr < &str.data[str.length])
            return (int_conversion_t){
                .tag=PARTIAL_SUCCESS,
                .data.partial.value=n,
                .data.partial.remaining=(string_t){
                    .data=endptr,
                    .length=str.length - (int32_t)(endptr - str.data),
                    .stride=1,
                    .free=0,
                },
            };
        else
            return (int_conversion_t){.tag=SUCCESS, .data.value=n};
    }
}

typedef struct {
    unsigned char tag;
    union {
        double value;
        struct {
            double value;
            string_t remaining;
        } partial;
    } data;
} num_conversion_t;

num_conversion_t sss_string_to_num(string_t str)
{
    str = flatten(str);
    char *endptr = (char*)&str.data[str.length];
    errno = 0;
    double num = strtod(str.data, &endptr);
    switch (errno) {
    case ERANGE: return (num_conversion_t){.tag=INVALID_RANGE, .data.value=num};
    default:
        if (endptr == str.data)
            return (num_conversion_t){.tag=FAILURE, .data.value=num};
        else if (endptr < &str.data[str.length])
            return (num_conversion_t){
                .tag=PARTIAL_SUCCESS,
                .data.partial.value=num,
                .data.partial.remaining=(string_t){
                    .data=endptr,
                    .length=str.length - (int32_t)(endptr - str.data),
                    .stride=1,
                    .free=0,
                },
            };
        else
            return (num_conversion_t){.tag=SUCCESS, .data.value=num};
    }
}
