#pragma once

#include <assert.h>
#include <bp/match.h>
#include <bp/files.h>
#include <stdio.h>
#include <string.h>

#include "libblang/utils.h"

#define new(t, ...) ((t*)memcpy(GC_MALLOC(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t)))
#define Match(x, _tag) ((x)->tag == _tag ? &(x)->__data._tag : (fail(__FILE__ ":%d This was supposed to be a " # _tag "\n", __LINE__), &(x)->__data._tag))
#define Tagged(t, _tag, ...) new(t, .tag=_tag, .__data._tag={__VA_ARGS__})

#ifndef auto
#define auto __auto_type
#endif

typedef void (*defer_fn_t)(void*);
typedef struct defer_s {
    struct defer_s *next;
    defer_fn_t fn;
    void *ptr;
} defer_t;

#define use_defer defer_t *__local_defer = NULL
#define defer(f, p) __local_defer = new(defer_t, .next=__local_defer, .fn=(defer_fn_t)(f), .ptr=(p))
#define defer_return(...) do {\
    for (;__local_defer;__local_defer=__local_defer->next) __local_defer->fn(__local_defer->ptr); \
    return __VA_ARGS__; } while (0)

void highlight_match(FILE *out, file_t *f, match_t *m, int context);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
