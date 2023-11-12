#include <stdarg.h>
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

public char *heap_strn(const char *str, size_t len)
{
    if (!str) return NULL;
    if (len == 0) return "";
    char *heaped = GC_MALLOC_ATOMIC(len + 1);
    memcpy(heaped, str, len);
    heaped[len] = '\0';
    return heaped;
}

public char *heap_str(const char *str)
{
    return heap_strn(str, strlen(str));
}

public char *heap_strf(const char *fmt, ...)
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

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
