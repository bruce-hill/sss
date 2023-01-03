#pragma once

#include <stdio.h>

#include "files.h"

typedef struct {
    bl_file_t *file;
    const char *start, *end;
} span_t;

int fprint_span(FILE *out, span_t *span, const char *hl_color, size_t context_lines);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
