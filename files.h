//
// files.h - Definitions of an API for loading files.
//
#pragma once

#include <stdio.h>
#include <unistd.h>

#include "libblang/list.h"

typedef struct {
    size_t offset;
    size_t indent;
    bool is_empty;
} bl_line_t;

typedef struct {
    const char *filename, *relative_filename;
    const char *text;
    List(bl_line_t) lines;
} bl_file_t;

char *resolve_path(const char *path, const char *relative_to);
__attribute__((nonnull))
bl_file_t *bl_load_file(const char *filename);
__attribute__((nonnull, returns_nonnull))
bl_file_t *bl_spoof_file(const char *filename, const char *text);
__attribute__((pure, nonnull))
size_t bl_get_line_number(bl_file_t *f, const char *p);
__attribute__((pure, nonnull))
size_t bl_get_line_column(bl_file_t *f, const char *p);
__attribute__((pure, nonnull))
size_t bl_get_indent(bl_file_t *f, const char *p);
__attribute__((pure, nonnull))
const char *bl_get_line(bl_file_t *f, size_t line_number);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
