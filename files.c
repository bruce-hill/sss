//
// files.c - Implementation of some file loading functionality.
//

#include <err.h>
#include <gc.h>
#include <intern.h>
#include <fcntl.h>
#include <limits.h>
#include <stdarg.h>
#include <string.h>
#include <sys/param.h>

#include "files.h"
#include "util.h"

static bl_file_t *_load_file(istr_t filename, FILE *file)
{
    if (!file) return NULL;

    size_t file_size = 0, line_cap = 0;
    char *file_buf = NULL, *line_buf = NULL;
    FILE *mem = open_memstream(&file_buf, &file_size);
    ssize_t line_len = 0;
    NEW_LIST(bl_line_t, lines);
    while ((line_len = getline(&line_buf, &line_cap, file)) >= 0) {
        bl_line_t line_info = {.offset=file_size, .indent=0, .is_empty=false};
        char *p;
        for (p = line_buf; *p == ' ' || *p == '\t'; ++p)
            line_info.indent += *p == ' ' ? 1 : 4;
        line_info.is_empty = *p != '\r' && *p != '\n';
        list_append((list_t*)lines, sizeof(line_info), &line_info);
        fwrite(line_buf, sizeof(char), line_len, mem);
        fflush(mem);
    }
    fclose(file);

    char *copy = GC_MALLOC_ATOMIC(file_size+1);
    memcpy(copy, file_buf, file_size);
    fclose(mem);

    free(file_buf);
    return new(bl_file_t, .filename=filename, .text=copy, .lines=lines);
}

//
// Read an entire file into memory.
//
bl_file_t *bl_load_file(istr_t filename)
{
    FILE *file = filename[0] ? fopen(filename, "r") : stdin;
    return _load_file(filename, file);
}

//
// Create a virtual file from a string.
//
bl_file_t *bl_spoof_file(istr_t filename, const char *text)
{
    FILE *file = fmemopen((char*)text, strlen(text), "r");
    return _load_file(filename, file);
}

//
// Given a pointer, determine which line number it points to (1-indexed)
//
size_t bl_get_line_number(bl_file_t *f, const char *p)
{
    // Binary search:
    ssize_t lo = 0, hi = (ssize_t)LIST_LEN(f->lines)-1;
    if (p < f->text) return 0;
    size_t offset = (ssize_t)(p - f->text);
    while (lo <= hi) {
        ssize_t mid = (lo + hi) / 2;
        if (f->lines[0][mid].offset == offset)
            return mid + 1;
        else if (f->lines[0][mid].offset < offset)
            lo = mid + 1;    
        else if (f->lines[0][mid].offset > offset)
            hi = mid - 1;
    }
    return (size_t)lo; // Return the line number whose line starts closest before p
}

//
// Given a pointer, determine which line column it points to.
//
size_t bl_get_line_column(bl_file_t *f, const char *p)
{
    size_t line_no = bl_get_line_number(f, p);
    return 1 + (size_t)(p - (f->text + f->lines[0][line_no-1].offset));
}

//
// Given a pointer, get the indentation of the line it's on.
//
size_t bl_get_indent(bl_file_t *f, const char *p)
{
    ssize_t line_no = bl_get_line_number(f, p);
    return f->lines[0][line_no-1].indent;
}

//
// Return a pointer to the line with the specified line number (1-indexed)
//
const char *bl_get_line(bl_file_t *f, size_t line_number)
{
    if (line_number == 0 || line_number > (size_t)LIST_LEN(f->lines)) return NULL;
    return f->text + f->lines[0][line_number - 1].offset;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
