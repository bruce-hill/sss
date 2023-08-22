//
// files.c - Implementation of some file loading functionality.
//

#include <err.h>
#include <gc.h>
#include <fcntl.h>
#include <libgen.h>
#include <limits.h>
#include <stdarg.h>
#include <string.h>
#include <sys/param.h>

#include "files.h"
#include "util.h"

char *resolve_path(const char *path, const char *relative_to)
{
    if (!relative_to || streq(relative_to, "/dev/stdin")) relative_to = ".";
    if (!path || strlen(path) == 0) return NULL;

    // Resolve the path to an absolute path, assuming it's relative to the file
    // it was found in:
    char buf[PATH_MAX] = {0};
    if (streq(path, "~") || strncmp(path, "~/", 2) == 0) {
        char *resolved = realpath(heap_strf("%s%s", getenv("HOME"), path+1), buf);
        if (resolved) return heap_str(resolved);
    } else if (streq(path, ".") || strncmp(path, "./", 2) == 0) {
        char *relative_dir = dirname(heap_str(relative_to));
        char *resolved = realpath(heap_strf("%s/%s", relative_dir, path), buf);
        if (resolved) return heap_str(resolved);
    } else if (path[0] == '/') {
        // Absolute path:
        char *resolved = realpath(path, buf);
        if (resolved) return heap_str(resolved);
    } else {
        // Relative path:
        char *blpath = heap_str(getenv("SSSPATH"));
        char *relative_dir = dirname(heap_str(relative_to));
        for (char *dir; (dir = strsep(&blpath, ":")); ) {
            if (dir[0] == '/') {
                char *resolved = realpath(heap_strf("%s/%s", dir, path), buf);
                if (resolved) return heap_str(resolved);
            } else if (dir[0] == '~' && (dir[1] == '\0' || dir[1] == '/')) {
                char *resolved = realpath(heap_strf("%s%s/%s", getenv("HOME"), dir, path), buf);
                if (resolved) return heap_str(resolved);
            } else if (streq(dir, ".") || strncmp(dir, "./", 2) == 0) {
                char *resolved = realpath(heap_strf("%s/%s", relative_dir, path), buf);
                if (resolved) return heap_str(resolved);
            } else if (streq(dir, ".") || streq(dir, "..") || strncmp(dir, "./", 2) == 0 || strncmp(dir, "../", 3) == 0) {
                char *resolved = realpath(heap_strf("%s/%s/%s", relative_dir, dir, path), buf);
                if (resolved) return heap_str(resolved);
            } else {
                char *resolved = realpath(heap_strf("%s/%s", dir, path), buf);
                if (resolved) return heap_str(resolved);
            }
        }
    }
    return NULL;
}

static sss_file_t *_load_file(const char* filename, FILE *file)
{
    if (!file) return NULL;

    size_t file_size = 0, line_cap = 0;
    char *file_buf = NULL, *line_buf = NULL;
    FILE *mem = open_memstream(&file_buf, &file_size);
    ssize_t line_len = 0;
    NEW_LIST(sss_line_t, lines);
    while ((line_len = getline(&line_buf, &line_cap, file)) >= 0) {
        sss_line_t line_info = {.offset=file_size, .indent=0, .is_empty=false};
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
    copy[file_size] = '\0';
    fclose(mem);

    free(file_buf);
    const char *relative_filename = filename;
    if (filename && !streq(filename, "<repl>") && !streq(filename, "<argument>") && !streq(filename, "/dev/stdin")) {
        filename = resolve_path(filename, ".");
        // Convert to relative path (if applicable)
        char buf[PATH_MAX];
        char *cwd = getcwd(buf, sizeof(buf));
        size_t cwd_len = strlen(cwd);
        if (strncmp(cwd, filename, cwd_len) == 0 && filename[cwd_len] == '/')
            relative_filename = &filename[cwd_len+1];
    }
    return new(sss_file_t, .filename=filename, .relative_filename=relative_filename, .text=copy, .lines=lines);
}

//
// Read an entire file into memory.
//
sss_file_t *sss_load_file(const char* filename)
{
    FILE *file = filename[0] ? fopen(filename, "r") : stdin;
    return _load_file(filename, file);
}

//
// Create a virtual file from a string.
//
sss_file_t *sss_spoof_file(const char* filename, const char *text)
{
    FILE *file = fmemopen((char*)text, strlen(text)+1, "r");
    return _load_file(filename, file);
}

//
// Given a pointer, determine which line number it points to (1-indexed)
//
size_t sss_get_line_number(sss_file_t *f, const char *p)
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
size_t sss_get_line_column(sss_file_t *f, const char *p)
{
    size_t line_no = sss_get_line_number(f, p);
    return 1 + (size_t)(p - (f->text + f->lines[0][line_no-1].offset));
}

//
// Given a pointer, get the indentation of the line it's on.
//
size_t sss_get_indent(sss_file_t *f, const char *p)
{
    ssize_t line_no = sss_get_line_number(f, p);
    return f->lines[0][line_no-1].indent;
}

//
// Return a pointer to the line with the specified line number (1-indexed)
//
const char *sss_get_line(sss_file_t *f, size_t line_number)
{
    if (line_number == 0 || line_number > (size_t)LIST_LEN(f->lines)) return NULL;
    return f->text + f->lines[0][line_number - 1].offset;
}

//
// Return a value like /foo:line:col
//
const char *sss_get_file_pos(sss_file_t *f, const char *p)
{
    return heap_strf("%s:%ld:%ld", f->filename, sss_get_line_number(f, p), sss_get_line_column(f, p));
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
