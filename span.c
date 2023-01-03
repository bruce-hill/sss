
#include <unistd.h>
#include <stdio.h>
#include <sys/param.h>
#include <string.h>

#include "span.h"

int fprint_span(FILE *out, span_t *span, const char *hl_color, size_t context_lines)
{
    const char *lineno_fmt, *normal_color, *empty_marker;
    if (isatty(fileno(out))) {
        lineno_fmt = "\x1b[0;2m%*lu\x1b(0\x78\x1b(B\x1b[m ";
        normal_color = "\x1b[m";
        empty_marker = "\x1b(0\x61\x1b(B";
    } else {
        lineno_fmt = "%*lu| ";
        hl_color = "";
        normal_color = "";
        empty_marker = " ";
    }

    if (context_lines == 0)
        return fprintf(out, "%s%.*s%s", hl_color, (int)(span->end - span->start), span->start, normal_color);

    ssize_t span_start_line = bl_get_line_number(span->file, span->start),
            span_end_line = bl_get_line_number(span->file, span->end);

    ssize_t first_line = span_start_line - (context_lines - 1),
            last_line = span_end_line - (context_lines - 1);

    if (first_line < 1) first_line = 1;
    if (last_line > LIST_LEN(span->file->lines)) first_line = LIST_LEN(span->file->lines);

    int digits = 1;
    for (size_t i = last_line; i > 0; i /= 10) ++digits;

    int printed = 0;
    for (ssize_t line_no = first_line; line_no <= last_line; ++line_no) {
        printed += fprintf(out, lineno_fmt, digits, line_no);
        const char *line = bl_get_line(span->file, line_no);
        if (!line) break;

        const char *p = line;
        // Before match
        for (; *p && *p != '\r' && *p != '\n' && p < span->start; ++p)
            printed += fputc(*p, out);

        // Zero-width matches
        if (p == span->start && span->start == span->end)
            printed += fprintf(out, "%s%s%s", hl_color, empty_marker, normal_color);

        // Inside match
        if (span->start <= p && p < span->end) {
            printed += fputs(hl_color, out);
            for (; *p && *p != '\r' && *p != '\n' && p < span->end; ++p)
                printed += fputc(*p, out);
            printed += fputs(normal_color, out);
        }

        // After match
        for (; *p && *p != '\r' && *p != '\n'; ++p)
            printed += fputc(*p, out);

        printed += fprintf(out, "\n");
    }
    fflush(out);
    return printed;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
