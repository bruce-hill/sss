
#include <unistd.h>
#include <stdio.h>
#include <sys/param.h>
#include <string.h>

#include "span.h"

static const int tabstop = 4;

static int fputc_column(FILE *out, char c, char print_char, int *column)
{
    int printed = 0;
    if (print_char == '\t') print_char = ' ';
    if (c == '\t') {
        for (int to_fill = tabstop - (*column % tabstop); to_fill > 0; --to_fill) {
            printed += fputc(print_char, out);
            ++*column;
        }
    } else {
        printed += fputc(print_char, out);
        ++*column;
    }
    return printed;
}

int fprint_span(FILE *out, span_t span, const char *hl_color, size_t context_lines)
{
    const char *lineno_fmt, *normal_color, *empty_marker;
    bool print_carets = false;
    int printed = 0;
    if (isatty(fileno(out))) {
        lineno_fmt = "\x1b[0;2m%*lu\x1b(0\x78\x1b(B\x1b[m ";
        normal_color = "\x1b[m";
        empty_marker = "\x1b(0\x61\x1b(B";
        printed += fprintf(out, "\x1b[33;4;1m%s\x1b[m\n", span.file->filename);
    } else {
        lineno_fmt = "%*lu| ";
        hl_color = "";
        normal_color = "";
        empty_marker = " ";
        print_carets = true;
        printed += fprintf(out, "%s\n", span.file->filename);
    }

    if (context_lines == 0)
        return fprintf(out, "%s%.*s%s", hl_color, (int)(span.end - span.start), span.start, normal_color);

    ssize_t span_start_line = bl_get_line_number(span.file, span.start),
            span_end_line = bl_get_line_number(span.file, span.end);

    ssize_t first_line = span_start_line - (context_lines - 1),
            last_line = span_end_line + (context_lines - 1);

    if (first_line < 1) first_line = 1;
    if (last_line > LIST_LEN(span.file->lines)) first_line = LIST_LEN(span.file->lines);

    int digits = 1;
    for (size_t i = last_line; i > 0; i /= 10) ++digits;

    for (ssize_t line_no = first_line; line_no <= last_line; ++line_no) {
        printed += fprintf(out, lineno_fmt, digits, line_no);
        const char *line = bl_get_line(span.file, line_no);
        if (!line) break;

        int column = 0;
        const char *p = line;
        // Before match
        for (; *p && *p != '\r' && *p != '\n' && p < span.start; ++p)
            printed += fputc_column(out, *p, *p, &column);

        // Zero-width matches
        if (p == span.start && span.start == span.end) {
            printed += fprintf(out, "%s%s%s", hl_color, empty_marker, normal_color);
            column += 1;
        }

        // Inside match
        if (span.start <= p && p < span.end) {
            printed += fputs(hl_color, out);
            for (; *p && *p != '\r' && *p != '\n' && p < span.end; ++p)
                printed += fputc_column(out, *p, *p, &column);
            printed += fputs(normal_color, out);
        }

        // After match
        for (; *p && *p != '\r' && *p != '\n'; ++p)
            printed += fputc_column(out, *p, *p, &column);

        printed += fprintf(out, "\n");

        const char *eol = strchrnul(line, '\n');
        if (print_carets && span.start >= line && span.start < eol && line <= span.start) {
            for (int num = 0; num < digits; num++)
                printed += fputc(' ', out);
            printed += fputs(": ", out);
            int column = 0;
            for (const char *sp = line; *sp && *sp != '\n'; ++sp) {
                bool is_match_char = (span.start <= sp && sp < span.end) || (sp == span.start && sp == span.end);
                char c = is_match_char ? '^' : ' ';
                printed += fputc_column(out, *sp, c, &column);
            }
            printed += fputs("\n", out);
        }
    }
    fflush(out);
    return printed;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
