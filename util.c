#include <bp/match.h>
#include <bp/files.h>
#include <string.h>

#include "util.h"

//
// Print a match highlighted in red with context lines
//
void highlight_match(FILE *out, file_t *f, match_t *m, int context) {
    size_t firstline = get_line_number(f, m->start);
    size_t lastline = get_line_number(f, m->end);
    fprintf(out, "\x1b[1;4m%s\x1b[m\n", f->filename);
    for (size_t i = firstline - context; i <= lastline + context; i++) {
        const char *line = get_line(f, i);
        if (!line) continue;
        const char *eol = strchrnul(line, '\n');
        if (line >= f->end - 1 && line == eol) break;
        fprintf(out, "\x1b[2m% 4ld| \x1b[m", i);
        if (i == firstline) {
            fprintf(out, "\x1b[m%.*s", (int)(m->start - line), line);
            if (i == lastline) {
                fprintf(out, "\x1b[0;31;1m%.*s\x1b[m", (int)(m->end - m->start), m->start);
                fprintf(out, "\x1b[m%.*s", (int)(eol - m->end), m->end);
            } else {
                fprintf(out, "\x1b[0;31;1m%.*s\x1b[m", (int)(eol - m->start), m->start);
            }
        } else if (i == lastline) {
            fprintf(out, "\x1b[0;31;1m%.*s\x1b[m", (int)(m->end - line), line);
            fprintf(out, "\x1b[m%.*s", (int)(eol - m->end), m->end);
        } else if (i < firstline || i > lastline) {
            fprintf(out, "\x1b[m%.*s", (int)(eol - line), line);
        } else {
            fprintf(out, "\x1b[0;31;1m%.*s", (int)(eol - line), line);
        }
        fprintf(out, "\x1b[m\n");
    }
    fprintf(out, "\x1b[m\n");
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
