#pragma once
#define new(t) ((t*)GC_MALLOC(sizeof(t)))

#include <bp/match.h>
#include <bp/files.h>

void highlight_match(file_t *f, match_t *m);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
