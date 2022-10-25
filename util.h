#pragma once

#include <bp/match.h>
#include <bp/files.h>
#include <string.h>

#define new(t, ...) ((t*)memcpy(GC_MALLOC(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t)))

void highlight_match(file_t *f, match_t *m);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
