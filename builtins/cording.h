#pragma once

#include <gc/cord.h>
#include <stdbool.h>
#include <stdint.h>

#include "string.h"

typedef struct Cordable {
    enum { CordFunction, CordNamed, CordPointer, CordArray, CordTable } tag;
    union {
        struct {
            CORD (*fn)(const void*, bool);
        } CordFunction;
        struct {
            const char *name;
            struct Cordable *base;
        } CordNamed;
        struct {
            const char *sigil, *null_str;
            struct Cordable *pointed;
        } CordPointer;
        struct {
            struct Cordable *item;
        } CordArray;
        struct {
            size_t entry_size, value_offset;
            struct Cordable *key, *value;
        } CordTable;
    } __data;
} Cordable;

#define CordMethod(cord_tag, ...) ((Cordable){.tag=Cord##cord_tag, .__data.Cord##cord_tag={__VA_ARGS__}})

CORD generic_cord(const void *obj, Cordable what, bool colorize);

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
