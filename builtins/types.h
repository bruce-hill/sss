#pragma once
#include "cording.h"
#include "hashing.h"
#include "comparing.h"

typedef struct {
    const char *name, *type;
    void *value;
} NamespaceBinding;

typedef struct {
    string_t name;
    Comparable compare;
    Hashable hash;
    Cordable cord;
    NamespaceBinding *bindings;
    // const char *namespace_type;
    // void *namespace;
} Type;
