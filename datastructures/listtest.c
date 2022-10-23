#include <stdio.h>
#include <gc.h>
#include "list.h"

int main(void) {
    GC_INIT();
    NEW_LIST(unsigned long, x);
    APPEND(x, 10);
    APPEND(x, 11);
    APPEND(x, 12);
    for (size_t i = 0; i < LIST_LEN(x); i++)
        printf("[%ld] = %ld\n", i, LIST_ITEM(x, i));
    return 0;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
