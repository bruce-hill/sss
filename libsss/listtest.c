#include <stdio.h>
#include <gc.h>
#include "list.h"

int main(void) {
    GC_INIT();
    List(unsigned long) nums = LIST(unsigned long, 10, 11, 12);
    APPEND(nums, 13);
    APPEND(nums, 14);
    for (size_t i = 0; i < LIST_LEN(nums); i++)
        printf("[%ld] = %ld\n", i, LIST_ITEM(nums, i));
    return 0;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
