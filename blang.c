#include <gc/cord.h>
#include <gc.h>
#include <bp/pattern.h>
#include <bp/match.h>
#include <bp/files.h>

#include "parse.h"
#include "typecheck.h"
#include "compile.h"

#define streq(a,b) (strcmp(a,b) == 0)

int main(int argc, char *argv[])
{
    for (int i = 1; i < argc; i++) {
        ast_t *ast = parse(argv[i]);
        const char *dest;
        if (i + 2 < argc && streq(argv[i+1], "-o"))
            dest = argv[i+2];
        else
            dest = CORD_cat(argv[i], ".o");
        compile(ast, dest);
    }

    return 0;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
