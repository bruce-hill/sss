#include <gc/cord.h>
#include <gc.h>
#include <bp/pattern.h>
#include <bp/match.h>
#include <bp/files.h>
#include <bhash.h>

#include "parse.h"
#include "typecheck.h"
#include "compile.h"

#define streq(a,b) (strcmp(a,b) == 0)

int main(int argc, char *argv[])
{
    GC_INIT();
    hashmap_set_allocator(GC_malloc, NULL);
    for (int i = 1; i < argc; i++) {
        file_t *f = load_file(NULL, argv[i]);

        ast_t *ast = parse(f);

        const char *code = compile_file(f, ast);
        puts(code);

        recycle_all_matches();
        destroy_file(&f);
    }

    return 0;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
