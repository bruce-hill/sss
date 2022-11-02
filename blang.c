#include <bhash.h>
#include <bp/files.h>
#include <bp/match.h>
#include <bp/pattern.h>
#include <gc.h>
#include <gc/cord.h>
#include <libgccjit.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

#include "parse.h"
#include "typecheck.h"
#include "compile.h"

#define streq(a,b) (strcmp(a,b) == 0)
#define endswith(str,end) (strlen(str) >= strlen(end) && strcmp((str) + strlen(str) - strlen(end), end) == 0)

int main(int argc, char *argv[])
{
    GC_INIT();
    hashmap_set_allocator(GC_malloc, NULL);
    bool verbose = false;
    char *prog_name = strrchr(argv[0], '/');
    prog_name = prog_name ? prog_name + 1 : argv[0];
    bool run_program = !strstr(prog_name, "blangc");

    gcc_jit_context *ctx = gcc_jit_context_acquire();
    assert(ctx != NULL);

    for (int i = 1; i < argc; i++) {
        if (streq(argv[i], "-v") || streq(argv[i], "--verbose")) {
            gcc_jit_context_set_bool_option(ctx, GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE, 1);
            verbose = true;
            continue;
        } else if (streq(argv[i], "-r") || streq(argv[i], "--run")) {
            run_program = true;
            continue;
        }
        file_t *f = load_file(NULL, argv[i]);
        if (!f) {
            fprintf(stderr, "\x1b[31;1mNo such file: %s\n", argv[i]);
            exit(1);
        }

        if (verbose)
            printf("Parsing %s...\n", argv[i]);
        ast_t *ast = parse(f);

        if (verbose)
            print_ast(ast);

        if (verbose)
            printf("Compiling %s...\n", argv[i]);

        gcc_jit_result *result = compile_file(ctx, f, ast, true);
        assert(result != NULL);

        // Extract the generated code from "result".   
        typedef void (*fn_type)(void);
        fn_type run = (fn_type)gcc_jit_result_get_code(result, "run");
        if (!run) {
            fprintf(stderr, "NULL run func");
            exit(1);
        }

        if (run_program)
            run();
        gcc_jit_result_release(result);

        recycle_all_matches();
        destroy_file(&f);
    }

    gcc_jit_context_release(ctx);

    return 0;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
