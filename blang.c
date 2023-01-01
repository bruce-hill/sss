#include <bhash.h>
#include <bp/files.h>
#include <bp/match.h>
#include <bp/pattern.h>
#include <err.h>
#include <gc.h>
#include <gc/cord.h>
#include <libgccjit.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

#include "parse.h"
#include "typecheck.h"
#include "compile/compile.h"

#define streq(a,b) (strcmp(a,b) == 0)
#define endswith(str,end) (strlen(str) >= strlen(end) && strcmp((str) + strlen(str) - strlen(end), end) == 0)

int compile_to_file(gcc_jit_context *ctx, file_t *f, bool verbose, int argc, char *argv[])
{
    if (verbose)
        fprintf(stderr, "\x1b[33;4;1mParsing %s...\x1b[m\n", f->filename);
    ast_t *ast = parse(f, NULL);

    if (verbose)
        fprintf(stderr, "\x1b[33;4;1mCompiling %s...\n\x1b[0;34;1m", f->filename);

    gcc_jit_result *result;
    main_func_t run = compile_file(ctx, NULL, f, ast, true, &result);
    if (!run) errx(1, "run func is NULL");

    CORD binary_name;
    int i = 0;
    if (i+2 < argc && streq(argv[i+1], "-o")) {
        binary_name = CORD_from_char_star(argv[i+2]);
        i += 2;
    } else {
        binary_name = CORD_from_char_star(argv[i]);
        size_t i = CORD_rchr(binary_name, CORD_len(binary_name)-1, '.');
        if (i == CORD_NOT_FOUND) binary_name = CORD_cat(binary_name, ".o");
        else binary_name = CORD_substr(binary_name, 0, i);
    }

    if (CORD_ncmp(binary_name, 0, "/", 0, 1) != 0
        && CORD_ncmp(binary_name, 0, "./", 0, 2) != 0
        && CORD_ncmp(binary_name, 0, "~/", 0, 2) != 0)
        binary_name = CORD_cat("./", binary_name);

    binary_name = CORD_to_char_star(binary_name);
    gcc_jit_context_compile_to_file(ctx, GCC_JIT_OUTPUT_KIND_EXECUTABLE, binary_name);
    printf("\x1b[0;1;32mSuccessfully compiled %s to %s\x1b[m\n", argv[i], binary_name);
    gcc_jit_result_release(result);

    recycle_all_matches();
    destroy_file(&f);
    return 0;
}

int run_file(gcc_jit_context *ctx, jmp_buf *on_err, file_t *f, bool verbose, int argc, char *argv[])
{
    if (verbose)
        fprintf(stderr, "\x1b[33;4;1mParsing %s...\x1b[m\n", f->filename);
    ast_t *ast = parse(f, on_err);

    if (verbose)
        fprintf(stderr, "\x1b[33;4;1mCompiling %s...\n\x1b[0;34;1m", f->filename);

    gcc_jit_result *result;
    main_func_t main_fn = compile_file(ctx, on_err, f, ast, true, &result);
    if (!main_fn) errx(1, "run func is NULL");

    if (verbose)
        fprintf(stderr, "\x1b[0;33;4;1mProgram Output\x1b[m\n");
    main_fn(argc, argv);
    gcc_jit_result_release(result);
    return 0;
}

int run_repl(gcc_jit_context *ctx, bool verbose)
{
    char *line = NULL;
    size_t len = 0;
    ssize_t nread;
    fputs("> ", stdout);
    fflush(stdout);
    while ((nread = getline(&line, &len, stdin)) != -1) {
        if (nread == 0) break;
        file_t *f = spoof_file(NULL, "<repl>", line, nread);
        jmp_buf on_err;
        if (setjmp(on_err)) {
            fputs("> ", stdout);
            fflush(stdout);
            continue;
        }
        run_file(ctx, &on_err, f, verbose, 0, (char*[]){NULL});
        fputs("> ", stdout);
        fflush(stdout);
    }
    free(line);
    fputs("\n", stdout);
    fflush(stdout);
    return 0;
}

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

    const char *driver_flags[] = {
        "-lgc", "-lcord", "-lm", "-lintern", "-lbp", "-lbhash", "-lm", "-L.", "-lblang",
        "-Wl,-rpath", "-Wl,$ORIGIN",
    };
    for (size_t i = 0; i < sizeof(driver_flags)/sizeof(driver_flags[0]); i++)
        gcc_add_driver_opt(ctx, driver_flags[i]);
    gcc_jit_context_set_bool_option(ctx, GCC_JIT_BOOL_OPTION_DEBUGINFO, true);
    gcc_jit_context_set_bool_allow_unreachable_blocks(ctx, true);

    for (int i = 1; i < argc; i++) {
        if (streq(argv[i], "-v") || streq(argv[i], "--verbose")) {
            gcc_jit_context_set_bool_option(ctx, GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE, 1);
            verbose = true;
            continue;
        } else if (streq(argv[i], "-r") || streq(argv[i], "--run")) {
            run_program = true;
            continue;
        } else if (streq(argv[i], "-A") || streq(argv[i], "--asm")) {
            gcc_jit_context_set_bool_option(ctx, GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE, 1);
            verbose = true;
            continue;
        }

        file_t *f = load_file(NULL, argv[i]);
        if (!f) errx(1, "Couldn't open file: %s", argv[i]);
        if (run_program)
            return run_file(ctx, NULL, f, verbose, argc-i, &argv[i]);
        else
            return compile_to_file(ctx, f, verbose, argc-i, &argv[i]);
    }

    run_repl(ctx, verbose);

    gcc_jit_context_release(ctx);

    return 0;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
