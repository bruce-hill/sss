#include <bhash.h>
#include <bp/files.h>
#include <bp/match.h>
#include <bp/pattern.h>
#include <gc.h>
#include <gc/cord.h>
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
    bool stop_at_qbe = false;
    bool stop_at_asm = false;
    char *prog_name = strrchr(argv[0], '/');
    prog_name = prog_name ? prog_name + 1 : argv[0];
    bool run_program = !strstr(prog_name, "blangc");
    for (int i = 1; i < argc; i++) {
        if (streq(argv[i], "-v") || streq(argv[i], "--verbose")) {
            verbose = true;
            continue;
        } else if (streq(argv[i], "-Q") || streq(argv[i], "--qbe")) {
            stop_at_qbe = true;
            continue;
        } else if (streq(argv[i], "-A") || streq(argv[i], "--asm")) {
            stop_at_asm = true;
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
            printf("Compiling %s...\n", argv[i]);
        const char *code = compile_file(f, ast);
        if (verbose) {
            printf("\x1b[36;1m");
            puts(code);
            printf("\x1b[m");
        }

        if (stop_at_qbe) {
            puts(code);
            continue;
        }

        char qbe_filename[] = "/tmp/blang-XXXXXX.qbe";
        int fd = mkstemps(qbe_filename, 4);
        if (verbose)
            printf("Writing QBE to %s...\n", qbe_filename);
        write(fd, code, strlen(code));
        close(fd);

        char asm_filename[] = "/tmp/blang-XXXXXX.s";
        int asm_fd = mkstemps(asm_filename, 2);
        close(asm_fd);
        unlink(asm_filename);

        if (verbose)
            printf("Invoking QBE to compile from %s -> %s\n", qbe_filename, asm_filename);

        pid_t child = fork();
        if (child == 0) {
            if (stop_at_asm)
                execlp("qbe", "qbe", qbe_filename, NULL);
            else
                execlp("qbe", "qbe", qbe_filename, "-o", asm_filename, NULL);
            exit(1);
        }
        int status;
        waitpid(child, &status, 0);

        if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
            fprintf(stderr, "\x1b[31;1mQBE failed to compile\x1b[m\n");
            exit(1);
        }

        if (stop_at_asm) continue;

        CORD binary_name;
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

        if (verbose) {
            child = fork();
            printf("\x1b[34;1m");
            fflush(stdout);
            if (child == 0) {
                execlp("cat", "cat", asm_filename, NULL);
                exit(1);
            }
            waitpid(child, &status, 0);
            printf("\x1b[m");
        }

        if (verbose)
            printf("Invoking CC to compile from %s -> %s\n", asm_filename, binary_name);

        child = fork();
        if (child == 0) {
            execlp("cc", "cc", asm_filename, "-lgc", "-lc", "-lintern", "-lcord", "-lbp", "-o", binary_name, NULL);
            exit(1);
        }
        waitpid(child, &status, 0);

        if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
            fprintf(stderr, "\x1b[31;1mCC failed to compile\x1b[m\n");
            exit(1);
        }

        if (run_program) {
            if (verbose)
                printf("Running program %s...\n", binary_name);

            child = fork();
            if (child == 0) {
                execlp(binary_name, binary_name, NULL);
                exit(1);
            }
            waitpid(child, &status, 0);

            if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
                fprintf(stderr, "\x1b[31;1mProgram failed.\x1b[m\n");
                exit(1);
            }
        }

        recycle_all_matches();
        destroy_file(&f);
    }

    return 0;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
