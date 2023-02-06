#include <bhash.h>
#include <err.h>
#include <gc.h>
#include <gc/cord.h>
#include <libgccjit.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

#include "parse.h"
#include "files.h"
#include "typecheck.h"
#include "compile/compile.h"

#define streq(a,b) (strcmp(a,b) == 0)
#define endswith(str,end) (strlen(str) >= strlen(end) && strcmp((str) + strlen(str) - strlen(end), end) == 0)

#define BLANG_VERSION "0.1.0"

int compile_to_file(gcc_jit_context *ctx, bl_file_t *f, bool dll, bool verbose, int argc, char *argv[])
{
    if (verbose)
        fprintf(stderr, "\x1b[33;4;1mParsing %s...\x1b[m\n", f->filename);
    ast_t *ast = parse_file(f, NULL);

    if (verbose)
        fprintf(stderr, "Result: %s\n", ast_to_str(ast));

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
        if (i == CORD_NOT_FOUND && !dll) {
            binary_name = CORD_cat(binary_name, ".o");
        } else {
            if (i != CORD_NOT_FOUND)
                binary_name = CORD_substr(binary_name, 0, i);

            if (dll) {
                const char *path = CORD_to_char_star(binary_name);
                const char *slash = strrchr(path, '/');
                if (slash) {
                    CORD_sprintf(&binary_name, "%.*s/lib%s.so", (int)(slash-path)-1, path, slash+1);
                } else {
                    CORD_sprintf(&binary_name, "lib%s.so", path);
                }
            }
        }
    }

    if (CORD_ncmp(binary_name, 0, "/", 0, 1) != 0
        && CORD_ncmp(binary_name, 0, "./", 0, 2) != 0
        && CORD_ncmp(binary_name, 0, "~/", 0, 2) != 0)
        binary_name = CORD_cat("./", binary_name);

    binary_name = CORD_to_char_star(binary_name);
    gcc_jit_context_compile_to_file(ctx, dll ? GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY : GCC_JIT_OUTPUT_KIND_EXECUTABLE, binary_name);
    printf("\x1b[0;1;32mSuccessfully compiled %s to %s\x1b[m\n", argv[i], binary_name);
    gcc_jit_result_release(result);

    return 0;
}

int run_file(gcc_jit_context *ctx, jmp_buf *on_err, bl_file_t *f, bool verbose, int argc, char *argv[])
{
    if (verbose)
        fprintf(stderr, "\x1b[33;4;1mParsing %s...\x1b[m\n", f->filename);
    ast_t *ast = parse_file(f, on_err);

    if (verbose)
        fprintf(stderr, "Result: %s\n", ast_to_str(ast));

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
    const char *prompt = "\x1b[33;1m>>>\x1b[m ";
    const char *continue_prompt = "\x1b[33;1m...\x1b[m ";
    jmp_buf on_err;
    env_t *env = new_environment(ctx, &on_err, NULL, verbose);

    printf("\n    \x1b[1;4mWelcome to the Blang v%s interactive console!\x1b[m\n", BLANG_VERSION);
    printf("          press 'enter' twice to run a command\n");
    printf("     \x1b[2mnote: variables do not persist across commands\x1b[m\n\n\n");

    // Read lines until we get a blank line
    for (;;) {
        fputs(prompt, stdout);
        fflush(stdout);

        char *buf = NULL;
        size_t buf_size = 0;
        FILE *buf_file = open_memstream(&buf, &buf_size);

        char *line = NULL;
        size_t len = 0;
        ssize_t nread;
        while ((nread = getline(&line, &len, stdin)) != -1) {
            if (nread <= 1) break;
            fwrite(line, 1, nread, buf_file);
            fputs(continue_prompt, stdout);
            fflush(stdout);
        }
        if (line) free(line);

        fflush(buf_file);
        gcc_block_t *block = NULL;

        gcc_jit_result *result = NULL;
#define CLEANUP() do { \
        if (block) gcc_return_void(block, NULL); \
        if (result) gcc_jit_result_release(result); \
        fclose(buf_file); \
        free(buf); } while (0) \

        if (buf_size == 0 || strcmp(buf, "quit\n") == 0 || strcmp(buf, "exit\n") == 0) {
            printf("\x1b[A\x1b[G\x1b[K\x1b[1mGoodbye!\x1b[m\n");
            CLEANUP();
            break;
        }

        bl_file_t *f = bl_spoof_file("<repl>", buf);
        env->file = f;
        if (setjmp(on_err) != 0) {
            CLEANUP();
            continue;
        }

        ast_t *ast = parse_file(f, &on_err);
        ast = globalize_decls(ast);
        if (!is_discardable(env, ast)) {
            bl_type_t *t = get_type(env, ast);
            if (t->tag == GeneratorType) {
                printf("\x1b[31;1mGenerator types can't be used in the REPL\x1b[m\n");
                CLEANUP();
                continue;
            } else if (t->tag == ArrayType && Match(t, ArrayType)->item_type->tag == CharType) {
                // Quote the string:
                auto str = Match(t, ArrayType);
                if (str->dsl)
                    ast = WrapAST(ast, StringJoin, .children=LIST(ast_t*, WrapAST(ast, Interp, .value=ast)));
                List(ast_t*) args = LIST(ast_t*, WrapAST(ast, KeywordArg, .name=intern_str("colorize"), .arg=WrapAST(ast, Bool, .b=true)));
                ast = WrapAST(ast, FunctionCall, .fn=WrapAST(ast, FieldAccess, .fielded=ast, .field=intern_str("quoted")), .args=args);
            }
            ast_t *prefix = WrapAST(ast, StringLiteral, .str=intern_str("\x1b[0;2m= \x1b[0;35m"));
            ast_t *type_info = WrapAST(ast, StringLiteral, .str=intern_strf("\x1b[0;2m : %s\x1b[m", type_to_string(t)));
            // Stringify and add type info:
            ast = WrapAST(ast, StringJoin, .children=LIST(ast_t*, prefix, WrapAST(ast, Interp, .value=ast), type_info));
            // Call say(str):
            ast = WrapAST(ast, Block, .statements=LIST(ast_t*, WrapAST(ast, FunctionCall, .fn=WrapAST(ast, Var, .name=intern_str("say")), .args=LIST(ast_t*, ast))));
        }

        if (verbose)
            fprintf(stderr, "Result: %s\n", ast_to_str(ast));

        const char *repl_name = fresh("repl");
        gcc_func_t *repl_func = gcc_new_func(ctx, NULL, GCC_FUNCTION_EXPORTED, gcc_type(ctx, VOID), repl_name, 0, NULL, 0);
        block = gcc_new_block(repl_func, fresh("repl_body"));

        hashmap_t *old_globals = hashmap_new();
        for (const void *key = NULL; (key = hashmap_next(env->global_bindings, key)); )
            hashmap_set(old_globals, key, hashmap_get(env->global_bindings, key));

        env_t *fresh_env = fresh_scope(env);
        compile_statement(fresh_env, &block, ast);
        if (block) {
            gcc_return_void(block, NULL);
            block = NULL;
        }

        result = gcc_compile(ctx);
        if (result == NULL)
            compile_err(fresh_env, NULL, "Compilation failed");

        // Extract the generated code from "result".   
        void (*run_line)(void) = (void (*)(void))gcc_jit_result_get_code(result, repl_name);
        assert(run_line);
        fprintf(stdout, "\x1b[A\x1b[G\x1b[K\x1b[0;1m");
        fflush(stdout);
        run_line();
        fputs("\x1b[m", stdout);
        fflush(stdout);

        // Copy out the global variables to GC memory
        for (const void *key = NULL; (key = hashmap_next(env->global_bindings, key)); ) {
            if (hashmap_get(old_globals, key))
                continue;

            binding_t *b = hashmap_get(env->global_bindings, key);
            if (b->type->tag == FunctionType)
                continue;

            void *global = gcc_jit_result_get_global(result, (char*)key);
            gcc_type_t *gcc_t = bl_type_to_gcc(env, b->type);
            char *copy = GC_MALLOC(gcc_sizeof(env, b->type));
            memcpy(copy, (char*)global, gcc_sizeof(env, b->type));
            gcc_rvalue_t *ptr = gcc_jit_context_new_rvalue_from_ptr(env->ctx, gcc_get_ptr_type(gcc_t), copy);
            b->lval = gcc_jit_rvalue_dereference(ptr, NULL);
            b->rval = gcc_rval(b->lval);
            hashmap_set(old_globals, key, hashmap_get(env->global_bindings, key));
        }

        CLEANUP();
#undef CLEANUP
    }
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
    bool dll = false;

    gcc_jit_context *ctx = gcc_jit_context_acquire();
    assert(ctx != NULL);

    const char *driver_flags[] = {
        "-lgc", "-lcord", "-lm", "-lintern", "-lbhash", "-lm", "-L.", "-lblang",
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
        } else if (streq(argv[i], "-c")) {
            dll = true;
            continue;
        } else if (strncmp(argv[i], "-O", 2) == 0) {
            int opt = atoi(argv[i]+2);
            gcc_jit_context_set_int_option(ctx, GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, opt);
            continue;
        }

        bl_file_t *f = bl_load_file(argv[i]);
        if (!f) errx(1, "Couldn't open file: %s", argv[i]);
        if (run_program)
            return run_file(ctx, NULL, f, verbose, argc-i, &argv[i]);
        else
            return compile_to_file(ctx, f, dll, verbose, argc-i, &argv[i]);
    }

    run_repl(ctx, verbose);

    gcc_jit_context_release(ctx);

    return 0;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
