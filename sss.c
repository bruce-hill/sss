// The main program
#include <err.h>
#include <gc.h>
#include <gc/cord.h>
#include <libgccjit.h>
#include <printf.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

#include "parse.h"
#include "files.h"
#include "typecheck.h"
#include "compile/compile.h"
#include "builtins/table.h"
#include "util.h"

static bool verbose = false;
static bool tail_calls = false;

const char *PROGRAM_PATH = "sss";

#define endswith(str,end) (strlen(str) >= strlen(end) && strcmp((str) + strlen(str) - strlen(end), end) == 0)

static void enable_link_time_optimization(gcc_jit_context *ctx)
{
    static bool enabled = false;
    if (enabled) return;
    enabled = true;
    gcc_jit_context_add_command_line_option(ctx, "-flto");
    gcc_jit_context_add_command_line_option(ctx, "-ffat-lto-objects");
    gcc_add_driver_opt(ctx, "-Wl,-flto");
}

int compile_to_file(gcc_jit_context *ctx, sss_file_t *f, int argc, char *argv[])
{
    if (verbose)
        fprintf(stderr, "\x1b[33;4;1mParsing %s...\x1b[m\n", f->filename);
    ast_t *ast = parse_file(f, NULL);

    if (verbose)
        fprintf(stderr, "Result: %s\n", ast_to_str(ast));

    if (verbose)
        fprintf(stderr, "\x1b[33;4;1mCompiling %s...\n\x1b[0;34;1m", f->filename);

    gcc_jit_result *result;
    compile_object_file(ctx, NULL, f, ast, tail_calls, &result);

    CORD binary_name;
    int i = 0;
    if (i+2 < argc && streq(argv[i+1], "-o")) {
        binary_name = CORD_from_char_star(argv[i+2]);
        i += 2;
    } else {
        binary_name = CORD_from_char_star(argv[i]);
        size_t i = CORD_rchr(binary_name, CORD_len(binary_name)-1, '.');
        if (i != CORD_NOT_FOUND)
            CORD_substr(binary_name, 0, i);

        struct stat info;
        assert(stat(f->filename, &info) != -1);
        CORD_sprintf(&binary_name, "%r.%ld.o", binary_name, info.st_ino);
    }

    if (CORD_ncmp(binary_name, 0, "/", 0, 1) != 0
        && CORD_ncmp(binary_name, 0, "./", 0, 2) != 0
        && CORD_ncmp(binary_name, 0, "~/", 0, 2) != 0)
        binary_name = CORD_cat("./", binary_name);

    binary_name = CORD_to_char_star(binary_name);
    gcc_jit_context_compile_to_file(ctx, GCC_OUTPUT_KIND_OBJECT_FILE, binary_name);
    printf("\x1b[0;1;32mSuccessfully compiled \x1b[33m%s\x1b[32m -> \x1b[37m%s\x1b[m\n", f->relative_filename, binary_name);
    gcc_jit_result_release(result);

    return 0;
}

int run_file(gcc_jit_context *ctx, jmp_buf *on_err, sss_file_t *f, int argc, char *argv[])
{
    if (verbose)
        fprintf(stderr, "\x1b[33;4;1mParsing %s...\x1b[m\n", f->filename);
    ast_t *ast = parse_file(f, on_err);

    if (verbose)
        fprintf(stderr, "Result: %s\n", ast_to_str(ast));

    if (verbose)
        fprintf(stderr, "\x1b[33;4;1mCompiling %s...\n\x1b[0;34;1m", f->filename);

    gcc_jit_result *result;
    main_func_t main_fn = compile_file(ctx, on_err, f, ast, tail_calls, &result);
    if (!main_fn) errx(1, "run func is NULL");

    if (verbose)
        fprintf(stderr, "\x1b[0;33;4;1mProgram Output\x1b[m\n");
    main_fn(argc, argv);
    gcc_jit_result_release(result);
    return 0;
}

int run_repl(gcc_jit_context *ctx)
{
    bool use_color = !getenv("NO_COLOR");
    const char *prompt = !use_color ? ">>> " : "\x1b[33;1m>>>\x1b[m ";
    const char *continue_prompt = !use_color ? "... " : "\x1b[33;1m...\x1b[m ";
    jmp_buf on_err;
    env_t *env = new_environment(ctx, &on_err, NULL, tail_calls);

    // Seed the RNG used for Num.random()
    srand48(arc4random());

    if (use_color) {
        printf("\n     \x1b[1;4mWelcome to the SSS v%s interactive console!\x1b[m\n", SSS_VERSION);
        printf("          press 'enter' twice to run a command\n");
        printf("     \x1b[2mnote: importing with 'use' isn't supported yet\x1b[m\n\n\n");
    } else {
        printf("\n     Welcome to the SSS v%s interactive console!\n", SSS_VERSION);
        printf("          press 'enter' twice to run a command\n");
        printf("     note: importing with 'use' isn't supported yet\n\n\n");
    }

    // Set up `USE_COLOR`
    gcc_lvalue_t *use_color_var = gcc_global(env->ctx, NULL, GCC_GLOBAL_EXPORTED, gcc_type(env->ctx, BOOL), "USE_COLOR");
    gcc_jit_global_set_initializer_rvalue(use_color_var, gcc_rvalue_bool(ctx, use_color));
    Table_str_set(&env->global->bindings, "USE_COLOR",
         new(binding_t, .rval=gcc_rval(use_color_var), .type=Type(BoolType), .visible_in_closures=true));

    // Keep track of which functions have already been compiled:
    table_t compiled_functions = {0};

    // Keep these alive and clean them up at the end so we can continue accessing their memory:
    auto results = EMPTY_ARRAY(gcc_jit_result*);

    // Read lines until we get a blank line
    for (;;) {
      next_line:
        fputs(prompt, stdout);
        fflush(stdout);
        gcc_block_t *block = NULL;

        gcc_jit_result *result = NULL;
#define CLEANUP() do { \
        if (block) gcc_return_void(block, NULL); \
        if (result) append(results, result); \
        fclose(buf_file); \
        free(buf); } while (0) \

        char *buf = NULL;
        size_t buf_size = 0;
        FILE *buf_file = open_memstream(&buf, &buf_size);

        char *line = NULL;
        size_t len = 0;
        ssize_t nread;
        while ((nread = getline(&line, &len, stdin)) != -1) {
            if (nread <= 1) break;

            if (streq(line, "quit\n") || streq(line, "exit\n")) {
                printf("\x1b[0;1mGoodbye!\x1b[m\n");
                CLEANUP();
                fflush(stdout);
                return 0;
            } else if (streq(line, "help\n")) {
                if (use_color)
                    puts("\x1b[0;3;36;1m");
                puts(
                    "\nHello! This is the SSS read-evaluate-print-loop (REPL).\n"
                    "Here, you can type SSS statements or expressions to see them evaluted and printed.\n"
                    "For example:\n");
                if (use_color) {
                    puts("    \x1b[0;33;1m>>>\x1b[m 1+2\n"
                         "    \x1b[2m= \x1b[0;35m3\x1b[0;2m : Int\x1b[m\n"
                         "\x1b[0;3;36;1m");
                } else {
                    puts("    >>> 1+2\n"
                         "    = 3 : Int\n");
                }
                puts(
                    "The REPL reads expressions until it reaches a blank line,\n"
                    "so press Enter twice after each expression to see it evaluated and printed.\n");
                if (use_color)
                    puts("Hit \x1b[0;1mCtrl-D\x1b[0;3;36;1m to exit.\n\x1b[m");
                else
                    puts("Hit Ctrl-D to exit.\n");
                CLEANUP();
                goto next_line;
            }

            fwrite(line, 1, nread, buf_file);
            fputs(continue_prompt, stdout);
            fflush(stdout);
        }
        if (line) free(line);

        fflush(buf_file);
        if (buf_size == 0) {
            if (use_color)
                printf("\x1b[G\x1b[K\x1b[1mGoodbye!\x1b[m\n");
            else
                printf("Goodbye!\n");
            CLEANUP();
            break;
        }

        sss_file_t *f = sss_spoof_file("<repl>", buf);
        env->file = f;
        if (setjmp(on_err) != 0) {
            CLEANUP();
            goto next_line;
        }

        ast_t *ast = parse_file(f, &on_err);

        // Convert decls to globals and wrap all statements in doctests
        auto statements = Match(ast, Block)->statements;
        auto stmts = EMPTY_ARRAY(ast_t*);
        for (int64_t i = 0; i < LENGTH(statements); i++) {
            ast_t *stmt = ith(statements, i);
            if (stmt->tag == Use || (stmt->tag == Declare && Match(stmt, Declare)->value->tag == Use)) {
                if (use_color) fprintf(stdout, "\x1b[31;1m");
                fprintf(stdout, "I'm sorry, but 'use' statements are not currently supported in the REPL!\n");
                if (use_color) fprintf(stdout, "\x1b[m");
                CLEANUP();
                goto next_line;
            }

            if (stmt->tag == Declare)
                stmt = WrapAST(stmt, Declare, .var=Match(stmt, Declare)->var, .value=Match(stmt, Declare)->value, .is_global=true);
            stmt = WrapAST(stmt, DocTest, .expr=stmt, .skip_source=true);
            append(stmts, stmt);
        }
        ast = WrapAST(ast, Block, .statements=stmts, .keep_scope=true);

        if (verbose)
            fprintf(stderr, "Result: %s\n", ast_to_str(ast));

        const char *repl_name = fresh("repl");
        gcc_func_t *repl_func = gcc_new_func(ctx, NULL, GCC_FUNCTION_EXPORTED, gcc_type(ctx, VOID), repl_name, 0, NULL, 0);
        block = gcc_new_block(repl_func, fresh("repl_body"));

        env_t *fresh_env = fresh_scope(env);
        compile_statement(fresh_env, &block, ast);
        if (block) {
            gcc_return_void(block, NULL);
            block = NULL;
        }

        for (int64_t i = 1; i <= Table_length(&fresh_env->global->ast_functions); i++) {
            struct { const char *key; func_context_t *value; } *entry = Table_str_entry(&fresh_env->global->ast_functions, i);
            const char *func_addr = heap_strf("%p", entry->value->func);
            if (Table_str_get(&compiled_functions, func_addr))
                continue;
            ast_t *ast = (ast_t*)strtol(entry->key, NULL, 16); // ugly hack, key is a string-encoded pointer
            compile_function(&entry->value->env, entry->value->func, ast);
            Table_str_set(&compiled_functions, func_addr, entry->value->func);
        }

        result = gcc_compile(ctx);
        if (result == NULL)
            compiler_err(fresh_env, NULL, "Compilation failed");

        // Extract the generated code from "result".   
        void (*run_line)(void) = (void (*)(void))gcc_jit_result_get_code(result, repl_name);
        assert(run_line);
        if (use_color)
            fprintf(stdout, "\x1b[A\x1b[G\x1b[K\x1b[0;1m");
        fflush(stdout);
        run_line();
        if (use_color)
            fputs("\x1b[m", stdout);
        fflush(stdout);

        // Copy out the variables to GC memory
        for (table_t *bindings = fresh_env->bindings; bindings; bindings = bindings->fallback) {
            for (int64_t i = 1; i <= Table_length(bindings); i++) {
                struct {const char *key; binding_t *value;} *entry = Table_str_entry(bindings, i);
                binding_t *b = entry->value;
                if (!b->sym_name || Table_str_get(env->bindings, entry->key) == b)
                    continue;

                // Update the binding so it points to the global memory:
                void *global = gcc_jit_result_get_global(result, b->sym_name);
                assert(global);
                gcc_type_t *gcc_t = sss_type_to_gcc(fresh_env, b->type);
                gcc_rvalue_t *ptr = gcc_jit_context_new_rvalue_from_ptr(fresh_env->ctx, gcc_get_ptr_type(gcc_t), global);
                b->lval = gcc_jit_rvalue_dereference(ptr, NULL);
                b->rval = gcc_rval(b->lval);
                Table_str_set(env->bindings, entry->key, b);
            }
        }

        CLEANUP();
#undef CLEANUP
    }

    fputs("\n", stdout);
    fflush(stdout);

    foreach (results, result, _) {
        if (*result) gcc_jit_result_release(*result);
    }
    return 0;
}

int main(int argc, char *argv[])
{
    PROGRAM_PATH = argv[0];

#ifdef __OpenBSD__
    unveil("/include", "r");
    unveil("/lib", "r");
    unveil("/usr/lib", "r");
    unveil(getcwd(), "r");
#endif

    GC_INIT();
    char *prog_name = strrchr(argv[0], '/');
    prog_name = prog_name ? prog_name + 1 : argv[0];
    bool run_program = true;

    gcc_jit_context *ctx = gcc_jit_context_acquire();
    assert(ctx != NULL);

    // Set $SSSPATH (without overriding if it already exists)
    setenv("SSSPATH", heap_strf(".:%s/.local/share/sss/modules:/usr/local/share/sss/modules", getenv("HOME")), 0);

    const char *gcc_flags[] = {
        "-ftrapv", "-freg-struct-return",
    };
    for (size_t i = 0; i < sizeof(gcc_flags)/sizeof(gcc_flags[0]); i++)
        gcc_jit_context_add_command_line_option(ctx, gcc_flags[i]);

    const char *driver_flags[] = {
        "-lgc", "-lcord", "-lm", "-L.", "-l:libsss.so."SSS_VERSION,
        "-Wl,-rpath", "-Wl,$ORIGIN",
    };
    for (size_t i = 0; i < sizeof(driver_flags)/sizeof(driver_flags[0]); i++)
        gcc_add_driver_opt(ctx, driver_flags[i]);
    gcc_jit_context_set_bool_option(ctx, GCC_JIT_BOOL_OPTION_DEBUGINFO, true);
    gcc_jit_context_set_bool_allow_unreachable_blocks(ctx, true);

    // register_printf_modifier(L"p");
    if (register_printf_specifier('T', printf_type, printf_pointer_size))
        errx(1, "Couldn't set printf specifier");
    if (register_printf_specifier('W', printf_ast, printf_pointer_size))
        errx(1, "Couldn't set printf specifier");

    extern Type *CStringToVoidStarTable_type;
    extern Type CString_type;
    extern Type Memory_type;
    Type MemoryPointer_type = {
        .name="@Memory",
        .size=sizeof(void*),
        .align=alignof(void*),
        .tag=PointerInfo,
        .PointerInfo={
            .sigil="@",
            .pointed=&Memory_type,
        },
    };
    CStringToVoidStarTable_type = (Type[]){{
        .name="{CString=>@Memory}",
        .size=sizeof(table_t),
        .align=alignof(table_t),
        .tag=TableInfo,
        .TableInfo={.key=&CString_type,.value=&MemoryPointer_type,
            .entry_size=16, .value_offset=8},
    }};

    for (int i = 1; i < argc; i++) {
        if (streq(argv[i], "-h") || streq(argv[i], "--help")) {
            puts("sss - The SSS programming language runner");
            puts("Usage: sss [-h|--help] [-v|--verbose] [--version] [-c|--compile] [-o outfile] [-A|--asm] [-O<optimization>] [-G<GCC flag>] [file.sss | -e '<expr>']");
            return 0;
        } else if (streq(argv[i], "-V")) {
            ++i;
            continue;
        } else if (strncmp(argv[i], "-V", 2) == 0) {
            continue;
        } else if (streq(argv[i], "-v") || streq(argv[i], "--verbose")) {
            gcc_jit_context_set_bool_option(ctx, GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE, 1);
            verbose = true;
            continue;
        } else if (streq(argv[i], "--version")) {
            puts(SSS_VERSION);
            return 0;
        } else if (streq(argv[i], "-c") || streq(argv[i], "--compile")) {
            run_program = false;
            continue;
        } else if (streq(argv[i], "-A") || streq(argv[i], "--asm")) {
            gcc_jit_context_set_bool_option(ctx, GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE, 1);
            gcc_jit_context_add_command_line_option(ctx, "-fverbose-asm");
            verbose = true;
            continue;
        } else if (strncmp(argv[i], "-O", 2) == 0) { // Optimization level
            if (streq(argv[i]+2, "fast")) {
                tail_calls = true;
                enable_link_time_optimization(ctx);
                gcc_jit_context_set_int_option(ctx, GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, 3);
                gcc_jit_context_add_command_line_option(ctx, argv[i]);
            } else {
                int opt = atoi(argv[i]+2);
                tail_calls = (opt >= 2);
                if (opt >= 2)
                    enable_link_time_optimization(ctx);
                gcc_jit_context_set_int_option(ctx, GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, opt);
            }
            continue;
        } else if (strncmp(argv[i], "-G", 2) == 0) { // GCC Flag
            if (streq(argv[i]+2, "Ofast") || streq(argv[i]+2, "O2") || streq(argv[i]+2,"O3")) {
                tail_calls = true;
                enable_link_time_optimization(ctx);
            }
            gcc_jit_context_add_command_line_option(ctx, heap_strf("-%s", argv[i]+2));
            continue;
        } else if (streq(argv[i], "-e") || streq(argv[i], "--eval")) {
            if (i+1 >= argc)
                errx(1, "I expected an argument for a program to execute");
            const char *src = isatty(STDOUT_FILENO) ? heap_strf(">>> %s", argv[++i]) : heap_strf("say \"$(%s)\"", argv[++i]);
            sss_file_t *f = sss_spoof_file("<argument>", src);
            argv[i] = argv[0];
            return run_file(ctx, NULL, f, argc-i, &argv[i]);
        }

#ifdef __OpenBSD__
        unveil(argv[i]);
        if (pledge("stdio rpath wpath cpath tmppath", NULL))
            err(1, "could not pledge");
#endif

        sss_file_t *f = sss_load_file(argv[i]);
        if (!f) {
            if (argv[i][0] == '-')
                errx(1, "'%s' is not a recognized command-line argument", argv[i]);
            errx(1, "Couldn't open file: %s", argv[i]);
        }

        if (run_program) {
            argv[i] = argv[0];
            return run_file(ctx, NULL, f, argc-i, &argv[i]);
        } else {
            return compile_to_file(ctx, f, argc-i, &argv[i]);
        }
    }

    if (isatty(STDIN_FILENO)) {
        run_repl(ctx);
    } else {
        sss_file_t *f = sss_load_file("/dev/stdin");
        if (run_program) {
            return run_file(ctx, NULL, f, 1, argv);
        } else {
            return compile_to_file(ctx, f, 1, argv);
        }
    }

    gcc_jit_context_release(ctx);

    return 0;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
