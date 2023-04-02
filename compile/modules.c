// Loading of external modules
#include <dlfcn.h>
#include <err.h>
#include <gc.h>
#include <gc/cord.h>
#include <libgccjit.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "modules.h"
#include "../files.h"
#include "../parse.h"
#include "../typecheck.h"
#include "compile.h"

// Get nested namespaces for stuff like Foo.Baz.Qux => Foo[Baz]
bl_hashmap_t *get_qualified_ns(env_t *env, const char *qualified_name)
{
    bl_hashmap_t *ns = env->bindings;
    const char *segment = qualified_name;
    while (strchr(segment, '.')) {
        size_t len = strcspn(segment, ".");
        const char* name = heap_strn(segment, len);
        binding_t *b = hget(ns, name, binding_t*);
        assert(b);
        bl_type_t *t = Match(b->type, TypeType)->type;
        ns = get_namespace(env, t);
        assert(ns);
        segment += len + 1;
    }
    return ns;
}

void load_module(env_t *env, gcc_block_t **block, ast_t *use)
{
    const char *path = Match(use, Use)->path;
    auto loc = ast_loc(env, use);
    // printf("Loading: '%s'\n", path);
    void *dl_handle = dlopen(path, RTLD_LAZY);
    if (!dl_handle) errx(1, "DLL failed to open");
    load_fn_t *get_exports = dlsym(dl_handle, "get_exports");
    assert(get_exports);
    import_list_t imports = get_exports();
    // printf("Import len: %d\n", imports.len);
    // printf("Import stride: %d\n", imports.stride);
    // printf("Import items: %p\n", imports.imports);
    gcc_func_t *func = gcc_block_func(*block);
    gcc_type_t *i32_t = gcc_type(env->ctx, INT32);
    gcc_rvalue_t *rtld_lazy = gcc_rvalue_from_long(env->ctx, i32_t, RTLD_LAZY);
    gcc_type_t *void_star = gcc_get_type(env->ctx, GCC_T_VOID_PTR);
    gcc_lvalue_t *handle = gcc_local(func, loc, void_star, fresh("handle"));
    gcc_func_t *dlopen_fn = get_function(env, "dlopen");
    gcc_assign(*block, loc, handle, gcc_callx(env->ctx, loc, dlopen_fn, gcc_str(env->ctx, path), rtld_lazy));

    gcc_block_t *open_success = gcc_new_block(func, fresh("open_success")),
                *open_fail = gcc_new_block(func, fresh("open_fail"));
    gcc_jump_condition(*block, loc, gcc_comparison(env->ctx, loc, GCC_COMPARISON_NE, gcc_rval(handle), gcc_null(env->ctx, void_star)),
                       open_success, open_fail);
    *block = open_fail;
    gcc_func_t *puts_fn = get_function(env, "puts");
    gcc_eval(*block, loc, gcc_callx(env->ctx, loc, puts_fn, gcc_str(env->ctx, "FAILED TO OPEN DLL")));
    gcc_jump(*block, loc, open_success);

    *block = open_success;
    // Run load_module()
    gcc_func_t *dlsym_fn = get_function(env, "dlsym");
    gcc_type_t *void_fn_t = gcc_new_func_type(env->ctx, NULL, gcc_type(env->ctx, VOID), 0, NULL, 0);
    gcc_rvalue_t *gcc_load_fn = gcc_cast(
        env->ctx, loc, gcc_callx(env->ctx, loc, dlsym_fn, gcc_rval(handle), gcc_str(env->ctx, "get_exports")),
        void_fn_t);
    gcc_eval(*block, loc, gcc_callx_ptr(env->ctx, loc, gcc_load_fn));

    int32_t stride = imports.stride ? imports.stride : 1;
    for (int32_t i = 0; i < imports.len; ++i) {
        import_t import = imports.imports[i*stride];
        // printf("Importing: %s : %s = %s\n", import.name, import.signature, import.symbol);
        bl_file_t *type_file = bl_spoof_file(path, import.signature);
        ast_t *type_ast = parse_type(type_file, env->on_err);
        bl_type_t *t = parse_type_ast(env, type_ast);
        if (t->tag == TypeType) {
            // TODO: support importing types
        }
        // printf("Parsed type into: %s\n", type_to_string(t));
        gcc_type_t *gcc_t = bl_type_to_gcc(env, t);

        const char* sym_name = heap_strf(import.name);
        gcc_lvalue_t *lval = gcc_global(env->ctx, loc, GCC_GLOBAL_EXPORTED, gcc_t, sym_name);
        gcc_rvalue_t *rval = gcc_callx(env->ctx, loc, dlsym_fn, gcc_rval(handle), gcc_str(env->ctx, import.symbol));
        if (import.needs_deref)
            rval = gcc_rval(gcc_rvalue_dereference(gcc_cast(env->ctx, loc, rval, bl_type_to_gcc(env, Type(PointerType, .pointed=t))), loc));
        else
            rval = gcc_cast(env->ctx, loc, rval, gcc_t);
        gcc_assign(*block, loc, lval, rval);

        bl_hashmap_t *ns = get_qualified_ns(env, import.name);
        assert(ns);

        const char *name = strrchr(import.name, '.');
        if (name) ++name;
        else name = import.name;
        hset(ns, name, new(binding_t, .type=t, .rval=gcc_rval(lval), .sym_name=sym_name));
    }
    // dlclose(dl_handle);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
