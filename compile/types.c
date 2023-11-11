// Compilation logic for Arrays in this file
#include <assert.h>
#include <gc/cord.h>
#include <libgccjit.h>
#include <ctype.h>
#include <err.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>

#include "../ast.h"
#include "compile.h"
#include "libgccjit_abbrev.h"
#include "../typecheck.h"
#include "../types.h"
#include "../util.h"

gcc_rvalue_t *get_type_rvalue(env_t *env, sss_type_t *t)
{
    switch (t->tag) {
        case BoolType: return get_binding(env, "Bool")->rval;
        case CharType: return get_binding(env, "Char")->rval;
        case IntType: {
            auto info = Match(t, IntType);
            CORD name = info->is_unsigned ? "UInt" : "Int";
            if (info->bits != 64)
                CORD_sprintf(&name, "%r%d", info->bits);
            return get_binding(env, CORD_to_const_char_star(name))->rval;
        }
        case NumType: {
            auto info = Match(t, NumType);
            const char *name = info->bits == 64 ? "Num" : "Num32";
            return get_binding(env, name)->rval;
        }
        default: errx(1, "Not implemented");
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
