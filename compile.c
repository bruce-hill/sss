#include <bhash.h>
#include <err.h>
#include <gc/cord.h>
#include <stdarg.h>
#include <stdint.h>

#include "ast.h"
#include "typecheck.h"
#include "types.h"
#include "util.h"

typedef struct {
    file_t *f;
    int64_t *next_id;
    CORD *data_code;
    CORD *fn_code;
    CORD code;
    hashmap_t *string_regs;
    hashmap_t *tostring_regs;
    hashmap_t *function_regs;
    hashmap_t *bindings;
    hashmap_t *var_types;
} code_t;

hashmap_t *with_var(hashmap_t *bindings, istr_t varname, CORD reg, bl_type_t *type) {
    size_t depth = 0;
    for (hashmap_t *h = bindings; h; h = h->fallback)
        ++depth;

    binding_t *binding = new(binding_t);
    binding->reg = reg;
    binding->type = type;
    if (depth > 10) {
        hashmap_t *merged = hashmap_new();
        for (hashmap_t *h = bindings; h; h = h->fallback) {
            for (const istr_t *key = hashmap_next(h, NULL); key; key = hashmap_next(h, key)) {
                hashmap_set(merged, key, hashmap_get(h, key));
            }
        }
        hashmap_set(merged, varname, binding);
        return merged;
    } else {
        hashmap_t *chained = hashmap_new();
        chained->fallback = bindings;
        hashmap_set(chained, varname, binding);
        return chained;
    }
}

static CORD add_value(code_t *code, ast_t *ast);
static void add_statement(code_t *code, ast_t *ast);
__attribute__ ((format (printf, 2, 3)))
static inline void addf(code_t *code, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    CORD tmp;
    CORD_vsprintf(&tmp, fmt, args);
    code->code = CORD_cat(code->code, tmp);
    va_end(args);
}
#define add_line(code, fmt, ...) addf(code, fmt "\n" __VA_OPT__(,) __VA_ARGS__)

CORD fresh_local(code_t *code, const char *suggestion) {
    CORD reg;
    CORD_sprintf(&reg, "%%%s.%ld", suggestion, (*code->next_id)++);
    return reg;
}

CORD fresh_global(code_t *code, const char *suggestion) {
    CORD reg;
    CORD_sprintf(&reg, "$%s.%ld", suggestion, (*code->next_id)++);
    return reg;
}

CORD fresh_lable(code_t *code, const char *suggestion) {
    CORD reg;
    CORD_sprintf(&reg, "@%s.%ld", suggestion, (*code->next_id)++);
    return reg;
}

CORD add_value(code_t *code, ast_t *ast) {
    switch (ast->kind) {
    case Var: {
        binding_t *binding = hashmap_get(code->bindings, ast->str);
        if (binding) {
            if (binding->reg[0] == '*') {
                CORD tmp = fresh_local(code, ast->str);
                add_line(code, "%s =%s load%s %s", tmp, "l", "l", CORD_substr(binding->reg, 1, -1));
                return tmp;
            } else {
                return binding->reg;
            }
        } else {
            fprintf(stderr, "\x1b[31;7;1mError: variable is not defined\x1b[m\n\n"); 
            highlight_match(code->f, ast->match);
            exit(1);
        }
    }
    case Block: {
        for (int64_t i = 0; i < LIST_LEN(ast->children); i++) {
            ast_t *stmt = LIST_ITEM(ast->children, i);
            if (i == LIST_LEN(ast->children)-1)
                return add_value(code, stmt);
            else
                add_statement(code, stmt);
        }
        errx(1, "Unreachable");
    }
    case Int: {
        CORD ret;
        CORD_sprintf(&ret, "%ld", ast->i);
        return ret;
    }
    case Num: {
        CORD ret;
        CORD_sprintf(&ret, "d_%f", ast->n);
        return ret;
    }
    case StringLiteral: {
        return "$str";
    }
    case StringJoin: {
        CORD ret = fresh_local(code, "str");
        add_line(code, "%s =l copy 0", ret);
        for (int64_t i = 0; i < LIST_LEN(ast->children); i++) {
            CORD chunk = add_value(code, LIST_ITEM(ast->children, i));
            add_line(code, "%s =l call $CORD_cat(l %s, l %s)", ret, ret, chunk);
        }
        return ret;
    }
    case FunctionCall: {
        // Check type:
        (void)get_type(code->f, code->bindings, ast);
        CORD fn_reg = add_value(code, ast->call.fn);
        NEW_LIST(CORD, arg_regs);
        for (int64_t i = 0; i < LIST_LEN(ast->call.args); i++) {
            CORD reg = add_value(code, LIST_ITEM(ast->call.args, i));
            APPEND(arg_regs, reg);
        }
        CORD ret = fresh_local(code, "ret");
        addf(code, "%s =%s call %s(", ret, "l", fn_reg);
        for (int64_t i = 0; i < LIST_LEN(arg_regs); i++) {
            if (i > 0) addf(code, ", ");
            addf(code, "%s %s", get_base_type(code->f, code->bindings, LIST_ITEM(ast->call.args, i)), LIST_ITEM(arg_regs, i));
        }
        addf(code, ")\n");
        return ret;
    }
    case KeywordArg: {
        return add_value(code, ast->named.value);
    }
    case Bool: {
        return ast->b ? "1" : "0";
    }
    default: {
        fprintf(stderr, "\x1b[31;7;1mError: compiling is not yet implemented for %s\x1b[m\n\n", get_ast_kind_name(ast->kind)); 
        highlight_match(code->f, ast->match);
        exit(1);
    }
    }
}

void add_statement(code_t *code, ast_t *ast) {
    (void)add_value(code, ast);
}

const char *compile_file(file_t *f, ast_t *ast) {
    int64_t next_id = 0;
    CORD data_code = NULL;
    CORD fn_code = NULL;
    code_t main_code = {
        .f=f,
        .next_id=&next_id,
        .data_code=&data_code,
        .fn_code=&fn_code,
        .code=NULL,
        .string_regs=hashmap_new(),
        .tostring_regs=hashmap_new(),
        .function_regs=hashmap_new(),
        .bindings=hashmap_new(),
    };
    
    bl_type_t *string_type = Type(StringType);
    bl_type_t *nil_type = Type(NilType);
    bl_type_t *say_type = Type(
        FunctionType,
        .args=LIST(bl_type_t*, string_type, Type(OptionalType, .nonnil=string_type)),
        .ret=nil_type);

    main_code.bindings = with_var(main_code.bindings, intern_str("say"), "$puts", say_type);
    add_line(&main_code, "export function w $main() {");
    add_line(&main_code, "@start");
    add_statement(&main_code, ast);
    add_line(&main_code, "  ret 0");
    add_line(&main_code, "}");
    CORD all_code = CORD_cat(data_code, fn_code);
    all_code = CORD_cat(all_code, main_code.code);
    return CORD_to_char_star(all_code);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
