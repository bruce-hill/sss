#include <assert.h>
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
    CORD *init_code;
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
//__attribute__ ((format (printf, 2, 3)))
static inline void addf(CORD *code, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    CORD tmp;
    CORD_vsprintf(&tmp, fmt, args);
    *code = CORD_cat(*code, tmp);
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

CORD fresh_label(code_t *code, const char *suggestion) {
    CORD reg;
    CORD_sprintf(&reg, "@%s.%ld", suggestion, (*code->next_id)++);
    return reg;
}

static CORD add_fncall(code_t *code, ast_t *ast, bool give_register)
{
    // Check type:
    (void)get_type(code->f, code->bindings, ast);
    CORD fn_reg = add_value(code, ast->call.fn);
    NEW_LIST(CORD, arg_regs);
    for (int64_t i = 0; i < LIST_LEN(ast->call.args); i++) {
        CORD reg = add_value(code, LIST_ITEM(ast->call.args, i));
        APPEND(arg_regs, reg);
    }
    CORD ret = give_register ? fresh_local(code, "ret") : NULL;
    if (give_register)
        addf(&code->code, "%r =%r ", ret, get_base_type(code->f, code->bindings, ast));
    addf(&code->code, "call %r(", fn_reg);
    for (int64_t i = 0; i < LIST_LEN(arg_regs); i++) {
        if (i > 0) addf(&code->code, ", ");
        addf(&code->code, "%s %r", get_base_type(code->f, code->bindings, LIST_ITEM(ast->call.args, i)), LIST_ITEM(arg_regs, i));
    }
    // TODO: default args
    addf(&code->code, ")\n");
    return ret;
}

CORD get_string_reg(code_t *code, const char *str)
{
    str = intern_str(str);
    CORD reg = hashmap_get(code->string_regs, str);
    if (reg) return reg;

    reg = fresh_global(code, "string");
    addf(code->data_code, "data %r = {", reg);
    bool needs_comma = false;
    for (const char *p = str; *p; ) {
        size_t span_len = strcspn(p, "\"\\\n");
        if (needs_comma) addf(code->data_code, ", ");
        if (span_len > 0) {
            addf(code->data_code, "b\"%.*s\"", (int)span_len, p);
            p += span_len;
        } else {
            addf(code->data_code, "b %d", (int)*p);
            ++p;
        }
        needs_comma = true;
    }
    addf(code->data_code, ", b 0}\n");
    hashmap_set(code->string_regs, str, reg);

    return reg;
}

CORD get_tostring_reg(code_t *code, bl_type_t *t)
{
    CORD fn_reg = hashmap_get(code->tostring_regs, t);
    if (fn_reg) return fn_reg;
    fn_reg = fresh_global(code, "tostring");
    CORD val = fresh_local(code, "val");
    CORD rec = fresh_local(code, "rec");
    add_line(code->fn_code, "function l %r(l %r, l %r) {", fn_reg, val, rec);
    add_line(code->fn_code, "@start");
    switch (t->kind) {
    case Int: {
        CORD ret = fresh_local(code, "str");
        add_line(code->fn_code, "%r =l alloc8 8", ret);
        add_line(code->fn_code, "call $CORD_sprintf(l %r, l %r, l %r, ...)", ret, get_string_reg(code, "%ld"), val);
        add_line(code->fn_code, "%r =l loadl %r", ret, ret);
        add_line(code->fn_code, "ret %r", ret);
        break;
    }
    default: {
        fprintf(stderr, "\x1b[31;1mtostring(%s) function is not yet implemented!\n", type_to_string(t));
        exit(1);
    }
    }
    add_line(code->fn_code, "}");
    return fn_reg;
}

CORD add_value(code_t *code, ast_t *ast) {
    switch (ast->kind) {
    case Var: {
        binding_t *binding = hashmap_get(code->bindings, ast->str);
        if (binding) {
            if (binding->reg[0] == '*') {
                CORD tmp = fresh_local(code, ast->str);
                add_line(&code->code, "%r =%r load%s %r", tmp, "l", "l", CORD_substr(binding->reg, 1, -1));
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
    case StringLiteral: return get_string_reg(code, ast->str);
    case StringJoin: {
        CORD ret = fresh_local(code, "str");
        add_line(&code->code, "%r =l copy 0", ret);
        for (int64_t i = 0; i < LIST_LEN(ast->children); i++) {
            ast_t *chunk = LIST_ITEM(ast->children, i);
            CORD chunk_reg = add_value(code, chunk);
            bl_type_t *chunk_t = get_type(code->f, code->bindings, chunk);
            if (chunk_t == Type(StringType)) {
                add_line(&code->code, "%r =l call $CORD_cat(l %r, l %r)", ret, ret, chunk_reg);
            } else {
                CORD chunk_str = fresh_local(code, "str");
                add_line(&code->code, "%r =l call %r(l %r, l 0)", chunk_str, get_tostring_reg(code, chunk_t), chunk_reg);
                add_line(&code->code, "%r =l call $CORD_cat(l %r, l %r)", ret, ret, chunk_str);
            }
        }
        add_line(&code->code, "%r =l call $intern_str(l %r)", ret, ret);
        return ret;
    }
    case FunctionCall: return add_fncall(code, ast, true);
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
    check_discardable(code->f, code->bindings, ast);
    if (ast->kind == FunctionCall)
        (void)add_fncall(code, ast, false);
    else
        (void)add_value(code, ast);
}

const char *compile_file(file_t *f, ast_t *ast) {
    int64_t next_id = 0;
    CORD data_code = NULL;
    CORD fn_code = NULL;
    CORD init_code = NULL;
    code_t main_code = {
        .f=f,
        .next_id=&next_id,
        .data_code=&data_code,
        .fn_code=&fn_code,
        .init_code=&init_code,
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
    add_line(&main_code.code, "export function w $main() {");
    add_line(&main_code.code, "@start");
    assert(ast->kind == Block);
    for (int64_t i = 0; i < LIST_LEN(ast->children); i++) {
        ast_t *stmt = LIST_ITEM(ast->children, i);
        add_statement(&main_code, stmt);
    }
    add_line(&main_code.code, "  ret 0");
    add_line(&main_code.code, "}");
    CORD all_code = CORD_cat(data_code, fn_code);
    all_code = CORD_cat(all_code, main_code.code);
    return CORD_to_char_star(all_code);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
