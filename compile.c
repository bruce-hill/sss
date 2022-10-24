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

file_t *file = NULL;

typedef struct {
    int64_t *next_id;
    CORD *data_code;
    CORD *fn_code;
    CORD *init_code;
    CORD *code;
    hashmap_t *string_regs;
    hashmap_t *tostring_regs;
    hashmap_t *function_regs;
    hashmap_t *bindings;
    hashmap_t *var_types;
} code_t;

#define ERROR(m, fmt, ...) { fprintf(stderr, "\x1b[31;7;1m" fmt "\x1b[m\n\n" __VA_OPT__(,) __VA_ARGS__); \
            highlight_match(file, m); \
            exit(1); }

code_t *with_var(code_t *code, istr_t varname, CORD reg, bl_type_t *type) {
    size_t depth = 0;
    for (hashmap_t *h = code->bindings; h; h = h->fallback)
        ++depth;

    binding_t *binding = new(binding_t);
    binding->reg = reg;
    binding->type = type;
    if (depth > 10) {
        hashmap_t *merged = hashmap_new();
        for (hashmap_t *h = code->bindings; h; h = h->fallback) {
            for (const istr_t *key = hashmap_next(h, NULL); key; key = hashmap_next(h, key)) {
                hashmap_set(merged, key, hashmap_get(h, key));
            }
        }
        hashmap_set(merged, varname, binding);
        code_t *copy = new(code_t);
        memcpy(copy, code, sizeof(code_t));
        copy->bindings = merged;
        return copy;
    } else {
        hashmap_t *chained = hashmap_new();
        chained->fallback = code->bindings;
        hashmap_set(chained, varname, binding);
        code_t *copy = new(code_t);
        memcpy(copy, code, sizeof(code_t));
        copy->bindings = chained;
        return copy;
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
    bl_type_t *ret_type = get_type(file, code->bindings, ast);
    CORD fn_reg = add_value(code, ast->call.fn);
    NEW_LIST(CORD, arg_regs);
    for (int64_t i = 0; i < LIST_LEN(ast->call.args); i++) {
        CORD reg = add_value(code, LIST_ITEM(ast->call.args, i));
        APPEND(arg_regs, reg);
    }
    CORD ret = give_register ? fresh_local(code, "ret") : NULL;
    if (give_register)
        addf(code->code, "%r =%c ", ret, base_type_for(ret_type));
    addf(code->code, "call %r(", fn_reg);
    for (int64_t i = 0; i < LIST_LEN(arg_regs); i++) {
        if (i > 0) addf(code->code, ", ");
        ast_t *arg = LIST_ITEM(ast->call.args, i);
        bl_type_t *arg_type = get_type(file, code->bindings, arg);
        addf(code->code, "%c %r", base_type_for(arg_type), LIST_ITEM(arg_regs, i));
    }
    // TODO: default args
    addf(code->code, ")\n");
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

static void check_nil(code_t *code, CORD *which, bl_type_t *t, CORD val_reg, CORD nil_label, CORD nonnil_label)
{
    if (t->kind != OptionalType) {
        add_line(which, "jmp %r", nonnil_label);
        return;
    }
    
    CORD is_nil = fresh_local(code, "is_nil");
    CORD nil_reg = fresh_local(code, "nil");
    char base = base_type_for(t);
    add_line(which, "%r =%c copy %s", nil_reg, base, nil_value(t));
    switch (base) {
    case 'd': case 's': {
        CORD nil_bits = fresh_local(code, "nil_bits"), val_bits = fresh_local(code, "optional_bits");
        add_line(which, "%r =l cast %r", val_bits, val_reg);
        add_line(which, "%r =l cast %r", nil_bits, nil_reg);
        add_line(which, "%r =w ceql %r, %r", is_nil, base, val_bits, nil_bits);
        add_line(which, "jnz %r, %r, %r", is_nil, nil_label, nonnil_label);
        return;
    }
    default: {
        add_line(which, "%r =w ceq%c %r, %r", is_nil, base, val_reg, nil_reg);
        add_line(which, "jnz %r, %r, %r", is_nil, nil_label, nonnil_label);
        return;
    }
    }
}

CORD get_tostring_reg(code_t *code, bl_type_t *t)
{
    CORD fn_reg = hashmap_get(code->tostring_regs, t);
    if (fn_reg) return fn_reg;
    fn_reg = fresh_global(code, "tostring");
    CORD val = fresh_local(code, "val");
    CORD rec = fresh_local(code, "rec");
    add_line(code->fn_code, "function l %r(%c %r, l %r) {", fn_reg, base_type_for(t), val, rec);
    add_line(code->fn_code, "@start");
    switch (t->kind) {
    case NilType: {
        add_line(code->fn_code, "ret %r", get_string_reg(code, "(nil)"));
        break;
    }
    case BoolType: {
        CORD yes_label = fresh_label(code, "yes");
        CORD no_label = fresh_label(code, "no");
        add_line(code->fn_code, "jnz %r, %r, %r", val, yes_label, no_label);
        add_line(code->fn_code, "%r", yes_label);
        add_line(code->fn_code, "ret %r", get_string_reg(code, "yes"));
        add_line(code->fn_code, "%r", no_label);
        add_line(code->fn_code, "ret %r", get_string_reg(code, "no"));
        break;
    }
    case IntType: case Int32Type: case Int16Type: case Int8Type: case NumType: case Num32Type: {
        const char *fmt;
        switch (t->kind) {
        case Int32Type: case Int16Type: case Int8Type: fmt = "%d"; break;
        case NumType: case Num32Type: fmt = "%g"; break;
        default: fmt = "%ld"; break;
        }
        CORD ret = fresh_local(code, "str");
        add_line(code->fn_code, "%r =l alloc8 8", ret);
        add_line(code->fn_code, "call $CORD_sprintf(l %r, l %r, %c %r, ...)", ret, get_string_reg(code, fmt), base_type_for(t), val);
        add_line(code->fn_code, "%r =l loadl %r", ret, ret);
        add_line(code->fn_code, "ret %r", ret);
        break;
    }
    case OptionalType: {
        CORD ifnil = fresh_label(code, "is_nil"), ifnonnil = fresh_label(code, "is_not_nil");
        check_nil(code, code->fn_code, t, val, ifnil, ifnonnil);
        add_line(code->fn_code, "%r", ifnil);
        add_line(code->fn_code, "ret %r", get_string_reg(code, "(nil)"));
        add_line(code->fn_code, "%r", ifnonnil);
        CORD ret = fresh_local(code, "ret");
        char nonnil_base = base_type_for(t->nonnil);
        CORD val2;
        if (base_type_for(t) == nonnil_base) {
            val2 = val;
        } else {
            val2 = fresh_local(code, "nonnil");
            add_line(code->fn_code, "%r =%c stosi %r", val2, nonnil_base, val);
        }
        if (t->nonnil->kind == StringType) {
            add_line(code->fn_code, "ret %r", val2);
        } else {
            add_line(code->fn_code, "%r =l call %r(%c %r, l 0)", ret, get_tostring_reg(code, t->nonnil), nonnil_base, val2);
            add_line(code->fn_code, "ret %r", ret);
        }
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
                add_line(code->code, "%r =%r load%s %r", tmp, "l", "l", CORD_substr(binding->reg, 1, -1));
                return tmp;
            } else {
                return binding->reg;
            }
        } else {
            ERROR(ast->match, "Error: variable is not defined"); 
        }
    }
    case Declare: {
        binding_t *binding = hashmap_get(code->bindings, ast->lhs->str);
        if (!binding) errx(1, "Failed to find declaration binding");
        CORD val_reg = add_value(code, ast->rhs);
        add_line(code->code, "%s =%c copy %s", binding->reg, base_type_for(binding->type), val_reg);
        return binding->reg;
    }
    case Assign: {
        int64_t len = LIST_LEN(ast->multiassign.lhs);
        NEW_LIST(CORD, lhs_regs);
        for (int64_t i = 0; i < len; i++) {
            CORD reg = add_value(code, LIST_ITEM(ast->multiassign.lhs, i));
            APPEND(lhs_regs, reg);
        }
        for (int64_t i = 0; i < len; i++) {
            CORD val_reg = add_value(code, LIST_ITEM(ast->multiassign.rhs, i));
            bl_type_t *t_lhs = get_type(file, code->bindings, LIST_ITEM(ast->multiassign.lhs, i));
            bl_type_t *t_rhs = get_type(file, code->bindings, LIST_ITEM(ast->multiassign.rhs, i));
            if (!type_is_a(t_rhs, t_lhs)) {
                ERROR(LIST_ITEM(ast->multiassign.rhs, i)->match, "This value is a %s, but it needs to be a %s",
                      type_to_string(t_rhs), type_to_string(t_lhs));
            }
            add_line(code->code, "%s =%c copy %s", LIST_ITEM(lhs_regs, i), base_type_for(t_lhs), val_reg);
        }
        return "0";
    }
    case Block: {
        for (int64_t i = 0; i < LIST_LEN(ast->children); i++) {
            ast_t *stmt = LIST_ITEM(ast->children, i);
            if (stmt->kind == Declare) {
                CORD reg = fresh_local(code, stmt->lhs->str);
                bl_type_t *t = get_type(file, code->bindings, stmt->rhs);
                code = with_var(code, stmt->lhs->str, reg, t);
            }
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
        add_line(code->code, "%r =l copy 0", ret);
        for (int64_t i = 0; i < LIST_LEN(ast->children); i++) {
            ast_t *chunk = LIST_ITEM(ast->children, i);
            CORD chunk_reg = add_value(code, chunk);
            bl_type_t *chunk_t = get_type(file, code->bindings, chunk);
            if (chunk_t == Type(StringType)) {
                add_line(code->code, "%r =l call $CORD_cat(l %r, l %r)", ret, ret, chunk_reg);
            } else {
                CORD chunk_str = fresh_local(code, "str");
                add_line(code->code, "%r =l call %r(%c %r, l 0)", chunk_str, get_tostring_reg(code, chunk_t), base_type_for(chunk_t), chunk_reg);
                add_line(code->code, "%r =l call $CORD_cat(l %r, l %r)", ret, ret, chunk_str);
            }
        }
        add_line(code->code, "%r =l call $intern_str(l %r)", ret, ret);
        return ret;
    }
    case FunctionCall: return add_fncall(code, ast, true);
    case KeywordArg: {
        return add_value(code, ast->named.value);
    }
    case Bool: {
        return ast->b ? "1" : "0";
    }
    case Cast: {
        bl_type_t *raw_t = get_type(file, code->bindings, ast->expr);
        bl_type_t *cast_t = get_type(file, code->bindings, ast->type);
        CORD raw_reg = add_value(code, ast->expr);
        char raw_base = base_type_for(raw_t);
        char cast_base = base_type_for(cast_t);
        if (raw_base == cast_base)
            return raw_reg;
        CORD cast_reg = fresh_local(code, "cast");
        if (raw_base == 's' && cast_base == 'd')
            add_line(code->code, "%r =d exts %r", cast_reg, raw_reg);
        else if (raw_base == 'd' && cast_base == 's')
            add_line(code->code, "%r =s truncd %r", cast_reg, raw_reg);
        else if (raw_base == 's' || raw_base == 'd')
            add_line(code->code, "%r =%c %ctosi %r", cast_reg, cast_base, raw_base, raw_reg);
        else if (cast_base == 's' || cast_base == 'd')
            add_line(code->code, "%r =%c s%ctof %r", cast_reg, cast_base, raw_base, raw_reg);
        else if (raw_base == 'w' && cast_base == 'l')
            add_line(code->code, "%r =%c exts%c %r", cast_reg, cast_base, abi_type_for(raw_t), raw_reg);
        else if (raw_base == 'l' && cast_base == 'w')
            add_line(code->code, "%r =%c copy %r", cast_reg, cast_base, raw_reg);
        return cast_reg;
    }
    case Nil: {
        // TODO: different nil type values
        return "0";
    }
    default: {
        ERROR(ast->match, "Error: compiling is not yet implemented for %s", get_ast_kind_name(ast->kind)); 
    }
    }
}

void add_statement(code_t *code, ast_t *ast) {
    check_discardable(file, code->bindings, ast);
    if (ast->kind == FunctionCall)
        (void)add_fncall(code, ast, false);
    else
        (void)add_value(code, ast);
}

const char *compile_file(file_t *f, ast_t *ast) {
    file = f;
    int64_t next_id = 0;
    CORD data_code = NULL;
    CORD fn_code = NULL;
    CORD init_code = NULL;
    CORD main_code = NULL;
    code_t codes = {
        .next_id=&next_id,
        .data_code=&data_code,
        .fn_code=&fn_code,
        .init_code=&init_code,
        .code=&main_code,
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

    codes = *with_var(&codes, intern_str("say"), "$puts", say_type);
#define DEFTYPE(t) codes = *with_var(&codes, intern_str(#t), get_string_reg(&codes, #t), Type(TypeType, .type=Type(t##Type)))
    // Primitive types:
    DEFTYPE(Bool); DEFTYPE(Nil); DEFTYPE(Abort);
    DEFTYPE(Int); DEFTYPE(Int32); DEFTYPE(Int16); DEFTYPE(Int8);
    DEFTYPE(Num); DEFTYPE(Num32);
    DEFTYPE(String);
#undef DEFTYPE

    add_line(codes.code, "export function w $main() {");
    add_line(codes.code, "@start");
    assert(ast->kind == Block);
    add_statement(&codes, ast);
    // for (int64_t i = 0; i < LIST_LEN(ast->children); i++) {
    //     ast_t *stmt = LIST_ITEM(ast->children, i);
    //     add_statement(&codes, stmt);
    // }
    add_line(codes.code, "  ret 0");
    add_line(codes.code, "}");
    CORD all_code = CORD_cat(data_code, fn_code);
    all_code = CORD_cat(all_code, main_code);
    file = NULL;
    return CORD_to_char_star(all_code);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
