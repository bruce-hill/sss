#include <assert.h>
#include <bhash.h>
#include <bp/files.h>
#include <ctype.h>
#include <err.h>
#include <gc/cord.h>
#include <stdarg.h>
#include <stdint.h>

#include "ast.h"
#include "typecheck.h"
#include "types.h"
#include "util.h"

typedef struct loop_label_s {
    struct loop_label_s *enclosing;
    CORD skip_label, stop_label;
    istr_t name;
} loop_label_t;

typedef struct {
    file_t *file;
    int64_t *next_id;
    CORD *data_code;
    CORD *fn_code;
    CORD *init_code;
    hashmap_t *string_regs;
    hashmap_t *tostring_regs;
    hashmap_t *function_regs;
    hashmap_t *bindings;
    hashmap_t *var_types;
    loop_label_t *loop_label;
    bool debug;
} env_t;

#define ERROR(env, ast, fmt, ...) { fprintf(stderr, "\x1b[31;7;1m" fmt "\x1b[m\n\n" __VA_OPT__(,) __VA_ARGS__); \
            highlight_match(stderr, env->file, (ast)->match); \
            exit(1); }

#define foreach LIST_FOR
#define length LIST_LEN
#define ith LIST_ITEM
#define append APPEND

hashmap_t *global_bindings(hashmap_t *bindings)
{
    hashmap_t *globals = hashmap_new();
    for (hashmap_t *h = bindings; h; h = h->fallback) {
        for (istr_t key = NULL; (key = hashmap_next(h, key)); ) {
            binding_t *val = hashmap_get_raw(h, key);
            assert(val);
            if (val->is_global)
                hashmap_set(globals, key, val);
        }
    }
    return globals;
}

static CORD add_value(env_t *env, CORD *code, ast_t *ast);
static void add_statement(env_t *env, CORD *code, ast_t *ast);
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

CORD fresh_local(env_t *env, const char *suggestion) {
    CORD reg;
    CORD_sprintf(&reg, "%%%s.%ld", suggestion, (*env->next_id)++);
    return reg;
}

CORD fresh_global(env_t *env, const char *suggestion) {
    CORD reg;
    CORD_sprintf(&reg, "$%s.%ld", suggestion, (*env->next_id)++);
    return reg;
}

CORD fresh_label(env_t *env, const char *suggestion) {
    CORD reg;
    CORD_sprintf(&reg, "@%s.%ld", suggestion, (*env->next_id)++);
    return reg;
}

static CORD add_fncall(env_t *env, CORD *code, ast_t *ast, bool give_register)
{
    bl_type_t *ret_type = get_type(env->file, env->bindings, ast);
    CORD fn_reg = add_value(env, code, ast->call.fn);
    NEW_LIST(CORD, arg_regs);
    foreach (ast->call.args, arg, _) {
        CORD reg = add_value(env, code, *arg);
        append(arg_regs, reg);
    }
    CORD ret = give_register ? fresh_local(env, "ret") : NULL;
    if (give_register)
        addf(code, "%r =%c ", ret, base_type_for(ret_type));
    addf(code, "call %r(", fn_reg);
    for (int64_t i = 0; i < length(arg_regs); i++) {
        if (i > 0) addf(code, ", ");
        ast_t *arg = ith(ast->call.args, i);
        bl_type_t *arg_type = get_type(env->file, env->bindings, arg);
        addf(code, "%c %r", base_type_for(arg_type), ith(arg_regs, i));
    }
    // TODO: default args
    addf(code, ")\n");
    return ret;
}

CORD get_string_reg(env_t *env, const char *str)
{
    str = intern_str(str);
    CORD reg = hashmap_get(env->string_regs, str);
    if (reg) return reg;

    reg = fresh_global(env, "string");
    addf(env->data_code, "data %r = {", reg);
    bool needs_comma = false;
    for (const char *p = str; *p; ) {
        if (isprint(*p) && *p != '\n' && *p != '"' && *p != '\\') {
            if (needs_comma) addf(env->data_code, ", ");
            addf(env->data_code, "b\"");
            for ( ; *p && isprint(*p) && *p != '\n' && *p != '"' && *p != '\\'; p++)
                addf(env->data_code, "%c", *p);
            addf(env->data_code, "\"");
            needs_comma = true;
        } else {
            for (; *p && !(isprint(*p) && *p != '\n' && *p != '"' && *p != '\\'); p++) {
                if (needs_comma) addf(env->data_code, ", ");
                addf(env->data_code, "b %d", *p);
                needs_comma = true;
            }
        }
    }
    addf(env->data_code, ", b 0}\n");
    hashmap_set(env->string_regs, str, reg);

    return reg;
}

static void check_nil(env_t *env, CORD *code, bl_type_t *t, CORD val_reg, CORD nil_label, CORD nonnil_label)
{
    if (t->kind != OptionalType) {
        add_line(code, "jmp %r", nonnil_label);
        return;
    }
    
    CORD is_nil = fresh_local(env, "is_nil");
    CORD nil_reg = fresh_local(env, "nil");
    char base = base_type_for(t);
    add_line(code, "%r =%c copy %s", nil_reg, base, nil_value(t));
    switch (base) {
    case 'd': case 's': {
        CORD nil_bits = fresh_local(env, "nil_bits"), val_bits = fresh_local(env, "optional_bits");
        add_line(code, "%r =l cast %r", val_bits, val_reg);
        add_line(code, "%r =l cast %r", nil_bits, nil_reg);
        add_line(code, "%r =w ceql %r, %r", is_nil, base, val_bits, nil_bits);
        add_line(code, "jnz %r, %r, %r", is_nil, nil_label, nonnil_label);
        return;
    }
    default: {
        add_line(code, "%r =w ceq%c %r, %r", is_nil, base, val_reg, nil_reg);
        add_line(code, "jnz %r, %r, %r", is_nil, nil_label, nonnil_label);
        return;
    }
    }
}

static CORD check_truthiness(env_t *env, CORD *code, ast_t *val, CORD truthy_label, CORD falsey_label)
{
    bl_type_t *t = get_type(env->file, env->bindings, val);
    add_line(code, "# Check truthiness of:");
    CORD val_reg = add_value(env, code, val);
    if (t->kind == OptionalType) {
        check_nil(env, code, t, val_reg, falsey_label, truthy_label);
        return val_reg;
    }

    if (t->kind != BoolType)
        ERROR(env, val, "This value has type %s, which means it will always be truthy", type_to_string(t)); 
    
    add_line(code, "jnz %r, %r, %r", val_reg, truthy_label, falsey_label);
    return val_reg;
}

CORD get_tostring_reg(env_t *env, bl_type_t *t)
{
    CORD fn_reg = hashmap_get(env->tostring_regs, t);
    if (fn_reg) return fn_reg;
    fn_reg = fresh_global(env, "tostring");
    CORD val = fresh_local(env, "val");
    CORD rec = fresh_local(env, "rec");
    add_line(env->fn_code, "# %s", type_to_string(t));
    add_line(env->fn_code, "function l %r(%c %r, l %r) {", fn_reg, base_type_for(t), val, rec);
    add_line(env->fn_code, "@start");
    switch (t->kind) {
    case NilType: {
        add_line(env->fn_code, "ret %r", get_string_reg(env, "(nil)"));
        break;
    }
    case BoolType: {
        CORD yes_label = fresh_label(env, "yes");
        CORD no_label = fresh_label(env, "no");
        add_line(env->fn_code, "jnz %r, %r, %r", val, yes_label, no_label);
        add_line(env->fn_code, "%r", yes_label);
        add_line(env->fn_code, "ret %r", get_string_reg(env, "yes"));
        add_line(env->fn_code, "%r", no_label);
        add_line(env->fn_code, "ret %r", get_string_reg(env, "no"));
        break;
    }
    case IntType: case Int32Type: case Int16Type: case Int8Type: case NumType: case Num32Type: {
        const char *fmt;
        switch (t->kind) {
        case Int32Type: case Int16Type: case Int8Type: fmt = "%d"; break;
        case NumType: case Num32Type: fmt = "%g"; break;
        default: fmt = "%ld"; break;
        }
        CORD ret = fresh_local(env, "str");
        add_line(env->fn_code, "%r =l alloc8 8", ret);
        add_line(env->fn_code, "call $CORD_sprintf(l %r, l %r, %c %r, ...)", ret, get_string_reg(env, fmt), base_type_for(t), val);
        add_line(env->fn_code, "%r =l loadl %r", ret, ret);
        add_line(env->fn_code, "ret %r", ret);
        break;
    }
    case OptionalType: {
        CORD ifnil = fresh_label(env, "is_nil"), ifnonnil = fresh_label(env, "is_not_nil");
        check_nil(env, env->fn_code, t, val, ifnil, ifnonnil);
        add_line(env->fn_code, "%r", ifnil);
        add_line(env->fn_code, "ret %r", get_string_reg(env, "(nil)"));
        add_line(env->fn_code, "%r", ifnonnil);
        CORD ret = fresh_local(env, "ret");
        char nonnil_base = base_type_for(t->nonnil);
        CORD val2;
        if (base_type_for(t) == nonnil_base) {
            val2 = val;
        } else {
            val2 = fresh_local(env, "nonnil");
            add_line(env->fn_code, "%r =%c stosi %r", val2, nonnil_base, val);
        }
        if (t->nonnil->kind == StringType) {
            add_line(env->fn_code, "ret %r", val2);
        } else {
            add_line(env->fn_code, "%r =l call %r(%c %r, l 0)", ret, get_tostring_reg(env, t->nonnil), nonnil_base, val2);
            add_line(env->fn_code, "ret %r", ret);
        }
        break;
    }
    default: {
        fprintf(stderr, "\x1b[31;1mtostring(%s) function is not yet implemented!\n", type_to_string(t));
        exit(1);
    }
    }
    add_line(env->fn_code, "}");
    return fn_reg;
}

static bool ends_with_jump(CORD *code)
{
    size_t len = CORD_len(*code);
    size_t nl_index = CORD_rchr(*code, len-2, '\n');
    return CORD_ncmp(*code, nl_index+1, "ret", 0, 3) == 0
        || CORD_ncmp(*code, nl_index+1, "jmp", 0, 3) == 0
        || CORD_ncmp(*code, nl_index+1, "jnz", 0, 3) == 0;
}

static CORD compile_function(env_t *env, CORD *code, ast_t *def)
{
    binding_t *binding = hashmap_get(env->bindings, def->fn.name);

    env_t body_env = *env;
    // Use a set of bindings that don't include any closures
    body_env.bindings = global_bindings(env->bindings);

    bl_type_t *t;
    CORD fn_reg;
    if (binding) {
        hashmap_set(body_env.bindings, def->fn.name, binding);
        fn_reg = binding->reg;
        t = binding->type;
    } else {
        fn_reg = fresh_global(&body_env, "lambda");
        t = get_type(body_env.file, body_env.bindings, def);
    }
    add_line(code, "# %s", type_to_string(t));
    addf(code, "function %c %r(", base_type_for(t->ret), fn_reg);
    for (int64_t i = 0; i < length(def->fn.arg_names); i++) {
        if (i > 0) addf(code, ", ");
        istr_t argname = ith(def->fn.arg_names, i);
        bl_type_t *argtype = ith(t->args, i);
        CORD argreg = fresh_local(&body_env, argname);
        addf(code, "%c %s", base_type_for(argtype), argreg);
        assert(argtype);
        hashmap_set(body_env.bindings, argname, new(binding_t, .reg=argreg, .type=argtype));
    }
    addf(code, ") {\n@start\n");
    add_statement(&body_env, code, def->fn.body);
    if (!ends_with_jump(code))
        add_line(code, "ret 0");
    add_line(code, "}");
    return fn_reg;
}

static void set_nil(CORD *code, bl_type_t *t, CORD reg)
{
    add_line(code, "%r =%c copy %s", reg, base_type_for(t), nil_value(t));
}

CORD add_clause(env_t *env, CORD *code, ast_t *condition, ast_t *body, CORD falsey_label)
{
    CORD truthy_label = fresh_label(env, "if_true");
    if (condition->kind == Declare) {
        env_t branch_env = *env;
        branch_env.bindings = hashmap_new();
        branch_env.bindings->fallback = env->bindings;
        bl_type_t *t = get_type(env->file, env->bindings, condition);
        assert(t);
        CORD cond_reg = fresh_local(&branch_env, condition->lhs->str);
        binding_t b = {.reg=cond_reg, .type=t};
        hashmap_set(branch_env.bindings, condition->lhs->str, &b);
        check_truthiness(&branch_env, code, condition, truthy_label, falsey_label);
        add_line(code, "%r", truthy_label);
        return add_value(&branch_env, code, body);
    } else {
        (void)check_truthiness(env, code, condition, truthy_label, falsey_label);
        add_line(code, "%r", truthy_label);
        return add_value(env, code, body);
    }
}

CORD add_value(env_t *env, CORD *code, ast_t *ast)
{
    switch (ast->kind) {
    case Var: {
        binding_t *binding = hashmap_get(env->bindings, ast->str);
        if (binding) {
            if (binding->reg[0] == '*') {
                CORD tmp = fresh_local(env, ast->str);
                add_line(code, "%r =%r load%s %r", tmp, "l", "l", CORD_substr(binding->reg, 1, -1));
                return tmp;
            } else {
                return binding->reg;
            }
        } else {
            ERROR(env, ast, "Error: variable is not defined"); 
        }
    }
    case Declare: {
        binding_t *binding = hashmap_get(env->bindings, ast->lhs->str);
        assert(binding);
        if (!binding) errx(1, "Failed to find declaration binding");
        CORD val_reg = add_value(env, code, ast->rhs);
        add_line(code, "%s =%c copy %s", binding->reg, base_type_for(binding->type), val_reg);
        return binding->reg;
    }
    case Assign: {
        int64_t len = length(ast->multiassign.lhs);
        NEW_LIST(CORD, lhs_regs);
        for (int64_t i = 0; i < len; i++) {
            CORD reg = add_value(env, code, ith(ast->multiassign.lhs, i));
            append(lhs_regs, reg);
        }
        for (int64_t i = 0; i < len; i++) {
            CORD val_reg = add_value(env, code, ith(ast->multiassign.rhs, i));
            bl_type_t *t_lhs = get_type(env->file, env->bindings, ith(ast->multiassign.lhs, i));
            bl_type_t *t_rhs = get_type(env->file, env->bindings, ith(ast->multiassign.rhs, i));
            if (!type_is_a(t_rhs, t_lhs)) {
                ERROR(env, ith(ast->multiassign.rhs, i), "This value is a %s, but it needs to be a %s",
                      type_to_string(t_rhs), type_to_string(t_lhs));
            }
            add_line(code, "%s =%c copy %s", ith(lhs_regs, i), base_type_for(t_lhs), val_reg);
        }
        return "0";
    }
    case Block: {
        env_t block_env = *env;
        block_env.bindings = hashmap_new();
        block_env.bindings->fallback = env->bindings;

        // Function defs are visible in the entire block (allowing corecursive funcs)
        foreach (ast->children, stmt, last_stmt) {
            if ((*stmt)->kind == FunctionDef) {
                CORD reg = fresh_global(env, (*stmt)->fn.name);
                bl_type_t *t = get_type(env->file, block_env.bindings, *stmt);
                assert(t);
                hashmap_set(block_env.bindings, (*stmt)->fn.name, new(binding_t, .reg=reg, .type=t, .is_global=true));
            }
        }

        foreach (ast->children, stmt, last_stmt) {
            // Declarations are visible from here onwards:
            if ((*stmt)->kind == Declare) {
                CORD reg = fresh_local(&block_env, (*stmt)->lhs->str);
                bl_type_t *t = get_type(block_env.file, block_env.bindings, (*stmt)->rhs);
                assert(t);
                hashmap_set(block_env.bindings, (*stmt)->lhs->str, new(binding_t, .reg=reg, .type=t));
            }
            if (stmt == last_stmt) {
                if (block_env.debug)
                    add_line(code, "loc %d, %d", 1, get_line_number(block_env.file, (*stmt)->match->start));
                return add_value(&block_env, code, *stmt);
            } else {
                add_statement(&block_env, code, *stmt);
            }
        }
        errx(1, "Unreachable");
    }
    case FunctionDef: case Lambda: {
        CORD fn_code = NULL;
        CORD fn_reg = compile_function(env, &fn_code, ast);
        addf(env->fn_code, "\n%r\n", fn_code);
        return fn_reg;
    }
    case Return: {
        if (ast->child) {
            CORD ret = add_value(env, code, ast->child);
            add_line(code, "ret %r", ret);
        } else {
            add_line(code, "ret");
        }
        add_line(code, "%r", fresh_label(env, "unreachable_after_ret"));
        return "0";
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
    case StringLiteral: return get_string_reg(env, ast->str);
    case StringJoin: {
        CORD ret = fresh_local(env, "str");
        add_line(code, "%r =l copy 0", ret);
        foreach (ast->children, chunk, _) {
            CORD chunk_reg = add_value(env, code, *chunk);
            bl_type_t *chunk_t = get_type(env->file, env->bindings, *chunk);
            if (chunk_t == Type(StringType)) {
                add_line(code, "%r =l call $CORD_cat(l %r, l %r)", ret, ret, chunk_reg);
            } else {
                CORD chunk_str = fresh_local(env, "str");
                add_line(code, "%r =l call %r(%c %r, l 0)", chunk_str, get_tostring_reg(env, chunk_t), base_type_for(chunk_t), chunk_reg);
                add_line(code, "%r =l call $CORD_cat(l %r, l %r)", ret, ret, chunk_str);
            }
        }
        add_line(code, "%r =l call $intern_str(l %r)", ret, ret);
        return ret;
    }
    case FunctionCall: return add_fncall(env, code, ast, true);
    case KeywordArg: {
        return add_value(env, code, ast->named.value);
    }
    case Bool: {
        return ast->b ? "1" : "0";
    }
    case Cast: {
        bl_type_t *raw_t = get_type(env->file, env->bindings, ast->expr);
        bl_type_t *cast_t = get_type(env->file, env->bindings, ast->type);
        CORD raw_reg = add_value(env, code, ast->expr);
        char raw_base = base_type_for(raw_t);
        char cast_base = base_type_for(cast_t);
        if (raw_base == cast_base)
            return raw_reg;
        CORD cast_reg = fresh_local(env, "cast");
        if (raw_base == 's' && cast_base == 'd')
            add_line(code, "%r =d exts %r", cast_reg, raw_reg);
        else if (raw_base == 'd' && cast_base == 's')
            add_line(code, "%r =s truncd %r", cast_reg, raw_reg);
        else if (raw_base == 's' || raw_base == 'd')
            add_line(code, "%r =%c %ctosi %r", cast_reg, cast_base, raw_base, raw_reg);
        else if (cast_base == 's' || cast_base == 'd')
            add_line(code, "%r =%c s%ctof %r", cast_reg, cast_base, raw_base, raw_reg);
        else if (raw_base == 'w' && cast_base == 'l')
            add_line(code, "%r =%c exts%c %r", cast_reg, cast_base, abi_type_for(raw_t), raw_reg);
        else if (raw_base == 'l' && cast_base == 'w')
            add_line(code, "%r =%c copy %r", cast_reg, cast_base, raw_reg);
        return cast_reg;
    }
    case Nil: {
        // TODO: different nil type values
        return "0";
    }
    case Equal: case NotEqual: {
        bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
        bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
        if (!(type_is_a(lhs_t, rhs_t) || type_is_a(rhs_t, lhs_t)))
            ERROR(env, ast, "These two values have incompatible types: %s vs %s", type_to_string(lhs_t), type_to_string(rhs_t));
        CORD lhs_reg = add_value(env, code, ast->lhs);
        CORD rhs_reg = add_value(env, code, ast->rhs);
        add_line(code, "# Not Equals RHS: %r", rhs_reg);
        CORD result = fresh_local(env, "comparison");
        char b = base_type_for(lhs_t);
        add_line(code, "%r =w c%s%c %r, %r", result, ast->kind == Equal ? "eq" : "ne", b, lhs_reg, rhs_reg);
        return result;
    }
    case Less: case LessEqual: case Greater: case GreaterEqual: {
        bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
        bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
        CORD lhs_reg = add_value(env, code, ast->lhs);
        CORD rhs_reg = add_value(env, code, ast->rhs);
        CORD result = fresh_local(env, "comparison");
        const char *cmp = NULL;
        switch (ast->kind) {
        case Less: cmp = "lt"; break;
        case LessEqual: cmp = "le"; break;
        case Greater: cmp = "gt"; break;
        case GreaterEqual: cmp = "ge"; break;
        default: break;
        }
        if (lhs_t == rhs_t && (lhs_t->kind == StringType || lhs_t->kind == DSLType)) {
            add_line(code, "%r =w call $strcmp(l %r, l %r)", result, lhs_reg, rhs_reg);
            add_line(code, "%r =w cs%sw %r, 0", result, cmp, result);
            return result;
        } else if (lhs_t == rhs_t && is_numeric(lhs_t)) {
            char b = base_type_for(lhs_t);
            const char *sign = (b == 's' || b == 'd') ? "" : "s";
            add_line(code, "%r =w c%s%s%c %r, %r", result, sign, cmp, b, lhs_reg, rhs_reg);
            return result;
        }
        ERROR(env, ast, "Ordered comparison is not supported for %s and %s", type_to_string(lhs_t), type_to_string(rhs_t));
    }
    case Add: case Subtract: case Divide: case Multiply: case Modulus: {
        bl_type_t *lhs_t = get_type(env->file, env->bindings, ast->lhs);
        bl_type_t *rhs_t = get_type(env->file, env->bindings, ast->rhs);
        // TODO: support percentages and measures
        if (lhs_t != rhs_t || !is_numeric(lhs_t))
            ERROR(env, ast, "Math operatings between %s and %s are not supported", type_to_string(lhs_t), type_to_string(rhs_t));

        bl_type_t *t = get_type(env->file, env->bindings, ast->lhs);
        char t_base = base_type_for(t);
        const char *ops[NUM_TYPES] = {[Add]="add", [Subtract]="sub", [Divide]="div", [Multiply]="mul", [Modulus]="urem"};
        CORD op = ops[ast->kind];
        if (!op) ERROR(env, ast, "Unsupported math operation");

        CORD lhs_reg = add_value(env, code, ast->lhs);
        CORD rhs_reg = add_value(env, code, ast->rhs);
        CORD result = fresh_local(env, "result");
        if (ast->kind == Modulus) {
            if (t_base == 'l' || t_base == 'w') {
                add_line(code, "%r =%c %r %r, %r", result, base_type_for(t), op, lhs_reg, rhs_reg);
                return result;
            } else {
                ERROR(env, ast, "Modulus is only supported for integer types, not %s", type_to_string(lhs_t));
            }
        }
        add_line(code, "%r =%c %r %r, %r", result, base_type_for(t), op, lhs_reg, rhs_reg);
        return result;
    }
    case Power: {
        bl_type_t *t = get_type(env->file, env->bindings, ast->lhs);
        bl_type_t *t2 = get_type(env->file, env->bindings, ast->rhs);
        CORD lhs_reg = add_value(env, code, ast->lhs);
        CORD rhs_reg = add_value(env, code, ast->rhs);
        CORD result = fresh_local(env, "result");
        if (t == t2 && base_type_for(t) == 'd') {
            add_line(code, "%r =d pow(d %r, d %r)", result, lhs_reg, rhs_reg);
            return result;
        } else if (t == t2 && base_type_for(t) == 's') {
            add_line(code, "%r =s powf(s %r, s %r)", result, lhs_reg, rhs_reg);
            return result;
        }
        ERROR(env, ast, "Exponentiation is not supported for %s and %s", type_to_string(t), type_to_string(t2));
        break;
    }
    case If: {
        CORD if_ret = fresh_local(env, "if_result");
        CORD endif = fresh_label(env, "endif");
        bl_type_t *t = get_type(env->file, env->bindings, ast);
        foreach (ast->clauses, clause, last_clause) {
            CORD iffalse = (clause < last_clause || ast->else_body) ? fresh_label(env, "if_false") : endif;
            CORD branch_val = add_clause(env, code, clause->condition, clause->body, iffalse);
            if (!ends_with_jump(code)) {
                add_line(code, "%r =%c copy %r", if_ret, base_type_for(t), branch_val);
                add_line(code, "jmp %r", endif);
            }
            if (iffalse != endif)
                add_line(code, "%r", iffalse);
        }
        if (ast->else_body) {
            CORD branch_val = add_value(env, code, ast->else_body);
            add_line(code, "%r =%c copy %r", if_ret, base_type_for(t), branch_val);
        }
        add_line(code, "%r", endif);
        return if_ret;
    }
    case While: {
        CORD loop_label = fresh_label(env, "loop");
        CORD end_label = fresh_label(env, "end_loop");
        env_t env2 = *env;
        env2.loop_label = &(loop_label_t){
            .enclosing = env->loop_label,
            .name = intern_str("while"),
            .skip_label = loop_label,
            .stop_label = end_label,
        };

        bl_type_t *t = get_type(env2.file, env2.bindings, ast);
        CORD ret_reg = (t->kind == NilType) ? NULL : fresh_local(&env2, "while_value");
        if (ret_reg)
            set_nil(code, t, ret_reg);

        add_line(code, "jmp %r", loop_label);
        add_line(code, "%r", loop_label);
        CORD body_reg;
        if (ast->loop.condition)
            body_reg = add_clause(&env2, code, ast->loop.condition, ast->loop.body, end_label);
        else
            body_reg = add_value(&env2, code, ast->loop.body);

        if (!ends_with_jump(code)) {
            if (ret_reg)
                add_line(code, "%r =%c copy %r", ret_reg, base_type_for(t), body_reg);
            
            // TODO: support `between`
            add_line(code, "jmp %r", loop_label);
        }
        add_line(code, "%r", end_label);
        return ret_reg ? ret_reg : "0";
    }
    case Skip: {
        CORD label = NULL;
        for (loop_label_t *lbl = env->loop_label; lbl; lbl = lbl->enclosing) {
            if (lbl->name == ast->str) {
                label = lbl->skip_label;
                break;
            }
        }
        if (!label) ERROR(env, ast, "I'm not sure what %s is referring to", ast->str);
        add_line(code, "jmp %r", label);
        return "0";
    }
    case Fail: {
        CORD reg = fresh_local(env, "fail_msg");
        if (ast->child) {
            add_line(code, "%r =l copy %r", reg, get_string_reg(env, intern_str("\x1b[31;7m")));
            CORD msg_reg = add_value(env, code, ast->child);
            add_line(code, "%r =l call $CORD_cat(l %r, l %r)", reg, reg, msg_reg);
            add_line(code, "%r =l call $CORD_cat(l %r, l %r)", reg, reg, get_string_reg(env, intern_str("\x1b[m\n\n")));
        } else {
            add_line(code, "%r =l copy %r", get_string_reg(env, intern_str("\x1b[31;7mA failure occurred\x1b[m\n\n")));
        }

        char *info = NULL;
        size_t size = 0;
        FILE *f = open_memstream(&info, &size);
        highlight_match(f, env->file, ast->match);
        fputc('\0', f);
        fflush(f);
        add_line(code, "%r =l call $CORD_cat(l %r, l %r)", reg, reg, get_string_reg(env, intern_str(info)));
        add_line(code, "%r =l call $CORD_to_char_star(l %r)", reg, reg);
        add_line(code, "call $fail(l %r)", reg, reg);
        fclose(f);
        return "0";
    }
    default: break;
    }
    ERROR(env, ast, "Error: compiling is not yet implemented for %s", get_ast_kind_name(ast->kind)); 
}

void add_statement(env_t *env, CORD *code, ast_t *ast) {
    if (env->debug)
        add_line(code, "loc %d, %d", 1, get_line_number(env->file, ast->match->start));
    check_discardable(env->file, env->bindings, ast);
    if (ast->kind == FunctionCall)
        (void)add_fncall(env, code, ast, false);
    else
        (void)add_value(env, code, ast);
}

const char *compile_file(file_t *f, ast_t *ast, bool debug) {
    int64_t next_id = 0;
    CORD data_code = NULL;
    CORD fn_code = NULL;
    CORD init_code = NULL;
    env_t env = {
        .file = f,
        .next_id = &next_id,
        .data_code = &data_code,
        .fn_code = &fn_code,
        .init_code = &init_code,
        .string_regs = hashmap_new(),
        .tostring_regs = hashmap_new(),
        .function_regs = hashmap_new(),
        .bindings = hashmap_new(),
        .debug = debug,
    };
    if (debug)
        add_line(&data_code, "file %d \"%s\"", 1, f->filename);
    
    bl_type_t *string_type = Type(StringType);
    bl_type_t *nil_type = Type(NilType);
    bl_type_t *say_type = Type(
        FunctionType,
        .args=LIST(bl_type_t*, string_type, Type(OptionalType, .nonnil=string_type)),
        .ret=nil_type);

    hashmap_set(env.bindings, intern_str("say"), new(binding_t, .reg="$puts", .type=say_type));
    hashmap_set(env.bindings, intern_str("random"), new(binding_t, .reg="$random", .type=Type(FunctionType, .args=LIST(bl_type_t*), .ret=Type(IntType))));
#define DEFTYPE(t) hashmap_set(env.bindings, intern_str(#t), new(binding_t, .is_global=true, .reg=get_string_reg(&env, intern_str(#t)), .type=Type(TypeType, .type=Type(t##Type))));
    // Primitive types:
    DEFTYPE(Bool); DEFTYPE(Nil); DEFTYPE(Abort);
    DEFTYPE(Int); DEFTYPE(Int32); DEFTYPE(Int16); DEFTYPE(Int8);
    DEFTYPE(Num); DEFTYPE(Num32);
    DEFTYPE(String);
#undef DEFTYPE

    CORD main_code = NULL;
    add_line(&main_code, "export function w $main() {");
    add_line(&main_code, "@start");
    assert(ast->kind == Block);
    add_statement(&env, &main_code, ast);
    add_line(&main_code, "  ret 0");
    add_line(&main_code, "}");
    CORD all_code = CORD_cat(data_code, fn_code);
    all_code = CORD_cat(all_code, main_code);
    return CORD_to_char_star(all_code);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
