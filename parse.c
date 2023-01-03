// Parse Blang code (the hard way)
#include <ctype.h>
#include <stdbool.h>
#include <string.h>
#include <stdarg.h>
#include <gc.h>
#include <setjmp.h>

#include "ast.h"
#include "parse.h"
#include "libblang/list.h"
#include "units.h"

typedef struct {
    bl_file_t *file;
    jmp_buf *on_err;
} parse_ctx_t;

#define PARSER(name) ast_t *name(parse_ctx_t *ctx, const char *pos)

static int op_tightness[NUM_AST_TAGS] = {
    [Power]=1,
    [Multiply]=2, [Divide]=2,
    [Add]=3, [Subtract]=3,
    [Modulus]=4,
    [Greater]=5, [GreaterEqual]=5, [Less]=5, [LessEqual]=5,
    [Equal]=6, [NotEqual]=6,
    [And]=7, [Or]=7, [Xor]=7,
};

static const char *keywords[] = {
    "yes","xor","with","while","when","use","unless","unit","typeof","then","stop","skip","sizeof","return","repeat",
    "pass","or","not","no","mod","macro","is","if","for","fail","extern","export","enum","elseif","else","do","deftype",
    "def","between","as","and", NULL,
};

static inline size_t chars(const char **pos, const char *allow);
static inline size_t not_chars(const char **pos, const char *forbid);
static inline size_t spaces(const char **pos);
static inline size_t whitespace(const char **pos);
static inline size_t match(const char **pos, const char *target);
static inline size_t match_word(const char **pos, const char *word);
static inline istr_t get_word(const char **pos);
static inline istr_t get_id(const char **pos);
static inline bool comment(const char **pos);
static inline bool indent(parse_ctx_t *ctx, const char **pos);
static inline bool nodent(parse_ctx_t *ctx, const char **pos);
static ast_t *parse_fncall(parse_ctx_t *ctx, const char *pos, bool requires_parens);
PARSER(parse_expr);
PARSER(parse_term);

__attribute__((noreturn))
static void parse_err(parse_ctx_t *ctx, const char *start, const char *end, const char *fmt, ...) {
    fputs("\x1b[31;1;7m", stderr);

    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);

    fputs("\x1b[m\n\n", stderr);

    span_t span = {.file=ctx->file, .start=start, .end=end};
    fprint_span(stderr, &span, "\x1b[31;1;7m", 2);

    fputs("\x1b[m\n", stderr);

    if (ctx->on_err)
        longjmp(*ctx->on_err, 1);
    exit(1);
}

istr_t unescape(const char **out) {
    const char **endpos = out;
    const char *escape = *out;
    static const char *unescapes[255] = {['a']="\a",['b']="\b",['e']="\e",['f']="\f",['n']="\n",['r']="\r",['t']="\t",['v']="\v"};
    assert(*escape == '\\');
    if (unescapes[(int)escape[1]]) {
        *endpos = escape + 2;
        return intern_str(unescapes[(int)escape[1]]);
    } else if (escape[1] == 'x' && escape[2] && escape[3]) {
        char *endptr = (char*)&escape[3+1];
        char c = (char)strtol(escape+2, &endptr, 16);
        *endpos = escape + 4;
        return intern_strn(&c, 1);
    } else if ('0' <= escape[1] && escape[1] <= '7' && '0' <= escape[2] && escape[2] <= '7' && '0' <= escape[3] && escape[3] <= '7') {
        char *endptr = (char*)&escape[4];
        char c = (char)strtol(escape+1, &endptr, 8);
        *endpos = escape + 4;
        return intern_strn(&c, 1);
    } else {
        *endpos = escape + 2;
        return intern_strn(escape+1, 1);
    }
}

size_t chars(const char **pos, const char *allow) {
    size_t len = strspn(*pos, allow);
    *pos += len;
    return len;
}

size_t not_chars(const char **pos, const char *forbid) {
    size_t len = strcspn(*pos, forbid);
    *pos += len;
    return len;
}

size_t spaces(const char **pos) { return chars(pos, " \t"); }
size_t whitespace(const char **pos) {
    const char *p0 = *pos;
    while (chars(pos, " \t\r\n") || comment(pos))
        continue;
    return (size_t)(*pos - p0);
}

size_t match(const char **pos, const char *target) {
    size_t len = strlen(target);
    if (strncmp(*pos, target, len) != 0)
        return 0;
    *pos += len;
    return len;
}

size_t match_word(const char **pos, const char *word) {
    const char *p0 = *pos;
    if (match(pos, word) && !isalpha(*pos) && !isdigit(*pos) && **pos != '_')
        return strlen(word);
    *pos = p0;
    return 0;
}

istr_t get_word(const char **pos) {
    if (!isalpha(**pos) && **pos != '_')
        return NULL;
    const char *word = *pos;
    ++(*pos);
    while (isalpha(**pos) || isdigit(**pos) || **pos == '_')
        ++(*pos);
    return intern_strn(word, (size_t)(*pos - word));
}

istr_t get_id(const char **pos) {
    istr_t word = get_word(pos);
    if (!word) return word;
    for (int i = 0; keywords[i]; i++)
        if (strcmp(word, keywords[i]) == 0)
            return NULL;
    return word;
}

bool comment(const char **pos) {
    if (!match(pos, "//"))
        return false;
    not_chars(pos, "\r\n");
    return true;
}

bool indent(parse_ctx_t *ctx, const char **out) {
    const char *pos = *out;
    size_t starting_indent = bl_get_indent(ctx->file, pos);
    whitespace(&pos);
    if (bl_get_line_number(ctx->file, pos) == bl_get_line_number(ctx->file, *out))
        return false;

    if (bl_get_indent(ctx->file, pos) > starting_indent) {
        *out = pos;
        return true;
    }

    return false;
}

bool nodent(parse_ctx_t *ctx, const char **out) {
    const char *pos = *out;
    size_t starting_indent = bl_get_indent(ctx->file, pos);
    whitespace(&pos);
    if (bl_get_line_number(ctx->file, pos) == bl_get_line_number(ctx->file, *out))
        return false;

    if (bl_get_indent(ctx->file, pos) == starting_indent) {
        *out = pos;
        return true;
    }

    return false;
}

bool match_indentation(const char **out, size_t indentation) {
    const char *pos = *out;
    while (indentation > 0) {
        switch (*pos) {
        case ' ': indentation += 1; ++pos; break;
        case '\t': indentation += 4; ++pos; break;
        default: return false;
        }
    }
    return true;
}

PARSER(parse_parens) {
    const char *start = pos;
    spaces(&pos);
    if (!match(&pos, "(")) return NULL;
    whitespace(&pos);
    ast_t *expr = parse_expr(ctx, pos);
    if (!expr) return NULL;
    pos = expr->span.end;
    if (!match(&pos, ")")) return NULL;
    expr->span.start = start;
    expr->span.end = pos;
    return expr;
}

istr_t match_units(const char **out) {
    const char *pos = *out;
    if (!match(&pos, "<")) return NULL;
    pos += strspn(pos, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ/^0123456789-");
    if (!match(&pos, ">")) return NULL;
    istr_t buf = intern_strn(*out + 1, (size_t)(pos-1-(*out+1)));
    istr_t ret = unit_string(buf);
    *out = pos;
    return ret;
}

PARSER(parse_int) {
    const char *start = pos;
    bool negative = match(&pos, "-");
    if (!isdigit(*pos)) return false;
    int64_t i = 0;
    if (match(&pos, "0x")) { // Hex
        size_t span = strspn(pos, "0123456789abcdefABCDEF_");
        char *buf = GC_MALLOC_ATOMIC(span+1);
        for (char *src = (char*)pos, *dest = buf; src < pos+span; ++src) {
            if (*src != '_') *(dest++) = *src;
        }
        i = strtol(buf, NULL, 16);
        pos += span;
    } else if (match(&pos, "0b")) { // Binary
        size_t span = strspn(pos, "01_");
        char *buf = GC_MALLOC_ATOMIC(span+1);
        for (char *src = (char*)pos, *dest = buf; src < pos+span; ++src) {
            if (*src != '_') *(dest++) = *src;
        }
        i = strtol(buf, NULL, 2);
        pos += span;
    } else if (match(&pos, "0o")) { // Octal
        size_t span = strspn(pos, "01234567_");
        char *buf = GC_MALLOC_ATOMIC(span+1);
        for (char *src = (char*)pos, *dest = buf; src < pos+span; ++src) {
            if (*src != '_') *(dest++) = *src;
        }
        i = strtol(buf, NULL, 8);
        pos += span;
    } else { // Decimal
        size_t span = strspn(pos, "0123456789_");
        char *buf = GC_MALLOC_ATOMIC(span+1);
        for (char *src = (char*)pos, *dest = buf; src < pos+span; ++src) {
            if (*src != '_') *(dest++) = *src;
        }
        i = strtol(buf, NULL, 10);
        pos += span;
    }

    if (negative) i *= -1;

    int64_t precision = 64;
    if (match(&pos, "i64")) precision = 64;
    else if (match(&pos, "i32")) precision = 32;
    else if (match(&pos, "i16")) precision = 16;
    else if (match(&pos, "i8")) precision = 8;

    istr_t units = match_units(&pos);
    return NewAST(ctx, start, pos, Int, .i=i, .precision=precision, .units=units);
}

PARSER(parse_num) {
    const char *start = pos;
    bool negative = match(&pos, "-");
    if (!isdigit(*pos)) return false;

    size_t len = strspn(pos, "0123456789_");
    if (pos[len] == 'e')
        len += 1 + strspn(pos + len + 1, "-0123456789_");
    char *buf = GC_MALLOC_ATOMIC(len+1);
    for (char *src = (char*)pos, *dest = buf; src < pos+len; ++src) {
        if (*src != '_') *(dest++) = *src;
    }
    double d = strtod(buf, NULL);
    pos += len;

    if (negative) d *= -1;

    int64_t precision = 64;
    if (match(&pos, "f64")) precision = 64;
    else if (match(&pos, "f32")) precision = 32;

    istr_t units = match_units(&pos);
    return NewAST(ctx, start, pos, Num, .n=d, .precision=precision, .units=units);
}

ast_t *parse_fielded(parse_ctx_t *ctx, ast_t *lhs) {
    if (!lhs) return NULL;
    const char *pos = lhs->span.end;
    whitespace(&pos);
    if (!match(&pos, ".")) return NULL;
    istr_t field = get_id(&pos);
    if (!field) return NULL;
    return NewAST(ctx, lhs->span.start, pos, FieldAccess, .fielded=lhs, .field=field);
}

ast_t *parse_index(parse_ctx_t *ctx, ast_t *lhs) {
    if (!lhs) return NULL;
    const char *pos = lhs->span.end;
    whitespace(&pos);
    if (!match(&pos, "[")) return NULL;
    whitespace(&pos);
    ast_t *index = parse_expr(ctx, pos);
    if (!index) return NULL;
    pos = index->span.end;
    whitespace(&pos);
    if (!match(&pos, "]")) return NULL;
    return NewAST(ctx, lhs->span.start, pos, Index, .indexed=lhs, .index=index);
}


#define UNARY_OP(name, tagname, prefix) \
    PARSER(name) { \
        const char *start = pos; \
        if (!match(&pos, prefix)) return NULL; \
        whitespace(&pos); \
        ast_t *term = parse_term(ctx, pos); \
        if (!term) return NULL; \
        return NewAST(ctx, start, pos, tagname, .value=term); \
    }
UNARY_OP(parse_negative, Negative, "-")
UNARY_OP(parse_heap_alloc, HeapAllocate, "@")
UNARY_OP(parse_len, Len, "#")
UNARY_OP(parse_maybe, Maybe, "?")
PARSER(parse_not) { 
    const char *start = pos; 
    if (!match_word(&pos, "not")) return NULL; 
    whitespace(&pos); 
    ast_t *term = parse_term(ctx, pos); 
    if (!term) return NULL; 
    return NewAST(ctx, start, pos, Not, .value=term); 
}
#undef UNARY_OP

PARSER(parse_bool) {
    const char *start = pos;
    if (match_word(&pos, "yes"))
        return NewAST(ctx, start, pos, Bool, .b=true);
    else if (match_word(&pos, "no"))
        return NewAST(ctx, start, pos, Bool, .b=false);
    else
        return NULL;
}

PARSER(parse_char) {
    const char *start = pos;
    if (!match(&pos, "`")) return NULL;
    char c = *pos;
    if (!c) return NULL;
    return NewAST(ctx, start, pos, Char, .c=c);
}

PARSER(parse_interpolation) {
    const char *start = pos;
    if (!match(&pos, "$")) return NULL;
    bool labelled = match(&pos, ":");
    ast_t *value = parse_term(ctx, pos);
    if (!value) return NULL;
    pos = value->span.end;
    return NewAST(ctx, start, pos, Interp, .value=value, .labelled=labelled);
}

PARSER(parse_string) {
    const char *string_start = pos;
    static struct {
        const char *start, *special, *open, *close;
    } delims[] = {
        {"\"", "$\\\"\r\n",  NULL, "\""},
        {"'",  "'\r\n",      NULL, "'"},
        {"%{", "$\\{}\r\n",  "{",  "}"},
        {"%[", "$\\[]\r\n",  "[",  "]"},
        {"%(", "()\r\n",     "(",  ")"},
        {"%<", "$\\<>\r\n",  "<",  ">"},
    };
    size_t d;
    for (d = 0; d < sizeof(delims)/sizeof(delims[0]); d++) {
        if (match(&pos, delims[d].start))
            goto found_delim;
    }
    return NULL;

  found_delim:;

    NEW_LIST(ast_t*, chunks);
    if (*pos == '\r' || *pos == '\n') {
        size_t starting_indent = bl_get_indent(ctx->file, pos); 
        // indentation-delimited string
        match(&pos, "\r");
        match(&pos, "\n");
        size_t first_line = bl_get_line_number(ctx->file, pos);
        size_t indented = starting_indent + 4;
        for (size_t i = first_line; i < (size_t)LIST_LEN(ctx->file->lines); i++) {
            auto line = LIST_ITEM(ctx->file->lines, i);
            if (line.is_empty) {
                continue;
            } else if (line.indent > starting_indent) {
                indented = line.indent;
                break;
            } else {
                // TODO: error
                return NULL;
            }
        }
        for (size_t i = first_line; i < (size_t)LIST_LEN(ctx->file->lines); i++) {
            auto line = LIST_ITEM(ctx->file->lines, i);
            pos = ctx->file->text + line.offset;
            if (line.is_empty) {
                ast_t *ast = NewAST(ctx, pos, pos, StringLiteral, .str="\n");
                APPEND(chunks, ast);
                continue;
            }
            if (!match_indentation(&pos, starting_indent))
                return NULL;

            if (match(&pos, delims[d].close))
                goto finished;

            if (!match_indentation(&pos, (indented - starting_indent)))
                return NULL; // TODO: error

            for (;;) {
                size_t len = strcspn(pos, delims[d].special[0] == '$' ? "\\$\r\n" : "\r\n");
                if (pos[len] == '\r') ++len;
                if (pos[len] == '\n') ++len;

                if (len > 0) {
                    ast_t *chunk = NewAST(ctx, pos, pos+len-1, StringLiteral, .str=intern_strn(pos, len));
                    APPEND(chunks, chunk);
                }

                pos += len;

                switch (*pos) {
                case '$': { // interpolation
                    ast_t *chunk = parse_interpolation(ctx, pos);
                    if (!chunk)
                        parse_err(ctx, pos, pos+1, "This interpolation didn't parse");
                    APPEND(chunks, chunk);
                    pos = chunk->span.end;
                    break;
                }
                case '\\': { // escape
                    const char *start = pos;
                    istr_t unescaped = unescape(&pos);
                    ast_t *chunk = NewAST(ctx, start, pos, StringLiteral, .str=unescaped);
                    APPEND(chunks, chunk);
                    break;
                }
                default: break;
                }
            }
        }
    } else {
        // Inline string:
        int depth = 1;
        while (depth > 0 && *pos) {
            size_t len = strcspn(pos, delims[d].special);
            if (len > 0) {
                ast_t *chunk = NewAST(ctx, pos, pos+len-1, StringLiteral, .str=intern_strn(pos, len));
                APPEND(chunks, chunk);
                pos = chunk->span.end;
            }

            switch (*pos) {
            case '$': { // interpolation
                ast_t *chunk = parse_interpolation(ctx, pos);
                if (!chunk)
                    parse_err(ctx, pos, pos+1, "This interpolation didn't parse");
                APPEND(chunks, chunk);
                pos = chunk->span.end;
                break;
            }
            case '\\': { // escape
                const char *start = pos;
                istr_t unescaped = unescape(&pos);
                ast_t *chunk = NewAST(ctx, start, pos, StringLiteral, .str=unescaped);
                APPEND(chunks, chunk);
                pos = chunk->span.end;
                break;
            }
            default: {
                const char *start = pos;
                if (delims[d].open && match(&pos, delims[d].open)) {
                    ast_t *chunk = NewAST(ctx, start, pos, StringLiteral, .str=intern_str(delims[d].open));
                    APPEND(chunks, chunk);
                    pos = chunk->span.end;
                    ++depth;
                    continue;
                }
                start = pos;
                if (match(&pos, delims[d].close)) {
                    --depth;
                    if (depth > 0) {
                        ast_t *chunk = NewAST(ctx, start, pos, StringLiteral, .str=intern_str(delims[d].close));
                        APPEND(chunks, chunk);
                        pos = chunk->span.end;
                    } else {
                        break;
                    }
                }
                ++pos;
                break;
            }
            }
        }
    }
  finished:;
    return NewAST(ctx, string_start, pos, StringJoin, .children=chunks);
}

#define STUB_PARSER(name) PARSER(name) { (void)ctx; (void)pos; return NULL; }
STUB_PARSER(parse_dsl)
STUB_PARSER(parse_type)
STUB_PARSER(parse_skip)
STUB_PARSER(parse_stop)
STUB_PARSER(parse_return)
STUB_PARSER(parse_lambda)
STUB_PARSER(parse_struct)
STUB_PARSER(parse_array)
STUB_PARSER(parse_cast)
STUB_PARSER(parse_bitcast)
#undef STUB

PARSER(parse_nil) {
    const char *start = pos;
    if (!match(&pos, "!")) return NULL;
    ast_t *type = parse_type(ctx, pos);
    if (!type) return NULL;
    return NewAST(ctx, start, type->span.end, Nil, .type=type);
}

PARSER(parse_fail) {
    const char *start = pos;
    if (!match_word(&pos, "fail")) return NULL;
    ast_t *msg = parse_expr(ctx, pos);
    return NewAST(ctx, start, msg ? msg->span.end : pos, Fail, .message=msg);
}

PARSER(parse_var) {
    const char *start = pos;
    istr_t name = get_id(&pos);
    if (!name) return NULL;
    return NewAST(ctx, start, pos, Var, .name=name);
}

PARSER(parse_term) {
    spaces(&pos);
    ast_t *term = NULL;
    bool success = (
        (term=parse_num(ctx, pos))
        || (term=parse_int(ctx, pos))
        || (term=parse_negative(ctx, pos))
        || (term=parse_heap_alloc(ctx, pos))
        || (term=parse_len(ctx, pos))
        || (term=parse_maybe(ctx, pos))
        || (term=parse_not(ctx, pos))
        || (term=parse_bool(ctx, pos))
        || (term=parse_char(ctx, pos))
        || (term=parse_string(ctx, pos))
        || (term=parse_dsl(ctx, pos))
        || (term=parse_nil(ctx, pos))
        || (term=parse_fail(ctx, pos))
        || (term=parse_skip(ctx, pos))
        || (term=parse_stop(ctx, pos))
        || (term=parse_return(ctx, pos))
        || (term=parse_parens(ctx, pos))
        || (term=parse_lambda(ctx, pos))
        || (term=parse_struct(ctx, pos))
        || (term=parse_var(ctx, pos))
        || (term=parse_array(ctx, pos))
        || (term=parse_cast(ctx, pos))
        || (term=parse_bitcast(ctx, pos))
        || (term=parse_fncall(ctx, pos, true)));

    if (!success) return NULL;

    for (;;) {
        ast_t *new_term = NULL;
        bool progress = (
            (new_term=parse_index(ctx, term))
            || (new_term=parse_fielded(ctx, term)));

        if (progress)
            term = new_term;
        else
            break;
    }
    return term;
}

ast_t *parse_fncall(parse_ctx_t *ctx, const char *pos, bool requires_parens) {
    const char *start = pos;
    ast_t *fn = parse_term(ctx, pos);
    if (!fn) return NULL;
    pos = fn->span.end;

    spaces(&pos);
    const char *paren_pos = pos;
    if (match(&pos, "("))
        requires_parens = true;
    else if (requires_parens)
        return NULL;

    NEW_LIST(ast_t*, args);
    for (;;) {
        const char *arg_start = pos;
        istr_t name = get_id(&pos);
        if (name) {
            spaces(&pos);
            if (match(&pos, "=")) {
                spaces(&pos);
                ast_t *arg = parse_expr(ctx, pos);
                if (!arg) parse_err(ctx, arg_start, pos, "I couldn't parse this keyword argument value");
                ast_t *kwarg = NewAST(ctx, arg_start, arg->span.end, KeywordArg,
                                      .name=name, .arg=arg);
                APPEND(args, kwarg);
                pos = kwarg->span.end;
                goto got_arg;
            }
            pos = arg_start;
        }

        ast_t *arg = parse_expr(ctx, pos);
        if (!arg) break;
        APPEND(args, arg);
        pos = arg->span.end;

      got_arg:
        ;

        spaces(&pos);
        if (!match(&pos, ","))
            break;
        spaces(&pos);
    }

    spaces(&pos);
    if (requires_parens && !match(&pos, "("))
        parse_err(ctx, paren_pos, pos, "This parenthesis is unclosed");

    if (LIST_LEN(args) < 1)
        return NULL;

    return NewAST(ctx, start, pos, FunctionCall, .fn=fn, .args=args);
}

ast_t *parse_expr(parse_ctx_t *ctx, const char *pos) {
    ast_t *term = parse_term(ctx, pos);
    if (!term) return NULL;

    pos = term->span.end;

    NEW_LIST(ast_t*, terms);
    APPEND(terms, term);
    NEW_LIST(ast_tag_e, binops);
    for (;;) {
        spaces(&pos);
        ast_tag_e tag = Unknown;
        switch (*pos) {
        case '+': ++pos; tag = Add; break;
        case '-': ++pos; tag = Subtract; break;
        case '*': ++pos; tag = Multiply; break;
        case '/': ++pos; tag = Divide; break;
        case '^': ++pos; tag = Power; break;
        case '<': ++pos; tag = match(&pos, "=") ? LessEqual : Less; break;
        case '>': ++pos; tag = match(&pos, "=") ? LessEqual : Less; break;
        case '!': {
            if (!match(&pos, "=")) goto no_more_binops;
            tag = NotEqual;
            break;
        }
        case '=': {
            if (!match(&pos, "=")) goto no_more_binops;
            tag = Equal;
            break;
        }
        default: {
            if (match_word(&pos, "and")) {
                tag = And; break;
            } else if (match_word(&pos, "or")) {
                tag = Or; break;
            } else if (match_word(&pos, "xor")) {
                tag = Xor; break;
            } else if (match_word(&pos, "xor")) {
                tag = Xor; break;
            } else {
                goto no_more_binops;
            }
        }
        }

        assert(op_tightness[tag]);

        spaces(&pos);
        ast_t *rhs = parse_term(ctx, pos);
        if (!rhs) goto no_more_binops;
        pos = rhs->span.end;
        APPEND(terms, rhs);
        APPEND(binops, tag);
    }

  no_more_binops:
    ;

    // Sort out the op precedence (everything is left-associative here)
    while (LIST_LEN(terms) > 1) {
        // Find tightest-binding op
        int tightest_op = 0;
        for (int i = 1; i < LIST_LEN(binops); i++) {
            if (op_tightness[LIST_ITEM(binops, i)]
                > op_tightness[LIST_ITEM(binops, tightest_op)]) {
                tightest_op = i;
            }
        }

        auto tag = LIST_ITEM(binops, tightest_op);
        // Bind two terms into one:
        LIST_REMOVE(binops, tightest_op);
        ast_t *lhs = LIST_ITEM(terms, tightest_op);
        ast_t *rhs = LIST_ITEM(terms, tightest_op + 1);
        LIST_REMOVE(terms, tightest_op);

        // Unsafe, relies on these having the same type:
        ast_t *merged = new(
            ast_t,
            .span.file=ctx->file, .span.start=lhs->span.start, .span.end=rhs->span.end,
            .tag=tag,
            .__data.Add.lhs=lhs,
            .__data.Add.rhs=rhs);
        // End unsafe
        terms[0][tightest_op] = merged;
        assert(LIST_ITEM(terms, 0) == merged);
    }

    ast_t *expr = LIST_ITEM(terms, 0);
    return expr;
}

PARSER(parse_declaration) {
    const char *start = pos;
    ast_t *var = parse_var(ctx, pos);
    if (!var) return NULL;
    pos = var->span.end;
    spaces(&pos);
    if (!match(&pos, ":=")) return NULL;
    spaces(&pos);
    ast_t *val = parse_expr(ctx, pos);
    if (!val) parse_err(ctx, pos, strchrnul(pos, '\n'), "This declaration value didn't parse");
    pos = val->span.end;
    return NewAST(ctx, start, pos, Declare, .var=var, .value=val);
}

PARSER(parse_statement) {
    ast_t *stmt = NULL;
    if (!((stmt=parse_declaration(ctx, pos))
          || (stmt=parse_fncall(ctx, pos, false))
          || (stmt=parse_expr(ctx, pos))))
        return NULL;
    return stmt;
}

bool blank_lines(const char **pos) {
    const char *start = *pos;
    for (;;) {
        const char *line = *pos;
        spaces(&line);
        comment(&line);
        if (*line == '\n')
            *pos = line + 1;
        else
            break;
    }
    return *pos > start;
}

PARSER(parse_block) {
    const char *start = pos;
    blank_lines(&pos);
    NEW_LIST(ast_t*, statements);
    size_t starting_indent = bl_get_indent(ctx->file, pos);
    while (*pos) {
        ast_t *stmt = parse_statement(ctx, pos);
        if (!stmt) break;
        pos = stmt->span.end;
        APPEND(statements, stmt);
        printf("Statement: %s\n", ast_to_str(stmt));
        fprint_span(stdout, &stmt->span, "\x1b[7m", 1);
        fputs("\n", stdout);
        if (!blank_lines(&pos))
            break;
        if (!match_indentation(&pos, starting_indent))
            break;
    }
    return NewAST(ctx, start, pos, Block, .statements=statements);
}

ast_t *parse_file(bl_file_t *file, jmp_buf *on_err) {
    parse_ctx_t ctx = {
        .file=file,
        .on_err=on_err,
    };

    const char *pos = file->text;
    if (match(&pos, "#!")) { // shebang
        not_chars(&pos, "\r\n");
        chars(&pos, "\r\n");
    }

    ast_t *ast = parse_block(&ctx, pos);
    pos = ast->span.end;
    blank_lines(&pos);
    match(&pos, "\n");
    // if (strlen(pos) > 0) {
    //     parse_err(&ctx, pos, pos + strlen(pos), "I couldn't parse this part of the file");
    // }
    return ast;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
