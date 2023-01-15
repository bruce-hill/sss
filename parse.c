// Parse Blang code using recursive descent
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

typedef ast_t* (parser_t)(parse_ctx_t*,const char*);

#define PARSER(name) ast_t *name(parse_ctx_t *ctx, const char *pos)

#define STUB_PARSER(name) PARSER(name) { (void)ctx; (void)pos; return NULL; }
STUB_PARSER(parse_dsl)
STUB_PARSER(parse_cast)
STUB_PARSER(parse_bitcast)
#undef STUB

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

static inline size_t some_of(const char **pos, const char *allow);
static inline size_t some_not(const char **pos, const char *forbid);
static inline size_t spaces(const char **pos);
static inline size_t whitespace(const char **pos);
static inline size_t match(const char **pos, const char *target);
static inline void expect_str(parse_ctx_t *ctx, const char *start, const char **pos, const char *target, const char *fmt, ...);
static inline void expect_closing(parse_ctx_t *ctx, const char **pos, const char *target, const char *fmt, ...);
static inline size_t match_word(const char **pos, const char *word);
static inline istr_t get_word(const char **pos);
static inline istr_t get_id(const char **pos);
static inline bool comment(const char **pos);
static inline bool indent(parse_ctx_t *ctx, const char **pos);
static ast_t *parse_fncall_suffix(parse_ctx_t *ctx, ast_t *fn, bool requires_parens);
static ast_t *parse_field_suffix(parse_ctx_t *ctx, ast_t *lhs);
static ast_t *parse_suffix_if(parse_ctx_t *ctx, ast_t *body, bool require_else);
static ast_t *parse_suffix_for(parse_ctx_t *ctx, ast_t *body);
static ast_t *parse_suffix_while(parse_ctx_t *ctx, ast_t *body);
PARSER(parse_expr);
PARSER(parse_extended_expr);
PARSER(parse_term);
PARSER(parse_inline_block);
PARSER(parse_type);
PARSER(parse_statement);
PARSER(parse_block);
PARSER(parse_opt_indented_block);
PARSER(parse_var);
PARSER(parse_def);

//
// Print a parse error and exit (or use the on_err longjmp)
//
__attribute__((noreturn))
static void vparser_err(parse_ctx_t *ctx, const char *start, const char *end, const char *fmt, va_list args) {
    fputs("\x1b[31;1;7m", stderr);
    vfprintf(stderr, fmt, args);
    fputs("\x1b[m\n\n", stderr);

    span_t span = {.file=ctx->file, .start=start, .end=end};
    fprint_span(stderr, span, "\x1b[31;1;7m", 2);
    fputs("\n", stderr);

    if (ctx->on_err)
        longjmp(*ctx->on_err, 1);
    exit(1);
}

//
// Wrapper for vparser_err
//
__attribute__((noreturn))

static void parser_err(parse_ctx_t *ctx, const char *start, const char *end, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vparser_err(ctx, start, end, fmt, args);
    va_end(args);
}

//
// Convert an escape sequence like \n to a string
//
istr_t unescape(const char **out) {
    const char **endpos = out;
    const char *escape = *out;
    static const char *unescapes[256] = {['a']="\a",['b']="\b",['e']="\e",['f']="\f",['n']="\n",['r']="\r",['t']="\t",['v']="\v"};
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

///////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////     Text-based parsing primitives     ///////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////
size_t some_of(const char **pos, const char *allow) {
    size_t len = strspn(*pos, allow);
    *pos += len;
    return len;
}

size_t some_not(const char **pos, const char *forbid) {
    size_t len = strcspn(*pos, forbid);
    *pos += len;
    return len;
}

size_t spaces(const char **pos) { return some_of(pos, " \t"); }
size_t whitespace(const char **pos) {
    const char *p0 = *pos;
    while (some_of(pos, " \t\r\n") || comment(pos))
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

//
// Expect a string (potentially after whitespace) and emit a parser error if it's not there
//
static void expect_str(
    parse_ctx_t *ctx, const char *start, const char **pos, const char *target, const char *fmt, ...) {
    spaces(pos);
    if (match(pos, target)) {
        char lastchar = target[strlen(target)-1];
        if (!(isalpha(lastchar) || isdigit(lastchar) || lastchar == '_'))
            return;
        char nextchar = **pos;
        if (!(isalpha(nextchar) || isdigit(nextchar) || lastchar == '_'))
            return;
    }

    fputs("\x1b[31;1;7m", stderr);
    va_list args;
    va_start(args, fmt);
    vparser_err(ctx, start, *pos, fmt, args);
    va_end(args);
}

//
// Helper for matching closing parens with good error messages
//
static void expect_closing(
    parse_ctx_t *ctx, const char **pos, const char *closing, const char *fmt, ...) {
    const char *start = *pos;
    spaces(pos);
    if (match(pos, closing))
        return;

    const char *eol = strchr(*pos, '\n');
    const char *next = strstr(*pos, closing);
    
    const char *end = eol < next ? eol : next;

    fputs("\x1b[31;1;7m", stderr);
    va_list args;
    va_start(args, fmt);
    vparser_err(ctx, start, end, fmt, args);
    va_end(args);
}

//
// Expect an AST (potentially after whitespace) and emit a parser error if it's not there
//
static ast_t *expect_ast(
    parse_ctx_t *ctx, const char *start, const char **pos, parser_t *parser, const char *fmt, ...) {
    spaces(pos);
    ast_t *ast = parser(ctx, *pos);
    if (ast) {
        *pos = ast->span.end;
        return ast;
    }

    fputs("\x1b[31;1;7m", stderr);
    va_list args;
    va_start(args, fmt);
    vparser_err(ctx, start, *pos, fmt, args);
    va_end(args);
}

//
// Helper function for when an AST can optionally appear after some spaces
//
static inline ast_t *optional_ast(parse_ctx_t *ctx, const char **pos, parser_t *parser) {
    spaces(pos);
    ast_t *ast = parser(ctx, *pos);
    if (ast) *pos = ast->span.end;
    return ast;
}

size_t match_word(const char **out, const char *word) {
    const char *pos = *out;
    spaces(&pos);
    if (match(&pos, word) && !isalpha(*pos) && !isdigit(*pos) && *pos != '_') {
        *out = pos;
        return strlen(word);
    }
    return 0;
}

bool match_group(const char **out, char open) {
    static char mirror_delim[256] = {['(']=')', ['{']='}', ['<']='>', ['[']=']'};
    const char *pos = *out;
    if (*pos != open) return 0;
    char close = mirror_delim[(int)open] ? mirror_delim[(int)open] : open;
    int depth = 1;
    for (++pos; *pos && depth > 0; ++pos) {
        if (*pos == close) --depth;
        else if (*pos == open) ++depth;
    }
    if (depth == 0) {
        *out = pos;
        return true;
    } else return false;
}

istr_t get_word(const char **inout) {
    const char *pos = *inout;
    if (!isalpha(*pos) && *pos != '_')
        return NULL;
    spaces(&pos);
    const char *word = pos;
    ++pos;
    while (isalpha(*pos) || isdigit(*pos) || *pos == '_')
        ++pos;
    *inout = pos;
    return intern_strn(word, (size_t)(pos - word));
}

istr_t get_id(const char **inout) {
    const char *pos = *inout;
    istr_t word = get_word(&pos);
    if (!word) return word;
    for (int i = 0; keywords[i]; i++)
        if (strcmp(word, keywords[i]) == 0)
            return NULL;
    *inout = pos;
    return word;
}

bool comment(const char **pos) {
    if (!match(pos, "//"))
        return false;
    some_not(pos, "\r\n");
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

///////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////     AST-based parsers    /////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////

PARSER(parse_parens) {
    const char *start = pos;
    spaces(&pos);
    if (!match(&pos, "(")) return NULL;
    whitespace(&pos);
    ast_t *expr = optional_ast(ctx, &pos, parse_extended_expr);
    if (!expr) return NULL;
    expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this expression");

    // Update the span to include the parens:
    return new(ast_t, .span.file=(ctx)->file, .span.start=start, .span.end=pos,
               .tag=expr->tag, .__data=expr->__data);
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
        memset(buf, 0, span+1);
        for (char *src = (char*)pos, *dest = buf; src < pos+span; ++src) {
            if (*src != '_') *(dest++) = *src;
        }
        i = strtol(buf, NULL, 16);
        pos += span;
    } else if (match(&pos, "0b")) { // Binary
        size_t span = strspn(pos, "01_");
        char *buf = GC_MALLOC_ATOMIC(span+1);
        memset(buf, 0, span+1);
        for (char *src = (char*)pos, *dest = buf; src < pos+span; ++src) {
            if (*src != '_') *(dest++) = *src;
        }
        i = strtol(buf, NULL, 2);
        pos += span;
    } else if (match(&pos, "0o")) { // Octal
        size_t span = strspn(pos, "01234567_");
        char *buf = GC_MALLOC_ATOMIC(span+1);
        memset(buf, 0, span+1);
        for (char *src = (char*)pos, *dest = buf; src < pos+span; ++src) {
            if (*src != '_') *(dest++) = *src;
        }
        i = strtol(buf, NULL, 8);
        pos += span;
    } else { // Decimal
        size_t span = strspn(pos, "0123456789_");
        char *buf = GC_MALLOC_ATOMIC(span+1);
        memset(buf, 0, span+1);
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
    // else if (match(&pos, ".") || match(&pos, "e")) return NULL; // looks like a float

    istr_t units = match_units(&pos);
    return NewAST(ctx, start, pos, Int, .i=i, .precision=precision, .units=units);
}

STUB_PARSER(parse_measure_type)
STUB_PARSER(parse_table_type)
STUB_PARSER(parse_tuple_type)

PARSER(parse_func_type) {
    const char *start = pos;
    if (!match(&pos, "(")) return NULL;
    NEW_LIST(ast_t*, arg_types);
    for (;;) {
        ast_t *arg_t = optional_ast(ctx, &pos, parse_type);
        if (!arg_t) break;
        APPEND(arg_types, arg_t);
        spaces(&pos);
        if (!match(&pos, ",")) break;
    }
    expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this function call");
    spaces(&pos);
    if (!match(&pos, "=>")) return NULL;
    ast_t *ret = optional_ast(ctx, &pos, parse_type);
    return NewAST(ctx, start, pos, TypeFunction, .arg_types=arg_types, .ret_type=ret);
}

PARSER(parse_array_type) {
    const char *start = pos;
    if (!match(&pos, "[")) return NULL;
    ast_t *type = expect_ast(ctx, start, &pos, parse_type,
                             "I couldn't parse an array item type after this point");
    expect_closing(ctx, &pos, "]", "I wasn't able to parse the rest of this array type");
    return NewAST(ctx, start, pos, TypeArray, .item_type=type);
}

PARSER(parse_pointer_type) {
    const char *start = pos;
    if (!match(&pos, "@")) return NULL;
    ast_t *type = expect_ast(ctx, start, &pos, parse_type,
                             "I couldn't parse a pointer type after this point");
    return NewAST(ctx, start, pos, TypePointer, .pointed=type);
}

PARSER(parse_optional_type) {
    const char *start = pos;
    if (!match(&pos, "?")) return NULL;
    ast_t *type = expect_ast(ctx, start, &pos, parse_type,
                             "I couldn't parse a pointer type after this point");
    return NewAST(ctx, start, pos, TypeOptional, .type=type);
}

PARSER(parse_type_name) {
    const char *start = pos;
    istr_t id = get_id(&pos);
    if (!id) return NULL;
    ast_t *ast = NewAST(ctx, start, pos, Var, .name=id);
    ast_t *fielded = parse_field_suffix(ctx, ast);
    return fielded ? fielded : ast;
}

PARSER(parse_type) {
    const char *start = pos;
    ast_t *type = NULL;
    bool success = (
        (type=parse_optional_type(ctx, pos))
        || (type=parse_pointer_type(ctx, pos))
        || (type=parse_array_type(ctx, pos))
        || (type=parse_measure_type(ctx, pos))
        || (type=parse_table_type(ctx, pos))
        || (type=parse_tuple_type(ctx, pos))
        || (type=parse_type_name(ctx, pos))
        || (type=parse_func_type(ctx, pos))
    );
    if (!success && match(&pos, "(")) {
        whitespace(&pos);
        type = optional_ast(ctx, &pos, parse_type);
        if (!type) return NULL;
        whitespace(&pos);
        expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this type");
        // Use the enclosing span
        type = new(ast_t, .span.file=(ctx)->file, .span.start=start, .span.end=pos,
                   .tag=type->tag, .__data=type->__data);
    }
    return type;
}

PARSER(parse_num) {
    const char *start = pos;
    bool negative = match(&pos, "-");
    if (!isdigit(*pos) && *pos != '.') return NULL;

    size_t len = strspn(pos, "0123456789_");
    if (strncmp(pos+len, "..", 2) == 0)
        return NULL;
    else if (pos[len] == '.')
        len += 1 + strspn(pos + len + 1, "0123456789");
    else
        return NULL;
    if (pos[len] == 'e')
        len += 1 + strspn(pos + len + 1, "-0123456789_");
    char *buf = GC_MALLOC_ATOMIC(len+1);
    memset(buf, 0, len+1);
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

PARSER(parse_array) {
    const char *start = pos;
    if (!match(&pos, "[")) return NULL;

    whitespace(&pos);

    NEW_LIST(ast_t*, items);
    ast_t *item_type = NULL;
    if (match(&pos, ":")) {
        whitespace(&pos);
        item_type = expect_ast(ctx, pos-1, &pos, parse_type, "I couldn't parse a type for this array");
    }

    for (;;) {
        whitespace(&pos);
        ast_t *item = optional_ast(ctx, &pos, parse_expr);
        if (!item) break;

        for (bool progress = true; progress; ) {
            ast_t *new_item;
            progress = (false
                || (new_item=parse_suffix_if(ctx, item, false))
                || (new_item=parse_suffix_for(ctx, item))
                || (new_item=parse_suffix_while(ctx, item))
            );
            if (progress) {
                item = new_item;
                pos = item->span.end;
            }
        }

        APPEND(items, item);
        whitespace(&pos);
        if (!match(&pos, ",")) break;
    }
    whitespace(&pos);
    match(&pos, ",");
    whitespace(&pos);
    expect_closing(ctx, &pos, "]", "I wasn't able to parse the rest of this array");

    if (!item_type && LIST_LEN(items) == 0)
        parser_err(ctx, start, pos, "Empty arrays must specify what type they would contain (e.g. [:Int])");

    return NewAST(ctx, start, pos, Array, .type=item_type, .items=items);
}

PARSER(parse_struct) {
    const char *start = pos;
    ast_t *type = optional_ast(ctx, &pos, parse_type_name);
    spaces(&pos);
    if (!match(&pos, "{")) return NULL;
    NEW_LIST(ast_t*, members);
    for (;;) {
        whitespace(&pos);
        const char *field_start = pos;
        istr_t field_name = NULL;
        if ((field_name=get_id(&pos))) {
            whitespace(&pos);
            if (!match(&pos, "=")) goto no_field_name;
            whitespace(&pos);
        } else {
          no_field_name: field_name = NULL;
          pos = field_start;
        }
        ast_t *value = field_name ? expect_ast(ctx, field_start, &pos, parse_expr, "I couldn't parse the value for this field") : optional_ast(ctx, &pos, parse_expr);
        if (!value) break;
        APPEND(members, NewAST(ctx, field_start, pos, StructField, .name=field_name, .value=value));
        whitespace(&pos);
        match(&pos, ",");
    }
    whitespace(&pos);
    expect_closing(ctx, &pos, "}", "I wasn't able to parse the rest of this struct");
    return NewAST(ctx, start, pos, Struct, .type=type, .members=members);
}

ast_t *parse_field_suffix(parse_ctx_t *ctx, ast_t *lhs) {
    if (!lhs) return NULL;
    const char *pos = lhs->span.end;
    whitespace(&pos);
    if (!match(&pos, ".")) return NULL;
    istr_t field = get_id(&pos);
    if (!field) return NULL;
    return NewAST(ctx, lhs->span.start, pos, FieldAccess, .fielded=lhs, .field=field);
}

PARSER(parse_range) {
    const char *start = pos;
    ast_t *first = optional_ast(ctx, &pos, parse_term);
    if (!match(&pos, "..")) return NULL;
    match(&pos, "+");
    ast_t *step = optional_ast(ctx, &pos, parse_term);
    ast_t *last = NULL;
    if (step && match(&pos, "..")) {
        last = optional_ast(ctx, &pos, parse_term);
    } else {
        last = step;
        step = NULL;
    }
    return NewAST(ctx, start, pos, Range, .first=first, .step=step, .last=last);
}

ast_t *parse_index_suffix(parse_ctx_t *ctx, ast_t *lhs) {
    if (!lhs) return NULL;
    const char *start = lhs->span.start;
    const char *pos = lhs->span.end;
    whitespace(&pos);
    if (!match(&pos, "[")) return NULL;
    ast_t *index = expect_ast(ctx, start, &pos, parse_extended_expr,
                              "I expected to find an expression here to index with");
    expect_closing(ctx, &pos, "]", "I wasn't able to parse the rest of this index");
    return NewAST(ctx, start, pos, Index, .indexed=lhs, .index=index);
}

ast_t *parse_if(parse_ctx_t *ctx, const char *pos) {
    // if cond then body elseif body else body
    const char *start = pos;
    bool is_unless;
    if (match_word(&pos, "if")) is_unless = false;
    else if (match_word(&pos, "unless")) is_unless = true;
    else return NULL;
    size_t starting_indent = bl_get_indent(ctx->file, pos);
    ast_t *cond = expect_ast(ctx, start, &pos, parse_expr,
                             "I expected to find a condition for this 'if'");
    if (is_unless) cond = FakeAST(Not, .value=cond);

    match_word(&pos, "then");
    parser_t *body_parser = indent(ctx, &pos) ? parse_block : parse_statement;
    ast_t *body = expect_ast(ctx, start, &pos, body_parser, "I expected a body for this 'if'"); 

    List(ast_t*) conditions = LIST(ast_t*, cond);
    List(ast_t*) blocks = LIST(ast_t*, body);

    while (match_word(&pos, "elseif")) {
        cond = expect_ast(ctx, pos, &pos, parse_expr, "I couldn't parse this condition");
        match_word(&pos, "then");
        body_parser = indent(ctx, &pos) ? parse_block : parse_statement;
        body = expect_ast(ctx, start, &pos, body_parser, "I expected a body for this 'elseif'"); 
        APPEND(conditions, cond);
        APPEND(blocks, body);
        whitespace(&pos);
        if (bl_get_indent(ctx->file, pos) != starting_indent) {
            pos = body->span.end;
            break;
        }
    }

    ast_t *else_ = NULL;
    const char *else_start = pos;
    whitespace(&else_start);
    if (bl_get_indent(ctx->file, else_start) == starting_indent && match_word(&else_start, "else")) {
        pos = else_start;
        body_parser = indent(ctx, &pos) ? parse_block : parse_statement;
        else_ = expect_ast(ctx, start, &pos, body_parser, "I expected a body for this 'else'"); 
    }
    return NewAST(ctx, start, pos, If, .conditions=conditions, .blocks=blocks, .else_body=else_);
}

ast_t *parse_for(parse_ctx_t *ctx, const char *pos) {
    // for [k,] v in iter [do] [<indent>] body [<nodent> between [<indent>] body]
    const char *start = pos;
    if (!match_word(&pos, "for")) return NULL;
    size_t starting_indent = bl_get_indent(ctx->file, pos);
    ast_t *key = expect_ast(ctx, start, &pos, parse_var, "I expected an iteration variable for this 'for'");
    spaces(&pos);
    ast_t *value = NULL;
    if (match(&pos, ",")) {
        value = expect_ast(ctx, pos-1, &pos, parse_var, "I expected a variable after this comma");
    }
    expect_str(ctx, start, &pos, "in", "I expected an 'in' for this 'for'");
    ast_t *iter = expect_ast(ctx, start, &pos, parse_expr, "I expected an iterable value for this 'for'");

    match_word(&pos, "do"); // Optional

    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'for'"); 
    whitespace(&pos);
    ast_t *between = NULL;
    const char *between_start = NULL;
    if (bl_get_indent(ctx->file, pos) == starting_indent && match_word(&pos, "between")) {
        between = expect_ast(ctx, between_start, &pos, parse_opt_indented_block, "I expected a body for this 'between'");
    }
    return NewAST(ctx, start, pos, For, .key=value ? key : NULL, .value=value ? value : key, .iter=iter, .body=body, .between=between);
}

ast_t *parse_suffix_for(parse_ctx_t *ctx, ast_t *body) {
    if (!body) return NULL;
    const char *start = body->span.start;
    const char *pos = body->span.end;
    if (!match_word(&pos, "for")) return NULL;
    ast_t *key = expect_ast(ctx, start, &pos, parse_var, "I expected an iteration variable for this 'for'");
    spaces(&pos);
    ast_t *value = NULL;
    if (match(&pos, ","))
        value = expect_ast(ctx, pos-1, &pos, parse_var, "I expected a variable after this comma");
    expect_str(ctx, start, &pos, "in", "I expected an 'in' for this 'for'");
    ast_t *iter = expect_ast(ctx, start, &pos, parse_expr, "I expected an iterable value for this 'for'");
    // TODO: filter
    return NewAST(ctx, start, pos, For, .key=value ? key : NULL, .value=value ? value : key, .iter=iter, .body=body);
}

ast_t *parse_repeat(parse_ctx_t *ctx, const char *pos) {
    // repeat [<indent>] body [<nodent> between [<indent>] body]
    const char *start = pos;
    if (!match_word(&pos, "repeat")) return NULL;
    size_t starting_indent = bl_get_indent(ctx->file, pos);
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'repeat'"); 
    whitespace(&pos);
    ast_t *between = NULL;
    const char *between_start = NULL;
    if (bl_get_indent(ctx->file, pos) == starting_indent && match_word(&pos, "between")) {
        between = expect_ast(ctx, between_start, &pos, parse_opt_indented_block, "I expected a body for this 'between'");
    }
    return NewAST(ctx, start, pos, Repeat, .body=body, .between=between);
}

ast_t *parse_while(parse_ctx_t *ctx, const char *pos) {
    // while condition [do] [<indent>] body [<nodent> between [<indent>] body]
    const char *start = pos;
    if (!match_word(&pos, "while")) return NULL;
    size_t starting_indent = bl_get_indent(ctx->file, pos);
    ast_t *condition = expect_ast(ctx, start, &pos, parse_expr, "I don't see a viable condition for this 'while'");
    match(&pos, "do"); // optional
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'while'"); 
    whitespace(&pos);
    ast_t *between = NULL;
    const char *between_start = NULL;
    if (bl_get_indent(ctx->file, pos) == starting_indent && match_word(&pos, "between"))
        between = expect_ast(ctx, between_start, &pos, parse_opt_indented_block, "I expected a body for this 'between'");
    return NewAST(ctx, start, pos, While, .condition=condition, .body=body, .between=between);
}

ast_t *parse_suffix_while(parse_ctx_t *ctx, ast_t *body) {
    if (!body) return NULL;
    const char *start = body->span.start;
    const char *pos = body->span.end;
    if (!match_word(&pos, "while")) return NULL;
    ast_t *cond = expect_ast(ctx, start, &pos, parse_expr, "I don't see a viable condition for this 'while'");
    return NewAST(ctx, start, pos, While, .condition=cond, .body=body);
}

ast_t *parse_suffix_if(parse_ctx_t *ctx, ast_t *body, bool require_else) {
    // true_val if cond elseif cond then elseif_val else else_val
    if (!body) return NULL;
    const char *start = body->span.start;
    const char *pos = body->span.end;
    bool is_unless;
    if (match_word(&pos, "unless")) is_unless = true;
    else if (match_word(&pos, "if")) is_unless = false;
    else return NULL;
    ast_t *cond = expect_ast(ctx, start, &pos, parse_expr,
                             "I expected to find a condition for this 'if'");

    if (is_unless) cond = FakeAST(Not, .value=cond);
    List(ast_t*) conditions = LIST(ast_t*, cond);
    List(ast_t*) blocks = LIST(ast_t*, body);

    while (match_word(&pos, "elseif")) {
        if (!(cond = optional_ast(ctx, &pos, parse_expr)))
            parser_err(ctx, pos, pos+strcspn(pos,"\r\n"), "I couldn't parse this condition");
        if (!match_word(&pos, "then"))
            parser_err(ctx, cond->span.start, pos, "I expected a 'then' after this 'elseif'");
        if (!(body = optional_ast(ctx, &pos, parse_expr)))
            parser_err(ctx, cond->span.start, pos, "I couldn't find the body for this 'elseif'");
    }

    ast_t *else_ = NULL;
    const char *else_start = pos;
    if (match_word(&pos, "else")) {
        whitespace(&pos);
        else_ = expect_ast(ctx, else_start, &pos, parse_expr, "I couldn't find a body for this 'else' block");
    }
    if (else_) pos = else_->span.end;
    else if (require_else) return NULL;

    return NewAST(ctx, start, pos, If, .conditions=conditions, .blocks=blocks, .else_body=else_);
}

ast_t *parse_unary(parse_ctx_t *ctx, const char *pos, ast_tag_e tag, const char *prefix) {
    const char *start = pos;
    if (!match(&pos, prefix)) return NULL;
    if (isalpha(prefix[0]) && (isalpha(*pos) || isdigit(*pos) || *pos == '_')) return NULL;
    whitespace(&pos);
    ast_t *term = parse_term(ctx, pos);
    if (!term) return NULL;
    // Unsafe: all the unary ops have a '.value' field in the same spot, so this is a hack
    return new(ast_t, .span.file=ctx->file, .span.start=start, .span.end=term->span.end,
               .tag=tag, .__data.Not.value=term);
    // End unsafe area
}
#define parse_negative(...) parse_unary(__VA_ARGS__, Negative, "-")
#define parse_heap_alloc(...) parse_unary(__VA_ARGS__, HeapAllocate, "@")
#define parse_len(...) parse_unary(__VA_ARGS__, Len, "#")
#define parse_maybe(...) parse_unary(__VA_ARGS__, Maybe, "?")
#define parse_not(...) parse_unary(__VA_ARGS__, Not, "not")

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
    ast_t *value = optional_ast(ctx, &pos, parse_term);
    if (!value) {
        match_group(&pos, '(');
        parser_err(ctx, start, pos, "This interpolation didn't parse");
    }
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
            case '\r': case '\n': {
                parser_err(ctx, string_start, pos, "This line ended without closing the string");
            }
            default: {
                const char *start = pos;
                if (delims[d].open && match(&pos, delims[d].open)) {
                    ast_t *chunk = NewAST(ctx, start, pos, StringLiteral, .str=intern_str(delims[d].open));
                    APPEND(chunks, chunk);
                    ++depth;
                    continue;
                }
                start = pos;
                if (match(&pos, delims[d].close)) {
                    --depth;
                    if (depth > 0) {
                        ast_t *chunk = NewAST(ctx, start, pos, StringLiteral, .str=intern_str(delims[d].close));
                        APPEND(chunks, chunk);
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

PARSER(parse_skip) {
    const char *start = pos;
    if (!match_word(&pos, "skip")) return NULL;
    spaces(&pos);
    istr_t target = get_word(&pos);
    return NewAST(ctx, start, pos, Skip, .target=target);
}

PARSER(parse_stop) {
    const char *start = pos;
    if (!match_word(&pos, "stop")) return NULL;
    spaces(&pos);
    istr_t target = get_word(&pos);
    return NewAST(ctx, start, pos, Stop, .target=target);
}

PARSER(parse_return) {
    const char *start = pos;
    if (!match_word(&pos, "return")) return NULL;
    spaces(&pos);
    ast_t *value = optional_ast(ctx, &pos, parse_expr);
    return NewAST(ctx, start, pos, Return, .value=value);
}

PARSER(parse_lambda) {
    const char *start = pos;
    if (!match(&pos, "(")) return NULL;
    spaces(&pos);
    NEW_LIST(istr_t, arg_names);
    NEW_LIST(ast_t*, arg_types);
    for (;;) {
        spaces(&pos);
        istr_t name = get_id(&pos);
        if (!name) break;
        APPEND(arg_names, name);
        spaces(&pos);
        if (!match(&pos, ":")) {
            // Only an error if we're definitely in a lambda
            if (LIST_LEN(arg_types) > 0)
                parser_err(ctx, pos, pos, "I expected a type annotation here");
            return NULL; // otherwise just a parse failure
        }
        ast_t *type = parse_type(ctx, pos);
        if (!type) parser_err(ctx, pos, pos + strcspn(pos, ",;|\r\n"), "I wasn't able to parse this type");
        APPEND(arg_types, type);
        pos = type->span.end;
    }

    spaces(&pos);
    if (!match(&pos, ")")) {
        if (LIST_LEN(arg_names) == 0) return NULL;
        parser_err(ctx, start, pos, "This lambda doesn't have a closing '|'");
    }
    spaces(&pos);
    if (!match(&pos, "=>"))
        return NULL;

    ast_t *body = optional_ast(ctx, &pos, parse_inline_block);

    return NewAST(ctx, start, pos, Lambda, .arg_names=arg_names, .arg_types=arg_types, .body=body);
}

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
    ast_t *msg = parse_term(ctx, pos);
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
        false
        || (term=parse_num(ctx, pos))
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
        || (term=parse_lambda(ctx, pos))
        || (term=parse_parens(ctx, pos))
        || (term=parse_struct(ctx, pos))
        || (term=parse_var(ctx, pos))
        || (term=parse_array(ctx, pos))
        || (term=parse_cast(ctx, pos))
        || (term=parse_bitcast(ctx, pos))
        );

    if (!success) return NULL;

    for (bool progress = true; progress; ) {
        ast_t *new_term;
        progress = (false
            || (new_term=parse_index_suffix(ctx, term))
            || (new_term=parse_field_suffix(ctx, term))
            || (new_term=parse_fncall_suffix(ctx, term, true))
            );
        if (progress) term = new_term;
    }
    return term;
}

ast_t *parse_fncall_suffix(parse_ctx_t *ctx, ast_t *fn, bool requires_parens) {
    if (!fn) return NULL;
    const char *start = fn->span.start;
    const char *pos = fn->span.end;
    const char *paren_pos = pos;
    if (match(&pos, "{")) return NULL;
    if (match(&pos, "("))
        requires_parens = true;
    else if (requires_parens)
        return NULL;

    spaces(&pos);

    NEW_LIST(ast_t*, args);
    if (match(&pos, ")")) { // no arguments
        if (!requires_parens) return NULL;
        return NewAST(ctx, start, pos, FunctionCall, .fn=fn, .args=args);
    }

    for (;;) {
        const char *arg_start = pos;
        istr_t name = get_id(&pos);
        if (name) {
            spaces(&pos);
            if (match(&pos, "=")) {
                spaces(&pos);
                ast_t *arg = parse_expr(ctx, pos);
                if (!arg) parser_err(ctx, arg_start, pos, "I couldn't parse this keyword argument value");
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
        whitespace(&pos);
    }

    spaces(&pos);
    if (requires_parens && !match(&pos, ")"))
        parser_err(ctx, paren_pos, pos, "This parenthesis is unclosed");

    if (LIST_LEN(args) < 1)
        return NULL;

    return NewAST(ctx, start, pos, FunctionCall, .fn=fn, .args=args);
}

ast_t *parse_expr(parse_ctx_t *ctx, const char *pos) {
    ast_t *term = optional_ast(ctx, &pos, parse_range);
    if (!term) term = optional_ast(ctx, &pos, parse_term);
    if (!term) return NULL;

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
        case '>': ++pos; tag = match(&pos, "=") ? GreaterEqual : Greater; break;
        case '!': {
            if (!match(&pos, "!=")) goto no_more_binops;
            tag = NotEqual;
            break;
        }
        case '=': {
            if (!match(&pos, "==")) goto no_more_binops;
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
            } else if (match_word(&pos, "mod")) {
                tag = Modulus; break;
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
                < op_tightness[LIST_ITEM(binops, tightest_op)]) {
                tightest_op = i;
            }
        }

        auto tag = LIST_ITEM(binops, tightest_op);
        // Bind two terms into one:
        LIST_REMOVE(binops, tightest_op);
        ast_t *lhs = LIST_ITEM(terms, tightest_op);
        ast_t *rhs = LIST_ITEM(terms, tightest_op + 1);

        // Unsafe, relies on these having the same type:
        ast_t *merged = new(
            ast_t,
            .span.file=ctx->file, .span.start=lhs->span.start, .span.end=rhs->span.end,
            .tag=tag,
            .__data.Add.lhs=lhs,
            .__data.Add.rhs=rhs);
        // End unsafe

        LIST_REMOVE(terms, tightest_op);
        terms[0][tightest_op] = merged;
        // assert(LIST_ITEM(terms, tightest_op) == merged);
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
    ast_t *val = parse_extended_expr(ctx, pos);
    if (!val) parser_err(ctx, pos, strchrnul(pos, '\n'), "This declaration value didn't parse");
    pos = val->span.end;
    return NewAST(ctx, start, pos, Declare, .var=var, .value=val);
}

PARSER(parse_update) {
    const char *start = pos;
    ast_t *lhs = optional_ast(ctx, &pos, parse_expr);
    if (!lhs) return NULL;
    spaces(&pos);
    ast_tag_e tag;
    if (match(&pos, "+=")) tag = AddUpdate;
    else if (match(&pos, "-=")) tag = SubtractUpdate;
    else if (match(&pos, "*=")) tag = MultiplyUpdate;
    else if (match(&pos, "/=")) tag = DivideUpdate;
    else if (match(&pos, "and=")) tag = AndUpdate;
    else if (match(&pos, "or=")) tag = AndUpdate;
    else return NULL;
    ast_t *rhs = expect_ast(ctx, start, &pos, parse_extended_expr, "I expected an expression here");
    switch (tag) {
    case AddUpdate: return NewAST(ctx, start, pos, AddUpdate, .lhs=lhs, .rhs=rhs);
    case SubtractUpdate: return NewAST(ctx, start, pos, SubtractUpdate, .lhs=lhs, .rhs=rhs);
    case MultiplyUpdate: return NewAST(ctx, start, pos, MultiplyUpdate, .lhs=lhs, .rhs=rhs);
    case DivideUpdate: return NewAST(ctx, start, pos, DivideUpdate, .lhs=lhs, .rhs=rhs);
    case AndUpdate: return NewAST(ctx, start, pos, AndUpdate, .lhs=lhs, .rhs=rhs);
    case OrUpdate: return NewAST(ctx, start, pos, OrUpdate, .lhs=lhs, .rhs=rhs);
    default: return NULL;
    }
}

PARSER(parse_assignment) {
    const char *start = pos;
    NEW_LIST(ast_t*, targets);
    for (;;) {
        ast_t *lhs = optional_ast(ctx, &pos, parse_term);
        if (!lhs) break;
        APPEND(targets, lhs);
        spaces(&pos);
        if (!match(&pos, ",")) break;
        whitespace(&pos);
    }

    if (LIST_LEN(targets) == 0) return NULL;

    spaces(&pos);
    if (!match(&pos, "=")) return NULL;
    if (match(&pos, "=")) return NULL; // == comparison

    NEW_LIST(ast_t*, values);
    if (LIST_LEN(targets) == 1) {
        ast_t *rhs = expect_ast(ctx, start, &pos, parse_extended_expr, "I expected an expression here");
        APPEND(values, rhs);
    } else {
        for (;;) {
            ast_t *rhs = optional_ast(ctx, &pos, parse_extended_expr);
            if (!rhs) break;
            APPEND(values, rhs);
            spaces(&pos);
            if (!match(&pos, ",")) break;
            whitespace(&pos);
        }
    }

    return NewAST(ctx, start, pos, Assign, .targets=targets, .values=values);
}

PARSER(parse_statement) {
    ast_t *stmt = NULL;
    if (false 
        || (stmt=parse_if(ctx, pos))
        || (stmt=parse_for(ctx, pos))
        || (stmt=parse_repeat(ctx, pos))
        || (stmt=parse_while(ctx, pos))
        || (stmt=parse_def(ctx, pos))
        || (stmt=parse_declaration(ctx, pos))
        || (stmt=parse_update(ctx, pos))
        || (stmt=parse_assignment(ctx, pos))
        || (stmt=parse_fncall_suffix(ctx, parse_term(ctx, pos), false)))
        return stmt;

    stmt = parse_expr(ctx, pos);
    if (!stmt) return NULL;

    for (bool progress = true; progress; ) {
        ast_t *new_stmt;
        progress = (
            (new_stmt=parse_index_suffix(ctx, stmt))
            || (new_stmt=parse_field_suffix(ctx, stmt))
            || (new_stmt=parse_suffix_if(ctx, stmt, false))
            || (new_stmt=parse_suffix_for(ctx, stmt))
            || (new_stmt=parse_suffix_while(ctx, stmt))
            || (new_stmt=parse_fncall_suffix(ctx, stmt, true))
            );
        if (progress) stmt = new_stmt;
    }
    return stmt;
}

PARSER(parse_extended_expr) {
    ast_t *stmt = NULL;
    if ((stmt=parse_fncall_suffix(ctx, parse_term(ctx, pos), false)))
        return stmt;

    stmt = parse_expr(ctx, pos);
    if (!stmt) return NULL;

    for (bool progress = true; progress; ) {
        ast_t *new_stmt;
        progress = (
            (new_stmt=parse_index_suffix(ctx, stmt))
            || (new_stmt=parse_field_suffix(ctx, stmt))
            || (new_stmt=parse_suffix_if(ctx, stmt, false))
            || (new_stmt=parse_suffix_for(ctx, stmt))
            || (new_stmt=parse_suffix_while(ctx, stmt))
            || (new_stmt=parse_fncall_suffix(ctx, stmt, true))
            );
        if (progress) stmt = new_stmt;
    }
    return stmt;
}

PARSER(parse_block) {
    size_t block_indent = bl_get_indent(ctx->file, pos);
    const char *start = pos;
    whitespace(&pos);
    NEW_LIST(ast_t*, statements);
    while (*pos) {
        ast_t *stmt = optional_ast(ctx, &pos, parse_statement);
        if (!stmt) {
            spaces(&pos);
            if (*pos && *pos != '\r' && *pos != '\n')
                parser_err(ctx, pos, strchrnul(pos, '\n'), "I couldn't parse this line");
            break;
        }
        APPEND(statements, stmt);
        whitespace(&pos);
        if (bl_get_indent(ctx->file, pos) != block_indent) {
            pos = stmt->span.end; // backtrack
            break;
        }
    }
    return NewAST(ctx, start, pos, Block, .statements=statements);
}

PARSER(parse_indented_block) {
    return indent(ctx, &pos) ? parse_block(ctx, pos) : NULL;
}

PARSER(parse_opt_indented_block) {
    return indent(ctx, &pos) ? parse_block(ctx, pos) : parse_statement(ctx, pos);
}

PARSER(parse_struct_field_def) {
    const char *start = pos;
    NEW_LIST(istr_t, names);
    for (;;) {
        spaces(&pos);
        istr_t name = get_id(&pos);
        if (!name) break;
        APPEND(names, name);
        spaces(&pos);
        if (!match(&pos, ",")) break;
    }
    if (LIST_LEN(names) == 0) return NULL;
    spaces(&pos);
    if (!match(&pos, ":")) return NULL;
    ast_t *type = expect_ast(ctx, pos-1, &pos, parse_type, "I expected a type here");
    return NewAST(ctx, start, pos, StructFieldDef, .names=names, .type=type);
}

ast_t *parse_rest_of_struct_def(parse_ctx_t *ctx, const char *start, const char **pos, istr_t name) {
    NEW_LIST(ast_t*, members);
    for (;;) {
        whitespace(pos);
        ast_t *memb = NULL;
        bool success = (
            false
            || (memb=parse_struct_field_def(ctx, *pos))
            || (memb=parse_def(ctx, *pos))
            || (memb=parse_declaration(ctx, *pos)));
        if (!success) break;
        APPEND(members, memb);
        *pos = memb->span.end;
    }
    whitespace(pos);
    expect_closing(ctx, pos, "}", "I wasn't able to parse the rest of this struct");
    return NewAST(ctx, start, *pos, StructDef, .name=name, .members=members);
}

PARSER(parse_def) {
    const char *start = pos;
    if (!match_word(&pos, "def")) return NULL;
    spaces(&pos);
    istr_t name = get_id(&pos);
    if (!name) parser_err(ctx, start, pos, "I expected to see a name after this 'def'");
    spaces(&pos);

    if (match(&pos, "(")) { // Function def foo(...)
        NEW_LIST(istr_t, arg_names);
        NEW_LIST(ast_t*, arg_types);
        NEW_LIST(ast_t*, arg_defaults);
        for (;;) {
            whitespace(&pos);
            const char *arg_start = pos;
            istr_t arg_name = get_id(&pos);
            if (!arg_name) break;
            APPEND(arg_names, arg_name);
            spaces(&pos);
            if (match(&pos, ":")) {
                ast_t *type = expect_ast(ctx, arg_start, &pos, parse_type,
                                         "I expected a type for this argument");
                APPEND(arg_types, type);
                APPEND(arg_defaults, NULL);
            } else if (match(&pos, "=")) {
                ast_t *def_val = expect_ast(ctx, arg_start, &pos, parse_expr,
                                            "I expected a default value for this argument");
                APPEND(arg_defaults, def_val);
                APPEND(arg_types, NULL);
            } else {
                parser_err(ctx, arg_start, pos, "This argument needs a type or a default value");
            }
            spaces(&pos);
            if (!match(&pos, ",")) break;
        }

        whitespace(&pos);
        expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this function definition");

        ast_t *ret_type = NULL;
        spaces(&pos);
        if (match(&pos, ":")) {
            ret_type = optional_ast(ctx, &pos, parse_type);
        }
        ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block,
                                 "This function needs a body block");
        return NewAST(ctx, start, pos, FunctionDef,
                      .name=name, .arg_names=arg_names, .arg_types=arg_types,
                      .arg_defaults=arg_defaults, .ret_type=ret_type, .body=body);
    } else if (match(&pos, "{")) { // Struct def Foo{...}
        return parse_rest_of_struct_def(ctx, start, &pos, name);
    } else if (match_word(&pos, "oneof")) { // Enum def Foo oneof {...}
        expect_str(ctx, start, &pos, "{", "I expected a '{' after 'oneof'");

        NEW_LIST(istr_t, tag_names);
        NEW_LIST(int64_t, tag_values);
        NEW_LIST(ast_t*, tag_types);
        int64_t next_value = 0;
        for (;;) {
            whitespace(&pos);
            const char *tag_start = pos;
            istr_t tag_name = get_id(&pos);
            if (!tag_name) break;
            spaces(&pos);
            if (match(&pos, "=")) {
                ast_t *val = expect_ast(ctx, tag_start, &pos, parse_int, "I expected an integer literal after this '='");
                next_value = Match(val, Int)->i;
            }

            spaces(&pos);
            ast_t *type = NULL;
            if (match(&pos, ":")) {
                type = expect_ast(ctx, pos-1, &pos, parse_type, "I couldn't parse a type here");
            } else if (match(&pos, "{")) {
                type = parse_rest_of_struct_def(ctx, tag_start, &pos, tag_name);
            }

            APPEND(tag_names, tag_name);
            APPEND(tag_values, next_value);
            APPEND(tag_types, type);

            whitespace(&pos);
            match(&pos, ",");

            ++next_value;
        }
        expect_closing(ctx, &pos, "}", "I wasn't able to parse the rest of this 'oneof'");
        return NewAST(ctx, start, pos, EnumDef, .name=name, .tag_names=tag_names, .tag_values=tag_values, .tag_types=tag_types);
    }
    return NULL;
}

PARSER(parse_inline_block) {
    spaces(&pos);
    const char *start = pos;
    NEW_LIST(ast_t*, statements);
    while (*pos) {
        spaces(&pos);
        ast_t *stmt = parse_statement(ctx, pos);
        if (!stmt) break;
        pos = stmt->span.end;
        APPEND(statements, stmt);
        if (!match(&pos, ";")) break;
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
        some_not(&pos, "\r\n");
        some_of(&pos, "\r\n");
    }

    whitespace(&pos);
    ast_t *ast = parse_block(&ctx, pos);
    pos = ast->span.end;
    whitespace(&pos);
    if (strlen(pos) > 0) {
        parser_err(&ctx, pos, pos + strlen(pos), "I couldn't parse this part of the file");
    }
    return ast;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
