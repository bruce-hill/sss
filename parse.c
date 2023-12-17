// Parse SSS code using recursive descent
#include <ctype.h>
#include <gc.h>
#include <libgen.h>
#include <linux/limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <unistr.h>
#include <unictype.h>
#include <signal.h>

#include "ast.h"
#include "parse.h"

typedef struct {
    sss_file_t *file;
    jmp_buf *on_err;
} parse_ctx_t;

typedef ast_t* (parser_t)(parse_ctx_t*,const char*);

#define PARSER(name) ast_t *name(parse_ctx_t *ctx, const char *pos)

#define STUB_PARSER(name) PARSER(name) { (void)ctx; (void)pos; return NULL; }

int op_tightness[] = {
    [OP_UNKNOWN]=0,
    [OP_POWER]=1,
    [OP_MULT]=2, [OP_DIVIDE]=2, [OP_MOD]=2, [OP_MOD1]=2,
    [OP_PLUS]=3, [OP_MINUS]=3,
    [OP_CONCAT]=4,
    [OP_LSHIFT]=5, [OP_RSHIFT]=5,
    [OP_MIN]=6, [OP_MAX]=6, [OP_MIX]=6,
    [OP_RANGE]=7,
    [OP_BY]=8,
    [OP_AS]=9,
    [OP_IN]=10, [OP_NOT_IN]=10,
    [OP_EQ]=11, [OP_NE]=11,
    [OP_LT]=11, [OP_LE]=11, [OP_GT]=11, [OP_GE]=11,
    [OP_AND]=12, [OP_OR]=12, [OP_XOR]=12,

};

static const char *keywords[] = {
    "yes", "xor", "with", "while", "using", "use", "unless", "then", "struct", "stop", "skip",
    "sizeof", "return", "repeat", "or", "of", "not", "no", "mod1", "mod", "matches", "in", "if", "func",
    "for", "fail", "extern", "enum", "else", "do", "defer", "convert", "by", "bitcast", "between", "as",
    "and", "alias", "_typeinfo_", "_mix_", "_min_", "_max_",
    NULL,
};

enum {OPTIONAL_PARENS=0, NEEDS_PARENS=1};
enum {NORMAL_FUNCTION=0, EXTERN_FUNCTION=1};

static inline size_t some_of(const char **pos, const char *allow);
static inline size_t some_not(const char **pos, const char *forbid);
static inline size_t spaces(const char **pos);
static inline size_t whitespace(const char **pos);
static inline size_t match(const char **pos, const char *target);
static inline void expect_str(parse_ctx_t *ctx, const char *start, const char **pos, const char *target, const char *fmt, ...);
static inline void expect_closing(parse_ctx_t *ctx, const char **pos, const char *target, const char *fmt, ...);
static inline size_t match_word(const char **pos, const char *word);
static inline const char* get_word(const char **pos);
static inline const char* get_id(const char **pos);
static inline bool comment(const char **pos);
static inline bool indent(parse_ctx_t *ctx, const char **pos);
static inline operator_e match_binary_operator(const char **pos);
static ast_t *parse_fncall_suffix(parse_ctx_t *ctx, ast_t *fn, bool needs_parens, bool is_extern);
static ast_t *parse_field_suffix(parse_ctx_t *ctx, ast_t *lhs);
static ast_t *parse_suffix_for(parse_ctx_t *ctx, ast_t *body);
static ast_t *parse_suffix_while(parse_ctx_t *ctx, ast_t *body);
static ast_t *parse_index_suffix(parse_ctx_t *ctx, ast_t *lhs);
static args_t parse_args(parse_ctx_t *ctx, const char **pos, bool allow_unnamed);
static PARSER(parse_variant);
static PARSER(parse_for);
static PARSER(parse_while);
static PARSER(parse_repeat);
static PARSER(parse_defer);
static PARSER(parse_with);
static PARSER(parse_using);
static PARSER(parse_do);
static PARSER(parse_if);
static PARSER(parse_expr);
static PARSER(parse_extended_expr);
static PARSER(parse_term_no_suffix);
static PARSER(parse_term);
static PARSER(parse_inline_block);
static PARSER(parse_type);
static PARSER(parse_statement);
static PARSER(parse_block);
static PARSER(parse_opt_indented_block);
static PARSER(parse_var);
static PARSER(parse_type_def);
static PARSER(parse_func_def);
static PARSER(parse_enum_type);
static PARSER(parse_convert_def);
static PARSER(parse_extern);
static PARSER(parse_declaration);
static PARSER(parse_doctest);
static PARSER(parse_use);
static PARSER(parse_linker);
static PARSER(parse_ellipsis);
static PARSER(parse_namespace);
static ast_t *optional_suffix_condition(parse_ctx_t *ctx, ast_t *ast, const char **pos, ast_t *else_ast);

//
// Print a parse error and exit (or use the on_err longjmp)
//
__attribute__((noreturn))
static void vparser_err(parse_ctx_t *ctx, const char *start, const char *end, const char *fmt, va_list args) {
    if (isatty(STDERR_FILENO) && !getenv("NO_COLOR"))
        fputs("\x1b[31;1;7m", stderr);
    fprintf(stderr, "%s:%ld.%ld: ", ctx->file->relative_filename, sss_get_line_number(ctx->file, start),
            sss_get_line_column(ctx->file, start));
    vfprintf(stderr, fmt, args);
    if (isatty(STDERR_FILENO) && !getenv("NO_COLOR"))
        fputs(" \x1b[m", stderr);
    fputs("\n\n", stderr);

    fprint_span(stderr, ctx->file, start, end, "\x1b[31;1;7m", 2, isatty(STDERR_FILENO) && !getenv("NO_COLOR"));
    fputs("\n", stderr);

    if (ctx->on_err)
        longjmp(*ctx->on_err, 1);
    raise(SIGABRT);
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
const char *unescape(const char **out) {
    const char **endpos = out;
    const char *escape = *out;
    static const char *unescapes[256] = {['a']="\a",['b']="\b",['e']="\e",['f']="\f",['n']="\n",['r']="\r",['t']="\t",['v']="\v",['_']=" "};
    assert(*escape == '\\');
    if (unescapes[(int)escape[1]]) {
        *endpos = escape + 2;
        return heap_str(unescapes[(int)escape[1]]);
    } else if (escape[1] == 'x' && escape[2] && escape[3]) {
        char *endptr = (char*)&escape[3+1];
        char c = (char)strtol(escape+2, &endptr, 16);
        *endpos = escape + 4;
        return heap_strn(&c, 1);
    } else if ('0' <= escape[1] && escape[1] <= '7' && '0' <= escape[2] && escape[2] <= '7' && '0' <= escape[3] && escape[3] <= '7') {
        char *endptr = (char*)&escape[4];
        char c = (char)strtol(escape+1, &endptr, 8);
        *endpos = escape + 4;
        return heap_strn(&c, 1);
    } else {
        *endpos = escape + 2;
        return heap_strn(escape+1, 1);
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

static inline bool is_xid_continue_next(const char *pos) {
    ucs4_t point = 0;
    u8_next(&point, (const uint8_t*)pos);
    return uc_is_property_xid_continue(point);
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
        if (!is_xid_continue_next(*pos))
            return;
    }

    if (isatty(STDERR_FILENO) && !getenv("NO_COLOR"))
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

    if (isatty(STDERR_FILENO) && !getenv("NO_COLOR"))
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
        *pos = ast->end;
        return ast;
    }

    if (isatty(STDERR_FILENO) && !getenv("NO_COLOR"))
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
    if (ast) *pos = ast->end;
    return ast;
}

size_t match_word(const char **out, const char *word) {
    const char *pos = *out;
    spaces(&pos);
    if (!match(&pos, word) || is_xid_continue_next(pos))
        return 0;

    *out = pos;
    return strlen(word);
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

const char *get_word(const char **inout) {
    const char *word = *inout;
    spaces(&word);
    const uint8_t *pos = (const uint8_t*)word;
    ucs4_t point;
    pos = u8_next(&point, pos);
    if (!uc_is_property_xid_start(point) && point != '_')
        return NULL;

    for (const uint8_t *next; (next = u8_next(&point, pos)); pos = next) {
        if (!uc_is_property_xid_continue(point))
            break;
    }
    *inout = (const char*)pos;
    return heap_strn(word, (size_t)((const char*)pos - word));
}

const char *get_id(const char **inout) {
    const char *pos = *inout;
    const char *word = get_word(&pos);
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
    int64_t starting_indent = sss_get_indent(ctx->file, pos);
    whitespace(&pos);
    if (sss_get_line_number(ctx->file, pos) == sss_get_line_number(ctx->file, *out))
        return false;

    if (sss_get_indent(ctx->file, pos) > starting_indent) {
        *out = pos;
        return true;
    }

    return false;
}

bool match_indentation(const char **out, int64_t target) {
    const char *pos = *out;
    for (int64_t indentation = 0; indentation < target; ) {
        switch (*pos) {
        case ' ': indentation += 1; ++pos; break;
        case '\t': indentation += 4; ++pos; break;
        default: return false;
        }
    }
    *out = pos;
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
    ast_t *expr;
    if (match(&pos, ":")) {
        expr = optional_ast(ctx, &pos, parse_type);
    } else {
        expr = optional_ast(ctx, &pos, parse_extended_expr);
    }
    if (!expr) return NULL;
    expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this expression");

    // Update the span to include the parens:
    return new(ast_t, .file=(ctx)->file, .start=start, .end=pos,
               .tag=expr->tag, .__data=expr->__data);
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

    if (match(&pos, "e") || match(&pos, "f")) // floating point literal
        return NULL;

    if (negative) i *= -1;

    if (match(&pos, "%")) {
        double d = (double)i / 100.;
        return NewAST(ctx->file, start, pos, Num, .n=d, .precision=64);
    }

    match(&pos, "_");
    int64_t precision = 64;
    if (match(&pos, "i64")) precision = 64;
    else if (match(&pos, "i32")) precision = 32;
    else if (match(&pos, "i16")) precision = 16;
    else if (match(&pos, "i8")) precision = 8;

    // else if (match(&pos, ".") || match(&pos, "e")) return NULL; // looks like a float

    return NewAST(ctx->file, start, pos, Int, .i=i, .precision=precision);
}

PARSER(parse_table_type) {
    const char *start = pos;
    if (!match(&pos, "{")) return NULL;
    whitespace(&pos);
    ast_t *key_type = optional_ast(ctx, &pos, parse_type);
    if (!key_type) return NULL;
    whitespace(&pos);
    if (!match(&pos, "=>")) return NULL;
    ast_t *value_type = expect_ast(ctx, start, &pos, parse_type, "I couldn't parse the rest of this table type");
    whitespace(&pos);
    expect_closing(ctx, &pos, "}", "I wasn't able to parse the rest of this table type");
    return NewAST(ctx->file, start, pos, TypeTable, .key_type=key_type, .value_type=value_type);
}

PARSER(parse_struct_type) {
    const char *start = pos;
    if (!match(&pos, "struct")) return NULL;
    spaces(&pos);
    if (!match(&pos, "(")) return NULL;
    args_t args = parse_args(ctx, &pos, false);
    whitespace(&pos);
    expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this struct type");
    return NewAST(ctx->file, start, pos, TypeStruct, .members=args);
}

PARSER(parse_func_type) {
    const char *start = pos;
    if (!match_word(&pos, "func")) return NULL;
    spaces(&pos);
    if (!match(&pos, "(")) return NULL;
    args_t args = parse_args(ctx, &pos, true);
    expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this function type");
    spaces(&pos);
    if (!match(&pos, "->")) return NULL;
    ast_t *ret = optional_ast(ctx, &pos, parse_type);
    return NewAST(ctx->file, start, pos, TypeFunction, .args=args, .ret_type=ret);
}

PARSER(parse_array_type) {
    const char *start = pos;
    if (!match(&pos, "[")) return NULL;
    ast_t *type = expect_ast(ctx, start, &pos, parse_type,
                             "I couldn't parse an array item type after this point");
    expect_closing(ctx, &pos, "]", "I wasn't able to parse the rest of this array type");
    return NewAST(ctx->file, start, pos, TypeArray, .item_type=type);
}

PARSER(parse_pointer_type) {
    const char *start = pos;
    bool optional = false, is_stack = false;
    if (match(&pos, "@"))
        optional = false;
    else if (match(&pos, "?"))
        optional = true;
    else if (match(&pos, "&"))
        is_stack = true;
    else
        return NULL;

    spaces(&pos);
    bool is_readonly = match(&pos, "(readonly)");
    spaces(&pos);
    ast_t *type = expect_ast(ctx, start, &pos, parse_type,
                             "I couldn't parse a pointer type after this point");
    return NewAST(ctx->file, start, pos, TypePointer, .pointed=type, .is_optional=optional, .is_stack=is_stack, .is_readonly=is_readonly);
}

// PARSER(parse_type_type) {
//     const char *start = pos;
//     if (!match_word(&pos, "Type")) return NULL;
//     spaces(&pos);
//     if (!match(&pos, "(")) return NULL;
//     ast_t *type = expect_ast(ctx, start, &pos, parse_type,
//                              "I couldn't parse a pointer type after this point");
//     expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this type");
//     return NewAST(ctx->file, start, pos, TypeTypeAST, .type=type);
// }

PARSER(parse_type_name) {
    const char *start = pos;
    const char *id = get_id(&pos);
    if (!id) return NULL;
    for (;;) {
        const char *next = pos;
        spaces(&next);
        if (!match(&next, ".")) break;
        const char *next_id = get_id(&next);
        if (!next_id) break;
        id = heap_strf("%s.%s", id, next_id);
        pos = next;
    }
    ast_t *ast = NewAST(ctx->file, start, pos, TypeVar, .name=id);
    ast_t *fielded = parse_field_suffix(ctx, ast);
    return fielded ? fielded : ast;
}

PARSER(parse_type) {
    const char *start = pos;
    ast_t *type = NULL;
    bool success = (false
        || (type=parse_enum_type(ctx, pos))
        || (type=parse_pointer_type(ctx, pos))
        // || (type=parse_type_type(ctx, pos))
        || (type=parse_array_type(ctx, pos))
        || (type=parse_table_type(ctx, pos))
        || (type=parse_struct_type(ctx, pos))
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
        type = new(ast_t, .file=(ctx)->file, .start=start, .end=pos,
                   .tag=type->tag, .__data=type->__data);
    }

    if (!type) return NULL;

    pos = type->end;
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
    else if (pos[len] != 'e' && pos[len] != 'f' && pos[len] != '%')
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
    match(&pos, "_");
    if (match(&pos, "f64")) precision = 64;
    else if (match(&pos, "f32")) precision = 32;

    if (match(&pos, "%")) {
        d /= 100.;
    }

    return NewAST(ctx->file, start, pos, Num, .n=d, .precision=precision);
}

PARSER(parse_array) {
    const char *start = pos;
    if (!match(&pos, "[")) return NULL;

    whitespace(&pos);

    auto items = EMPTY_ARRAY(ast_t*);
    ast_t *item_type = NULL;
    if (match(&pos, ":")) {
        whitespace(&pos);
        item_type = expect_ast(ctx, pos-1, &pos, parse_type, "I couldn't parse a type for this array");
    }

    for (;;) {
        whitespace(&pos);
        ast_t *item = optional_ast(ctx, &pos, parse_extended_expr);
        if (!item) break;
        item = optional_suffix_condition(ctx, item, &pos, NewAST(ctx->file, pos, pos, Skip));
        append(items, item);
        whitespace(&pos);
        if (!match(&pos, ",")) break;
    }
    whitespace(&pos);
    expect_closing(ctx, &pos, "]", "I wasn't able to parse the rest of this array");

    if (!item_type && LENGTH(items) == 0)
        parser_err(ctx, start, pos, "Empty arrays must specify what type they would contain (e.g. [:Int])");

    return NewAST(ctx->file, start, pos, Array, .type=item_type, .items=items);
}

PARSER(parse_table) {
    const char *start = pos;
    if (!match(&pos, "{")) return NULL;

    whitespace(&pos);

    auto entries = EMPTY_ARRAY(ast_t*);
    ast_t *key_type = NULL, *value_type = NULL;
    if (match(&pos, ":")) {
        whitespace(&pos);
        key_type = expect_ast(ctx, pos-1, &pos, parse_type, "I couldn't parse a key type for this table");
        whitespace(&pos);
        if (!match(&pos, "=>"))
            parser_err(ctx, pos, pos, "I expected an '=>' for this table type");
        value_type = expect_ast(ctx, pos-1, &pos, parse_type, "I couldn't parse a value type for this table");
    }

    for (;;) {
        whitespace(&pos);
        const char *entry_start = pos;
        ast_t *key = optional_ast(ctx, &pos, parse_extended_expr);
        if (!key) break;
        whitespace(&pos);
        if (!match(&pos, "=>")) return NULL;
        ast_t *value = expect_ast(ctx, pos-1, &pos, parse_expr, "I couldn't parse the value for this table entry");

        ast_t *entry = NewAST(ctx->file, entry_start, pos, TableEntry, .key=key, .value=value);
        for (bool progress = true; progress; ) {
            ast_t *new_entry;
            progress = (false
                || (new_entry=parse_index_suffix(ctx, entry))
                || (new_entry=parse_field_suffix(ctx, entry))
                || (new_entry=parse_suffix_for(ctx, entry))
                || (new_entry=parse_suffix_while(ctx, entry))
                || (new_entry=parse_fncall_suffix(ctx, entry, NEEDS_PARENS, NORMAL_FUNCTION))
            );
            if (progress) entry = new_entry;
        }
        entry = optional_suffix_condition(ctx, entry, &pos, NewAST(ctx->file, pos, pos, Skip));
        pos = entry->end;

        append(entries, entry);
        whitespace(&pos);
        if (!match(&pos, ",")) break;
    }

    if (!key_type && !value_type && LENGTH(entries) == 0)
        return NULL;

    whitespace(&pos);

    ast_t *fallback = NULL, *default_val = NULL;
    if (match(&pos, ";")) {
        for (;;) {
            whitespace(&pos);
            const char *attr_start = pos;
            if (match(&pos, "fallback")) {
                whitespace(&pos);
                if (!match(&pos, "=")) parser_err(ctx, attr_start, pos, "I expected an '=' after 'fallback'");
                if (fallback)
                    parser_err(ctx, attr_start, pos, "This table already has a fallback");
                fallback = expect_ast(ctx, attr_start, &pos, parse_expr, "I expected a fallback table");
            } else if (match(&pos, "default")) {
                whitespace(&pos);
                if (!match(&pos, "=")) parser_err(ctx, attr_start, pos, "I expected an '=' after 'default'");
                if (default_val)
                    parser_err(ctx, attr_start, pos, "This table already has a default value");
                default_val = expect_ast(ctx, attr_start, &pos, parse_expr, "I expected a default value for this table");
            } else {
                break;
            }
            whitespace(&pos);
            if (!match(&pos, ";")) break;
        }
    }

    whitespace(&pos);
    expect_closing(ctx, &pos, "}", "I wasn't able to parse the rest of this table");

    return NewAST(ctx->file, start, pos, Table, .key_type=key_type, .value_type=value_type, .entries=entries, .fallback=fallback, .default_value=default_val);
}

PARSER(parse_struct) {
    const char *start = pos;
    ast_t *type = optional_ast(ctx, &pos, parse_type_name);
    spaces(&pos);
    if (!match(&pos, "{")) return NULL;
    auto members = EMPTY_ARRAY(ast_t*);
    for (int i = 1; ; i++) {
        whitespace(&pos);
        const char *field_start = pos;
        const char* field_name = NULL;
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
        append(members, NewAST(ctx->file, field_start, pos, KeywordArg, .name=field_name, .arg=value));
        whitespace(&pos);
        match(&pos, ",");
    }
    whitespace(&pos);
    expect_closing(ctx, &pos, "}", "I wasn't able to parse the rest of this struct");
    return NewAST(ctx->file, start, pos, Struct, .type=type, .members=members);
}

ast_t *parse_field_suffix(parse_ctx_t *ctx, ast_t *lhs) {
    if (!lhs) return NULL;
    const char *pos = lhs->end;
    whitespace(&pos);
    if (!match(&pos, ".")) return NULL;
    if (*pos == '.') return NULL;
    whitespace(&pos);
    bool dollar = match(&pos, "$");
    const char* field = get_id(&pos);
    if (!field) return NULL;
    if (dollar) field = heap_strf("$%s", field);
    return NewAST(ctx->file, lhs->start, pos, FieldAccess, .fielded=lhs, .field=field);
}

PARSER(parse_ellipsis) {
    const char *start = pos;
    if (!match(&pos, "..")) return NULL;
    ast_t *last = optional_ast(ctx, &pos, parse_term);
    return NewAST(ctx->file, start, pos, Range, .last=last);
}

PARSER(parse_reduction) {
    const char *start = pos;
    if (!match(&pos, "(")) return NULL;

    spaces(&pos);
    const char *combo_start = pos;
    operator_e op = match_binary_operator(&pos);
    ast_t *combination;
    ast_t *lhs = NewAST(ctx->file, combo_start, combo_start, Var, .name="x.0");
    ast_t *rhs = NewAST(ctx->file, pos, pos, Var, .name="y.0");
    if (op == OP_MIN || op == OP_MAX) {
        ast_t *key = NewAST(ctx->file, pos, pos, Var, op == OP_MIN ? "_min_" : "_max_");
        for (bool progress = true; progress; ) {
            ast_t *new_term;
            progress = (false
                || (new_term=parse_index_suffix(ctx, key))
                || (new_term=parse_field_suffix(ctx, key))
                || (new_term=parse_fncall_suffix(ctx, key, NEEDS_PARENS, NORMAL_FUNCTION))
                );
            if (progress) key = new_term;
        }
        if (key->tag == Var) key = NULL;
        else pos = key->end;
        combination = op == OP_MIN ?
            NewAST(ctx->file, combo_start, pos, Min, .lhs=lhs, .rhs=rhs, .key=key)
            : NewAST(ctx->file, combo_start, pos, Max, .lhs=lhs, .rhs=rhs, .key=key);
    } else if (op == OP_MIX) {
        ast_t *key = expect_ast(ctx, start, &pos, parse_expr, "I expected an amount to mix by");
        if (!match_word(&pos, "of"))
            parser_err(ctx, pos, pos, "I expected the word 'of' here");
        combination = NewAST(ctx->file, start, pos, Mix, .lhs=lhs, .rhs=rhs, .key=key);
    } else if (op != OP_UNKNOWN) {
        combination = NewAST(ctx->file, combo_start, pos, BinaryOp, .op=op, .lhs=lhs, .rhs=rhs);
    } else {
        return NULL;
    }

    spaces(&pos);
    expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this reduction");

    ast_t *iter = optional_ast(ctx, &pos, parse_extended_expr);
    if (!iter) return NULL;

    ast_t *fallback = NULL;
    if (match_word(&pos, "else"))
        fallback = optional_ast(ctx, &pos, parse_extended_expr);

    return NewAST(ctx->file, start, pos, Reduction, .iter=iter, .combination=combination, .fallback=fallback);
}

ast_t *parse_index_suffix(parse_ctx_t *ctx, ast_t *lhs) {
    if (!lhs) return NULL;
    const char *start = lhs->start;
    const char *pos = lhs->end;
    if (!match(&pos, "[")) return NULL;
    whitespace(&pos);
    ast_t *index;
    if (match(&pos, ".")) {
        // array[.field]
        const char *field_start = pos-1;
        const char *field = get_id(&pos);
        if (!field) parser_err(ctx, field_start, pos, "I expected a field name here");
        index = NewAST(ctx->file, field_start, pos, FieldAccess, .field=field);
    } else {
        // obj[expr]
        index = optional_ast(ctx, &pos, parse_extended_expr);
    }
    whitespace(&pos);
    bool unchecked = match(&pos, ";") && (spaces(&pos), match_word(&pos, "unchecked") != 0);
    expect_closing(ctx, &pos, "]", "I wasn't able to parse the rest of this index");
    return NewAST(ctx->file, start, pos, Index, .indexed=lhs, .index=index, .unchecked=unchecked);
}

PARSER(parse_variant) {
    const char *start = pos;
    ast_t *type_var = optional_ast(ctx, &pos, parse_type_name);
    if (!type_var) return NULL;
    spaces(&pos);
    if (!match(&pos, "::")) return NULL;
    spaces(&pos);
    ast_t *value = expect_ast(ctx, start, &pos, parse_term_no_suffix, "I expected a term for this variant");
    return NewAST(ctx->file, start, pos, Variant, .type=type_var, .value=value);
}

ast_t *optional_suffix_condition(parse_ctx_t *ctx, ast_t *ast, const char **pos, ast_t *else_ast)
{
    const char *start = *pos;
    bool is_unless;
    if (match_word(pos, "if"))
        is_unless = false;
    else if (match_word(pos, "unless"))
        is_unless = true;
    else
        return ast;

    ast_t *subj = optional_ast(ctx, pos, parse_declaration);
    if (!subj) subj = expect_ast(ctx, start, pos, parse_expr, "I expected to find an expression for this 'if'");

    ast_t *pattern;
    if (match_word(pos, "matches")) {
        pattern = expect_ast(ctx, *pos, pos, parse_expr, "I expected a pattern to match here");
        if (is_unless)
            return NewAST(ctx->file, start, *pos, If, .subject=subj, .patterns=ARRAY(pattern, WrapAST(ast, Wildcard)),
                          .blocks=ARRAY(else_ast, ast));
        else
            return NewAST(ctx->file, start, *pos, If, .subject=subj, .patterns=ARRAY(pattern, WrapAST(ast, Wildcard)),
                          .blocks=ARRAY(ast, else_ast));
    } else {
        return NewAST(ctx->file, start, *pos, If, .subject=subj, .patterns=ARRAY(WrapAST(ast, Bool, .b=!is_unless), WrapAST(ast, Bool, .b=is_unless)),
                      .blocks=ARRAY(ast, else_ast));
    }
}

PARSER(parse_if) {
    // if <expr> [matches <pattern> (; <pattern>)* <body> (matches ...)*] [else <body>]
    const char *start = pos;
    int64_t starting_indent = sss_get_indent(ctx->file, pos);

    if (match_word(&pos, "unless")) {
        ast_t *subj = optional_ast(ctx, &pos, parse_declaration);
        if (!subj) subj = expect_ast(ctx, start, &pos, parse_expr, "I expected to find an expression for this 'unless'");

        ast_t *pattern = match_word(&pos, "matches") ? 
            expect_ast(ctx, pos, &pos, parse_expr, "I expected a pattern to match here")
            : NewAST(ctx->file, pos, pos, Bool, true);

        match_word(&pos, "then");
        ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'unless' statement"); 

        if (sss_get_indent(ctx->file, pos) == starting_indent && match_word(&pos, "else")) {
            ast_t *else_body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'else'"); 
            return NewAST(ctx->file, start, pos, If, .subject=subj, .patterns=ARRAY(pattern, NewAST(ctx->file, pos, pos, Wildcard)),
                          .blocks=ARRAY(else_body, body));
        } else {
            return NewAST(ctx->file, start, pos, If, .subject=subj, .patterns=ARRAY(pattern, NewAST(ctx->file, pos, pos, Wildcard)),
                          .blocks=ARRAY(NewAST(ctx->file, pos, pos, Pass), body));
        }
    }

    if (!match_word(&pos, "if"))
        return NULL;

    ast_t *subj = optional_ast(ctx, &pos, parse_declaration);
    if (!subj) subj = expect_ast(ctx, start, &pos, parse_expr,
                                 "I expected to find an expression for this 'if'");

    auto patterns = EMPTY_ARRAY(ast_t*);
    auto blocks = EMPTY_ARRAY(ast_t*);

    const char *tmp = pos;
    if (!match_word(&tmp, "matches")) {
        match_word(&pos, "then");
        ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'if' statement"); 
        append(patterns, NewAST(ctx->file, pos, pos, Bool, .b=true));
        append(blocks, body);

        tmp = pos;
        whitespace(&tmp);
        if (sss_get_indent(ctx->file, tmp) == starting_indent && match_word(&tmp, "else")) {
            pos = tmp;
            ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'else'"); 
            append(patterns, NewAST(ctx->file, pos, pos, Wildcard));
            append(blocks, body);
        }
        return NewAST(ctx->file, start, pos, If, .subject=subj, .patterns=patterns, .blocks=blocks);
    }

    for (;;) {
        const char *clause = pos;
        whitespace(&clause);
        if (sss_get_indent(ctx->file, clause) != starting_indent) break;

        if (match_word(&clause, "else")) {
            pos = clause;
            ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'else'"); 
            append(patterns, NewAST(ctx->file, clause, clause, Wildcard));
            append(blocks, body);
            break;
        }

        if (!match_word(&clause, "matches")) break;
        pos = clause;
        ast_t *pattern = expect_ast(ctx, pos, &pos, parse_expr, "I expected a pattern to match here");
        auto clause_patterns = ARRAY(pattern);
        while (spaces(&pos), match(&pos, ";")) {
            spaces(&pos);
            pattern = expect_ast(ctx, pos, &pos, parse_expr, "I expected a pattern to match here");
            if (!pattern) break;
            append(clause_patterns, pattern);
        }

        match_word(&pos, "then");
        ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'if' clause"); 
        for (int64_t i = 0, len = LENGTH(clause_patterns); i < len; i++) {
            append(patterns, ith(clause_patterns, i));
            append(blocks, body);
        }
    }

    return NewAST(ctx->file, start, pos, If, .subject=subj, .patterns=patterns, .blocks=blocks);
}

PARSER(parse_do) {
    // do [label] [<indent>] body [else else-body]
    const char *start = pos;
    if (!match_word(&pos, "do")) return NULL;
    int64_t starting_indent = sss_get_indent(ctx->file, pos);
    const char* label = get_id(&pos);
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'do'"); 
    ast_t *else_body = NULL;
    const char *else_start = pos;
    whitespace(&pos);
    if (sss_get_indent(ctx->file, pos) == starting_indent && match_word(&pos, "else"))
        else_body = expect_ast(ctx, else_start, &pos, parse_opt_indented_block, "I expected a body for this 'else'");
    else
        pos = else_start;
    return NewAST(ctx->file, start, pos, Do, .label=label, .body=body, .else_body=else_body);
}

PARSER(parse_defer) {
    // defer [<indent>] body
    const char *start = pos;
    if (!match_word(&pos, "defer")) return NULL;
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'defer'");
    return NewAST(ctx->file, start, pos, Defer, .body=body);
}

PARSER(parse_with) {
    // with [var :=] expr [then cleanup] body
    const char *start = pos;
    if (!match_word(&pos, "with")) return NULL;
    ast_t *var = optional_ast(ctx, &pos, parse_var);
    ast_t *expr;
    if (var && match_word(&pos, ":=")) {
        expr = expect_ast(ctx, var->start, &pos, parse_expr, "I expected an expression for this variable");
    } else {
        pos = var ? var->start : pos;
        var = NULL;
        expr = expect_ast(ctx, start, &pos, parse_expr, "I expected an expression for this 'with'");
    }
    ast_t *cleanup = NULL;
    if (match_word(&pos, "then"))
        cleanup = expect_ast(ctx, start, &pos, parse_statement, "I expected a cleanup expression for this 'with'");
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'with'");
    return NewAST(ctx->file, start, pos, With, .var=var, .expr=expr, .cleanup=cleanup, .body=body);
}

PARSER(parse_using) {
    // using expr *[; expr] body
    const char *start = pos;
    if (!match_word(&pos, "using")) return NULL;
    auto exprs = EMPTY_ARRAY(ast_t*);
    for (;;) {
        ast_t *expr = optional_ast(ctx, &pos, parse_expr);
        if (!expr) break;
        append(exprs, expr);
        spaces(&pos);
        if (!match(&pos, ";")) break;
        whitespace(&pos);
    }
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'using'");
    return NewAST(ctx->file, start, pos, Using, .used=exprs, .body=body);
}

PARSER(parse_for) {
    // for [k,] v in iter [do] [<indent>] body [<nodent> between [<indent>] body]
    const char *start = pos;
    if (!match_word(&pos, "for")) return NULL;
    int64_t starting_indent = sss_get_indent(ctx->file, pos);
    ast_t *index = expect_ast(ctx, start, &pos, parse_var, "I expected an iteration variable for this 'for'");
    spaces(&pos);
    ast_t *value = NULL;
    if (match(&pos, ",")) {
        value = expect_ast(ctx, pos-1, &pos, parse_var, "I expected a variable after this comma");
    }
    expect_str(ctx, start, &pos, "in", "I expected an 'in' for this 'for'");
    ast_t *iter = expect_ast(ctx, start, &pos, parse_expr, "I expected an iterable value for this 'for'");

    ast_t *first = NULL, *empty = NULL;
    if (match_word(&pos, "first")) {
        first = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'for'");
        whitespace(&pos);
    }

    if (sss_get_indent(ctx->file, pos) == starting_indent)
        match_word(&pos, first ? "then" : "do"); // optional

    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'for'"); 
    ast_t *between = NULL;
    const char *between_start = pos;
    whitespace(&between_start);
    if (sss_get_indent(ctx->file, between_start) == starting_indent && match_word(&between_start, "between")) {
        pos = between_start;
        between = expect_ast(ctx, pos, &pos, parse_opt_indented_block, "I expected a body for this 'between'");
    } else {
        pos = body->end;
    }

    const char *else_start = pos;
    whitespace(&else_start);
    if (sss_get_indent(ctx->file, else_start) == starting_indent && match_word(&else_start, "else")) {
        pos = else_start;
        empty = expect_ast(ctx, pos, &pos, parse_opt_indented_block, "I expected a body for this 'else'");
    }
    return NewAST(ctx->file, start, pos, For, .index=value ? index : NULL, .value=value ? value : index, .iter=iter,
                  .first=first, .body=body, .between=between, .empty=empty);
}

ast_t *parse_suffix_for(parse_ctx_t *ctx, ast_t *body) {
    if (!body) return NULL;
    const char *start = body->start;
    const char *pos = body->end;
    if (!match_word(&pos, "for")) return NULL;
    ast_t *index = expect_ast(ctx, start, &pos, parse_var, "I expected an iteration variable for this 'for'");
    spaces(&pos);
    ast_t *value = NULL;
    if (match(&pos, ","))
        value = expect_ast(ctx, pos-1, &pos, parse_var, "I expected a variable after this comma");
    expect_str(ctx, start, &pos, "in", "I expected an 'in' for this 'for'");
    ast_t *iter = expect_ast(ctx, start, &pos, parse_expr, "I expected an iterable value for this 'for'");
    body = optional_suffix_condition(ctx, body, &pos, NewAST(ctx->file, pos, pos, Skip));
    return NewAST(ctx->file, start, pos, For, .index=value ? index : NULL, .value=value ? value : index, .iter=iter, .body=body);
}

PARSER(parse_repeat) {
    // repeat [<indent>] body [<nodent> between [<indent>] body]
    const char *start = pos;
    if (!match_word(&pos, "repeat")) return NULL;
    int64_t starting_indent = sss_get_indent(ctx->file, pos);
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'repeat'"); 
    whitespace(&pos);
    ast_t *between = NULL;
    const char *between_start = NULL;
    if (sss_get_indent(ctx->file, pos) == starting_indent && match_word(&pos, "between")) {
        between = expect_ast(ctx, between_start, &pos, parse_opt_indented_block, "I expected a body for this 'between'");
    }
    return NewAST(ctx->file, start, pos, Repeat, .body=body, .between=between);
}

PARSER(parse_while) {
    // while condition [do] [<indent>] body [<nodent> between [<indent>] body]
    const char *start = pos;
    if (!match_word(&pos, "while")) return NULL;
    int64_t starting_indent = sss_get_indent(ctx->file, pos);
    ast_t *condition = expect_ast(ctx, start, &pos, parse_expr, "I don't see a viable condition for this 'while'");
    match(&pos, "do"); // optional
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'while'"); 
    const char *tmp = pos;
    whitespace(&tmp);
    ast_t *between = NULL;
    const char *between_start = NULL;
    if (sss_get_indent(ctx->file, tmp) == starting_indent && match_word(&tmp, "between")) {
        pos = tmp;
        between = expect_ast(ctx, between_start, &pos, parse_opt_indented_block, "I expected a body for this 'between'");
    }
    return NewAST(ctx->file, start, pos, While, .condition=condition, .body=body, .between=between);
}

ast_t *parse_suffix_while(parse_ctx_t *ctx, ast_t *body) {
    if (!body) return NULL;
    const char *start = body->start;
    const char *pos = body->end;
    if (!match_word(&pos, "while")) return NULL;
    ast_t *cond = expect_ast(ctx, start, &pos, parse_expr, "I don't see a viable condition for this 'while'");
    body = optional_suffix_condition(ctx, body, &pos, NewAST(ctx->file, pos, pos, Skip));
    return NewAST(ctx->file, start, pos, While, .condition=cond, .body=body);
}

ast_t *parse_unary(parse_ctx_t *ctx, const char *pos, ast_tag_e tag, const char *prefix, bool exprs_ok) {
    const char *start = pos;
    if (!match(&pos, prefix)) return NULL;
    if ((isalpha(prefix[0]) || prefix[0] == '_') && is_xid_continue_next(pos))
        return NULL;
    whitespace(&pos);
    ast_t *val = exprs_ok ? parse_expr(ctx, pos) : parse_term(ctx, pos);
    if (!val) return NULL;
    // Unsafe: all the unary ops have a '.value' field in the same spot, so this is a hack
    return new(ast_t, .file=ctx->file, .start=start, .end=val->end,
               .tag=tag, .__data.HeapAllocate.value=val);
    // End unsafe area
}
#define parse_heap_alloc(...) parse_unary(__VA_ARGS__, HeapAllocate, "@", false)
#define parse_stack_reference(...) parse_unary(__VA_ARGS__, StackReference, "&", false)
#define parse_typeinfo(...) parse_unary(__VA_ARGS__, GetTypeInfo, "_typeinfo_", true)
#define parse_sizeof(...) parse_unary(__VA_ARGS__, SizeOf, "sizeof", true)

PARSER(parse_not) {
    const char *start = pos;
    if (!match_word(&pos, "not")) return NULL;
    spaces(&pos);
    ast_t *val = expect_ast(ctx, start, &pos, parse_expr, "I expected an expression for this 'not'");
    return NewAST(ctx->file, start, pos, UnaryOp, .op=OP_NOT, .value=val);
}

PARSER(parse_negative) {
    const char *start = pos;
    if (!match(&pos, "-")) return NULL;
    spaces(&pos);
    ast_t *val = expect_ast(ctx, start, &pos, parse_term, "I expected an expression for this '-'");
    return NewAST(ctx->file, start, pos, UnaryOp, .op=OP_NEGATIVE, .value=val);
}

PARSER(parse_wildcard) {
    const char *start = pos;
    if (!match(&pos, "?")) return NULL;
    const char *name = get_id(&pos);
    return NewAST(ctx->file, start, pos, Wildcard, .name=name);
}

PARSER(parse_bool) {
    const char *start = pos;
    if (match_word(&pos, "yes"))
        return NewAST(ctx->file, start, pos, Bool, .b=true);
    else if (match_word(&pos, "no"))
        return NewAST(ctx->file, start, pos, Bool, .b=false);
    else
        return NULL;
}

PARSER(parse_splat) {
    const char *start = pos;
    if (!match(&pos, "++")) return NULL;
    ast_t *item = optional_ast(ctx, &pos, parse_expr);
    if (!item) return NULL;
    return NewAST(ctx->file, start, item->end, For,
                  .iter=item,
                  .value=WrapAST(item, Var, .name="item"),
                  .body=WrapAST(item, Var, .name="item"));
}

PARSER(parse_char) {
    const char *start = pos;
    if (*pos == '`') {
        ++pos;
        char c = *pos;
        ++pos;
        return NewAST(ctx->file, start, pos, Char, .c=c);
    } else if (*pos == '\\') {
        char c = unescape(&pos)[0];
        return NewAST(ctx->file, start, pos, Char, .c=c);
    } else {
        return NULL;
    }
}

PARSER(parse_interpolation) {
    const char *start = pos;
    ++pos; // ignore the initial character, typically a '$', but might be other stuff like '@' in different contexts
    bool labelled = match(&pos, ":");
    ast_t *value = optional_ast(ctx, &pos, parse_parens);
    if (!value) value = optional_ast(ctx, &pos, parse_term);
    if (!value) {
        match_group(&pos, '(');
        parser_err(ctx, start, pos, "This interpolation didn't parse");
    }
    return NewAST(ctx->file, start, pos, Interp, .value=value, .labelled=labelled);
}

PARSER(parse_string) {
    static const char closing[128] = {['(']=')', ['[']=']', ['<']='>', ['{']='}'};
    static const bool escapes[128] = {['\'']='\x1B', ['(']='\x1B', ['>']='\x1B', ['/']='\x1B'};
    static const char interps[128] = {['>']='@', ['/']='@', ['\'']='\x1A', ['(']='\x1A'};

    const char *string_start = pos;
    char open, close;
    if (match(&pos, "$")) {
        open = *pos;
        close = closing[(int)open] ? closing[(int)open] : open;
        ++pos;
    } else {
        if (*pos != '\'' && *pos != '"')
            return NULL;
        open = *pos;
        close = *pos;
        ++pos;
    }

    char interp_char = interps[(int)open] ? interps[(int)open] : '$';
    char escape_char = escapes[(int)open] ? escapes[(int)open] : '\\';

    if (open == ':' || open == '>')
        spaces(&pos);

    auto chunks = EMPTY_ARRAY(ast_t*);
    if (*pos == '\r' || *pos == '\n') { // Multiline string
        char special[] = {'\n','\r',interp_char,escape_char,'\0'};
        int64_t starting_indent = sss_get_indent(ctx->file, pos); 
        // indentation-delimited string
        match(&pos, "\r");
        match(&pos, "\n");
        int64_t first_line = sss_get_line_number(ctx->file, pos);
        int64_t indented = sss_get_indent(ctx->file, pos);
        pos = sss_get_line(ctx->file, first_line);
        while (pos < ctx->file->text + ctx->file->len) {
            const char *eol = strchrnul(pos, '\n');
            if (eol == pos + strspn(pos, " \t\r")) { // Empty line
                ast_t *ast = NewAST(ctx->file, pos, eol, StringLiteral, .str="\n");
                append(chunks, ast);
                pos = eol + 1;
                continue;
            }
            if (!match_indentation(&pos, starting_indent))
                parser_err(ctx, pos, strchrnul(pos, '\n'), "This isn't a valid indentation level for this unterminated string");

            if (*pos == close) {
                ++pos;
                goto finished;
            }

            if (!match_indentation(&pos, (indented - starting_indent)))
                parser_err(ctx, pos, strchrnul(pos, '\n'), "I was expecting this to have %lu extra indentation beyond %lu",
                           (indented - starting_indent), starting_indent);

            while (pos < eol+1) {
                size_t len = strcspn(pos, special);
                if (pos[len] == '\r') ++len;
                if (pos[len] == '\n') ++len;

                if (len > 0) {
                    ast_t *chunk = NewAST(ctx->file, pos, pos+len-1, StringLiteral, .str=heap_strn(pos, len));
                    append(chunks, chunk);
                }

                pos += len;

                if (*pos == escape_char) {
                    const char *start = pos;
                    const char* unescaped = unescape(&pos);
                    ast_t *chunk = NewAST(ctx->file, start, pos, StringLiteral, .str=unescaped);
                    append(chunks, chunk);
                    ++pos;
                } else if (*pos == interp_char) {
                    ast_t *chunk = parse_interpolation(ctx, pos);
                    append(chunks, chunk);
                    pos = chunk->end;
                }
            }
        }
      finished:;
        // Strip trailing newline:
        if (LENGTH(chunks) > 0) {
            ast_t *last_chunk = ith(chunks, LENGTH(chunks)-1);
            if (last_chunk->tag == StringLiteral) {
                auto str = Match(last_chunk, StringLiteral);
                const char* trimmed = heap_strn(str->str, strlen(str->str)-1);
                chunks[0][LENGTH(chunks)-1] = NewAST(ctx->file, last_chunk->start, last_chunk->end-1, StringLiteral, .str=trimmed);
            }
        }
    } else { // Inline string
        char special[] = {'\n','\r',open,close,interp_char,escape_char,'\0'};
        int depth = 1;
        while (depth > 0 && *pos) {
            size_t len = strcspn(pos, special);
            if (len > 0) {
                ast_t *chunk = NewAST(ctx->file, pos, pos+len-1, StringLiteral, .str=heap_strn(pos, len));
                append(chunks, chunk);
                pos += len;
            }

            if (*pos == interp_char) {
                ast_t *chunk = parse_interpolation(ctx, pos);
                append(chunks, chunk);
                pos = chunk->end;
            } else if (*pos == escape_char) {
                const char *start = pos;
                const char* unescaped = unescape(&pos);
                ast_t *chunk = NewAST(ctx->file, start, pos, StringLiteral, .str=unescaped);
                append(chunks, chunk);
            } else if (*pos == '\r' || *pos == '\n') {
                if (open == ' ' || open == ':' || open == '>') goto string_finished;
                parser_err(ctx, string_start, pos, "This line ended without closing the string");
            } else if (*pos == close) { // if open == close, then don't do nesting (i.e. check 'close' first)
                --depth;
                if (depth > 0) {
                    ast_t *chunk = NewAST(ctx->file, pos, pos+1, StringLiteral, .str=heap_strn(pos, 1));
                    append(chunks, chunk);
                }
                ++pos;
            } else if (*pos == open) {
                ++depth;
                ast_t *chunk = NewAST(ctx->file, pos, pos+1, StringLiteral, .str=heap_strn(pos, 1));
                append(chunks, chunk);
                ++pos;
            } else {
                ast_t *chunk = NewAST(ctx->file, pos, pos+1, StringLiteral, .str=heap_strn(pos, 1));
                ++pos;
                append(chunks, chunk);
            }
        }
    }
  string_finished:;
    return NewAST(ctx->file, string_start, pos, StringJoin, .children=chunks);
}

PARSER(parse_skip) {
    const char *start = pos;
    if (!match_word(&pos, "skip")) return NULL;
    spaces(&pos);
    const char* target;
    if (match_word(&pos, "for")) target = "for";
    else if (match_word(&pos, "while")) target = "while";
    else if (match_word(&pos, "repeat")) target = "repeat";
    else target = get_id(&pos);
    ast_t *skip = NewAST(ctx->file, start, pos, Skip, .target=target);
    return optional_suffix_condition(ctx, skip, &pos, NewAST(ctx->file, pos, pos, Pass));
}

PARSER(parse_stop) {
    const char *start = pos;
    if (!match_word(&pos, "stop")) return NULL;
    spaces(&pos);
    const char* target;
    if (match_word(&pos, "for")) target = "for";
    else if (match_word(&pos, "while")) target = "while";
    else if (match_word(&pos, "repeat")) target = "repeat";
    else target = get_id(&pos);
    ast_t *stop = NewAST(ctx->file, start, pos, Stop, .target=target);
    return optional_suffix_condition(ctx, stop, &pos, NewAST(ctx->file, pos, pos, Pass));
}

PARSER(parse_return) {
    const char *start = pos;
    if (!match_word(&pos, "return")) return NULL;
    spaces(&pos);
    ast_t *value = optional_ast(ctx, &pos, parse_expr);
    ast_t *ret = NewAST(ctx->file, start, pos, Return, .value=value);
    if (!value) ret = optional_suffix_condition(ctx, ret, &pos, NewAST(ctx->file, pos, pos, Pass));
    return ret;
}

PARSER(parse_lambda) {
    const char *start = pos;
    if (!match_word(&pos, "func"))
        return NULL;
    spaces(&pos);
    if (!match(&pos, "("))
        return NULL;
    args_t args = parse_args(ctx, &pos, false);
    spaces(&pos);
    expect_closing(ctx, &pos, ")", "I was expecting a ')' to finish this anonymous function's arguments");
    ast_t *body = optional_ast(ctx, &pos, parse_opt_indented_block);
    return NewAST(ctx->file, start, pos, Lambda, .args=args, .body=body);
}

PARSER(parse_nil) {
    const char *start = pos;
    if (!match(&pos, "!")) return NULL;
    ast_t *type = parse_type(ctx, pos);
    if (!type) return NULL;
    return NewAST(ctx->file, start, type->end, Nil, .type=type);
}

PARSER(parse_fail) {
    const char *start = pos;
    if (!match_word(&pos, "fail")) return NULL;
    ast_t *msg = parse_term(ctx, pos);
    if (msg) pos = msg->end;
    ast_t *fail = NewAST(ctx->file, start, pos, Fail, .message=msg);
    return optional_suffix_condition(ctx, fail, &pos, NewAST(ctx->file, pos, pos, Pass));
}

PARSER(parse_var) {
    const char *start = pos;
    const char* name = get_id(&pos);
    if (!name) return NULL;
    return NewAST(ctx->file, start, pos, Var, .name=name);
}

PARSER(parse_bitcast) {
    const char *start = pos;
    if (!match_word(&pos, "bitcast")) return NULL;
    ast_t *expr = expect_ast(ctx, start, &pos, parse_term, "I expected an expression here");
    if (!match_word(&pos, "as")) parser_err(ctx, start, pos, "I expected a 'as' and type for this bitcast");
    ast_t *t = expect_ast(ctx, start, &pos, parse_type, "I couldn't parse the type for this bitcast");
    return NewAST(ctx->file, start, pos, Cast, .value=expr, .type=t, .bitcast=true);
}

PARSER(parse_term_no_suffix) {
    spaces(&pos);
    ast_t *term = NULL;
    (void)(
        false
        || (term=parse_nil(ctx, pos))
        || (term=parse_ellipsis(ctx, pos))
        || (term=parse_num(ctx, pos))
        || (term=parse_int(ctx, pos))
        || (term=parse_negative(ctx, pos))
        || (term=parse_heap_alloc(ctx, pos))
        || (term=parse_wildcard(ctx, pos))
        || (term=parse_stack_reference(ctx, pos))
        || (term=parse_splat(ctx, pos))
        || (term=parse_bool(ctx, pos))
        || (term=parse_char(ctx, pos))
        || (term=parse_string(ctx, pos))
        || (term=parse_lambda(ctx, pos))
        || (term=parse_parens(ctx, pos))
        || (term=parse_table(ctx, pos))
        || (term=parse_struct(ctx, pos))
        || (term=parse_variant(ctx, pos)) // This needs to come before 'var'
        || (term=parse_var(ctx, pos))
        || (term=parse_array(ctx, pos))
        || (term=parse_reduction(ctx, pos))
        || (term=parse_fail(ctx, pos))
        || (term=parse_skip(ctx, pos))
        || (term=parse_stop(ctx, pos))
        || (term=parse_return(ctx, pos))
        || (term=parse_bitcast(ctx, pos))
        || (term=parse_typeinfo(ctx, pos))
        || (term=parse_sizeof(ctx, pos))
        || (term=parse_not(ctx, pos))
        || (term=parse_extern(ctx, pos))
        );
    return term;
}

PARSER(parse_term) {
    ast_t *term = parse_term_no_suffix(ctx, pos);
    if (!term) return NULL;

    for (bool progress = true; progress; ) {
        ast_t *new_term;
        progress = (false
            || (new_term=parse_index_suffix(ctx, term))
            || (new_term=parse_field_suffix(ctx, term))
            || (new_term=parse_fncall_suffix(ctx, term, NEEDS_PARENS, NORMAL_FUNCTION))
            );
        if (progress) term = new_term;
    }
    return term;
}

ast_t *parse_fncall_suffix(parse_ctx_t *ctx, ast_t *fn, bool needs_parens, bool is_extern) {
    if (!fn) return NULL;
    const char *start = fn->start;
    const char *pos = fn->end;
    auto args = EMPTY_ARRAY(ast_t*);

    // No arguments fn()
    if (match(&pos, "(")) {
        spaces(&pos);
        if (match(&pos, ")"))
            goto return_function;
        pos = fn->end;
    }

    const char *paren_pos = pos;
    if (match(&pos, "{")) return NULL;
    if (match(&pos, "(")) {
        whitespace(&pos);
        needs_parens = true;
    } else if (needs_parens) {
        return NULL;
    }

    spaces(&pos);

    for (;;) {
        const char *arg_start = pos;
        const char* name = get_id(&pos);
        spaces(&pos);
        if (LENGTH(args) == 0 && !needs_parens) {
            // Prevent matching infix ops:
            switch (*pos) {
            case '.':
            case '<': case '>': case '=': case ':': case '!':
            case '+': case '*': case '/': case '^': return NULL;
            case '-': {
                if (pos[-1] == ' ' && pos[1] != ' ')
                    break;
                else return NULL;
            }
            case 'a': if (match_word(&pos, "and")) return NULL; else break;
            case 'o': if (match_word(&pos, "or")) return NULL; else break;
            case 'x': if (match_word(&pos, "xor")) return NULL; else break;
            case 'm': if (match_word(&pos, "mod") || match_word(&pos, "mod1")) return NULL; else break;
            default: break;
            }
        }
        if (name) {
            if (match(&pos, "=")) {
                spaces(&pos);
                ast_t *arg = parse_expr(ctx, pos);
                if (!arg) parser_err(ctx, arg_start, pos, "I couldn't parse this keyword argument value");
                ast_t *kwarg = NewAST(ctx->file, arg_start, arg->end, KeywordArg,
                                      .name=name, .arg=arg);
                append(args, kwarg);
                pos = kwarg->end;
                goto got_arg;
            }
            pos = arg_start;
        }

        ast_t *arg = parse_expr(ctx, pos);
        if (!arg) break;
        append(args, arg);
        pos = arg->end;

      got_arg:
        ;

        spaces(&pos);
        if (!match(&pos, ","))
            break;
        whitespace(&pos);
    }

    spaces(&pos);
    if (needs_parens && !match(&pos, ")"))
        parser_err(ctx, paren_pos, pos, "This parenthesis is unclosed");

    if (LENGTH(args) < 1)
        return NULL;

  return_function:;
    ast_t *extern_return_type = NULL;
    if (is_extern) {
        if (match(&pos, ":"))
            extern_return_type = expect_ast(ctx, start, &pos, parse_type, "I couldn't parse the return type of this external function call");
        else
            extern_return_type = NewAST(ctx->file, pos, pos, TypeVar, .name="Void");
    }
    return NewAST(ctx->file, start, pos, FunctionCall, .fn=fn, .args=args, .extern_return_type=extern_return_type);
}

operator_e match_binary_operator(const char **pos)
{
    switch (**pos) {
    case '+': {
        *pos += 1;
        return match(pos, "+") ? OP_CONCAT : OP_PLUS;
    }
    case '-': {
        *pos += 1;
        if ((*pos)[0] != ' ' && (*pos)[-2] == ' ') // looks like `fn -5`
            return OP_UNKNOWN;
        return OP_MINUS;
    }
    case '*': *pos += 1; return OP_MULT;
    case '/': *pos += 1; return OP_DIVIDE;
    case '^': *pos += 1; return OP_POWER;
    case '<': *pos += 1; return match(pos, "=") ? OP_LE : (match(pos, "<") ? OP_LSHIFT : OP_LT);
    case '>': *pos += 1; return match(pos, "=") ? OP_GE : (match(pos, ">") ? OP_RSHIFT : OP_GT);
    default: {
        if (match(pos, "!=")) return OP_NE;
        else if (match(pos, "==") && **pos != '=') return OP_EQ;
        else if (match_word(pos, "and")) return OP_AND;
        else if (match_word(pos, "or")) return OP_OR;
        else if (match_word(pos, "xor")) return OP_XOR;
        else if (match_word(pos, "mod1")) return OP_MOD1;
        else if (match_word(pos, "mod")) return OP_MOD;
        else if (match_word(pos, "as")) return OP_AS;
        else if (match_word(pos, "by")) return OP_BY;
        else if (match_word(pos, "not in")) return OP_NOT_IN;
        else if (match_word(pos, "in")) return OP_IN;
        else if (match_word(pos, "_min_")) return OP_MIN;
        else if (match_word(pos, "_max_")) return OP_MAX;
        else if (match_word(pos, "_mix_")) return OP_MIX;
        else if (match(pos, "..")) return OP_RANGE;
        else return OP_UNKNOWN;
    }
    }
}

ast_t *parse_expr(parse_ctx_t *ctx, const char *pos) {
    ast_t *term = parse_term(ctx, pos);
    if (!term) return NULL;

    pos = term->end;

    auto terms = ARRAY(term);
    auto binops = EMPTY_ARRAY(operator_e);
    auto minmax_keys = EMPTY_ARRAY(ast_t*);
    for (;;) {
        spaces(&pos);
        operator_e op = match_binary_operator(&pos);
        if (op == OP_UNKNOWN) break;

        ast_t *key = NULL;
        if (op == OP_MIN || op == OP_MAX) {
            key = NewAST(ctx->file, pos, pos, Var, op == OP_MIN ? "_min_" : "_max_");
            for (bool progress = true; progress; ) {
                ast_t *new_term;
                progress = (false
                    || (new_term=parse_index_suffix(ctx, key))
                    || (new_term=parse_field_suffix(ctx, key))
                    || (new_term=parse_fncall_suffix(ctx, key, NEEDS_PARENS, NORMAL_FUNCTION))
                    );
                if (progress) key = new_term;
            }
            if (key->tag == Var) key = NULL;
            else pos = key->end;
        } else if (op == OP_MIX) {
            key = expect_ast(ctx, pos, &pos, parse_expr, "I expected an amount to mix by");
            if (!match_word(&pos, "of"))
                parser_err(ctx, pos, pos, "I expected the word 'of' here");
        }

        assert(op_tightness[op]);
        const char *next = pos;
        if (op == OP_RANGE)
            spaces(&next);
        else
            whitespace(&next);
        ast_t *rhs = op == OP_AS ? parse_type(ctx, next) : parse_term(ctx, next);
        if (!rhs && op == OP_RANGE) {
            ast_t **prev_addr = ith_addr(terms, LENGTH(terms)-1);
            *prev_addr = NewAST(ctx->file, next, next, Range, .first=*prev_addr);
            continue;
        }
        if (!rhs) break;
        pos = rhs->end;
        append(terms, rhs);
        append(binops, op);
        append(minmax_keys, key);
    }

    // Sort out the op precedence (everything is left-associative here)
    while (LENGTH(terms) > 1) {
        // Find tightest-binding op
        int tightest_op = 0;
        for (int64_t i = 1; i < LENGTH(binops); i++) {
            if (op_tightness[ith(binops, i)]
                < op_tightness[ith(binops, tightest_op)]) {
                tightest_op = i;
            }
        }

        auto op = ith(binops, tightest_op);
        ast_t *key = ith(minmax_keys, tightest_op);
        // Bind two terms into one:
        remove(binops, tightest_op);
        remove(minmax_keys, tightest_op);
        ast_t *lhs = ith(terms, tightest_op);
        ast_t *rhs = ith(terms, tightest_op + 1);

        ast_t *merged;
        switch (op) {
        case OP_RANGE:
            merged = NewAST(ctx->file, lhs->start, rhs->end, Range, .first=lhs, .last=rhs);
            break;
        case OP_AS:
            merged = NewAST(ctx->file, lhs->start, rhs->end, Cast, .value=lhs, .type=rhs);
            break;
        case OP_BY: {
            if (lhs->tag != Range)
                parser_err(ctx, lhs->start, rhs->end, "This 'by' is not attached to a Range");

            merged = NewAST(ctx->file, lhs->start, rhs->end, Range,
                            .first=Match(lhs, Range)->first,
                            .last=Match(lhs, Range)->last,
                            .step=rhs);
            break;
        }
        case OP_MIN:
            merged = NewAST(ctx->file, lhs->start, rhs->end, Min, .lhs=lhs, .rhs=rhs, .key=key);
            break;
        case OP_MAX:
            merged = NewAST(ctx->file, lhs->start, rhs->end, Max, .lhs=lhs, .rhs=rhs, .key=key);
            break;
        case OP_MIX:
            merged = NewAST(ctx->file, lhs->start, rhs->end, Mix, .lhs=lhs, .rhs=rhs, .key=key);
            break;
        case OP_EQ: case OP_NE: case OP_LT: case OP_LE: case OP_GT: case OP_GE:
            merged = NewAST(ctx->file, lhs->start, rhs->end, Comparison, .lhs=lhs, .rhs=rhs, .op=op);
            break;
        default:
            merged = NewAST(ctx->file, lhs->start, rhs->end, BinaryOp, .lhs=lhs, .rhs=rhs, .op=op);
            break;
        }

        remove(terms, tightest_op);
        terms[0][tightest_op] = merged;
        // assert(ith(terms, tightest_op) == merged);
    }

    ast_t *expr = ith(terms, 0);
    return expr;
}

PARSER(parse_declaration) {
    const char *start = pos;
    ast_t *var = parse_var(ctx, pos);
    if (!var) return NULL;
    pos = var->end;
    spaces(&pos);
    if (!match(&pos, ":=")) return NULL;
    spaces(&pos);
    ast_t *val = optional_ast(ctx, &pos, parse_use);
    if (!val) val = optional_ast(ctx, &pos, parse_extended_expr);
    if (!val) parser_err(ctx, pos, strchrnul(pos, '\n'), "This declaration value didn't parse");
    return NewAST(ctx->file, start, pos, Declare, .var=var, .value=val, .is_public=false);
}

PARSER(parse_update) {
    const char *start = pos;
    ast_t *lhs = optional_ast(ctx, &pos, parse_expr);
    if (!lhs) return NULL;
    spaces(&pos);
    operator_e op;
    if (match(&pos, "+=")) op = OP_PLUS;
    else if (match(&pos, "++=")) op = OP_CONCAT;
    else if (match(&pos, "-=")) op = OP_MINUS;
    else if (match(&pos, "*=")) op = OP_MULT;
    else if (match(&pos, "/=")) op = OP_DIVIDE;
    else if (match(&pos, "and=")) op = OP_AND;
    else if (match(&pos, "or=")) op = OP_OR;
    else if (match(&pos, "xor=")) op = OP_XOR;
    else return NULL;
    ast_t *rhs = expect_ast(ctx, start, &pos, parse_extended_expr, "I expected an expression here");
    return NewAST(ctx->file, start, pos, UpdateAssign, .lhs=lhs, .rhs=rhs, .op=op);
}

PARSER(parse_assignment) {
    const char *start = pos;
    auto targets = EMPTY_ARRAY(ast_t*);
    for (;;) {
        ast_t *lhs = optional_ast(ctx, &pos, parse_term);
        if (!lhs) break;
        append(targets, lhs);
        spaces(&pos);
        if (!match(&pos, ",")) break;
        whitespace(&pos);
    }

    if (LENGTH(targets) == 0) return NULL;

    spaces(&pos);
    if (!match(&pos, "=")) return NULL;
    if (match(&pos, "=")) return NULL; // == comparison

    auto values = EMPTY_ARRAY(ast_t*);
    for (;;) {
        ast_t *rhs = optional_ast(ctx, &pos, parse_extended_expr);
        if (!rhs) break;
        append(values, rhs);
        spaces(&pos);
        if (!match(&pos, ",")) break;
        whitespace(&pos);
    }

    return NewAST(ctx->file, start, pos, Assign, .targets=targets, .values=values);
}

PARSER(parse_statement) {
    ast_t *stmt = NULL;
    if ((stmt=parse_declaration(ctx, pos))
        || (stmt=parse_doctest(ctx, pos))
        || (stmt=parse_func_def(ctx, pos))
        || (stmt=parse_convert_def(ctx, pos))
        || (stmt=parse_use(ctx,pos)))
        return stmt;

    if (!(false 
        || (stmt=parse_update(ctx, pos))
        || (stmt=parse_assignment(ctx, pos))
    ))
        stmt = parse_extended_expr(ctx, pos);
    
    for (bool progress = (stmt != NULL); progress; ) {
        ast_t *new_stmt;
        progress = (false
            || (new_stmt=parse_suffix_for(ctx, stmt))
            || (new_stmt=parse_suffix_while(ctx, stmt))
        );

        if (stmt->tag == Var && !progress)
            progress = (new_stmt=parse_fncall_suffix(ctx, stmt, OPTIONAL_PARENS, NORMAL_FUNCTION));

        if (progress) stmt = new_stmt;
    }
    return stmt;

}

PARSER(parse_extended_expr) {
    ast_t *expr = NULL;

    if (false
        || (expr=optional_ast(ctx, &pos, parse_for))
        || (expr=optional_ast(ctx, &pos, parse_while))
        || (expr=optional_ast(ctx, &pos, parse_if))
        || (expr=optional_ast(ctx, &pos, parse_repeat))
        || (expr=optional_ast(ctx, &pos, parse_do))
        || (expr=optional_ast(ctx, &pos, parse_defer))
        || (expr=optional_ast(ctx, &pos, parse_with))
        || (expr=optional_ast(ctx, &pos, parse_using))
        )
        return expr;

    if (!(false
          || (expr=parse_fncall_suffix(ctx, parse_term(ctx, pos), NEEDS_PARENS, NORMAL_FUNCTION))
          || (expr=parse_expr(ctx, pos))
          || (expr=parse_term(ctx, pos))
          ))
        return NULL;

    for (bool progress = true; progress; ) {
        ast_t *new_stmt;
        progress = (false
            || (new_stmt=parse_index_suffix(ctx, expr))
            || (new_stmt=parse_field_suffix(ctx, expr))
            || (new_stmt=parse_suffix_for(ctx, expr))
            || (new_stmt=parse_suffix_while(ctx, expr))
            || (new_stmt=parse_fncall_suffix(ctx, expr, NEEDS_PARENS, NORMAL_FUNCTION))
            );
        if (progress) expr = new_stmt;
    }
    return expr;
}

PARSER(parse_block) {
    int64_t block_indent = sss_get_indent(ctx->file, pos);
    const char *start = pos;
    whitespace(&pos);
    auto statements = EMPTY_ARRAY(ast_t*);
    while (*pos) {
        ast_t *stmt = optional_ast(ctx, &pos, parse_statement);
        if (!stmt) {
            spaces(&pos);
            if (*pos && *pos != '\r' && *pos != '\n')
                parser_err(ctx, pos, strchrnul(pos, '\n'), "I couldn't parse this line");
            break;
        }
        append(statements, stmt);
        whitespace(&pos);
        if (sss_get_indent(ctx->file, pos) != block_indent) {
            pos = stmt->end; // backtrack
            break;
        }
    }
    return NewAST(ctx->file, start, pos, Block, .statements=statements);
}

PARSER(parse_opt_indented_block) {
    return indent(ctx, &pos) ? parse_block(ctx, pos) : parse_inline_block(ctx, pos);
}

PARSER(parse_namespace) {
    const char *start = pos;
    whitespace(&pos);
    int64_t indent = sss_get_indent(ctx->file, pos);
    auto statements = EMPTY_ARRAY(ast_t*);
    for (;;) {
        const char *next = pos;
        whitespace(&next);
        if (sss_get_indent(ctx->file, next) != indent) break;
        ast_t *stmt;
        if ((stmt=optional_ast(ctx, &pos, parse_type_def))
            ||(stmt=optional_ast(ctx, &pos, parse_linker))
            ||(stmt=optional_ast(ctx, &pos, parse_statement)))
        {
            append(statements, stmt);
            pos = stmt->end;
            whitespace(&pos);
        } else {
            if (sss_get_indent(ctx->file, next) > indent && next < strchrnul(next, '\n'))
                parser_err(ctx, next, strchrnul(next, '\n'), "I couldn't parse this namespace statement");
            break;
        }
    }
    return NewAST(ctx->file, start, pos, Block, .statements=statements, .is_namespace=true);
}

PARSER(parse_type_def) {
    // type Foo := Type... \n body...
    const char *start = pos;
    if (!match_word(&pos, "type")) return NULL;

    int64_t starting_indent = sss_get_indent(ctx->file, pos);

    ast_t *name = optional_ast(ctx, &pos, parse_type_name);
    if (!name) return NULL;
    spaces(&pos);

    if (!match(&pos, ":=")) return NULL;
    ast_t *type_ast = expect_ast(ctx, start, &pos, parse_type, "I expected a type after this ':='");

    const char *ns_pos = pos;
    whitespace(&ns_pos);
    int64_t ns_indent = sss_get_indent(ctx->file, ns_pos);
    ast_t *namespace = NULL;
    if (ns_indent > starting_indent) {
        pos = ns_pos;
        namespace = optional_ast(ctx, &pos, parse_namespace);
    }
    if (!namespace)
        namespace = NewAST(ctx->file, pos, pos, Block, .statements=EMPTY_ARRAY(ast_t*), .is_namespace=true);
    return NewAST(ctx->file, start, pos, TypeDef, .name=name, .type=type_ast, .namespace=namespace);
}

PARSER(parse_enum_type) {
    // tagged union: enum Foo := a|b(x:Int,y:Int)=5|...
    const char *start = pos;

    if (!match_word(&pos, "enum")) return NULL;
    spaces(&pos);
    if (!match(&pos, "(")) return NULL;

    auto tag_names = EMPTY_ARRAY(const char*);
    auto tag_values = EMPTY_ARRAY(int64_t);
    auto tag_args = EMPTY_ARRAY(args_t);
    int64_t next_value = 0;

    whitespace(&pos);
    for (;;) {
        const char *tag_start = pos;

        spaces(&pos);
        if (match(&pos, ":")) {
            spaces(&pos);
            ast_t *type_ast = expect_ast(ctx, tag_start, &pos, parse_type, "I expected a type after this ':'");
            char *name = GC_malloc_atomic(type_ast->end - type_ast->start + 1);
            char *dest = name;
            for (const char *src = type_ast->start; src < type_ast->end; src++) {
                if (isalnum(*src) || *src == '_')
                    *(dest++) = *src;
            }
            *dest = '\0';
            append(tag_names, name);
            append(tag_values, next_value);
            args_t args = (args_t){ARRAY(FakeAST(Var, .name="value")), ARRAY(type_ast), ARRAY((ast_t*)NULL)};
            append(tag_args, args);
            pos = type_ast->end;
            goto carry_on;
        }

        const char *tag_name = get_id(&pos);
        if (!tag_name) break;

        spaces(&pos);
        args_t args;
        if (match(&pos, "(")) {
            whitespace(&pos);
            args = parse_args(ctx, &pos, false);
            whitespace(&pos);
            expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this tagged union member");
        } else {
            args = (args_t){EMPTY_ARRAY(ast_t*), EMPTY_ARRAY(ast_t*), EMPTY_ARRAY(ast_t*)};
        }

        spaces(&pos);
        if (match(&pos, "=")) {
            ast_t *val = expect_ast(ctx, tag_start, &pos, parse_int, "I expected an integer literal after this '='");
            next_value = Match(val, Int)->i;
        }

        // Check for duplicate values:
        for (int64_t i = 0, len = LENGTH(tag_values); i < len; i++) {
            if (ith(tag_values, i) == next_value)
                parser_err(ctx, tag_start, pos, "This tag value (%ld) is a duplicate of an earlier tag value", next_value);
        }

        append(tag_names, tag_name);
        append(tag_values, next_value);
        append(tag_args, args);

      carry_on:
        const char *next_pos = pos;
        whitespace(&next_pos);
        if (!match(&next_pos, "|"))
            break;
        whitespace(&next_pos);
        pos = next_pos;
        ++next_value;
    }

    whitespace(&pos);
    expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this enum definition");

    return NewAST(ctx->file, start, pos, TypeTaggedUnion, .tag_names=tag_names, .tag_values=tag_values,
                  .tag_args=tag_args);
}

args_t parse_args(parse_ctx_t *ctx, const char **pos, bool allow_unnamed)
{
    args_t args = {EMPTY_ARRAY(ast_t*), EMPTY_ARRAY(ast_t*), EMPTY_ARRAY(ast_t*)};
    for (;;) {
        const char *batch_start = *pos;
        int64_t first = LENGTH(args.args);
        ast_t *default_val = NULL;
        ast_t *type = NULL;
        for (;;) {
            whitespace(pos);
            ast_t *name = optional_ast(ctx, pos, parse_var);
            whitespace(pos);
            if (strncmp(*pos, "==", 2) != 0 && match(pos, "=")) {
                default_val = expect_ast(ctx, *pos-1, pos, parse_term, "I expected a value after this '='");
                append(args.args, name);
                break;
            } else if (match(pos, ":")) {
                type = expect_ast(ctx, *pos-1, pos, parse_type, "I expected a type here");
                append(args.args, name);
                break;
            } else if (allow_unnamed) {
                *pos = name->start;
                type = optional_ast(ctx, pos, parse_type);
                if (type)
                    append(args.args, NULL);
                break;
            } else if (name) {
                append(args.args, name);
                spaces(pos);
                if (!match(pos, ",")) break;
            } else {
                break;
            }
        }
        if (LENGTH(args.args) == first) break;
        if (!default_val && !type)
            parser_err(ctx, batch_start, *pos, "I expected a ':' and type, or '=' and a default value after this parameter (%s)",
                       Match(ith(args.args, first), Var)->name);

        for (int64_t i = first; i < LENGTH(args.args); i++) {
            append(args.defaults, default_val);
            append(args.types, type);
        }
        whitespace(pos);
        match(pos, ",");
    }

    return args;
}

PARSER(parse_func_def) {
    const char *start = pos;
    if (!match_word(&pos, "func")) return NULL;

    ast_t *name = optional_ast(ctx, &pos, parse_var);
    if (!name) return NULL;

    spaces(&pos);

    if (!match(&pos, "(")) return NULL;

    args_t args = parse_args(ctx, &pos, false);
    whitespace(&pos);
    bool is_inline = false;
    ast_t *cache_ast = NULL;
    for (; whitespace(&pos), (match(&pos, ";") || match(&pos, ",")); ) {
        const char *flag_start = pos;
        if (match_word(&pos, "inline")) {
            is_inline = true;
        } else if (match_word(&pos, "cached")) {
            if (!cache_ast) cache_ast = NewAST(ctx->file, pos, pos, Int, .i=INT64_MAX, .precision=64);
        } else if (match_word(&pos, "cache_size")) {
            if (whitespace(&pos), !match(&pos, "="))
                parser_err(ctx, flag_start, pos, "I expected a value for 'cache_size'");
            whitespace(&pos);
            cache_ast = expect_ast(ctx, start, &pos, parse_expr, "I expected a maximum size for the cache");
        }
    }
    expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this function definition");

    ast_t *ret_type = NULL;
    spaces(&pos);
    if (match(&pos, "->") || match(&pos, ":"))
        ret_type = optional_ast(ctx, &pos, parse_type);

    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block,
                             "This function needs a body block");
    return NewAST(ctx->file, start, pos, FunctionDef,
                  .name=name, .args=args, .ret_type=ret_type, .body=body, .cache=cache_ast,
                  .is_inline=is_inline);
}

PARSER(parse_convert_def) {
    // convert x:Foo as Baz
    const char *start = pos;
    if (!match_word(&pos, "convert")) return NULL;

    ast_t *name = optional_ast(ctx, &pos, parse_var);
    if (!name) return NULL;

    spaces(&pos);
    if (!match(&pos, ":")) return NULL;

    ast_t *source_type = expect_ast(ctx, start, &pos, parse_type, "I expected a conversion source type here");
    expect_str(ctx, start, &pos, "as", "I expected an 'as' for a conversion definition");
    ast_t *target_type = expect_ast(ctx, start, &pos, parse_type, "I expected a conversion target type here");
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a function body for this conversion definition"); 
    return NewAST(ctx->file, start, pos, ConvertDef, .var=name, .source_type=source_type, .target_type=target_type, .body=body);
}

PARSER(parse_extern) {
    const char *start = pos;
    if (!match_word(&pos, "extern")) return NULL;
    spaces(&pos);
    bool address = (match(&pos, "&") != 0);
    const char* name = get_id(&pos);
    spaces(&pos);
    // extern function call:
    if (match(&pos, "(")) {
        return parse_fncall_suffix(ctx, NewAST(ctx->file, start, pos-1, Var, .name=name), NEEDS_PARENS, EXTERN_FUNCTION);
    }
    if (!match(&pos, ":"))
        parser_err(ctx, start, pos, "I couldn't get a type for this extern");
    ast_t *type = expect_ast(ctx, start, &pos, parse_type, "I couldn't parse the type for this extern");
    return NewAST(ctx->file, start, pos, Extern, .name=name, .type=type, .address=address);
}

PARSER(parse_doctest) {
    const char *start = pos;
    if (!match(&pos, ">>>")) return NULL;
    spaces(&pos);
    ast_t *expr = expect_ast(ctx, start, &pos, parse_statement, "I couldn't parse the expression for this doctest");
    whitespace(&pos);
    const char* output = NULL;
    if (match(&pos, "===")) {
        spaces(&pos);
        const char *output_start = pos,
                   *output_end = strchrnul(pos, '\n');
        if (output_end <= output_start)
            parser_err(ctx, output_start, output_end, "You're missing expected output here");
        output = heap_strn(output_start, (size_t)(output_end - output_start));
        pos = output_end;
    }
    return NewAST(ctx->file, start, pos, DocTest, .expr=expr, .output=output);
}

PARSER(parse_use) {
    const char *start = pos;
    if (!match_word(&pos, "use")) return NULL;
    spaces(&pos);
    size_t path_len = strcspn(pos, " \t\r\n;");
    if (path_len < 1)
        parser_err(ctx, start, pos, "There is no filename here to use");
    char *path = heap_strf("%.*s.sss", (int)path_len, pos);
    pos += path_len;
    char *resolved_path = resolve_path(path, ctx->file->filename);
    if (!resolved_path)
        parser_err(ctx, start, pos, "No such file exists: \"%s\"", path);
    while (match(&pos, ";")) continue;
    return NewAST(ctx->file, start, pos, Use, .path=resolved_path);
}

PARSER(parse_linker) {
    const char *start = pos;
    if (!match_word(&pos, "!link")) return NULL;
    auto directives = EMPTY_ARRAY(const char*);
    for (;;) {
        const char *p = pos;
        spaces(&p);
        size_t len = strcspn(p, " \t\r\n;");
        if (len < 1) break;
        char *directive = heap_strn(p, len);
        append(directives, directive);
        pos = p + len;
    }
    return NewAST(ctx->file, start, pos, LinkerDirective, .directives=directives);
}

PARSER(parse_inline_block) {
    spaces(&pos);
    const char *start = pos;
    auto statements = EMPTY_ARRAY(ast_t*);
    while (*pos) {
        spaces(&pos);
        ast_t *stmt = optional_ast(ctx, &pos, parse_statement);
        if (!stmt) break;
        append(statements, stmt);
        spaces(&pos);
        if (!match(&pos, ";")) break;
    }
    return NewAST(ctx->file, start, pos, Block, .statements=statements);
}

ast_t *parse_file(sss_file_t *file, jmp_buf *on_err) {
    parse_ctx_t ctx = {
        .file=file,
        .on_err=on_err,
    };

    const char *pos = file->text;
    if (match(&pos, "#!")) // shebang
        some_not(&pos, "\r\n");

    whitespace(&pos);
    ast_t *ast = parse_namespace(&ctx, pos);
    pos = ast->end;
    whitespace(&pos);
    if (strlen(pos) > 0) {
        parser_err(&ctx, pos, pos + strlen(pos), "I couldn't parse this part of the file");
    }
    return ast;
}

ast_t *parse_type_str(const char *str) {
    sss_file_t *file = sss_spoof_file("<type>", str);
    parse_ctx_t ctx = {
        .file=file,
        .on_err=NULL,
    };

    const char *pos = file->text;
    whitespace(&pos);
    ast_t *ast = parse_type(&ctx, pos);
    if (!ast) return ast;
    pos = ast->end;
    whitespace(&pos);
    if (strlen(pos) > 0) {
        parser_err(&ctx, pos, pos + strlen(pos), "I couldn't parse this part of the type");
    }
    return ast;
}

ast_t *parse_expression_str(const char *str) {
    sss_file_t *file = sss_spoof_file("<expression>", str);
    parse_ctx_t ctx = {
        .file=file,
        .on_err=NULL,
    };

    const char *pos = file->text;
    whitespace(&pos);
    ast_t *ast = parse_extended_expr(&ctx, pos);
    if (!ast) return ast;
    pos = ast->end;
    whitespace(&pos);
    if (strlen(pos) > 0) {
        parser_err(&ctx, pos, pos + strlen(pos), "I couldn't parse this part of the expression");
    }
    return ast;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
