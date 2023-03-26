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

#define RANGE_STEP NUM_AST_TAGS

static int op_tightness[NUM_AST_TAGS+1] = {
    [Power]=1,
    [Multiply]=2, [Divide]=2,
    [Add]=3, [Subtract]=3,
    [Modulus]=4,
    [Range]=5,
    [RANGE_STEP]=6,
    [Cast]=6,
    [Greater]=7, [GreaterEqual]=7, [Less]=7, [LessEqual]=7,
    [Equal]=8, [NotEqual]=8,
    [And]=9, [Or]=9, [Xor]=9,
};

static const char *keywords[] = {
    "yes","xor","with","while","when","using","use","unless","unit","typeof","to","then","stop","skip","sizeof","return","repeat",
    "pass","or","not","no","mod","macro","is","inline","if","global","for","fail","extern","extend","export","enum","else","do","del",
    "deftype", "defer","def","bitcast","between","as","and", NULL,
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
static ast_t *parse_index_suffix(parse_ctx_t *ctx, ast_t *lhs);
static PARSER(parse_indented_block);
static PARSER(parse_if);
static PARSER(parse_for);
static PARSER(parse_while);
static PARSER(parse_repeat);
static PARSER(parse_defer);
static PARSER(parse_with);
static PARSER(parse_do);
static PARSER(parse_using);
static PARSER(parse_when);
static PARSER(parse_extend);
static PARSER(parse_expr);
static PARSER(parse_extended_expr);
static PARSER(parse_term);
static PARSER(parse_inline_block);
static PARSER(_parse_type);
static PARSER(parse_statement);
static PARSER(parse_block);
static PARSER(parse_opt_indented_block);
static PARSER(parse_var);
static PARSER(parse_def);
static PARSER(parse_extern);
static PARSER(parse_declaration);
static PARSER(parse_doctest);
static PARSER(parse_use);
static PARSER(parse_export);
static PARSER(parse_ellipsis);

//
// Print a parse error and exit (or use the on_err longjmp)
//
__attribute__((noreturn))
static void vparser_err(parse_ctx_t *ctx, const char *start, const char *end, const char *fmt, va_list args) {
    if (isatty(STDERR_FILENO))
        fputs("\x1b[31;1;7m", stderr);
    fprintf(stderr, "%s:%ld.%ld: ", ctx->file->filename, bl_get_line_number(ctx->file, start),
            bl_get_line_column(ctx->file, start));
    vfprintf(stderr, fmt, args);
    if (isatty(STDERR_FILENO))
        fputs(" \x1b[m", stderr);
    fputs("\n\n", stderr);

    span_t span = {.file=ctx->file, .start=start, .end=end};
    fprint_span(stderr, span, "\x1b[31;1;7m", 2, isatty(STDERR_FILENO));
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

    if (isatty(STDERR_FILENO))
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

    if (isatty(STDERR_FILENO))
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

    if (isatty(STDERR_FILENO))
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
    spaces(&pos);
    if (!isalpha(*pos) && *pos != '_')
        return NULL;
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

bool match_indentation(const char **out, size_t target) {
    const char *pos = *out;
    for (size_t indentation = 0; indentation < target; ) {
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
    pos += strspn(pos, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ/^0123456789- ");
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

    if (match(&pos, "e") || match(&pos, "f")) // floating point literal
        return NULL;

    if (negative) i *= -1;

    if (match(&pos, "%")) {
        double d = (double)i / 100.;
        return NewAST(ctx->file, start, pos, Num, .n=d, .precision=64, .units=intern_str("%"));
    }

    match(&pos, "_");
    int64_t precision = 64;
    bool is_unsigned = false;
    if (match(&pos, "i64")) precision = 64;
    else if (match(&pos, "i32")) precision = 32;
    else if (match(&pos, "i16")) precision = 16;
    else if (match(&pos, "i8")) precision = 8;
    else if (match(&pos, "u64")) { precision = 64; is_unsigned = true; }
    else if (match(&pos, "u32")) { precision = 32; is_unsigned = true; }
    else if (match(&pos, "u16")) { precision = 16; is_unsigned = true; }
    else if (match(&pos, "u8")) { precision = 8; is_unsigned = true; }

    // else if (match(&pos, ".") || match(&pos, "e")) return NULL; // looks like a float

    istr_t units = match_units(&pos);
    return NewAST(ctx->file, start, pos, Int, .i=i, .precision=precision, .units=units, .is_unsigned=is_unsigned);
}

PARSER(parse_table_type) {
    const char *start = pos;
    if (!match(&pos, "{")) return NULL;
    whitespace(&pos);
    ast_t *key_type = optional_ast(ctx, &pos, _parse_type);
    if (!key_type) return NULL;
    whitespace(&pos);
    if (!match(&pos, "=")) return NULL;
    ast_t *value_type = expect_ast(ctx, start, &pos, _parse_type, "I couldn't parse the rest of this table type");
    whitespace(&pos);
    expect_closing(ctx, &pos, "}", "I wasn't able to parse the rest of this table type");
    return NewAST(ctx->file, start, pos, TypeTable, .key_type=key_type, .value_type=value_type);
}

PARSER(parse_struct_type) {
    const char *start = pos;
    istr_t name = get_id(&pos);
    if (!match(&pos, "{")) return NULL;
    NEW_LIST(istr_t, member_names);
    NEW_LIST(ast_t*, member_types);
    for (int i = 1; ; i++) {
        whitespace(&pos);
        const char *field_start = pos;
        istr_t field_name = get_id(&pos);
        whitespace(&pos);
        if (match(&pos, ":")) {
            whitespace(&pos);
            field_name = intern_str(field_name);
        } else {
            field_name = intern_strf("_%d", i);
            pos = field_start;
        }
        ast_t *t = expect_ast(ctx, field_start, &pos, _parse_type, "I couldn't parse the type for this field");
        APPEND(member_names, field_name);
        APPEND(member_types, t);
        whitespace(&pos);
        if (!match(&pos, ",")) break;
    }
    whitespace(&pos);
    expect_closing(ctx, &pos, "}", "I wasn't able to parse the rest of this tuple type");
    return NewAST(ctx->file, start, pos, TypeStruct, .name=name, .member_names=member_names, .member_types=member_types);
}

PARSER(parse_tagged_union_type) {
    const char *start = pos;
    istr_t name = get_id(&pos);
    if (!match_word(&pos, "oneof")) return NULL;
    spaces(&pos);
    if (!match(&pos, "{")) return NULL;
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
            whitespace(&pos);
            type = expect_ast(ctx, pos-1, &pos, _parse_type, "I couldn't parse a type here");
        }

        // Check for duplicate values:
        for (int64_t i = 0, len = LIST_LEN(tag_values); i < len; i++) {
            if (LIST_ITEM(tag_values, i) == next_value)
                parser_err(ctx, tag_start, pos, "This tag value (%ld) is a duplicate of an earlier tag value", next_value);
        }

        APPEND(tag_names, tag_name);
        APPEND(tag_values, next_value);
        APPEND(tag_types, type);

        whitespace(&pos);
        match(&pos, ",");

        ++next_value;
    }
    expect_closing(ctx, &pos, "}", "I wasn't able to parse the rest of this 'oneof'");
    return NewAST(ctx->file, start, pos, TypeTaggedUnion, .name=name, .tag_names=tag_names, .tag_values=tag_values, .tag_types=tag_types);
}

PARSER(parse_func_type) {
    const char *start = pos;
    if (!match(&pos, "(")) return NULL;
    NEW_LIST(ast_t*, arg_types);
    for (;;) {
        ast_t *arg_t = optional_ast(ctx, &pos, _parse_type);
        if (!arg_t) break;
        APPEND(arg_types, arg_t);
        spaces(&pos);
        if (!match(&pos, ",")) break;
    }
    expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this function type");
    spaces(&pos);
    if (!match(&pos, "=>")) return NULL;
    ast_t *ret = optional_ast(ctx, &pos, _parse_type);
    return NewAST(ctx->file, start, pos, TypeFunction, .arg_types=arg_types, .ret_type=ret);
}

PARSER(parse_array_type) {
    const char *start = pos;
    if (!match(&pos, "[")) return NULL;
    ast_t *type = expect_ast(ctx, start, &pos, _parse_type,
                             "I couldn't parse an array item type after this point");
    expect_closing(ctx, &pos, "]", "I wasn't able to parse the rest of this array type");
    return NewAST(ctx->file, start, pos, TypeArray, .item_type=type);
}

PARSER(parse_pointer_type) {
    const char *start = pos;
    if (!match(&pos, "@")) return NULL;
    ast_t *type = expect_ast(ctx, start, &pos, _parse_type,
                             "I couldn't parse a pointer type after this point");
    return NewAST(ctx->file, start, pos, TypePointer, .pointed=type);
}

PARSER(parse_type_type) {
    const char *start = pos;
    if (!match_word(&pos, "Type")) return NULL;
    spaces(&pos);
    if (!match(&pos, "(")) return NULL;
    ast_t *type = expect_ast(ctx, start, &pos, _parse_type,
                             "I couldn't parse a pointer type after this point");
    expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this type");
    return NewAST(ctx->file, start, pos, TypeTypeAST, .type=type);
}

PARSER(parse_optional_type) {
    const char *start = pos;
    if (!match(&pos, "?")) return NULL;
    ast_t *type = expect_ast(ctx, start, &pos, _parse_type,
                             "I couldn't parse a pointer type after this point");
    return NewAST(ctx->file, start, pos, TypeOptional, .type=type);
}

PARSER(parse_type_name) {
    const char *start = pos;
    istr_t id = get_id(&pos);
    if (!id) return NULL;
    ast_t *ast = NewAST(ctx->file, start, pos, Var, .name=id);
    ast_t *fielded = parse_field_suffix(ctx, ast);
    return fielded ? fielded : ast;
}

PARSER(parse_dsl_type) {
    const char *start = pos;
    if (!match(&pos, "$")) return NULL;
    istr_t id = get_id(&pos);
    if (!id) return NULL;
    return NewAST(ctx->file, start, pos, TypeDSL, .name=id);
}

static istr_t get_units(const char **pos) {
    // +@(@unit=(id !~ keyword) [`^ @power=([`-] +`0-9)]) % (_[`*,/_])
    size_t len = strcspn(*pos, ">");
    // TODO: verify
    istr_t units = intern_strn(*pos, len);
    *pos += len;
    return units;
}

PARSER(_parse_type) {
    const char *start = pos;
    ast_t *type = NULL;
    bool success = (false
        || (type=parse_optional_type(ctx, pos))
        || (type=parse_pointer_type(ctx, pos))
        || (type=parse_type_type(ctx, pos))
        || (type=parse_dsl_type(ctx, pos))
        || (type=parse_array_type(ctx, pos))
        || (type=parse_table_type(ctx, pos))
        || (type=parse_struct_type(ctx, pos))
        || (type=parse_tagged_union_type(ctx, pos))
        || (type=parse_type_name(ctx, pos))
        || (type=parse_func_type(ctx, pos))
    );
    if (!success && match(&pos, "(")) {
        whitespace(&pos);
        type = optional_ast(ctx, &pos, _parse_type);
        if (!type) return NULL;
        whitespace(&pos);
        expect_closing(ctx, &pos, ")", "I wasn't able to parse the rest of this type");
        // Use the enclosing span
        type = new(ast_t, .span.file=(ctx)->file, .span.start=start, .span.end=pos,
                   .tag=type->tag, .__data=type->__data);
    }

    if (!type) return NULL;

    pos = type->span.end;
    // Measure type
    if (match(&pos, "<")) {
        istr_t units = get_units(&pos);
        expect_closing(ctx, &pos, ">", "I expected a closing '>' for these units");
        type = NewAST(ctx->file, type->span.start, pos, TypeMeasure, .type=type, .units=units);
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

    istr_t units;
    if (match(&pos, "%")) {
        d /= 100.;
        units = intern_str("%");
    } else {
        units = match_units(&pos);
    }

    return NewAST(ctx->file, start, pos, Num, .n=d, .precision=precision, .units=units);
}

PARSER(parse_array) {
    const char *start = pos;
    if (!match(&pos, "[")) return NULL;

    whitespace(&pos);

    NEW_LIST(ast_t*, items);
    ast_t *item_type = NULL;
    if (match(&pos, ":")) {
        whitespace(&pos);
        item_type = expect_ast(ctx, pos-1, &pos, _parse_type, "I couldn't parse a type for this array");
    }

    for (;;) {
        whitespace(&pos);
        ast_t *item = optional_ast(ctx, &pos, parse_extended_expr);
        if (!item) break;
        APPEND(items, item);
        whitespace(&pos);
        if (!match(&pos, ",")) break;
    }
    whitespace(&pos);
    expect_closing(ctx, &pos, "]", "I wasn't able to parse the rest of this array");

    if (!item_type && LIST_LEN(items) == 0)
        parser_err(ctx, start, pos, "Empty arrays must specify what type they would contain (e.g. [:Int])");

    return NewAST(ctx->file, start, pos, Array, .type=item_type, .items=items);
}

PARSER(parse_table) {
    const char *start = pos;
    if (!match(&pos, "{")) return NULL;

    whitespace(&pos);

    NEW_LIST(ast_t*, entries);
    ast_t *key_type = NULL, *value_type = NULL;
    if (match(&pos, ":")) {
        whitespace(&pos);
        key_type = expect_ast(ctx, pos-1, &pos, _parse_type, "I couldn't parse a key type for this table");
        whitespace(&pos);
        if (!match(&pos, "="))
            parser_err(ctx, pos, pos, "I expected an '=' for this table type");
        value_type = expect_ast(ctx, pos-1, &pos, _parse_type, "I couldn't parse a value type for this table");
    }

    for (;;) {
        whitespace(&pos);
        const char *entry_start = pos;
        if (!match(&pos, "[")) break;
        ast_t *key = optional_ast(ctx, &pos, parse_extended_expr);
        if (!key) break;
        expect_closing(ctx, &pos, "]", "I wasn't able to parse the rest of this table key");
        whitespace(&pos);
        if (!match(&pos, "=")) return NULL;
        ast_t *value = expect_ast(ctx, pos-1, &pos, parse_expr, "I couldn't parse the value for this table entry");

        ast_t *entry = NewAST(ctx->file, entry_start, pos, TableEntry, .key=key, .value=value);
        for (bool progress = true; progress; ) {
            ast_t *new_entry;
            progress = (false
                || (new_entry=parse_index_suffix(ctx, entry))
                || (new_entry=parse_field_suffix(ctx, entry))
                || (new_entry=parse_suffix_if(ctx, entry, false))
                || (new_entry=parse_suffix_for(ctx, entry))
                || (new_entry=parse_suffix_while(ctx, entry))
                || (new_entry=parse_fncall_suffix(ctx, entry, true))
            );
            if (progress) entry = new_entry;
        }
        pos = entry->span.end;

        APPEND(entries, entry);
        whitespace(&pos);
        if (!match(&pos, ",")) break;
    }

    if (!key_type && !value_type && LIST_LEN(entries) == 0)
        return NULL;

    whitespace(&pos);
    expect_closing(ctx, &pos, "}", "I wasn't able to parse the rest of this table");

    return NewAST(ctx->file, start, pos, Table, .key_type=key_type, .value_type=value_type, .entries=entries);
}

PARSER(parse_struct) {
    const char *start = pos;
    ast_t *type = optional_ast(ctx, &pos, parse_type_name);
    spaces(&pos);
    if (!match(&pos, "{")) return NULL;
    NEW_LIST(ast_t*, members);
    for (int i = 1; ; i++) {
        whitespace(&pos);
        const char *field_start = pos;
        istr_t field_name = NULL;
        if ((field_name=get_id(&pos))) {
            whitespace(&pos);
            if (!match(&pos, "=")) goto no_field_name;
            field_name = intern_str(field_name);
            whitespace(&pos);
        } else {
          no_field_name: field_name = NULL;
          pos = field_start;
        }
        ast_t *value = field_name ? expect_ast(ctx, field_start, &pos, parse_expr, "I couldn't parse the value for this field") : optional_ast(ctx, &pos, parse_expr);
        if (!field_name && !type)
            field_name = intern_strf("_%d", i);
        if (!value) break;
        APPEND(members, NewAST(ctx->file, field_start, pos, StructField, .name=field_name, .value=value));
        whitespace(&pos);
        match(&pos, ",");
    }
    whitespace(&pos);
    expect_closing(ctx, &pos, "}", "I wasn't able to parse the rest of this struct");
    istr_t units = match_units(&pos);
    return NewAST(ctx->file, start, pos, Struct, .type=type, .members=members, .units=units);
}

ast_t *parse_field_suffix(parse_ctx_t *ctx, ast_t *lhs) {
    if (!lhs) return NULL;
    const char *pos = lhs->span.end;
    whitespace(&pos);
    if (!match(&pos, ".")) return NULL;
    if (*pos == '.') return NULL;
    istr_t field = get_id(&pos);
    if (!field) return NULL;
    return NewAST(ctx->file, lhs->span.start, pos, FieldAccess, .fielded=lhs, .field=field);
}

PARSER(parse_ellipsis) {
    const char *start = pos;
    if (!match(&pos, "..")) return NULL;
    return NewAST(ctx->file, start, pos, Ellipsis);
}

PARSER(parse_reduction) {
    const char *start = pos;
    if (!match(&pos, "|")) return NULL;
    ast_t *iter = optional_ast(ctx, &pos, parse_extended_expr);
    if (!iter) return NULL;
    spaces(&pos);
    if (!match(&pos, ","))
        parser_err(ctx, pos, pos, "I expected a comma and another expression for this Reduction");
    ast_t *combination = expect_ast(ctx, start, &pos, parse_extended_expr,
                                    "I expected to find an expression here for how to merge two values");

    ast_t *fallback = NULL;
    spaces(&pos);
    if (match(&pos, ",")) {
        spaces(&pos);
        fallback = optional_ast(ctx, &pos, parse_extended_expr);
    }

    expect_closing(ctx, &pos, "|", "I wasn't able to parse the rest of this reduction");
    return NewAST(ctx->file, start, pos, Reduction, .iter=iter, .combination=combination, .fallback=fallback);
}

ast_t *parse_index_suffix(parse_ctx_t *ctx, ast_t *lhs) {
    if (!lhs) return NULL;
    const char *start = lhs->span.start;
    const char *pos = lhs->span.end;
    if (!match(&pos, "[")) return NULL;
    ast_t *index = expect_ast(ctx, start, &pos, parse_extended_expr,
                              "I expected to find an expression here to index with");
    expect_closing(ctx, &pos, "]", "I wasn't able to parse the rest of this index");
    auto access_type = INDEX_NORMAL;
    spaces(&pos);
    if (match_word(&pos, "unchecked"))
        access_type = INDEX_UNCHECKED;
    else if (match(&pos, "!"))
        access_type = INDEX_FAIL;
    return NewAST(ctx->file, start, pos, Index, .indexed=lhs, .index=index, .type=access_type);
}

PARSER(parse_if) {
    // if cond then body else body
    const char *start = pos;
    bool is_unless;
    if (match_word(&pos, "if")) is_unless = false;
    else if (match_word(&pos, "unless")) is_unless = true;
    else return NULL;
    size_t starting_indent = bl_get_indent(ctx->file, pos);
    ast_t *cond = is_unless ? NULL : optional_ast(ctx, &pos, parse_declaration);
    if (!cond)
        cond = expect_ast(ctx, start, &pos, parse_expr,
                          "I expected to find a condition for this 'if'");
    if (is_unless) cond = NewAST(ctx->file, cond->span.start, cond->span.end, Not, .value=cond);

    ast_t *body;
    if (match_word(&pos, "then"))
        body = optional_ast(ctx, &pos, parse_opt_indented_block);
    else
        body = optional_ast(ctx, &pos, parse_indented_block); 

    if (!body) {
        spaces(&pos);
        if (*pos == '\n')
            parser_err(ctx, start, pos, "I expected a body for this 'if'");
        else
            parser_err(ctx, pos, strchrnul(pos, '\n'), "I couldn't parse the rest of this 'if' condition");
    }

    ast_t *else_ = NULL;
    const char *else_start = pos;
    whitespace(&else_start);
    if (bl_get_indent(ctx->file, else_start) == starting_indent && match_word(&else_start, "else")) {
        pos = else_start;
        else_ = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'else'"); 
    }
    return NewAST(ctx->file, start, pos, If, .condition=cond, .body=body, .else_body=else_);
}

PARSER(parse_when) {
    // when <expr> is [<var>:]<tag>[*(,<tag>)] <body> *(is ...) else <body>
    const char *start = pos;
    if (!match_word(&pos, "when"))
        return NULL;
    size_t starting_indent = bl_get_indent(ctx->file, pos);
    ast_t *subj = expect_ast(ctx, start, &pos, parse_expr,
                             "I expected to find an expression for this 'when'");

    NEW_LIST(ast_case_t, cases);
    for (;;) {
        whitespace(&pos);
        if (bl_get_indent(ctx->file, pos) != starting_indent) break;
        if (!match_word(&pos, "is")) break;
        ast_t *var = optional_ast(ctx, &pos, parse_var);
        ast_t *tag;
        spaces(&pos);
        if (var && match(&pos, ":")) {
            tag = optional_ast(ctx, &pos, parse_var);
        } else {
            tag = var;
            var = NULL;
        }
        List(ast_t*) tags = LIST(ast_t*, tag);
        for (spaces(&pos); match(&pos, ","); spaces(&pos)) {
            tag = optional_ast(ctx, &pos, parse_var);
            if (!tag) break;
            APPEND(tags, tag);
        }
        ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'when'"); 
        for (int64_t i = 0, len = LIST_LEN(tags); i < len; i++) {
            ast_case_t case_ = {.var=var, .tag=LIST_ITEM(tags, i), .body=body};
            list_append((list_t*)cases, sizeof(ast_case_t), &case_);
        }
    }

    ast_t *else_ = NULL;
    const char *else_start = pos;
    whitespace(&else_start);
    if (bl_get_indent(ctx->file, else_start) == starting_indent && match_word(&else_start, "else")) {
        pos = else_start;
        else_ = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'else'"); 
    }
    return NewAST(ctx->file, start, pos, When, .subject=subj, .cases=cases, .default_body=else_);
}

PARSER(parse_do) {
    // do [label] [<indent>] body [else else-body]
    const char *start = pos;
    if (!match_word(&pos, "do")) return NULL;
    size_t starting_indent = bl_get_indent(ctx->file, pos);
    istr_t label = get_id(&pos);
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'do'"); 
    ast_t *else_body = NULL;
    const char *else_start = pos;
    whitespace(&pos);
    if (bl_get_indent(ctx->file, pos) == starting_indent && match_word(&pos, "else"))
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
        expr = expect_ast(ctx, var->span.start, &pos, parse_expr, "I expected an expression for this variable");
    } else {
        pos = var ? var->span.start : pos;
        var = NULL;
        expr = expect_ast(ctx, start, &pos, parse_expr, "I expected an expression for this 'with'");
    }
    ast_t *cleanup = NULL;
    if (match_word(&pos, "then"))
        cleanup = expect_ast(ctx, start, &pos, parse_statement, "I expected a cleanup expression for this 'with'");
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'with'");
    return NewAST(ctx->file, start, pos, With, .var=var, .expr=expr, .cleanup=cleanup, .body=body);
}

PARSER(parse_extend) {
    // extend <type> <indent> body
    const char *start = pos;
    if (!match_word(&pos, "extend")) return NULL;
    ast_t *type = expect_ast(ctx, start, &pos, _parse_type, "I expected a type to parse");
    ast_t *body = expect_ast(ctx, start, &pos, parse_indented_block, "I expected a body for this 'extend'");
    return NewAST(ctx->file, start, pos, Extend, .type=type, .body=body);
}

PARSER(parse_using) {
    // using var1,var2,... [<indent>] body
    const char *start = pos;
    if (!match_word(&pos, "using")) return NULL;
    NEW_LIST(ast_t*, vars);
    for (;;) {
        ast_t *var = optional_ast(ctx, &pos, parse_var);
        if (!var) break;
        APPEND(vars, var);
        spaces(&pos);
        if (!match(&pos, ",")) break;
    }
    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'do'"); 
    return NewAST(ctx->file, start, pos, Using, .vars=vars, .body=body);
}

PARSER(parse_loop_var) {
    const char *start = pos;
    bool deref = match(&pos, "*");
    ast_t *var = optional_ast(ctx, &pos, parse_var);
    return deref ? NewAST(ctx->file, start, pos, Dereference, .value=var) : var;
}

PARSER(parse_for) {
    // for [k,] v in iter [do] [<indent>] body [<nodent> between [<indent>] body]
    const char *start = pos;
    if (!match_word(&pos, "for")) return NULL;
    size_t starting_indent = bl_get_indent(ctx->file, pos);
    ast_t *key = expect_ast(ctx, start, &pos, parse_loop_var, "I expected an iteration variable for this 'for'");
    spaces(&pos);
    ast_t *value = NULL;
    if (match(&pos, ",")) {
        value = expect_ast(ctx, pos-1, &pos, parse_loop_var, "I expected a variable after this comma");
    }
    expect_str(ctx, start, &pos, "in", "I expected an 'in' for this 'for'");
    ast_t *iter = expect_ast(ctx, start, &pos, parse_expr, "I expected an iterable value for this 'for'");

    ast_t *first = NULL, *empty = NULL;
    if (match_word(&pos, "first")) {
        first = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'for'");
        whitespace(&pos);
    }

    if (bl_get_indent(ctx->file, pos) == starting_indent)
        match_word(&pos, first ? "then" : "do"); // optional

    ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a body for this 'for'"); 
    whitespace(&pos);
    ast_t *between = NULL;
    const char *between_start = pos;
    if (bl_get_indent(ctx->file, pos) == starting_indent && match_word(&pos, "between")) {
        between = expect_ast(ctx, between_start, &pos, parse_opt_indented_block, "I expected a body for this 'between'");
        whitespace(&pos);
    }

    const char *else_start = pos;
    if (bl_get_indent(ctx->file, pos) == starting_indent && match_word(&pos, "else")) {
        empty = expect_ast(ctx, else_start, &pos, parse_opt_indented_block, "I expected a body for this 'else'");
    }
    return NewAST(ctx->file, start, pos, For, .key=value ? key : NULL, .value=value ? value : key, .iter=iter,
                  .first=first, .body=body, .between=between, .empty=empty);
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
    return NewAST(ctx->file, start, pos, For, .key=value ? key : NULL, .value=value ? value : key, .iter=iter, .body=body);
}

PARSER(parse_repeat) {
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
    return NewAST(ctx->file, start, pos, Repeat, .body=body, .between=between);
}

PARSER(parse_while) {
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
    return NewAST(ctx->file, start, pos, While, .condition=condition, .body=body, .between=between);
}

ast_t *parse_suffix_while(parse_ctx_t *ctx, ast_t *body) {
    if (!body) return NULL;
    const char *start = body->span.start;
    const char *pos = body->span.end;
    if (!match_word(&pos, "while")) return NULL;
    ast_t *cond = expect_ast(ctx, start, &pos, parse_expr, "I don't see a viable condition for this 'while'");
    return NewAST(ctx->file, start, pos, While, .condition=cond, .body=body);
}

ast_t *parse_suffix_if(parse_ctx_t *ctx, ast_t *body, bool require_else) {
    // true_val if cond then val else else_val
    if (!body) return NULL;
    const char *start = body->span.start;
    const char *pos = body->span.end;
    bool is_unless;
    if (match_word(&pos, "unless")) is_unless = true;
    else if (match_word(&pos, "if")) is_unless = false;
    else return NULL;
    ast_t *cond = expect_ast(ctx, start, &pos, parse_expr,
                             "I expected to find a condition for this 'if'");

    if (is_unless) cond = NewAST(ctx->file, cond->span.start, cond->span.end, Not, .value=cond);

    ast_t *else_ = NULL;
    const char *else_start = pos;
    if (match_word(&pos, "else")) {
        whitespace(&pos);
        else_ = expect_ast(ctx, else_start, &pos, parse_expr, "I couldn't find a body for this 'else' block");
    }
    if (else_) pos = else_->span.end;
    else if (require_else) return NULL;

    return NewAST(ctx->file, start, pos, If, .condition=cond, .body=body, .else_body=else_);
}

ast_t *parse_unary(parse_ctx_t *ctx, const char *pos, ast_tag_e tag, const char *prefix, bool exprs_ok) {
    const char *start = pos;
    if (!match(&pos, prefix)) return NULL;
    if (isalpha(prefix[0]) && (isalpha(*pos) || isdigit(*pos) || *pos == '_')) return NULL;
    whitespace(&pos);
    ast_t *val = exprs_ok ? parse_expr(ctx, pos) : parse_term(ctx, pos);
    if (!val) return NULL;
    // Unsafe: all the unary ops have a '.value' field in the same spot, so this is a hack
    return new(ast_t, .span.file=ctx->file, .span.start=start, .span.end=val->span.end,
               .tag=tag, .__data.Not.value=val);
    // End unsafe area
}
#define parse_negative(...) parse_unary(__VA_ARGS__, Negative, "-", false)
#define parse_heap_alloc(...) parse_unary(__VA_ARGS__, HeapAllocate, "@", false)
#define parse_dereference(...) parse_unary(__VA_ARGS__, Dereference, "*", false)
#define parse_len(...) parse_unary(__VA_ARGS__, Len, "#", false)
#define parse_maybe(...) parse_unary(__VA_ARGS__, Maybe, "?", false)
#define parse_not(...) parse_unary(__VA_ARGS__, Not, "not", true)
#define parse_typeof(...) parse_unary(__VA_ARGS__, TypeOf, "typeof", true)
#define parse_sizeof(...) parse_unary(__VA_ARGS__, SizeOf, "sizeof", true)

PARSER(parse_bool) {
    const char *start = pos;
    if (match_word(&pos, "yes"))
        return NewAST(ctx->file, start, pos, Bool, .b=true);
    else if (match_word(&pos, "no"))
        return NewAST(ctx->file, start, pos, Bool, .b=false);
    else
        return NULL;
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
    const char *dsl = NULL;
    char open, close;
    if (match(&pos, "$")) {
        dsl = isalpha(*pos) || *pos == '_' ? get_id(&pos) : NULL;
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

    NEW_LIST(ast_t*, chunks);
    if (*pos == '\r' || *pos == '\n') {
        char special[] = {'\n','\r',interp_char,escape_char,'\0'};
        size_t starting_indent = bl_get_indent(ctx->file, pos); 
        // indentation-delimited string
        match(&pos, "\r");
        match(&pos, "\n");
        size_t first_line = bl_get_line_number(ctx->file, pos);
        auto line = LIST_ITEM(ctx->file->lines, first_line-1);
        size_t indented = line.indent;
        for (size_t i = first_line; i < (size_t)LIST_LEN(ctx->file->lines); i++) {
            auto line = LIST_ITEM(ctx->file->lines, i-1);
            pos = ctx->file->text + line.offset;
            if (strchrnul(pos, '\n') == pos + strspn(pos, " \t\r")) {
                ast_t *ast = NewAST(ctx->file, pos, pos, StringLiteral, .str="\n");
                APPEND(chunks, ast);
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

            for (const char *eol = strchrnul(pos, '\n'); pos < eol+1; ) {
                size_t len = strcspn(pos, special);
                if (pos[len] == '\r') ++len;
                if (pos[len] == '\n') ++len;

                if (len > 0) {
                    ast_t *chunk = NewAST(ctx->file, pos, pos+len-1, StringLiteral, .str=intern_strn(pos, len));
                    APPEND(chunks, chunk);
                }

                pos += len;

                if (*pos == escape_char) {
                    const char *start = pos;
                    istr_t unescaped = unescape(&pos);
                    ast_t *chunk = NewAST(ctx->file, start, pos, StringLiteral, .str=unescaped);
                    APPEND(chunks, chunk);
                    ++pos;
                } else if (*pos == interp_char) {
                    ast_t *chunk = parse_interpolation(ctx, pos);
                    APPEND(chunks, chunk);
                    pos = chunk->span.end;
                }
            }
        }
      finished:;
        // Strip trailing newline:
        if (LIST_LEN(chunks) > 0) {
            ast_t *last_chunk = LIST_ITEM(chunks, LIST_LEN(chunks)-1);
            if (last_chunk->tag == StringLiteral) {
                auto str = Match(last_chunk, StringLiteral);
                istr_t trimmed = intern_strn(str->str, strlen(str->str)-1);
                chunks[0][LIST_LEN(chunks)-1] = NewAST(ctx->file, last_chunk->span.start, last_chunk->span.end-1, StringLiteral, .str=trimmed);
            }
        }
    } else {
        char special[] = {'\n','\r',open,close,interp_char,escape_char,'\0'};
        // Inline string:
        int depth = 1;
        while (depth > 0 && *pos) {
            size_t len = strcspn(pos, special);
            if (len > 0) {
                ast_t *chunk = NewAST(ctx->file, pos, pos+len-1, StringLiteral, .str=intern_strn(pos, len));
                APPEND(chunks, chunk);
                pos += len;
            }

            if (*pos == interp_char) {
                ast_t *chunk = parse_interpolation(ctx, pos);
                APPEND(chunks, chunk);
                pos = chunk->span.end;
            } else if (*pos == escape_char) {
                const char *start = pos;
                istr_t unescaped = unescape(&pos);
                ast_t *chunk = NewAST(ctx->file, start, pos, StringLiteral, .str=unescaped);
                APPEND(chunks, chunk);
            } else if (*pos == '\r' || *pos == '\n') {
                if (open == ' ' || open == ':' || open == '>') goto string_finished;
                parser_err(ctx, string_start, pos, "This line ended without closing the string");
            } else if (*pos == close) { // if open == close, then don't do nesting (i.e. check 'close' first)
                --depth;
                if (depth <= 0) {
                    ++pos;
                }
            } else if (*pos == open) {
                ++depth;
            } else {
                ast_t *chunk = NewAST(ctx->file, pos, pos+1, StringLiteral, .str=intern_strn(pos, 1));
                ++pos;
                APPEND(chunks, chunk);
            }
        }
    }
  string_finished:;
    return NewAST(ctx->file, string_start, pos, StringJoin, .children=chunks, .dsl=dsl);
}

PARSER(parse_skip) {
    const char *start = pos;
    if (!match_word(&pos, "skip")) return NULL;
    spaces(&pos);
    istr_t target;
    if (match_word(&pos, "for")) target = intern_str("for");
    else if (match_word(&pos, "while")) target = intern_str("while");
    else if (match_word(&pos, "repeat")) target = intern_str("repeat");
    else target = get_id(&pos);
    return NewAST(ctx->file, start, pos, Skip, .target=target);
}

PARSER(parse_stop) {
    const char *start = pos;
    if (!match_word(&pos, "stop")) return NULL;
    spaces(&pos);
    istr_t target;
    if (match_word(&pos, "for")) target = intern_str("for");
    else if (match_word(&pos, "while")) target = intern_str("while");
    else if (match_word(&pos, "repeat")) target = intern_str("repeat");
    else target = get_id(&pos);
    return NewAST(ctx->file, start, pos, Stop, .target=target);
}

PARSER(parse_return) {
    const char *start = pos;
    if (!match_word(&pos, "return")) return NULL;
    spaces(&pos);
    ast_t *value = optional_ast(ctx, &pos, parse_expr);
    return NewAST(ctx->file, start, pos, Return, .value=value);
}

PARSER(parse_lambda) {
    const char *start = pos;
    NEW_LIST(istr_t, arg_names);
    NEW_LIST(ast_t*, arg_types);
    if (!match(&pos, "(")) {
        if (match(&pos, "=>")) goto thunk;
        return NULL;
    }
    for (spaces(&pos); ; spaces(&pos)) {
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
        ast_t *type = optional_ast(ctx, &pos, _parse_type);
        if (!type) parser_err(ctx, pos, pos + strcspn(pos, ",;)\r\n"), "I wasn't able to parse this type");
        APPEND(arg_types, type);
        spaces(&pos);
        if (!match(&pos, ",")) break;
    }

    spaces(&pos);
    if (!match(&pos, ")")) {
        if (LIST_LEN(arg_names) == 0) return NULL;
        parser_err(ctx, start, pos, "This lambda doesn't have a closing ')'");
    }
    spaces(&pos);
    if (!match(&pos, "=>"))
        return NULL;

  thunk:
    ast_t *body = optional_ast(ctx, &pos, parse_opt_indented_block);

    return NewAST(ctx->file, start, pos, Lambda, .arg_names=arg_names, .arg_types=arg_types, .body=body);
}

PARSER(parse_nil) {
    const char *start = pos;
    if (!match(&pos, "!")) return NULL;
    ast_t *type = _parse_type(ctx, pos);
    if (!type) return NULL;
    return NewAST(ctx->file, start, type->span.end, Nil, .type=type);
}

PARSER(parse_fail) {
    const char *start = pos;
    if (!match_word(&pos, "fail")) return NULL;
    ast_t *msg = parse_term(ctx, pos);
    return NewAST(ctx->file, start, msg ? msg->span.end : pos, Fail, .message=msg);
}

PARSER(parse_var) {
    const char *start = pos;
    istr_t name = get_id(&pos);
    if (!name) return NULL;
    return NewAST(ctx->file, start, pos, Var, .name=name);
}

PARSER(parse_bitcast) {
    const char *start = pos;
    if (!match_word(&pos, "bitcast")) return NULL;
    ast_t *expr = expect_ast(ctx, start, &pos, parse_term, "I expected an expression here");
    if (!match_word(&pos, "as")) parser_err(ctx, start, pos, "I expected a 'as' and type for this bitcast");
    ast_t *t = expect_ast(ctx, start, &pos, _parse_type, "I couldn't parse the type for this bitcast");
    return NewAST(ctx->file, start, pos, Bitcast, .value=expr, .type=t);
}

PARSER(parse_term) {
    spaces(&pos);
    ast_t *term = NULL;
    bool success = (
        false
        || (term=parse_nil(ctx, pos))
        || (term=parse_ellipsis(ctx, pos))
        || (term=parse_num(ctx, pos))
        || (term=parse_int(ctx, pos))
        || (term=parse_negative(ctx, pos))
        || (term=parse_heap_alloc(ctx, pos))
        || (term=parse_dereference(ctx, pos))
        || (term=parse_len(ctx, pos))
        || (term=parse_maybe(ctx, pos))
        || (term=parse_bool(ctx, pos))
        || (term=parse_char(ctx, pos))
        || (term=parse_string(ctx, pos))
        || (term=parse_lambda(ctx, pos))
        || (term=parse_parens(ctx, pos))
        || (term=parse_table(ctx, pos))
        || (term=parse_struct(ctx, pos))
        || (term=parse_var(ctx, pos))
        || (term=parse_array(ctx, pos))
        || (term=parse_reduction(ctx, pos))
        || (term=parse_fail(ctx, pos))
        || (term=parse_skip(ctx, pos))
        || (term=parse_stop(ctx, pos))
        || (term=parse_return(ctx, pos))
        || (term=parse_bitcast(ctx, pos))
        || (term=parse_typeof(ctx, pos))
        || (term=parse_sizeof(ctx, pos))
        || (term=parse_not(ctx, pos))
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

    // No arguments fn()
    if (match(&pos, "(")) {
        spaces(&pos);
        if (match(&pos, ")"))
            return NewAST(ctx->file, start, pos, FunctionCall, .fn=fn, .args=LIST(ast_t*));
        pos = fn->span.end;
    }

    const char *paren_pos = pos;
    if (match(&pos, "{")) return NULL;
    if (match(&pos, "("))
        requires_parens = true;
    else if (requires_parens)
        return NULL;

    spaces(&pos);

    NEW_LIST(ast_t*, args);
    for (;;) {
        const char *arg_start = pos;
        istr_t name = get_id(&pos);
        spaces(&pos);
        if (LIST_LEN(args) == 0 && !requires_parens) {
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
            case 'm': if (match_word(&pos, "mod")) return NULL; else break;
            default: break;
            }
        }
        if (name) {
            if (match(&pos, "=")) {
                spaces(&pos);
                ast_t *arg = parse_expr(ctx, pos);
                if (!arg) parser_err(ctx, arg_start, pos, "I couldn't parse this keyword argument value");
                ast_t *kwarg = NewAST(ctx->file, arg_start, arg->span.end, KeywordArg,
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

    return NewAST(ctx->file, start, pos, FunctionCall, .fn=fn, .args=args);
}

ast_t *parse_expr(parse_ctx_t *ctx, const char *pos) {
    ast_t *term = parse_term(ctx, pos);
    if (!term) return NULL;

    pos = term->span.end;

    auto terms = LIST(ast_t*, term);
    NEW_LIST(ast_tag_e, binops);
    for (;;) {
        spaces(&pos);
        ast_tag_e tag = Unknown;
        switch (*pos) {
        case '+': ++pos; tag = Add; break;
        case '-': ++pos; {
            if (pos[0] != ' ' && pos[-2] == ' ') // looks like `fn -5`
                goto no_more_binops;
            tag = Subtract;
            break;
        }
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
            } else if (match_word(&pos, "as")) {
                tag = Cast; break;
            } else if (match(&pos, "..")) {
                tag = Range; break;
            } else if (match_word(&pos, "to")) {
                tag = Range; break;
            } else if (match_word(&pos, "by")) {
                tag = RANGE_STEP; break;
            } else {
                goto no_more_binops;
            }
        }
        }

        assert(op_tightness[tag]);

        spaces(&pos);
        ast_t *rhs = tag == Cast ? _parse_type(ctx, pos) : parse_term(ctx, pos);
        if (!rhs && tag == Range) rhs = NewAST(ctx->file, pos, pos, Ellipsis);
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

        ast_t *merged;
        if (tag == RANGE_STEP) {
            if (lhs->tag != Range)
                parser_err(ctx, lhs->span.start, rhs->span.end, "This 'by' is not attached to a Range");

            merged = NewAST(ctx->file, lhs->span.start, rhs->span.end, Range,
                            .first=Match(lhs, Range)->first,
                            .last=Match(lhs, Range)->last,
                            .step=rhs);
        } else {
            // Unsafe, relies on these having the same type:
            merged = new(
                ast_t,
                .span.file=ctx->file, .span.start=lhs->span.start, .span.end=rhs->span.end,
                .tag=tag,
                .__data.Add.lhs=lhs,
                .__data.Add.rhs=rhs);
            // End unsafe
        }

        LIST_REMOVE(terms, tightest_op);
        terms[0][tightest_op] = merged;
        // assert(LIST_ITEM(terms, tightest_op) == merged);
    }

    ast_t *expr = LIST_ITEM(terms, 0);
    return expr;
}

PARSER(parse_declaration) {
    const char *start = pos;
    bool is_global = !!match_word(&pos, "global");
    ast_t *var = parse_var(ctx, pos);
    if (!var) return NULL;
    pos = var->span.end;
    spaces(&pos);
    if (!match(&pos, ":=")) return NULL;
    spaces(&pos);
    if (match_word(&pos, "extern")) return NULL;
    ast_t *val = parse_extended_expr(ctx, pos);
    if (!val) parser_err(ctx, pos, strchrnul(pos, '\n'), "This declaration value didn't parse");
    pos = val->span.end;
    return NewAST(ctx->file, start, pos, Declare, .var=var, .value=val, .is_global=is_global);
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
    case AddUpdate: return NewAST(ctx->file, start, pos, AddUpdate, .lhs=lhs, .rhs=rhs);
    case SubtractUpdate: return NewAST(ctx->file, start, pos, SubtractUpdate, .lhs=lhs, .rhs=rhs);
    case MultiplyUpdate: return NewAST(ctx->file, start, pos, MultiplyUpdate, .lhs=lhs, .rhs=rhs);
    case DivideUpdate: return NewAST(ctx->file, start, pos, DivideUpdate, .lhs=lhs, .rhs=rhs);
    case AndUpdate: return NewAST(ctx->file, start, pos, AndUpdate, .lhs=lhs, .rhs=rhs);
    case OrUpdate: return NewAST(ctx->file, start, pos, OrUpdate, .lhs=lhs, .rhs=rhs);
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
    for (;;) {
        ast_t *rhs = optional_ast(ctx, &pos, parse_extended_expr);
        if (!rhs) break;
        APPEND(values, rhs);
        spaces(&pos);
        if (!match(&pos, ",")) break;
        whitespace(&pos);
    }

    return NewAST(ctx->file, start, pos, Assign, .targets=targets, .values=values);
}

PARSER(parse_deletion) {
    const char *start = pos;
    if (!match_word(&pos, "del")) return NULL;
    ast_t *val = expect_ast(ctx, start, &pos, parse_term, "I expected a value after this 'del'");
    return NewAST(ctx->file, start, pos, Delete, .value=val);
}

PARSER(parse_statement) {
    ast_t *stmt = NULL;
    if ((stmt=parse_declaration(ctx, pos))
        || (stmt=parse_extern(ctx, pos))
        || (stmt=parse_def(ctx, pos))
        || (stmt=parse_use(ctx, pos))
        || (stmt=parse_export(ctx, pos))
        || (stmt=parse_doctest(ctx, pos)))
        return stmt;

    if (!(false 
        || (stmt=parse_deletion(ctx, pos))
        || (stmt=parse_update(ctx, pos))
        || (stmt=parse_assignment(ctx, pos))
    ))
        return parse_extended_expr(ctx, pos);

    for (bool progress = true; progress; ) {
        ast_t *new_stmt;
        progress = (false
            || (new_stmt=parse_index_suffix(ctx, stmt))
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
    ast_t *expr = NULL;

    if (false
        || (expr=optional_ast(ctx, &pos, parse_if))
        || (expr=optional_ast(ctx, &pos, parse_for))
        || (expr=optional_ast(ctx, &pos, parse_while))
        || (expr=optional_ast(ctx, &pos, parse_when))
        || (expr=optional_ast(ctx, &pos, parse_repeat))
        || (expr=optional_ast(ctx, &pos, parse_do))
        || (expr=optional_ast(ctx, &pos, parse_defer))
        || (expr=optional_ast(ctx, &pos, parse_with))
        || (expr=optional_ast(ctx, &pos, parse_extend))
        || (expr=optional_ast(ctx, &pos, parse_using))
        )
        return expr;

    if (!(false
          || (expr=parse_fncall_suffix(ctx, parse_term(ctx, pos), false))
          || (expr=parse_expr(ctx, pos))
          ))
        return NULL;

    for (bool progress = true; progress; ) {
        ast_t *new_stmt;
        progress = (false
            || (new_stmt=parse_index_suffix(ctx, expr))
            || (new_stmt=parse_field_suffix(ctx, expr))
            || (new_stmt=parse_suffix_if(ctx, expr, false))
            || (new_stmt=parse_suffix_for(ctx, expr))
            || (new_stmt=parse_suffix_while(ctx, expr))
            || (new_stmt=parse_fncall_suffix(ctx, expr, true))
            );
        if (progress) expr = new_stmt;
    }
    return expr;
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
    return NewAST(ctx->file, start, pos, Block, .statements=statements);
}

PARSER(parse_indented_block) {
    return indent(ctx, &pos) ? parse_block(ctx, pos) : NULL;
}

PARSER(parse_opt_indented_block) {
    return indent(ctx, &pos) ? parse_block(ctx, pos) : parse_inline_block(ctx, pos);
}

ast_t *parse_struct_def(parse_ctx_t *ctx, const char *start, const char **pos, istr_t name) {
    if (!match(pos, "{")) return NULL;
    size_t starting_indent = bl_get_indent(ctx->file, *pos);
    NEW_LIST(istr_t, field_names);
    NEW_LIST(ast_t*, field_types);
    NEW_LIST(ast_t*, field_defaults);
    for (;;) {
        const char *batch_start = *pos;
        int64_t first = LIST_LEN(field_names);
        ast_t *default_val = NULL;
        ast_t *type = NULL;
        for (;;) {
            whitespace(pos);
            istr_t name = get_id(pos);
            if (!name) break;
            APPEND(field_names, name);
            whitespace(pos);
            if (match(pos, "=")) {
                default_val = expect_ast(ctx, *pos-1, pos, parse_term, "I expected a value after this '='");
                break;
            } else if (match(pos, ":")) {
                type = expect_ast(ctx, *pos-1, pos, _parse_type, "I expected a type here");
                break;
            }
            if (!match(pos, ",")) break;
        }
        if (LIST_LEN(field_names) == first) break;
        whitespace(pos);
        if (!default_val && !type)
            parser_err(ctx, batch_start, *pos, "I expected a ':' and type, or '=' and a default value after this field(s)");

        for (int64_t i = first; i < LIST_LEN(field_names); i++) {
            APPEND(field_defaults, default_val);
            APPEND(field_types, type);
        }
        match(pos, ",");
    }
    whitespace(pos);
    expect_closing(ctx, pos, "}", "I wasn't able to parse the rest of this struct");

    whitespace(pos);
    NEW_LIST(ast_t*, definitions);
    size_t indent = bl_get_indent(ctx->file, *pos);
    if (indent > starting_indent) {
        for (;;) {
            whitespace(pos);
            if (bl_get_indent(ctx->file, *pos) != indent) break;
            ast_t *def = optional_ast(ctx, pos, parse_declaration);
            if (!def) def = optional_ast(ctx, pos, parse_def);
            if (!def) break;
            APPEND(definitions, def);
        }
    }
    return NewAST(ctx->file, start, *pos, StructDef, .name=name, .field_names=field_names, .field_types=field_types,
                  .field_defaults=field_defaults, .definitions=definitions);
}

PARSER(parse_def) {
    const char *start = pos;
    bool is_exported = false;
    if (match_word(&pos, "export"))
        is_exported = true;

    if (!match_word(&pos, "def")) return NULL;

    bool is_inline = match_word(&pos, "inline");
    if (is_exported && is_inline)
        parser_err(ctx, start, pos, "Functions can't be both 'inline' and exported");

    spaces(&pos);
    istr_t name = get_id(&pos);
    if (!name) {
        if (isdigit(*pos)) goto derived_unit;
        parser_err(ctx, start, pos, "I expected to see a name after this 'def'");
    }
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
                ast_t *type = expect_ast(ctx, arg_start, &pos, _parse_type,
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
            ret_type = optional_ast(ctx, &pos, _parse_type);
        }
        ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block,
                                 "This function needs a body block");
        return NewAST(ctx->file, start, pos, FunctionDef,
                      .name=name, .arg_names=arg_names, .arg_types=arg_types,
                      .arg_defaults=arg_defaults, .ret_type=ret_type, .body=body, .is_exported=is_exported,
                      .is_inline=is_inline);
    } else if (match(&pos, "{")) { // Struct def Foo{...}
        --pos;
        return parse_struct_def(ctx, start, &pos, name);
    } else if (match_word(&pos, "oneof")) { // tagged union: def Foo oneof {...}
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
                whitespace(&pos);
                type = expect_ast(ctx, pos-1, &pos, _parse_type, "I couldn't parse a type here");
            }

            // Check for duplicate values:
            for (int64_t i = 0, len = LIST_LEN(tag_values); i < len; i++) {
                if (LIST_ITEM(tag_values, i) == next_value)
                    parser_err(ctx, tag_start, pos, "This tag value (%ld) is a duplicate of an earlier tag value", next_value);
            }

            APPEND(tag_names, tag_name);
            APPEND(tag_values, next_value);
            APPEND(tag_types, type);

            whitespace(&pos);
            match(&pos, ",");

            ++next_value;
        }
        expect_closing(ctx, &pos, "}", "I wasn't able to parse the rest of this 'oneof'");
        return NewAST(ctx->file, start, pos, TaggedUnionDef, .name=name, .tag_names=tag_names, .tag_values=tag_values, .tag_types=tag_types);
    } else if (match(&pos, ":")) { // Conversion def x:T1 => T2 ...
        ast_t *source_type = expect_ast(ctx, start, &pos, _parse_type, "I expected a conversion source type here");
        expect_str(ctx, start, &pos, "as", "I expected an 'as' for a conversion definition");
        ast_t *target_type = expect_ast(ctx, start, &pos, _parse_type, "I expected a conversion target type here");
        ast_t *body = expect_ast(ctx, start, &pos, parse_opt_indented_block, "I expected a function body for this conversion definition"); 
        return NewAST(ctx->file, start, pos, ConvertDef, .var=name, .source_type=source_type, .target_type=target_type, .body=body);
    } else if (isdigit(*pos)) { // derived unit def
      derived_unit:;
        ast_t *derived;
        if (!(derived=optional_ast(ctx, &pos, parse_num))
            && !(derived=optional_ast(ctx, &pos, parse_int)))
            parser_err(ctx, start, pos, "Invalid derived unit definition");

        if (derived->tag == Int)
            derived = NewAST(ctx->file, derived->span.start, derived->span.end, Num, .n=(double)Match(derived, Int)->i, .units=Match(derived, Int)->units);

        if (derived->tag != Num || Match(derived, Num)->units == NULL)
            parser_err(ctx, derived->span.start, derived->span.end, "Derived units must have units");

        spaces(&pos);
        if (!match(&pos, ":="))
            parser_err(ctx, start, pos, "Invalid derived unit definition");

        ast_t *base;
        if (!(base=optional_ast(ctx, &pos, parse_num))
            && !(base=optional_ast(ctx, &pos, parse_int)))
            parser_err(ctx, start, pos, "Invalid derived unit definition");

        if (base->tag == Int)
            base = NewAST(ctx->file, base->span.start, base->span.end, Num, .n=(double)Match(base, Int)->i, .units=Match(base, Int)->units);

        if (base->tag != Num || Match(base, Num)->units == NULL)
            parser_err(ctx, base->span.start, base->span.end, "Derived units must have units");
        return NewAST(ctx->file, start, pos, UnitDef, .derived=derived, .base=base);
    }
    return NULL;
}

PARSER(parse_extern) {
    const char *start = pos;
    istr_t bl_name = get_id(&pos);
    if (bl_name) {
        spaces(&pos);
        if (!match(&pos, ":=")) return NULL;
    }
    if (!match_word(&pos, "extern")) return NULL;
    istr_t name = get_id(&pos);
    if (!bl_name) bl_name = name;
    ast_t *type;
    spaces(&pos);
    if (match(&pos, ":")) {
        type = expect_ast(ctx, start, &pos, _parse_type, "I couldn't parse the type for this extern");
    } else {
        parser_err(ctx, start, pos, "I couldn't get a type for this extern");
    }
    return NewAST(ctx->file, start, pos, Extern, .name=name, .bl_name=bl_name, .type=type);
}

PARSER(parse_doctest) {
    const char *start = pos;
    if (!match(&pos, ">>>")) return NULL;
    spaces(&pos);
    ast_t *expr = expect_ast(ctx, start, &pos, parse_statement, "I couldn't parse the expression for this doctest");
    whitespace(&pos);
    istr_t output = NULL;
    if (match(&pos, "===")) {
        spaces(&pos);
        const char *output_start = pos,
                   *output_end = strchrnul(pos, '\n');
        if (output_end <= output_start)
            parser_err(ctx, output_start, output_end, "You're missing expected output here");
        output = intern_strn(output_start, (size_t)(output_end - output_start));
        pos = output_end;
    }
    return NewAST(ctx->file, start, pos, DocTest, .expr=expr, .output=output);
}

PARSER(parse_use) {
    const char *start = pos;
    if (!match_word(&pos, "use")) return NULL;
    spaces(&pos);
    size_t path_len = strcspn(pos, "\n");
    istr_t path = intern_strn(pos, path_len);
    pos += path_len;
    return NewAST(ctx->file, start, pos, Use, .path=path);
}

PARSER(parse_export) {
    const char *start = pos;
    if (!match_word(&pos, "export")) return NULL;
    spaces(&pos);
    NEW_LIST(istr_t, vars);
    for (;;) {
        istr_t var = get_id(&pos);
        if (!var) break;
        APPEND(vars, var);
        spaces(&pos);
        match(&pos, ",");
    }
    return NewAST(ctx->file, start, pos, Export, .vars=vars);
}

PARSER(parse_inline_block) {
    spaces(&pos);
    const char *start = pos;
    NEW_LIST(ast_t*, statements);
    while (*pos) {
        spaces(&pos);
        ast_t *stmt = optional_ast(ctx, &pos, parse_statement);
        if (!stmt) break;
        APPEND(statements, stmt);
        spaces(&pos);
        if (!match(&pos, ";")) break;
    }
    return NewAST(ctx->file, start, pos, Block, .statements=statements);
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

ast_t *parse_type(bl_file_t *file, jmp_buf *on_err) {
    parse_ctx_t ctx = {
        .file=file,
        .on_err=on_err,
    };

    const char *pos = file->text;
    ast_t *ast = _parse_type(&ctx, pos);
    pos = ast->span.end;
    whitespace(&pos);
    if (strlen(pos) > 0) {
        parser_err(&ctx, pos, pos + strlen(pos), "I couldn't parse this part of the type");
    }
    return ast;
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
