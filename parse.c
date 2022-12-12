// Run a BP parser to get a match and convert that to an AST structure

#include <bp/files.h>
#include <bp/match.h>
#include <bp/pattern.h>
#include <bp/printmatch.h>
#include <bp/json.h>
#include <ctype.h>
#include <err.h>
#include <gc.h>
#include <gc/cord.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "libblang/list.h"
#include "parse.h"
#include "util.h"

#ifndef streq
#define streq(a,b) (strcmp(a,b) == 0)
#endif

#ifndef strneq
#define strneq(a,b,n) (strncmp(a,b,n) == 0)
#endif

#define AST(m, ast_tag, ...) (new(ast_t, .tag=ast_tag, .match=m, .__data.ast_tag={__VA_ARGS__}))

ast_t *match_to_ast(match_t *m);

static pat_t *grammar = NULL;
static file_t *loaded_files = NULL;

file_t *parsing = NULL;

//
// If there was a parse error while building a pattern, print an error message and exit.
//
static inline pat_t *assert_pat(file_t *f, maybe_pat_t maybe_pat)
{
    if (!maybe_pat.success) {
        const char *err_start = maybe_pat.value.error.start,
              *err_end = maybe_pat.value.error.end,
              *err_msg = maybe_pat.value.error.msg;

        const char *nl = memrchr(f->start, '\n', (size_t)(err_start - f->start));
        const char *sol = nl ? nl+1 : f->start;
        nl = memchr(err_start, '\n', (size_t)(f->end - err_start));
        const char *eol = nl ? nl : f->end;
        if (eol < err_end) err_end = eol;

        fprintf(stderr, "\033[31;7;1m%s\033[0m\n", err_msg);
        fprintf(stderr, "%.*s\033[41;30m%.*s\033[m%.*s\n",
                (int)(err_start - sol), sol,
                (int)(err_end - err_start), err_start,
                (int)(eol - err_end), err_end);
        fprintf(stderr, "\033[34;1m");
        const char *p = sol;
        for (; p < err_start; ++p) (void)fputc(*p == '\t' ? '\t' : ' ', stderr);
        if (err_start == err_end) ++err_end;
        for (; p < err_end; ++p)
            if (*p == '\t')
                // Some janky hacks: 8 ^'s, backtrack 8 spaces, move forward a tab stop, clear any ^'s that overshot
                fprintf(stderr, "^^^^^^^^\033[8D\033[I\033[K");
            else
                (void)fputc('^', stderr);
        fprintf(stderr, "\033[m\n");
        exit(EXIT_FAILURE);
    }
    return maybe_pat.value.pat;
}

//
// Initialize the Blang syntax pattern
//
static void load_grammar(void)
{
    file_t *builtins_file = load_file(&loaded_files, "/etc/bp/builtins.bp");
    file_t *blang_syntax = load_file(&loaded_files, "syntax.bp");
    grammar = chain_together(
        assert_pat(builtins_file, bp_pattern(builtins_file->start, builtins_file->end)),
        assert_pat(blang_syntax, bp_pattern(blang_syntax->start, blang_syntax->end)));
}

//
// Print error information from a match
//
static void print_err(file_t *f, match_t *m, int context) {
    fprintf(stderr, "\x1b[31;7;1m Syntax Error: \x1b[0;31;1m ");
    fprint_match(stderr, f->start, m, NULL);
    fprintf(stderr, "\x1b[m\n\n");
    highlight_match(stderr, f, m, context);
}

//
// Report any errors and exit
//
static void report_errors(file_t *f, match_t *m, bool stop_on_first)
{
    pat_t *pat = m->pat;
    if (pat->type == BP_TAGGED && strncmp(pat->args.capture.name, "ParseError", pat->args.capture.namelen) == 0) {
        print_err(f, m, 2);
        if (stop_on_first)
            exit(1);
    }
    if (m->children) {
        for (int i = 0; m->children[i]; i++)
            report_errors(f, m->children[i], stop_on_first);
    }
}

const char *tag_names[] = {
    [Unknown] = "???", [Nil]="Nil", [Bool]="Bool", [Var]="Var",
    [Int]="Int", [Num]="Num", [Range]="Range",
    [StringLiteral]=NULL, [StringJoin]="String", [DSL]="DSL", [Interp]="Interp",
    [Declare]="Declaration", [Assign]="Assignment",
    [AddUpdate]="AddUpdate", [SubtractUpdate]="SubUpdate", [MultiplyUpdate]="MulUpdate", [DivideUpdate]="DivUpdate",
    [AndUpdate]="AndUpdate", [OrUpdate]="OrUpdate",
    [Add]="Add", [Subtract]="Subtract", [Multiply]="Multiply", [Divide]="Divide", [Power]="Power", [Modulus]="Mod",
    [And]="And", [Or]="Or", [Xor]="Xor",
    [Equal]="Equal", [NotEqual]="NotEqual", [Greater]="Greater", [GreaterEqual]="GreaterEq", [Less]="Less", [LessEqual]="LessEq",
    [Not]="Not", [Negative]="Negative", [Len]="Len", [Maybe]="Maybe",
    [TypeOf]="TypeOf", [SizeOf]="SizeOf",
    [List]="List", [Table]="Table",
    [FunctionDef]="FnDef", [MethodDef]="MethodDef", [Lambda]="Lambda",
    [FunctionCall]="FnCall", [KeywordArg]="KeywordArg",
    [Block]="Block",
    [Do]="Do", [If]="If", [For]="For", [While]="While", [Repeat]="Repeat", [When]="When",
    [Skip]="Skip", [Stop]="Stop",
    [Return]="Return",
    [Fail]="Fail",
    [TypeList]="ListType", [TypeTable]="TableType",
    [TypeFunction]="FnType", [TypeOption]="OptionalType",
    [Cast]="Cast", [As]="As", [Extern]="Extern",
    [Struct]="Struct", [StructDef]="StructDef", [StructField]="StructField", [StructFieldDef]="StructFieldDef",
    [EnumDef]="EnumDef", [EnumField]="EnumField",
    [Index]="IndexedTerm", [FieldName]="FieldName",
};

static ast_tag_e get_tag(match_t *m)
{
    const char *tag = m->pat->args.capture.name;
    size_t len = m->pat->args.capture.namelen;
    for (size_t i = 0; i < sizeof(tag_names)/sizeof(tag_names[0]); i++) {
        if (!tag_names[i]) continue;
        if (strncmp(tag_names[i], tag, len) == 0 && tag_names[i][len] == '\0')
            return (ast_tag_e)i;
    }
    if (strncmp(tag, "AddSub", len) == 0) {
        match_t *op = get_named_capture(m, "op", -1);
        return *op->start == '+' ? Add : Subtract;
    } else if (strncmp(tag, "MulDiv", len) == 0) {
        match_t *op = get_named_capture(m, "op", -1);
        return *op->start == '*' ? Multiply : Divide;
    }
    return Unknown;
}


//
// Convert a match to an interned string
//
static istr_t match_to_istr(match_t *m)
{
    if (!m) return NULL;
    // Rough estimate of size
    FILE *f = fmemopen(NULL, 2*(size_t)(m->end - m->start) + 1, "r+");
    fprint_match(f, m->start, m, NULL);
    fputc('\0', f);
    fseek(f, 0, SEEK_SET);
    CORD c = CORD_from_file_eager(f);
    return intern_str(CORD_to_const_char_star(c));
}

static inline istr_t capture_istr(match_t *m, const char *name)
{
    return match_to_istr(get_named_capture(m, name, -1));
}

static inline ast_t *capture_ast(match_t *m, const char *name)
{
    return match_to_ast(get_named_capture(m, name, -1));
}

//
// Convert a match structure (from BP) into an AST structure (for Blang)
//
ast_t *match_to_ast(match_t *m)
{
    if (!m) return NULL;
    pat_t *pat = m->pat;
    if (pat->type == BP_TAGGED) {
        ast_tag_e tag = get_tag(m);
        switch (tag) {
        case Nil: {
            return AST(m, Nil, .type=capture_ast(m, "type"));
        }
        case Bool: return AST(m, Bool, .b=strncmp(m->start, "no", 2) != 0);
        case Var: {
            istr_t v = intern_strn(m->start, (size_t)(m->end - m->start));
            return AST(m, Var, .name=v);
        }
        case Int: {
            int64_t i;
            char buf[(int)(m->end - m->start + 1)];
            char *dest = buf;
            const char *start = m->start;
            bool negative = *start == '-';
            if (*start == '-')
                ++start;
            else if (*start == '+')
                ++start;
            for (const char *src = start; src < m->end; ++src)
                if (isalnum(*src))
                    *(dest++) = *src;
            *dest = '\0';
            if (strncmp(buf, "0x", 2) == 0)
                i = strtol(buf+2, NULL, 16);
            else if (strncmp(buf, "0o", 2) == 0)
                i = strtol(buf+2, NULL, 8);
            else if (strncmp(buf, "0b", 2) == 0)
                i = strtol(buf+2, NULL, 2);
            else
                i = strtol(buf, NULL, 10);

            return AST(m, Int, .i=negative ? -i : i);
        }
        case Num: {
            double n = strtod(m->start, NULL);
            return AST(m, Num, .n=n);
        }
        case Range: {
            return AST(m, Range,
                       .first=capture_ast(m, "first"),
                       .last=capture_ast(m, "last"),
                       .step=capture_ast(m, "step"));
        }
        case StringJoin: {
            match_t *content = get_named_capture(m, "content", -1);
            List(ast_t*) chunks = EMPTY_LIST(ast_t*);
            const char *prev = content->start;
            for (int i = 1; ; i++) { // index 1 == text content
                match_t *cm = get_numbered_capture(content, i);
                assert(cm != content);
                if (!cm) break;
                if (cm->start > prev) {
                    APPEND(chunks, AST(m, StringLiteral, .str=intern_strn(prev, (size_t)(cm->start - prev))));
                }
                assert(match_to_ast(cm));
                APPEND(chunks, match_to_ast(cm));
                prev = cm->end;
            }
            if (content->end > prev) {
                APPEND(chunks, AST(m, StringLiteral, .str=intern_strn(prev, (size_t)(content->end - prev))));
            }
            return AST(m, StringJoin, .children=chunks);
        }
        case Interp: {
            return match_to_ast(get_named_capture(m, "value", -1));
        }
        case List: {
            match_t *type_m = get_named_capture(m, "type", -1);
            if (type_m)
                return AST(m, List, .type=match_to_ast(type_m));
            
            List(ast_t*) items = EMPTY_LIST(ast_t*);
            for (int i = 1; ; i++) {
                match_t *im = get_numbered_capture(m, i);
                assert(im != m);
                if (!im) break;
                // Shim: [x if cond] is mapped to [x if cond else skip]
                // `skip` in a list means "don't add this item"
                ast_t *item = match_to_ast(im);
                if (item->tag == If && !Match(item, If)->else_body)
                    Match(item, If)->else_body = AST(item->match, Skip);
                APPEND(items, item);
            }
            return AST(m, List, .items=items);
        }
        case Do: {
            NEW_LIST(ast_t*, blocks);
            for (int i = 1; ; i++) {
                ast_t *block = match_to_ast(get_numbered_capture(m, i));
                if (!block) break;
                APPEND(blocks, block);
            }
            return AST(m, Do, .blocks=blocks);
        }
        case Block: {
            NEW_LIST(ast_t*, stmts);
            for (int i = 1; ; i++) {
                ast_t *stmt = match_to_ast(get_numbered_capture(m, i));
                if (!stmt) break;
                APPEND(stmts, stmt);
            }
            return AST(m, Block, .statements=stmts);
        }
        case FunctionDef: case MethodDef: case Lambda: {
            istr_t name = tag == Lambda ? NULL : capture_istr(m, "name");
            NEW_LIST(istr_t, arg_names);
            NEW_LIST(ast_t*, arg_types);
            match_t *args_m = get_named_capture(m, "args", -1);
            for (int i = 1; ; i++) {
                match_t *arg_m = get_numbered_capture(args_m, i);
                if (!arg_m) break;
                APPEND(arg_names, capture_istr(arg_m, "name"));
                APPEND(arg_types, capture_ast(arg_m, "type"));
            }
            ast_t *ret_type = capture_ast(m, "returnType");
            match_t *body_m = get_named_capture(m, "body", -1);
            ast_t *body = match_to_ast(body_m);

            if (tag == Lambda)
                body = AST(body_m, Return, .value=body);

            istr_t self = NULL;
            if (tag == MethodDef)
                self = match_to_istr(get_named_capture(m, "selfVar", -1));

            if (tag == FunctionDef) {
                return AST(m, FunctionDef, .name=name,
                           .arg_names=arg_names, .arg_types=arg_types,
                           .ret_type=ret_type, .body=body);
            } else if (tag == MethodDef) {
                return AST(m, MethodDef, .name=name, .self=self,
                           .arg_names=arg_names, .arg_types=arg_types,
                           .ret_type=ret_type, .body=body);
            } else {
                return AST(m, Lambda, .arg_names=arg_names, .arg_types=arg_types,
                           .ret_type=ret_type, .body=body);
            }
        }
        case FunctionCall: {
            match_t *fn_m = get_named_capture(m, "fn", -1);
            ast_t *fn = match_to_ast(fn_m);
            NEW_LIST(ast_t*, args);
            for (int i = 1; ; i++) {
                ast_t *arg = match_to_ast(get_numbered_capture(m, i));
                if (!arg) break;
                APPEND(args, arg);
            }
            return AST(m, FunctionCall, .fn=fn, .args=args);
        }
        case KeywordArg: {
            return AST(m, KeywordArg, .name=capture_istr(m, "name"), .arg=capture_ast(m, "value"));
        }
        case StructField: {
            return AST(m, StructField, .name=capture_istr(m, "name"), .value=capture_ast(m, "value"));
        }
        case EnumField: {
            return AST(m, EnumField, .name=capture_istr(m, "name"), .value=capture_ast(m, "value"));
        }
        case Return: {
            return AST(m, Return, .value=capture_ast(m, "value"));
        }
        case StructDef: {
            istr_t name = capture_istr(m, "name");
            NEW_LIST(ast_t*, members);
            for (int i = 1; ; i++) {
                ast_t *member = match_to_ast(get_numbered_capture(m, i));
                if (!member) break;
                APPEND(members, member);
            }
            return AST(m, StructDef, .name=name, .members=members);
        }
        case Struct: {
            ast_t *type = match_to_ast(get_named_capture(m, "type", -1));
            NEW_LIST(ast_t*, members);
            for (int i = 1; ; i++) {
                ast_t *member = match_to_ast(get_numbered_capture(m, i));
                if (!member) break;
                APPEND(members, member);
            }
            return AST(m, Struct, .type=type, .members=members);
        }
        case StructFieldDef: {
            ast_t *type = match_to_ast(get_named_capture(m, "type", -1));
            NEW_LIST(istr_t, names);
            match_t *names_m = get_named_capture(m, "names", -1);
            for (int i = 1; ; i++) {
                istr_t name = match_to_istr(get_numbered_capture(names_m, i));
                if (!name) break;
                APPEND(names, name);
            }
            return AST(m, StructFieldDef, .names=names, .type=type);
        }
        case EnumDef: {
            istr_t name = match_to_istr(get_named_capture(m, "name", -1));
            NEW_LIST(istr_t, tag_names);
            NEW_LIST(int64_t, tag_values);
            NEW_LIST(ast_t*, tag_types);
            int64_t next_value = 0;
            for (int i = 1; ; i++) {
                match_t *tag_m = get_numbered_capture(m, i);
                if (!tag_m) break;
                match_t *name_m = get_named_capture(tag_m, "name", -1);
                istr_t name = match_to_istr(name_m);
                ast_t *value = match_to_ast(get_named_capture(tag_m, "value", -1));
                if (value)
                    next_value = Match(value, Int)->i;
                APPEND(tag_names, name);
                APPEND(tag_values, next_value);
                match_t *data_m = get_named_capture(tag_m, "data", -1);
                if (data_m) {
                    NEW_LIST(ast_t*, members);
                    for (int j = 1; ; j++) {
                        ast_t *member = match_to_ast(get_numbered_capture(data_m, j));
                        if (!member) break;
                        APPEND(members, member);
                    }
                    ast_t *field = AST(m, StructDef, .name=name, .members=members);
                    APPEND(tag_types, field);
                } else {
                    APPEND(tag_types, NULL);
                }
                ++next_value;
            }
            return AST(m, EnumDef, .name=name, .tag_names=tag_names,
                       .tag_values=tag_values, .tag_types=tag_types);
        }
        case FieldName: {
            return AST(m, FieldName, .name=match_to_istr(m));
        }
        case Index: {
            ast_t *indexed = match_to_ast(get_named_capture(m, "value", -1));
            ast_t *index = match_to_ast(get_named_capture(m, "index", -1));
            if (index->tag == FieldName)
                return AST(m, FieldAccess, .fielded=indexed, .field=Match(index, FieldName)->name);
            return AST(m, Index, .indexed=indexed, .index=index);
        }
        case If: {
            NEW_LIST(ast_clause_t, clauses);
            for (int i = 1; ; i++) {
                match_t *clause_m = get_numbered_capture(m, i);
                if (!clause_m) break;
                match_t *condition_m = get_named_capture(clause_m, "condition", -1);
                match_t *body_m = get_named_capture(clause_m, "body", -1);
                assert(condition_m && body_m);
                ast_clause_t clause = {
                    .condition=match_to_ast(condition_m),
                    .body=match_to_ast(body_m),
                };
                // Workaround because this is an array-of-structs instead of pointers:
                // (Can't use APPEND() macro)
                list_append((list_t*)clauses, sizeof(ast_clause_t), &clause);
            }
            ast_t *else_block = match_to_ast(get_named_capture(m, "elseBody", -1));
            return AST(m, If, .clauses=clauses, .else_body=else_block);
        }
        case When: {
            ast_t *subject = match_to_ast(get_named_capture(m, "subject", -1));
            NEW_LIST(ast_cases_t, cases);
            for (int i = 1; ; i++) {
                match_t *clause_m = get_numbered_capture(m, i);
                if (!clause_m) break;
                match_t *cases_m = get_named_capture(clause_m, "cases", -1);
                NEW_LIST(ast_t*, values);
                for (int casenum = 1; ; casenum++) {
                    ast_t *caseval = match_to_ast(get_numbered_capture(cases_m, casenum));
                    if (!caseval) break;
                    APPEND(values, caseval);
                }
                ast_t *casebody = match_to_ast(get_named_capture(clause_m, "body", -1));
                ast_cases_t case_ = {
                    .cases=values,
                    .body=casebody,
                };
                // Workaround because this is an array-of-structs instead of pointers:
                // (Can't use APPEND() macro)
                list_append((list_t*)cases, sizeof(ast_cases_t), &case_);
            }
            ast_t *else_block = match_to_ast(get_named_capture(m, "elseBody", -1));
            return AST(m, When, .subject=subject, .cases=cases, .default_body=else_block);
        }
        case While: case Repeat: {
            ast_t *condition = match_to_ast(get_named_capture(m, "condition", -1));
            ast_t *body = match_to_ast(get_named_capture(m, "body", -1));
            ast_t *filter = match_to_ast(get_named_capture(m, "filter", -1));
            if (filter)
                body = AST(m, Block, .statements=LIST(ast_t*, filter, body));
            ast_t *between = match_to_ast(get_named_capture(m, "between", -1));
            if (tag == While)
                return AST(m, While, .condition=condition, .body=body, .between=between);
            else
                return AST(m, Repeat, .body=body, .between=between);
        }
        case For: {
            ast_t *iter = capture_ast(m, "iterable");
            istr_t key = capture_istr(m, "index");
            istr_t value = capture_istr(m, "val");
            ast_t *body = capture_ast(m, "body");
            ast_t *filter = capture_ast(m, "filter");
            if (filter)
                body = AST(m, Block, .statements=LIST(ast_t*, filter, body));
            ast_t *between = capture_ast(m, "between");
            return AST(m, For, .iter=iter, .key=key, .value=value,
                       .body=body, .between=between);
        }
        case Skip: {
            return AST(m, Skip, .target=capture_istr(m, "target"));
        }
        case Stop: {
            return AST(m, Stop, .target=capture_istr(m, "target"));
        }
#define BINOP(t) case t: return AST(m, t, .lhs=capture_ast(m, "lhs"), .rhs=capture_ast(m, "rhs"))
        BINOP(Add); BINOP(Subtract); BINOP(Multiply); BINOP(Divide); BINOP(Power);
        BINOP(Modulus); BINOP(AddUpdate); BINOP(SubtractUpdate); BINOP(MultiplyUpdate);
        BINOP(DivideUpdate); BINOP(And); BINOP(Or); BINOP(Xor); BINOP(Equal);
        BINOP(NotEqual); BINOP(Less); BINOP(LessEqual); BINOP(Greater); BINOP(GreaterEqual);
#undef BINOP
        case Declare: {
            return AST(m, Declare, .name=capture_istr(m, "lhs"), .value=capture_ast(m, "rhs"));
        }
        case Cast: {
            return AST(m, Cast, .value=capture_ast(m, "expr"), .type=capture_ast(m, "type"));
        }
        case As: {
            return AST(m, As, .value=capture_ast(m, "expr"), .type=capture_ast(m, "type"));
        }
        case Extern: {
            return AST(m, Extern, .name=capture_istr(m, "name"), .type=capture_ast(m, "type"));
        }
#define UNOP(t) case t: return AST(m, t, .value=capture_ast(m, "value"))
        UNOP(Not); UNOP(Negative); UNOP(Len); UNOP(Maybe); UNOP(TypeOf); UNOP(SizeOf);
#undef UNOP
        case Assign: {
            NEW_LIST(ast_t*, lhs);
            NEW_LIST(ast_t*, rhs);
            match_t *lhses = get_named_capture(m, "lhs", -1);
            match_t *rhses = get_named_capture(m, "rhs", -1);
            for (int64_t i = 1; ; i++) {
                ast_t *var = match_to_ast(get_numbered_capture(get_numbered_capture(lhses, 1), i));
                if (var && var->tag != Var) {
                    fprintf(stderr, "\x1b[31;7;1mOnly variables can be assigned to\x1b[m\n\n");
                    highlight_match(stderr, parsing, var->match, 2);
                    exit(1);
                }
                ast_t *val = match_to_ast(get_numbered_capture(get_numbered_capture(rhses, 1), i));
                if (!var && !val) {
                    break;
                } else if (var && !val) {
                    fprintf(stderr, "\x1b[31;7;1mThis term is missing a value to assign it\x1b[m\n\n");
                    highlight_match(stderr, parsing, var->match, 2);
                    exit(1);
                } else if (val && !var) {
                    fprintf(stderr, "\x1b[31;7;1mThis value doesn't have a corresponding term to assign to\x1b[m\n\n");
                    highlight_match(stderr, parsing, val->match, 2);
                    exit(1);
                }

                APPEND(lhs, var);
                APPEND(rhs, val);
            }
            return AST(m, Assign, .targets=lhs, .values=rhs);
        }
        case Fail: {
            return AST(m, Fail, .message=capture_ast(m, "message"));
        }
        case TypeOption: {
            return AST(m, TypeOption, .nonnil=capture_ast(m, "nonnil"));
        }
        case TypeList: {
            return AST(m, TypeList, .item_type=capture_ast(m, "itemType"));
        }
        case TypeFunction: {
            ast_t *ret = capture_ast(m, "returnType");
            assert(ret);
            match_t *args_m = get_named_capture(m, "args", -1);
            NEW_LIST(ast_t*, arg_types);
            NEW_LIST(istr_t, arg_names);
            for (int64_t i = 1; ; i++) {
                match_t *arg_m = get_numbered_capture(args_m, i);
                if (!arg_m) break;
                APPEND(arg_names, capture_istr(arg_m, "name"));
                APPEND(arg_types, capture_ast(arg_m, "type"));
            }
            return AST(m, TypeFunction, .ret_type=ret, .arg_names=arg_names, .arg_types=arg_types);
        }
        default: break;
        }

        const char *tagname = m->pat->args.capture.name;
        size_t taglen = m->pat->args.capture.namelen;
        if (strneq(tagname, "Newline", taglen)) {
            return AST(m, StringLiteral, .str=intern_str("\n"));
        } else if (strneq(tagname, "Escape", taglen)) {
            static const char *unescapes[255] = {['a']="\a",['b']="\b",['e']="\e",['f']="\f",['n']="\n",['r']="\r",['t']="\t",['v']="\v"};
            if (unescapes[(int)m->start[1]]) {
                return AST(m, StringLiteral, .str=intern_str(unescapes[(int)m->start[1]]));
            } else if (m->start[1] == 'x') {
                char *endptr = (char*)(m->start + 4);
                CORD c = CORD_cat_char(NULL, (char)strtol(m->start+2, &endptr, 16));
                return AST(m, StringLiteral, .str=intern_str(CORD_to_char_star(c)));
            } else if ('0' <= m->start[1] && m->start[1] <= '7') {
                char *endptr = (char*)(m->start + 4);
                CORD c = CORD_cat_char(NULL, (char)strtol(m->start+2, &endptr, 8));
                return AST(m, StringLiteral, .str=intern_str(CORD_to_char_star(c)));
            } else {
                return AST(m, StringLiteral, .str=intern_strn(m->start+1, 1));
            }
            return AST(m, StringLiteral, .str=match_to_istr(m));
        } else {
            fprintf(stderr, "\x1b[31;7;1mParsing isn't fully implemented for AST tag: %.*s\x1b[m\n\n", (int)pat->args.capture.namelen, pat->args.capture.name);
            highlight_match(stderr, parsing, m, 2);
            exit(1);
        }
    } else if (m->children) {
        for (int i = 0; m->children[i]; i++) {
            ast_t *ast = match_to_ast(m->children[i]);
            if (ast) return ast;
        }
    }
    return NULL;
}

ast_t *parse(file_t *f)
{
    if (grammar == NULL) load_grammar();
    parsing = f;
    match_t *m = NULL;
    ast_t *ast = NULL;
    if (next_match(&m, f->start, f->end, grammar, grammar, NULL, false)) {
        if (m->start > f->start) {
            fprintf(stderr, "File contains junk at the front\n");
            exit(1);
        } else if (m->end < f->end) {
            fprintf(stderr, "File contains junk at the end\n");
            exit(1);
        } else {
            report_errors(f, m, true);
            ast = match_to_ast(m);
        }
    }
    parsing = NULL;
    return ast;
}
// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
