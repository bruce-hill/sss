// Run a BP parser to get a match and convert that to an AST structure

#include <bp/files.h>
#include <bp/match.h>
#include <bp/pattern.h>
#include <bp/printmatch.h>
#include <bp/json.h>
#include <err.h>
#include <gc.h>
#include <gc/cord.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "datastructures/list.h"
#include "parse.h"
#include "util.h"

#ifndef streq
#define streq(a,b) (strcmp(a,b) == 0)
#endif

#ifndef strneq
#define strneq(a,b,n) (strncmp(a,b,n) == 0)
#endif

#define AST(m, mykind, ...) (new(ast_t, .kind=mykind, .match=m __VA_OPT__(,) __VA_ARGS__))

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
static void print_err(file_t *f, match_t *m) {
    fprintf(stderr, "\x1b[31;7;1mSyntax Error: ");
    fprint_match(stderr, f->start, m, NULL);
    fprintf(stderr, "\x1b[m\n\n");
    highlight_match(stderr, f, m);
}

//
// Report any errors and exit
//
static void report_errors(file_t *f, match_t *m, bool stop_on_first)
{
    pat_t *pat = m->pat;
    if (pat->type == BP_TAGGED && strncmp(pat->args.capture.name, "ParseError", pat->args.capture.namelen) == 0) {
        print_err(f, m);
        if (stop_on_first)
            exit(1);
    }
    if (m->children) {
        for (int i = 0; m->children[i]; i++)
            report_errors(f, m->children[i], stop_on_first);
    }
}

const char *kind_tags[] = {
    [Unknown] = "???", [Nil]="Nil", [Bool]="Bool", [Var]="Var",
    [Int]="Int", [Num]="Num",
    [StringLiteral]=NULL, [StringJoin]="String", [DSL]="DSL", [Interp]="Interp",
    [Declare]="Declaration", [Assign]="Assignment",
    [AddUpdate]="AddUpdate", [SubtractUpdate]="SubUpdate", [MultiplyUpdate]="MulUpdate", [DivideUpdate]="DivUpdate",
    [AndUpdate]="AndUpdate", [OrUpdate]="OrUpdate",
    [Add]="Add", [Subtract]="Subtract", [Multiply]="Multiply", [Divide]="Divide", [Power]="Power", [Modulus]="Mod",
    [And]="And", [Or]="Or", [Xor]="Xor",
    [Equal]="Equal", [NotEqual]="NotEqual", [Greater]="Greater", [GreaterEqual]="GreaterEq", [Less]="Less", [LessEqual]="LessEq",
    [Not]="Not", [Negative]="Negative",
    [List]="List", [Table]="Table",
    [FunctionDef]="FnDef", [Lambda]="Lambda",
    [FunctionCall]="FnCall", [KeywordArg]="KeywordArg",
    [Block]="Block",
    [If]="If", [For]="For", [While]="While", [Repeat]="Repeat",
    [Skip]="Skip", [Stop]="Stop",
    [Return]="Return",
    [Fail]="Fail",
    [TypeName]="TypeVar",
    [TypeList]="ListType", [TypeTable]="TableType",
    [TypeFunction]="FnType", [TypeOption]="OptionalType",
    [Cast]="Cast", [As]="As",
};

static astkind_e get_kind(match_t *m)
{
    const char *tag = m->pat->args.capture.name;
    size_t len = m->pat->args.capture.namelen;
    for (size_t i = 0; i < sizeof(kind_tags)/sizeof(kind_tags[0]); i++) {
        if (!kind_tags[i]) continue;
        if (strncmp(kind_tags[i], tag, len) == 0 && kind_tags[i][len] == '\0')
            return (astkind_e)i;
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

//
// Convert a match structure (from BP) into an AST structure (for Blang)
//
ast_t *match_to_ast(match_t *m)
{
    if (!m) return NULL;
    pat_t *pat = m->pat;
    if (pat->type == BP_TAGGED) {
        astkind_e kind = get_kind(m);
        switch (kind) {
        case Nil: {
            ast_t *type = match_to_ast(get_named_capture(m, "type", -1));
            return AST(m, kind, .child=type);
        }
        case Bool: return AST(m, Bool, .b=strncmp(m->start, "no", 2) != 0);
        case Var: {
            istr_t v = intern_strn(m->start, (size_t)(m->end - m->start));
            return AST(m, Var, .str=v);
        }
        case Int: {
            int64_t i = strtol(m->start, NULL, 10);
            return AST(m, Int, .i=i);
        }
        case Num: {
            double n = strtod(m->start, NULL);
            return AST(m, Num, .n=n);
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
                return AST(m, List, .list.type=match_to_ast(type_m));
            
            List(ast_t*) items = EMPTY_LIST(ast_t*);
            for (int i = 1; ; i++) {
                match_t *im = get_numbered_capture(m, i);
                assert(im != m);
                if (!im) break;
                APPEND(items, match_to_ast(im));
            }
            return AST(m, List, .list.items=items);
        }
        case Block: {
            NEW_LIST(ast_t*, stmts);
            for (int i = 1; ; i++) {
                ast_t *stmt = match_to_ast(get_numbered_capture(m, i));
                if (!stmt) break;
                APPEND(stmts, stmt);
            }
            return AST(m, Block, .children=stmts);
        }
        case FunctionDef: case Lambda: {
            istr_t name = kind == FunctionDef ? match_to_istr(get_named_capture(m, "name", -1)) : NULL;
            NEW_LIST(istr_t, arg_names);
            NEW_LIST(ast_t*, arg_types);
            match_t *args_m = get_named_capture(m, "args", -1);
            for (int i = 1; ; i++) {
                match_t *arg_m = get_numbered_capture(args_m, i);
                if (!arg_m) break;
                match_t *arg_name = get_named_capture(arg_m, "name", -1);
                match_t *arg_type = get_named_capture(arg_m, "type", -1);
                assert(arg_name != NULL && arg_type != NULL);
                APPEND(arg_names, match_to_istr(arg_name));
                APPEND(arg_types, match_to_ast(arg_type));
            }
            match_t *ret_m = get_named_capture(m, "returnType", -1);
            ast_t *ret_type = ret_m ? match_to_ast(ret_m) : NULL;
            match_t *body_m = get_named_capture(m, "body", -1);
            ast_t *body = match_to_ast(body_m);

            if (kind == Lambda)
                body = AST(body_m, Return, .child=body);

            return AST(m, kind, .fn.name=name,
                       .fn.arg_names=arg_names, .fn.arg_types=arg_types,
                       .fn.ret_type=ret_type, .fn.body=body);
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
            return AST(m, FunctionCall, .call.fn=fn, .call.args=args);
        }
        case KeywordArg: {
            match_t *name_m = get_named_capture(m, "name", -1);
            CORD c = CORD_substr(name_m->start, 0, (size_t)(name_m->end - name_m->start));
            istr_t name = intern_str(CORD_to_char_star(c));
            ast_t *value = match_to_ast(get_named_capture(m, "value", -1));
            return AST(m, KeywordArg, .named.name=name, .named.value=value);
        }
        case Return: {
            return AST(m, Return, .child=match_to_ast(get_named_capture(m, "value", -1)));
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
                list_insert((list_t*)clauses, sizeof(ast_clause_t), INT_NIL, &clause, "Invalid list index: %ld");
            }
            ast_t *else_block = NULL;
            match_t *else_m = get_named_capture(m, "elseBody", -1);
            if (else_m)
                else_block = match_to_ast(else_m);
            return AST(m, If, .clauses=clauses, .else_body=else_block);
        }
        case While: case Repeat: {
            ast_t *condition = match_to_ast(get_named_capture(m, "condition", -1));
            ast_t *body = match_to_ast(get_named_capture(m, "body", -1));
            ast_t *filter = match_to_ast(get_named_capture(m, "filter", -1));
            if (filter)
                body = AST(m, Block, .children=LIST(ast_t*, body, filter));
            ast_t *between = match_to_ast(get_named_capture(m, "between", -1));
            return AST(m, kind, .loop.condition=condition, .loop.body=body, .loop.between=between);
        }
        case Skip: case Stop: {
            istr_t target = match_to_istr(get_named_capture(m, "target", -1));
            return AST(m, kind, .str=target);
        }
        case Add: case Subtract: case Multiply: case Divide: case Power: case Modulus:
        case AddUpdate: case SubtractUpdate: case MultiplyUpdate: case DivideUpdate:
        case And: case Or: case Xor:
        case Equal: case NotEqual: case Less: case LessEqual: case Greater: case GreaterEqual:
        case Declare: {
            ast_t *lhs = match_to_ast(get_named_capture(m, "lhs", -1));
            ast_t *rhs = match_to_ast(get_named_capture(m, "rhs", -1));
            return AST(m, kind, .lhs=lhs, .rhs=rhs);
        }
        case Cast: case As: {
            ast_t *expr = match_to_ast(get_named_capture(m, "expr", -1));
            ast_t *type = match_to_ast(get_named_capture(m, "type", -1));
            return AST(m, kind, .expr=expr, .type=type);
        }
        case Not: case Negative: case Len: {
            ast_t *child = match_to_ast(get_named_capture(m, "value", -1));
            return AST(m, kind, .child=child);
        }
        case Assign: {
            NEW_LIST(ast_t*, lhs);
            NEW_LIST(ast_t*, rhs);
            match_t *lhses = get_named_capture(m, "lhs", -1);
            match_t *rhses = get_named_capture(m, "rhs", -1);
            for (int64_t i = 1; ; i++) {
                ast_t *var = match_to_ast(get_numbered_capture(get_numbered_capture(lhses, 1), i));
                if (var && var->kind != Var) {
                    fprintf(stderr, "\x1b[31;7;1mOnly variables can be declared\x1b[m\n\n");
                    highlight_match(stderr, parsing, var->match);
                    exit(1);
                }
                ast_t *val = match_to_ast(get_numbered_capture(get_numbered_capture(rhses, 1), i));
                if (!var && !val) {
                    break;
                } else if (var && !val) {
                    fprintf(stderr, "\x1b[31;7;1mThis term is missing a value to assign it\x1b[m\n\n");
                    highlight_match(stderr, parsing, var->match);
                    exit(1);
                } else if (val && !var) {
                    fprintf(stderr, "\x1b[31;7;1mThis value doesn't have a corresponding term to assign to\x1b[m\n\n");
                    highlight_match(stderr, parsing, val->match);
                    exit(1);
                }

                APPEND(lhs, var);
                APPEND(rhs, val);
            }
            return AST(m, kind, .multiassign.lhs=lhs, .multiassign.rhs=rhs);
        }
        case Fail: {
            ast_t *msg = match_to_ast(get_named_capture(m, "message", -1));
            return AST(m, Fail, .child=msg);
        }
        case TypeOption: {
            ast_t *nonnil = match_to_ast(get_named_capture(m, "nonnil", -1));
            return AST(m, TypeOption, .child=nonnil);
        }
        case TypeName: {
            istr_t name = match_to_istr(m);
            return AST(m, TypeName, .str=name);
        }
        case TypeList: {
            ast_t *item_t = match_to_ast(get_named_capture(m, "itemType", -1));
            return AST(m, TypeList, .child=item_t);
        }
        default: break;
        }

        const char *tag = m->pat->args.capture.name;
        size_t taglen = m->pat->args.capture.namelen;
        if (strneq(tag, "Newline", taglen)) {
            return AST(m, StringLiteral, .str=intern_str("\n"));
        } else if (strneq(tag, "Escape", taglen)) {
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
                return AST(m, StringLiteral, .str=intern_strn(m->start, 1));
            }
            return AST(m, StringLiteral, .str=match_to_istr(m));
        } else {
            fprintf(stderr, "\x1b[31;7;1mUnimplemented AST tag: %.*s\x1b[m\n\n", (int)pat->args.capture.namelen, pat->args.capture.name);
            highlight_match(stderr, parsing, m);
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

//
// Print an AST (for debugging)
//
void print_ast(ast_t *ast) {
    if (!ast) {
        printf("\x1b[31;1m(NULL)\x1b[m");
        return;
    }
    switch (ast->kind) {
    case Bool: printf("\x1b[35m%s\x1b[m", ast->b ? "yes" : "no"); break;
    case Int: printf("\x1b[35m%ld\x1b[m", ast->i); break;
    case Num: printf("\x1b[35m%g\x1b[m", ast->n); break;
    case Var: printf("\x1b[1m%s\x1b[m", ast->str); break;
    case FunctionCall: {
        print_ast(ast->call.fn);
        printf("(");
        for (int64_t i = 0; i < LIST_LEN(ast->call.args); i++) {
            if (i > 0)
                printf(", ");
            print_ast(LIST_ITEM(ast->call.args, i));
        }
        printf(")");
        break;
    }
    case KeywordArg: {
        printf("\x1b[0;2m%s=\x1b[m", ast->named.name);
        print_ast(ast->named.value);
        break;
    }
    case StringJoin: {
        for (int64_t i = 0; i < LIST_LEN(ast->children); i++) {
            if (i > 0) printf("..");
            print_ast(LIST_ITEM(ast->children, i));
        }
        break;
    }
    case StringLiteral: printf("\x1b[35m\"%s\"\x1b[m", ast->str); break;
    case Block: {
        for (int64_t i = 0; i < LIST_LEN(ast->children); i++) {
            printf("\x1b[2m%ld |\x1b[m ", i+1);
            print_ast(LIST_ITEM(ast->children, i));
            printf("\n");
        }
        break;
    }

    case Add: case Subtract: case Multiply: case Divide: case Power: case Modulus:
    case And: case Or: case Xor:
    case Equal: case NotEqual: case Less: case LessEqual: case Greater: case GreaterEqual:
    case Declare: case Cast: case As: {
        printf("%s(", get_ast_kind_name(ast->kind));
        print_ast(ast->lhs);
        printf(",");
        print_ast(ast->rhs);
        printf(")");
        break;
    }

    case While: {
        printf("While(");
        print_ast(ast->loop.condition);
        printf(",");
        print_ast(ast->loop.body);
        printf(")");
        break;
    }

    case Fail: {
        printf("\x1b[33mfail\x1b[m ");
        print_ast(ast->child);
        break;
    }
    default: printf("???");
    }
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
