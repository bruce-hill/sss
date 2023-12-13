// Some basic operations defined on AST nodes, mainly converting to
// strings for debugging.
#include <gc/cord.h>
#include <stdarg.h>
#include <printf.h>

#include "ast.h"

static const char *OP_NAMES[] = {
    [OP_UNKNOWN]="unknown",
    [OP_NOT]="not", [OP_NEGATIVE]="negative",
    [OP_POWER]="^", [OP_MULT]="*", [OP_DIVIDE]="/",
    [OP_MOD]="mod", [OP_MOD1]="mod1", [OP_PLUS]="+", [OP_MINUS]="minus",
    [OP_CONCAT]="++", [OP_LSHIFT]="<<", [OP_RSHIFT]=">>", [OP_MIN]="min",
    [OP_MAX]="max", [OP_MIX]="mix", [OP_RANGE]="..", [OP_BY]="by", [OP_AS]="as",
    [OP_IN]="in", [OP_NOT_IN]="not in", [OP_EQ]="==", [OP_NE]="!=", [OP_LT]="<",
    [OP_LE]="<=", [OP_GT]=">", [OP_GE]=">=", [OP_AND]="and", [OP_OR]="or", [OP_XOR]="xor",
};

static inline CORD CORD_asprintf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    CORD c = NULL;
    CORD_vsprintf(&c, fmt, args);
    va_end(args);
    return c;
}

static CORD ast_to_cord(const char *name, ast_t *ast);

static CORD ast_list_to_cord(const char *name, ARRAY_OF(ast_t*) asts)
{
    if (!asts)
        return "\x1b[35mNULL\x1b[m";

    CORD c = "[";
    if (name) CORD_sprintf(&c, "%s=[", name);
    for (int64_t i = 0, len = LENGTH(asts); i < len; i++) {
        if (i > 0) c = CORD_cat(c, ", ");
        c = CORD_cat(c, ast_to_cord(NULL, ith(asts, i)));
    }
    c = CORD_cat(c, "]");
    return c;
}

static CORD concat_cords(int len, CORD cords[len]) {
    CORD c = NULL;
    for (int i = 0; i < len; i++) {
        if (i > 0) c = CORD_cat(c, ", ");
        c = CORD_cat(c, cords[i]);
    }
    return c;
}

static CORD str_list_to_cord(const char *name, ARRAY_OF(const char*) strs)
{
    CORD c;
    if (name) CORD_sprintf(&c, "%s=", name);
    else c = NULL;
    if (!strs) return CORD_cat(c, "\x1b[35mNULL\x1b[m");
    c = CORD_cat(c, "[");
    for (int64_t i = 0, len = LENGTH(strs); i < len; i++) {
        if (i > 0) c = CORD_cat(c, ", ");
        if (ith(strs, i))
            c = CORD_cat(c, ith(strs, i));
        else
            c = CORD_cat(c, "(NULL)");
    }
    c = CORD_cat(c, "]");
    return c;
}

static CORD label_str(const char *name, const char *str) {
    if (str)
        return CORD_asprintf("%s=\x1b[35m\"%s\"\x1b[m", name, str);
    else
        return CORD_asprintf("%s=\x1b[35m(NULL)\x1b[m", name);
}
static CORD label_int(const char *name, int64_t i) { return CORD_asprintf("%s=\x1b[35m%ld\x1b[m", name, i); }
static CORD label_double(const char *name, double d) { return CORD_asprintf("%s=\x1b[35m%g\x1b[m", name, d); }
static CORD label_bool(const char *name, bool b) { return CORD_asprintf("%s=\x1b[35m%s\x1b[m", name, b ? "yes" : "no"); }
static CORD label_args(const char *name, args_t args) {
    CORD c = CORD_asprintf("%s={", name ? name : "(NULL)");
    for (int64_t i = 0; i < LENGTH(args.names); i++) {
        if (i > 0) c = CORD_cat(c, ", ");
        if (args.names && ith(args.names, i))
            c = CORD_cat(c, ith(args.names, i));
        if (args.types && ith(args.types, i))
            CORD_sprintf(&c, "%r:%s", c, ast_to_cord(NULL, ith(args.types, i)));
        if (args.defaults && ith(args.defaults, i))
            CORD_sprintf(&c, "%r=%s", c, ast_to_cord(NULL, ith(args.defaults, i)));
    }
    c = CORD_cat(c, "}");
    return c;
}
static CORD label_args_list(const char *name, ARRAY_OF(args_t) args) {
    (void)args;
    return CORD_asprintf("%s=...args...", name);
}

CORD ast_to_cord(const char *name, ast_t *ast)
{
    CORD c = NULL;
    if (name) CORD_sprintf(&c, "%s=", name);
    if (!ast) return CORD_cat(c, "\x1b[35mNULL\x1b[m");

    // A small DSL to make it easier to whip up string representations of these
    // values with minimal boilerplate.
#define F(field) _Generic((data->field), \
                          ast_t*: ast_to_cord, \
                          ARRAY_OF(ast_t*): ast_list_to_cord, \
                          const char *: label_str, \
                          int64_t: label_int, \
                          unsigned short int: label_int, \
                          double: label_double, \
                          bool: label_bool, \
                          unsigned char: label_bool, \
                          args_t: label_args, \
                          ARRAY_OF(args_t): label_args_list, \
                          ARRAY_OF(const char *): str_list_to_cord)(#field, data->field)
#define T(t, ...) case t: { auto data = Match(ast, t); (void)data; c = CORD_cat(c, "\x1b[1m" #t "(\x1b[m"); \
    c = CORD_cat(c, concat_cords((int)(sizeof (const char*[]){__VA_ARGS__})/(sizeof(const char*)), (const char*[]){__VA_ARGS__})); \
    c = CORD_cat(c, "\x1b[1m)\x1b[m"); break; }
#define BINOP(b) T(b, F(lhs), F(rhs))
#define UNOP(u) T(u, F(value))

    switch (ast->tag) {
        T(Unknown)
        T(Nil, F(type))
        T(Bool, F(b))
        T(Var, F(name))
        T(Wildcard, F(name))
        T(TypeVar, F(name))
        T(Int, F(i), F(precision))
        T(Num, F(n), F(precision))
        T(Range, F(first), F(last), F(step))
        T(Char, CORD_asprintf("'%c'", data->c))
        T(StringLiteral, F(str))
        T(StringJoin, F(children))
        T(Interp, F(value), data->labelled ? "labeled=true" : "labelled=false")
        T(Declare, F(var), F(value), F(is_public))
        T(Assign, F(targets), F(values))
        T(BinaryOp, F(lhs), OP_NAMES[data->op], F(rhs))
        T(UpdateAssign, F(lhs), OP_NAMES[data->op], F(rhs))
        T(UnaryOp, OP_NAMES[data->op], F(value))
        T(Comparison, F(lhs), OP_NAMES[data->op], F(rhs))
        UNOP(GetTypeInfo) UNOP(SizeOf) UNOP(HeapAllocate) UNOP(StackReference)
        T(Min, F(lhs), F(rhs), F(key))
        T(Max, F(lhs), F(rhs), F(key))
        T(Mix, F(lhs), F(rhs), F(key))
        T(Array, F(type), F(items))
        T(Table, F(key_type), F(value_type), F(entries))
        T(TableEntry, F(key), F(value))
        T(FunctionDef, F(name), F(args), F(ret_type), F(body), F(cache), F(is_inline))
        T(Lambda, F(args), F(body))
        T(FunctionCall, F(fn), F(args))
        T(KeywordArg, F(name), F(arg))
        T(Block, F(statements), F(keep_scope), F(is_namespace))
        T(Do, F(label), F(body), F(else_body))
        T(For, F(index), F(value), F(first), F(iter), F(body), F(between), F(empty))
        T(While, F(condition), F(body), F(between))
        T(Repeat, F(body), F(between))
        T(If, F(subject), F(patterns), F(blocks))
        T(Skip, F(target))
        T(Stop, F(target))
        T(Pass)
        UNOP(Return)
        T(Fail, F(message))
        T(Extern, F(name), F(type), F(address))
        T(TypeArray, F(item_type))
        T(TypeTable, F(key_type), F(value_type))
        T(TypeDef, F(name), F(type), F(namespace))
        T(TypeStruct, F(members))
        T(TypeFunction, F(args), F(ret_type))
        T(TypePointer, F(pointed), F(is_optional), F(is_stack))
        T(Variant, F(type), F(value))
        T(Cast, F(value), F(type), F(bitcast))
        T(Struct, F(type), F(members))
        T(TypeTaggedUnion, F(tag_names), "tag_values=...", F(tag_args))
        T(TaggedUnionField, F(name), F(value))
        T(Index, F(indexed), F(index))
        T(FieldAccess, F(fielded), F(field))
        T(ConvertDef, F(var), F(source_type), F(target_type), F(body))
        T(Reduction, F(iter), F(combination), F(fallback))
        T(DocTest, F(expr), F(output), F(skip_source))
        T(Defer, F(body))
        T(With, F(var), F(expr), F(cleanup), F(body))
        T(Extend, F(type), F(body))
        T(Use, F(path), F(main_program))
        T(LinkerDirective, F(directives))
        T(Using, F(used), F(body))
#undef BINOP
#undef UNOP
#undef F
#undef T
    }
    return c;
}

const char *ast_to_str(ast_t *ast) {
    CORD c = ast_to_cord(NULL, ast);
    return CORD_to_char_star(c);
}

int printf_ast(FILE *stream, const struct printf_info *info, const void *const args[])
{
    ast_t *ast = *(ast_t**)(args[0]);
    if (ast) {
        if (info->alt)
            return fprintf(stream, "%.*s", (int)(ast->end - ast->start), ast->start);
        else
            return fprintf(stream, "%s", ast_to_str(ast));
    } else {
        return fputs("(null)", stream);
    }
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
