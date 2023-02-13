// Some basic operations defined on AST nodes, mainly converting to
// strings for debugging.
#include "ast.h"

static const char *_ast_to_str(const char *name, ast_t *ast);

const char *ast_list_to_str(const char *name, List(ast_t*) asts)
{
    char *buf; size_t size;
    FILE *mem = open_memstream(&buf, &size);
    if (name) fprintf(mem, "%s=", name);
    if (asts) {
        fputs("[", mem);
        for (int64_t i = 0, len = LIST_LEN(asts); i < len; i++) {
            if (i > 0) fputs(", ", mem);
            fputs(_ast_to_str(NULL, LIST_ITEM(asts, i)), mem);
        }
        fputs("]", mem);
    } else {
        fputs("\x1b[35mNULL\x1b[m", mem);
    }
    fflush(mem);
    char *ret = GC_MALLOC_ATOMIC(size+1);
    memcpy(ret, buf, size);
    fclose(mem);
    free(buf);
    return ret;
}

static void fputs_list(FILE *out, int len, const char *strs[len]) {
    for (int i = 0; i < len; i++) {
        if (i > 0) fputs(", ", out);
        fputs(strs[i], out);
    }
}

const char *str_list_to_str(const char *name, List(const char*) strs)
{
    char *buf; size_t size;
    FILE *mem = open_memstream(&buf, &size);
    if (name) fprintf(mem, "%s=", name);
    if (strs) {
        fputs("[", mem);
        for (int64_t i = 0, len = LIST_LEN(strs); i < len; i++) {
            if (i > 0) fputs(", ", mem);
            fputs(LIST_ITEM(strs, i), mem);
        }
        fputs("]", mem);
    } else {
        fputs("\x1b[35mNULL\x1b[m", mem);
    }
    fflush(mem);
    char *ret = GC_MALLOC_ATOMIC(size+1);
    memcpy(ret, buf, size);
    fclose(mem);
    free(buf);
    return ret;
}

const char *label_str(const char *name, const char *str) {
    if (str)
        return intern_strf("%s=\x1b[35m\"%s\"\x1b[m", name, str);
    else
        return intern_strf("%s=\x1b[35m(NULL)\x1b[m", name);
}
const char *label_int(const char *name, int64_t i) { return intern_strf("%s=\x1b[35m%ld\x1b[m", name, i); }
const char *label_double(const char *name, double d) { return intern_strf("%s=\x1b[35m%g\x1b[m", name, d); }
const char *label_bool(const char *name, bool b) { return intern_strf("%s=\x1b[35m%s\x1b[m", name, b ? "yes" : "no"); }

const char *_ast_to_str(const char *name, ast_t *ast)
{
    char *buf; size_t size;
    FILE *mem = open_memstream(&buf, &size);
    if (name) fprintf(mem, "%s=", name);
    if (!ast) return intern_strf("%s=\x1b[35mNULL\x1b[m", name);

    // A small DSL to make it easier to whip up string representations of these
    // values with minimal boilerplate.
#define F(field) _Generic((data->field), \
                          ast_t*: _ast_to_str, \
                          List(ast_t*): ast_list_to_str, \
                          istr_t: label_str, \
                          int64_t: label_int, \
                          double: label_double, \
                          bool: label_bool, \
                          unsigned char: label_bool, \
                          List(istr_t): str_list_to_str)(#field, data->field)
#define T(t, ...) case t: { auto data = Match(ast, t); (void)data; fputs("\x1b[1m" #t "(\x1b[m", mem); \
    fputs_list(mem, (int)(sizeof (const char*[]){__VA_ARGS__})/(sizeof(const char*)), (const char*[]){__VA_ARGS__}); \
    fputs("\x1b[1m)\x1b[m", mem); break; }
#define BINOP(b) T(b, F(lhs), F(rhs))
#define UNOP(u) T(u, F(value))

    switch (ast->tag) {
        T(Unknown)
        T(Nil, F(type))
        T(Bool, F(b))
        T(Var, F(name))
        T(Int, F(i), F(precision), F(units))
        T(Num, F(n), F(precision), F(units))
        T(Range, F(first), F(last), F(step))
        T(Char, intern_strf("'%c'", data->c))
        T(StringLiteral, F(str))
        T(StringJoin, F(dsl), F(children))
        T(Interp, F(value), data->labelled ? "labeled=true" : "labelled=false")
        T(Declare, F(var), F(value), F(is_global))
        T(Assign, F(targets), F(values))
        BINOP(AddUpdate) BINOP(SubtractUpdate) BINOP(MultiplyUpdate) BINOP(DivideUpdate) BINOP(AndUpdate)
        BINOP(OrUpdate) BINOP(Add) BINOP(Subtract) BINOP(Multiply) BINOP(Divide) BINOP(Power) BINOP(Modulus)
        BINOP(And) BINOP(Or) BINOP(Xor) BINOP(Equal) BINOP(NotEqual) BINOP(Greater) BINOP(GreaterEqual)
        BINOP(Less) BINOP(LessEqual)
        UNOP(Not) UNOP(Negative) UNOP(Len) UNOP(Maybe) UNOP(TypeOf) UNOP(SizeOf) UNOP(HeapAllocate) UNOP(Dereference)
        T(Array, F(type), F(items))
        T(Table, F(key_type), F(value_type), F(items))
        T(FunctionDef, F(name), F(arg_names), F(arg_types), F(arg_defaults), F(ret_type), F(body), F(is_exported), F(is_inline))
        T(Lambda, F(arg_names), F(arg_types), F(body))
        T(FunctionCall, F(fn), F(args))
        T(KeywordArg, F(name), F(arg))
        T(Block, F(statements))
        T(Do, F(label), F(body), F(else_body))
        T(Using, F(vars), F(body))
        T(If, F(condition), F(body), F(else_body))
        T(For, F(key), F(value), F(first), F(iter), F(body), F(between), F(empty))
        T(While, F(condition), F(body), F(between))
        T(Repeat, F(body), F(between))
        T(When, F(subject), "...", F(default_body)) // TODO print cases
        T(Skip, F(target))
        T(Stop, F(target))
        UNOP(Return)
        T(Fail, F(message))
        T(Extern, F(name), F(bl_name), F(type))
        T(TypeArray, F(item_type))
        T(TypeTable, F(key_type), F(val_type))
        T(TypeStruct, F(name), F(member_names), F(member_types))
        T(TypeFunction, F(arg_names), F(arg_types), F(ret_type))
        T(TypePointer, F(pointed))
        T(TypeOptional, F(type))
        T(TypeMeasure, F(type), F(units))
        T(TypeDSL, F(name))
        T(Cast, F(value), F(type))
        T(Bitcast, F(value), F(type))
        T(Struct, F(type), F(members))
        T(StructDef, F(name), F(field_names), F(field_types), F(field_defaults), F(definitions))
        T(StructField, F(name), F(value))
        T(TaggedUnionDef, F(name), F(tag_names), "tag_values=...", F(tag_types))
        T(TaggedUnionField, F(name), F(value))
        T(Index, F(indexed), F(index), F(unchecked))
        T(FieldAccess, F(fielded), F(field))
        T(UnitDef, F(derived), F(base))
        T(ConvertDef, F(var), F(source_type), F(target_type), F(body))
        T(Reduction, F(iter), F(combination), F(fallback))
        T(DocTest, F(expr), F(output))
#undef BINOP
#undef UNOP
#undef F
#undef T
    }
    fflush(mem);
    char *ret = GC_MALLOC_ATOMIC(size+1);
    memcpy(ret, buf, size);
    fclose(mem);
    free(buf);
    return ret;
}

const char *ast_to_str(ast_t *ast) {
    return _ast_to_str(NULL, ast);
}

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
