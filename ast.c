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
            if (LIST_ITEM(strs, i))
                fputs(LIST_ITEM(strs, i), mem);
            else
                fputs("(NULL)", mem);
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
        return heap_strf("%s=\x1b[35m\"%s\"\x1b[m", name, str);
    else
        return heap_strf("%s=\x1b[35m(NULL)\x1b[m", name);
}
const char *label_int(const char *name, int64_t i) { return heap_strf("%s=\x1b[35m%ld\x1b[m", name, i); }
const char *label_double(const char *name, double d) { return heap_strf("%s=\x1b[35m%g\x1b[m", name, d); }
const char *label_bool(const char *name, bool b) { return heap_strf("%s=\x1b[35m%s\x1b[m", name, b ? "yes" : "no"); }
const char *label_args(const char *name, args_t args) { (void)args; return heap_strf("%s=...args...", name); }
const char *label_args_list(const char *name, List(args_t) args) { (void)args; return heap_strf("%s=...args...", name); }

const char *_ast_to_str(const char *name, ast_t *ast)
{
    char *buf; size_t size;
    FILE *mem = open_memstream(&buf, &size);
    if (name) fprintf(mem, "%s=", name);
    if (!ast) return heap_strf("%s=\x1b[35mNULL\x1b[m", name);

    // A small DSL to make it easier to whip up string representations of these
    // values with minimal boilerplate.
#define F(field) _Generic((data->field), \
                          ast_t*: _ast_to_str, \
                          List(ast_t*): ast_list_to_str, \
                          const char *: label_str, \
                          int64_t: label_int, \
                          unsigned short int: label_int, \
                          double: label_double, \
                          bool: label_bool, \
                          unsigned char: label_bool, \
                          args_t: label_args, \
                          List(args_t): label_args_list, \
                          List(const char *): str_list_to_str)(#field, data->field)
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
        T(Int, F(i), F(precision), F(is_unsigned), F(units))
        T(Num, F(n), F(precision), F(units))
        T(Range, F(first), F(last), F(step))
        T(Char, heap_strf("'%c'", data->c))
        T(StringLiteral, F(str))
        T(StringJoin, F(children))
        T(Interp, F(value), data->labelled ? "labeled=true" : "labelled=false")
        T(Declare, F(var), F(value), F(is_global))
        T(Assign, F(targets), F(values))
        BINOP(AddUpdate) BINOP(SubtractUpdate) BINOP(MultiplyUpdate) BINOP(DivideUpdate) BINOP(AndUpdate) BINOP(XorUpdate)
        BINOP(OrUpdate) BINOP(Add) BINOP(Subtract) BINOP(Multiply) BINOP(Divide) BINOP(Power) BINOP(Modulus) BINOP(Modulus1)
        BINOP(And) BINOP(Or) BINOP(Xor) BINOP(Equal) BINOP(NotEqual) BINOP(Greater) BINOP(GreaterEqual)
        BINOP(Less) BINOP(LessEqual) BINOP(LeftShift) BINOP(RightShift)
        T(In, F(member), F(container))
        T(NotIn, F(member), F(container))
        BINOP(Concatenate) BINOP(ConcatenateUpdate)
        UNOP(Not) UNOP(Negative) UNOP(Len) UNOP(Maybe) UNOP(TypeOf) UNOP(SizeOf) UNOP(HeapAllocate) UNOP(StackReference)
        UNOP(Dereference) UNOP(AssertNonNull)
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
        T(Block, F(statements), F(keep_scope))
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
        T(TypeStruct, F(name), F(members))
        T(TypeFunction, F(args), F(ret_type))
        T(TypePointer, F(pointed), F(is_optional), F(is_stack))
        T(TypeMeasure, F(type), F(units))
        T(Variant, F(type), F(value))
        T(TypeTypeAST, F(type))
        T(Cast, F(value), F(type))
        T(Bitcast, F(value), F(type))
        T(Struct, F(type), F(members))
        T(StructDef, F(name), F(field_names), F(field_types), F(field_defaults), F(definitions))
        T(TaggedUnionDef, F(name), F(tag_bits), F(tag_names), "tag_values=...", F(tag_args), F(definitions))
        T(TypeTaggedUnion, F(name), F(tag_bits), F(tag_names), "tag_values=...", F(tag_args))
        T(TaggedUnionField, F(name), F(value))
        T(Index, F(indexed), F(index))
        T(FieldAccess, F(fielded), F(field))
        T(UnitDef, F(derived), F(base))
        T(VariantDef, F(name), F(variant_of), F(body))
        T(ConvertDef, F(var), F(source_type), F(target_type), F(body))
        T(Reduction, F(iter), F(combination), F(fallback))
        T(DocTest, F(expr), F(output), F(skip_source))
        T(Defer, F(body))
        T(With, F(var), F(expr), F(cleanup), F(body))
        T(Extend, F(type), F(body))
        T(Use, F(path), F(main_program))
        T(Delete, F(value))
        T(LinkerDirective, F(directives))
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
