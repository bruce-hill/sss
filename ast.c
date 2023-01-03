#include "ast.h"

static const char *_ast_to_str(const char *name, ast_t *ast);

static const char *ast_tag_names[] = {
    [Unknown]="Unknown",
    [Nil]="Nil", [Bool]="Bool", [Var]="Var",
    [Int]="Int", [Num]="Num", [Range]="Range", [Char]="Char",
    [StringLiteral]="StringLiteral", [StringJoin]="StringJoin", [DSL]="DSL", [Interp]="Interp",
    [Declare]="Declare", [Assign]="Assign",
    [AddUpdate]="AddUpdate", [SubtractUpdate]="SubtractUpdate", [MultiplyUpdate]="MultiplyUpdate", [DivideUpdate]="DivideUpdate",
    [AndUpdate]="AndUpdate", [OrUpdate]="OrUpdate",
    [Add]="Add", [Subtract]="Subtract", [Multiply]="Multiply", [Divide]="Divide", [Power]="Power", [Modulus]="Modulus",
    [And]="And", [Or]="Or", [Xor]="Xor",
    [Equal]="Equal", [NotEqual]="NotEqual", [Greater]="Greater", [GreaterEqual]="GreaterEqual", [Less]="Less", [LessEqual]="LessEqual",
    [Not]="Not", [Negative]="Negative", [Len]="Len", [Maybe]="Maybe",
    [TypeOf]="TypeOf", [SizeOf]="SizeOf",
    [HeapAllocate]="HeapAllocate", [Dereference]="Dereference",
    [Array]="Array", [Table]="Table",
    [FunctionDef]="FunctionDef", [Lambda]="Lambda",
    [FunctionCall]="FunctionCall", [KeywordArg]="KeywordArg",
    [Block]="Block",
    [Do]="Do", [If]="If", [For]="For", [While]="While", [Repeat]="Repeat", [When]="When",
    [Skip]="Skip", [Stop]="Stop",
    [Return]="Return",
    [Fail]="Fail",
    [Extern]="Extern",
    [TypeArray]="TypeArray", [TypeTable]="TypeTable", [TypeTuple]="TypeTuple",
    [TypeFunction]="TypeFunction", [TypePointer]="TypePointer", [TypeOptional]="TypeOptional",
    [TypeMeasure]="TypeMeasure",
    [Cast]="Cast", [As]="As",
    [Struct]="Struct", [StructDef]="StructDef", [StructFieldDef]="StructFieldDef", [StructField]="StructField",
    [EnumDef]="EnumDef", [EnumField]="EnumField",
    [Index]="Index", [FieldAccess]="FieldAccess", [FieldName]="FieldName",
};

const char *get_ast_tag_name(ast_tag_e tag) {
    return ast_tag_names[tag];
}

const char *ast_list_to_str(const char *name, List(ast_t*) asts)
{
    char *buf; size_t size;
    FILE *mem = open_memstream(&buf, &size);
    if (name) fprintf(mem, "%s=", name);
    fputs("[", mem);
    for (int64_t i = 0, len = LIST_LEN(asts); i < len; i++) {
        if (i > 0) fputs(", ", mem);
        fputs(_ast_to_str(NULL, LIST_ITEM(asts, i)), mem);
    }
    fputs("]", mem);
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
    fputs("[", mem);
    for (int64_t i = 0, len = LIST_LEN(strs); i < len; i++) {
        if (i > 0) fputs(", ", mem);
        fputs(LIST_ITEM(strs, i), mem);
    }
    fputs("]", mem);
    fflush(mem);
    char *ret = GC_MALLOC_ATOMIC(size+1);
    memcpy(ret, buf, size);
    fclose(mem);
    free(buf);
    return ret;
}

const char *_ast_to_str(const char *name, ast_t *ast)
{
    char *buf; size_t size;
    FILE *mem = open_memstream(&buf, &size);
    if (name) fprintf(mem, "%s=", name);
    if (!ast) return "(NULL AST)";
#define FMT intern_strf
#define CHILD(mem) _ast_to_str(#mem, data->mem)
#define CHILDREN(mem) ast_list_to_str(#mem, data->mem)
#define CHILDREN_STRS(mem) str_list_to_str(#mem, data->mem)
#define CASE(t, ...) case t: { auto data = Match(ast, t); (void)data; fputs(#t "(", mem); \
    fputs_list(mem, (int)(sizeof (const char*[]){__VA_ARGS__})/(sizeof(const char*)), (const char*[]){__VA_ARGS__}); \
    fputs(")", mem); break; }

    switch (ast->tag) {
        CASE(Unknown)
        CASE(Nil, CHILD(type))
        CASE(Bool, FMT("%s", data->b ? "yes" : "no"))
        CASE(Var, data->name)
        CASE(Int, FMT("%ld, precision=%d, units=%s", data->i, data->precision, data->units))
        CASE(Num, FMT("%g, precision=%d, units=%s", data->n, data->precision, data->units))
        CASE(Range, CHILD(first), CHILD(last), CHILD(step))
        CASE(Char, FMT("'%c'", data->c))
        CASE(StringLiteral, FMT("\"%s\"", data->str))
        CASE(StringJoin, CHILDREN(children))
        CASE(DSL, data->name, CHILD(str))
        CASE(Interp, CHILD(value), data->labelled ? "labeled=true" : "")
        CASE(Declare, data->name, CHILD(value))
        CASE(Assign, CHILDREN(targets), CHILDREN(values))
#define BINOP(b) CASE(b, CHILD(lhs), CHILD(rhs))
        BINOP(AddUpdate) BINOP(SubtractUpdate) BINOP(MultiplyUpdate) BINOP(DivideUpdate) BINOP(AndUpdate)
        BINOP(OrUpdate) BINOP(Add) BINOP(Subtract) BINOP(Multiply) BINOP(Divide) BINOP(Power) BINOP(Modulus)
        BINOP(And) BINOP(Or) BINOP(Xor) BINOP(Equal) BINOP(NotEqual) BINOP(Greater) BINOP(GreaterEqual)
        BINOP(Less) BINOP(LessEqual)
#undef BINOP
#define UNOP(u) CASE(u, CHILD(value))
        UNOP(Not) UNOP(Negative) UNOP(Len) UNOP(Maybe) UNOP(TypeOf) UNOP(SizeOf) UNOP(HeapAllocate) UNOP(Dereference)
#undef UNOP
        CASE(Array, CHILD(type), CHILDREN(items))
        CASE(Table, CHILD(key_type), CHILD(value_type), CHILDREN(items))
        CASE(FunctionDef, data->name, CHILDREN_STRS(arg_names), CHILDREN(arg_types), CHILDREN(arg_defaults), CHILD(ret_type), CHILD(body))
        CASE(Lambda, CHILDREN_STRS(arg_names), CHILDREN(arg_types), CHILD(body))
        CASE(FunctionCall, CHILD(fn), CHILDREN(args))
        CASE(KeywordArg, data->name, CHILD(arg))
        CASE(Block, CHILDREN(statements))
        CASE(Do, CHILDREN(blocks))
        CASE(If, "...") // TODO: clean up
        CASE(For, data->key, data->value, CHILD(iter), CHILD(body), CHILD(between))
        default: return "???";
        // struct {
        //     ast_t *condition, *body, *between;
        // } While;
        // struct {
        //     ast_t *body, *between;
        // } Repeat;
        // struct {
        //     ast_t *subject;
        //     List(ast_case_t) cases;
        //     ast_t *default_body;
        // } When;
        // struct {
        //     istr_t target;
        // } Skip, Stop;
        // struct {
        //     ast_t *value;
        // } Return;
        // struct {
        //     ast_t *message;
        // } Fail;
        // struct {
        //     istr_t name, bl_name;
        //     ast_t *type;
        // } Extern;
        // struct {
        //     ast_t *item_type;
        // } TypeArray;
        // struct {
        //     ast_t *key_type, *val_type;
        // } TypeTable;
        // struct {
        //     List(istr_t) member_names;
        //     List(ast_t*) member_types;
        // } TypeTuple;
        // struct {
        //     List(istr_t) arg_names;
        //     List(ast_t*) arg_types;
        //     ast_t *ret_type;
        // } TypeFunction;
        // struct {
        //     ast_t *pointed;
        // } TypePointer;
        // struct {
        //     ast_t *type;
        // } TypeOptional;
        // struct {
        //     ast_t *type;
        //     istr_t units;
        // } TypeMeasure;
        // struct {
        //     ast_t *value, *type;
        // } Cast, As;
        // struct {
        //     ast_t *type;
        //     List(ast_t *) members;
        // } Struct;
        // struct {
        //     istr_t name;
        //     List(ast_t *) members;
        // } StructDef;
        // struct {
        //     List(istr_t) names;
        //     ast_t *type;
        // } StructFieldDef;
        // struct {
        //     istr_t name;
        //     ast_t *value;
        // } StructField;
        // struct {
        //     istr_t name;
        //     List(istr_t) tag_names;
        //     List(int64_t) tag_values;
        //     List(ast_t *) tag_types;
        // } EnumDef;
        // struct {
        //     istr_t name;
        //     ast_t *value;
        // } EnumField;
        // struct {
        //     ast_t *indexed, *index;
        // } Index;
        // struct {
        //     istr_t field;
        //     ast_t *fielded;
        // } FieldAccess;
        // struct {
        //     istr_t name;
        // } FieldName;
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
