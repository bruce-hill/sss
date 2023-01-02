#include "ast.h"

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
