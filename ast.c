#include "ast.h"

static const char *ast_tag_names[] = {
    [Unknown]="Unknown",
    [Nil]="Nil", [Bool]="Bool", [Var]="Var",
    [Int]="Int", [Num]="Num",
    [StringLiteral]="StringLiteral", [StringJoin]="StringJoin", [DSL]="DSL", [Interp]="Interp",
    [Declare]="Declare", [Assign]="Assign",
    [AddUpdate]="AddUpdate", [SubtractUpdate]="SubtractUpdate", [MultiplyUpdate]="MultiplyUpdate", [DivideUpdate]="DivideUpdate",
    [AndUpdate]="AndUpdate", [OrUpdate]="OrUpdate",
    [Add]="Add", [Subtract]="Subtract", [Multiply]="Multiply", [Divide]="Divide", [Power]="Power", [Modulus]="Modulus",
    [And]="And", [Or]="Or", [Xor]="Xor",
    [Equal]="Equal", [NotEqual]="NotEqual", [Greater]="Greater", [GreaterEqual]="GreaterEqual", [Less]="Less", [LessEqual]="LessEqual",
    [Not]="Not", [Negative]="Negative", [Len]="Len",
    [Array]="Array", [Table]="Table",
    [FunctionDef]="FunctionDef", [Lambda]="Lambda",
    [FunctionCall]="FunctionCall", [KeywordArg]="KeywordArg",
    [Block]="Block",
    [If]="If", [For]="For", [While]="While", [Repeat]="Repeat",
    [Skip]="Skip", [Stop]="Stop",
    [Return]="Return",
    [TypeArray]="TypeArray", [TypeTable]="TypeTable",
    [TypeFunction]="TypeFunction", [TypePointer]="TypePointer",
    [Cast]="Cast", [As]="As", [Extern]="Extern",
    [StructDef]="StructDef",
    [EnumDef]="EnumDef",
};

const char *get_ast_tag_name(ast_tag_e tag) {
    return ast_tag_names[tag];
}
