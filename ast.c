#include "ast.h"

static const char *astkind_names[] = {
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
    [List]="List", [Table]="Table",
    [FunctionDef]="FunctionDef", [Lambda]="Lambda",
    [FunctionCall]="FunctionCall", [KeywordArg]="KeywordArg",
    [Block]="Block",
    [If]="If", [For]="For", [While]="While", [Repeat]="Repeat",
    [Skip]="Skip", [Stop]="Stop",
    [Return]="Return",
    [TypeName]="TypeName",
    [TypeList]="TypeList", [TypeTable]="TypeTable",
    [TypeFunction]="TypeFunction", [TypeOption]="TypeOption",
    [Cast]="Cast", [As]="As",
};

const char *get_ast_kind_name(astkind_e kind) {
    return astkind_names[kind];
}
