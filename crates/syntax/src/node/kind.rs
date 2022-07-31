// Autogenerated file. To regenerate, please run `cargo run --bin generate_syntax`.
use core::fmt;
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum SyntaxKind {
    OptionGenericArgsNone,
    OptionGenericArgsSome,
    ItemFunctionSignature,
    FunctionSignature,
    ParameterList,
    Parameter,
    ItemUse,
    SyntaxFile,

    Empty,
    Terminal,
    SkippedTerminal,
    Trivia,
    TriviumSingleLineComment,
    TriviumWhitespace,
    TriviumNewline,
    Identifier,
    ExprList,
    ExprMissing,
    ExprPath,
    PathSegment,
    PathSegmentList,
    GenericArgList,
    GenericArgListMissing,
    GenericArgListSurrounded,
    GenericArgListSurroundedMissing,
    ExprLiteral,
    ExprParenthesized,
    ExprUnary,
    ExprBinary,
    ExprBlock,
    ExprFunctionCall,
    ExprConstructorCall,
    ExprTuple,
    StatementList,
    StatementMissing,
    StatementLet,
    StatementExpr,
    OptionSemicolonEmpty,
    StatementReturn,
    Semi,
    ArgExpr,
    ArgExprMissing,
    Arg,
    ArgWithComma,
    ArgList,
    ExprListParenthesized,
    ArgListBraced,
    StructUpdateTail,
    CtorArg,
    CtorArgWithComma,
    CtorArgList,
    CtorArgListBraced,
    ParamList,
    Param,
    ItemFunction,
    ItemTrait,
    ItemImpl,
    ItemStruct,
    ItemModule,
    ItemEnum,
    ItemMissing,
    ItemList,
    MemberList,
    Member,
    ItemUse,
    ExprTuple,
    ArgExpr,
    OptionArgExprEmpty,
    Arg,
    ExprFunctionCall,
    StructUpdateTail,
    CtorArgList,
    ArgListBraced,
    ExprConstructorCall,
    ExprListParenthesized,
    TypeClause,
    OptionTypeClauseEmpty,
    ReturnTypeClause,
    OptionReturnTypeClauseEmpty,
    Param,
    ParamList,
    ParamListParenthesized,
    ParamListBraced,
    SyntaxFile,
}
impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
