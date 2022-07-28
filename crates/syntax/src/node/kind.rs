// Autogenerated file. To regenerate, please run `cargo run --bin generate_syntax`.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum SyntaxKind {
    Terminal,
    Trivia,
    Identifier,
    ExprMissing,
    ExprPath,
    PathSegment,
    OptionGenericArgsNone,
    OptionGenericArgsSome,
    ExprLiteral,
    ExprParenthesized,
    ExprUnary,
    ExprBinary,
    ExprBlock,
    StatementList,
    StatementMissing,
    StatementLet,
    StatementExpr,
    StatementReturn,
    ItemList,
    ItemModule,
    ItemFunction,
    ItemFunctionSignature,
    FunctionSignature,
    ParameterList,
    Parameter,
    ItemTrait,
    ItemImpl,
    ItemStruct,
    ItemEnum,
    MemberList,
    Member,
    ItemUse,
    SyntaxFile,
}
