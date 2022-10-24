// Autogenerated file. To regenerate, please run `cargo run --bin generate_syntax`.
use core::fmt;
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum SyntaxKind {
    Trivia,
    ExprList,
    ExprMissing,
    PathSegmentSimple,
    PathSegmentWithGenericArgs,
    ExprPath,
    ExprParenthesized,
    ExprUnary,
    ExprBinary,
    ExprTuple,
    ExprFunctionCall,
    ExprListParenthesized,
    ExprStructCtorCall,
    ExprBlock,
    ExprMatch,
    MatchArms,
    MatchArm,
    ExprIf,
    ElseClause,
    OptionElseClauseEmpty,
    StructArgExpr,
    OptionStructArgExprEmpty,
    StructArgSingle,
    StructArgTail,
    StructArgList,
    ArgListBraced,
    PatternIdentifier,
    PatternStruct,
    PatternStructParamList,
    PatternTuple,
    PatternList,
    PatternStructParamWithExpr,
    PatternEnum,
    TypeClause,
    OptionTypeClauseEmpty,
    ReturnTypeClause,
    OptionReturnTypeClauseEmpty,
    StatementList,
    StatementMissing,
    StatementLet,
    OptionSemicolonEmpty,
    StatementExpr,
    StatementReturn,
    Param,
    ModifierList,
    ParamList,
    WithClause,
    OptionWithClauseEmpty,
    FunctionSignature,
    ItemList,
    ItemModule,
    ItemFreeFunction,
    ItemExternFunction,
    ItemExternType,
    ItemTrait,
    ItemImpl,
    ItemStruct,
    ItemEnum,
    ItemUse,
    GenericArgs,
    GenericArgList,
    OptionGenericParamsEmpty,
    WrappedGenericParamList,
    GenericParamList,
    GenericParam,
    SyntaxFile,
    TokenSingleLineComment,
    TokenWhitespace,
    TokenNewline,
    TokenMissing,
    TokenSkipped,
    TokenIdentifier,
    TerminalIdentifier,
    TokenLiteralNumber,
    TerminalLiteralNumber,
    TokenFalse,
    TerminalFalse,
    TokenTrue,
    TerminalTrue,
    TokenExtern,
    TerminalExtern,
    TokenType,
    TerminalType,
    TokenFunction,
    TerminalFunction,
    TokenModule,
    TerminalModule,
    TokenEnum,
    TerminalEnum,
    TokenStruct,
    TerminalStruct,
    TokenTrait,
    TerminalTrait,
    TokenImpl,
    TerminalImpl,
    TokenFor,
    TerminalFor,
    TokenLet,
    TerminalLet,
    TokenReturn,
    TerminalReturn,
    TokenMatch,
    TerminalMatch,
    TokenIf,
    TerminalIf,
    TokenElse,
    TerminalElse,
    TokenUse,
    TerminalUse,
    TokenWith,
    TerminalWith,
    TokenRef,
    TerminalRef,
    TokenMut,
    TerminalMut,
    TokenAnd,
    TerminalAnd,
    TokenAndAnd,
    TerminalAndAnd,
    TokenOrOr,
    TerminalOrOr,
    TokenEqEq,
    TerminalEqEq,
    TokenNeq,
    TerminalNeq,
    TokenGE,
    TerminalGE,
    TokenGT,
    TerminalGT,
    TokenLE,
    TerminalLE,
    TokenLT,
    TerminalLT,
    TokenNot,
    TerminalNot,
    TokenPlus,
    TerminalPlus,
    TokenMinus,
    TerminalMinus,
    TokenMul,
    TerminalMul,
    TokenDiv,
    TerminalDiv,
    TokenColon,
    TerminalColon,
    TokenColonColon,
    TerminalColonColon,
    TokenComma,
    TerminalComma,
    TokenDot,
    TerminalDot,
    TokenDotDot,
    TerminalDotDot,
    TokenEq,
    TerminalEq,
    TokenSemicolon,
    TerminalSemicolon,
    TokenUnderscore,
    TerminalUnderscore,
    TokenLBrace,
    TerminalLBrace,
    TokenRBrace,
    TerminalRBrace,
    TokenLBrack,
    TerminalLBrack,
    TokenRBrack,
    TerminalRBrack,
    TokenLParen,
    TerminalLParen,
    TokenRParen,
    TerminalRParen,
    TokenArrow,
    TerminalArrow,
    TokenMatchArrow,
    TerminalMatchArrow,
    TokenEndOfFile,
    TerminalEndOfFile,
    TokenBadCharacters,
    TerminalBadCharacters,
}
impl SyntaxKind {
    pub fn is_terminal(&self) -> bool {
        matches!(
            *self,
            SyntaxKind::TerminalIdentifier
                | SyntaxKind::TerminalLiteralNumber
                | SyntaxKind::TerminalFalse
                | SyntaxKind::TerminalTrue
                | SyntaxKind::TerminalExtern
                | SyntaxKind::TerminalType
                | SyntaxKind::TerminalFunction
                | SyntaxKind::TerminalModule
                | SyntaxKind::TerminalEnum
                | SyntaxKind::TerminalStruct
                | SyntaxKind::TerminalTrait
                | SyntaxKind::TerminalImpl
                | SyntaxKind::TerminalFor
                | SyntaxKind::TerminalLet
                | SyntaxKind::TerminalReturn
                | SyntaxKind::TerminalMatch
                | SyntaxKind::TerminalIf
                | SyntaxKind::TerminalElse
                | SyntaxKind::TerminalUse
                | SyntaxKind::TerminalWith
                | SyntaxKind::TerminalRef
                | SyntaxKind::TerminalMut
                | SyntaxKind::TerminalAnd
                | SyntaxKind::TerminalAndAnd
                | SyntaxKind::TerminalOrOr
                | SyntaxKind::TerminalEqEq
                | SyntaxKind::TerminalNeq
                | SyntaxKind::TerminalGE
                | SyntaxKind::TerminalGT
                | SyntaxKind::TerminalLE
                | SyntaxKind::TerminalLT
                | SyntaxKind::TerminalNot
                | SyntaxKind::TerminalPlus
                | SyntaxKind::TerminalMinus
                | SyntaxKind::TerminalMul
                | SyntaxKind::TerminalDiv
                | SyntaxKind::TerminalColon
                | SyntaxKind::TerminalColonColon
                | SyntaxKind::TerminalComma
                | SyntaxKind::TerminalDot
                | SyntaxKind::TerminalDotDot
                | SyntaxKind::TerminalEq
                | SyntaxKind::TerminalSemicolon
                | SyntaxKind::TerminalUnderscore
                | SyntaxKind::TerminalLBrace
                | SyntaxKind::TerminalRBrace
                | SyntaxKind::TerminalLBrack
                | SyntaxKind::TerminalRBrack
                | SyntaxKind::TerminalLParen
                | SyntaxKind::TerminalRParen
                | SyntaxKind::TerminalArrow
                | SyntaxKind::TerminalMatchArrow
                | SyntaxKind::TerminalEndOfFile
                | SyntaxKind::TerminalBadCharacters
        )
    }
    pub fn is_token(&self) -> bool {
        matches!(
            *self,
            SyntaxKind::TokenSingleLineComment
                | SyntaxKind::TokenWhitespace
                | SyntaxKind::TokenNewline
                | SyntaxKind::TokenMissing
                | SyntaxKind::TokenSkipped
                | SyntaxKind::TokenIdentifier
                | SyntaxKind::TokenLiteralNumber
                | SyntaxKind::TokenFalse
                | SyntaxKind::TokenTrue
                | SyntaxKind::TokenExtern
                | SyntaxKind::TokenType
                | SyntaxKind::TokenFunction
                | SyntaxKind::TokenModule
                | SyntaxKind::TokenEnum
                | SyntaxKind::TokenStruct
                | SyntaxKind::TokenTrait
                | SyntaxKind::TokenImpl
                | SyntaxKind::TokenFor
                | SyntaxKind::TokenLet
                | SyntaxKind::TokenReturn
                | SyntaxKind::TokenMatch
                | SyntaxKind::TokenIf
                | SyntaxKind::TokenElse
                | SyntaxKind::TokenUse
                | SyntaxKind::TokenWith
                | SyntaxKind::TokenRef
                | SyntaxKind::TokenMut
                | SyntaxKind::TokenAnd
                | SyntaxKind::TokenAndAnd
                | SyntaxKind::TokenOrOr
                | SyntaxKind::TokenEqEq
                | SyntaxKind::TokenNeq
                | SyntaxKind::TokenGE
                | SyntaxKind::TokenGT
                | SyntaxKind::TokenLE
                | SyntaxKind::TokenLT
                | SyntaxKind::TokenNot
                | SyntaxKind::TokenPlus
                | SyntaxKind::TokenMinus
                | SyntaxKind::TokenMul
                | SyntaxKind::TokenDiv
                | SyntaxKind::TokenColon
                | SyntaxKind::TokenColonColon
                | SyntaxKind::TokenComma
                | SyntaxKind::TokenDot
                | SyntaxKind::TokenDotDot
                | SyntaxKind::TokenEq
                | SyntaxKind::TokenSemicolon
                | SyntaxKind::TokenUnderscore
                | SyntaxKind::TokenLBrace
                | SyntaxKind::TokenRBrace
                | SyntaxKind::TokenLBrack
                | SyntaxKind::TokenRBrack
                | SyntaxKind::TokenLParen
                | SyntaxKind::TokenRParen
                | SyntaxKind::TokenArrow
                | SyntaxKind::TokenMatchArrow
                | SyntaxKind::TokenEndOfFile
                | SyntaxKind::TokenBadCharacters
        )
    }
}
impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
