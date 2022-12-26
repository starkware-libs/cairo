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
    ExprErrorPropagate,
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
    OptionTerminalSemicolonEmpty,
    StatementExpr,
    StatementReturn,
    Param,
    ModifierList,
    ParamList,
    ImplicitsClause,
    ImplicitsList,
    OptionImplicitsClauseEmpty,
    OptionTerminalNoPanicEmpty,
    FunctionSignature,
    Member,
    MemberList,
    ItemList,
    Attribute,
    AttributeList,
    ItemModule,
    ModuleBody,
    OptionAttributeArgsEmpty,
    AttributeArgs,
    AttributeArgList,
    ItemFreeFunction,
    ItemExternFunction,
    ItemExternType,
    ItemTrait,
    TraitBody,
    TraitItemList,
    TraitItemFunction,
    ItemImpl,
    ImplBody,
    ItemStruct,
    ItemEnum,
    ItemTypeAlias,
    ItemUse,
    GenericArgs,
    GenericArgList,
    OptionWrappedGenericParamListEmpty,
    WrappedGenericParamList,
    GenericParamList,
    GenericParam,
    TokenIdentifier,
    TerminalIdentifier,
    TokenLiteralNumber,
    TerminalLiteralNumber,
    TokenShortString,
    TerminalShortString,
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
    TokenOf,
    TerminalOf,
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
    TokenImplicits,
    TerminalImplicits,
    TokenRef,
    TerminalRef,
    TokenMut,
    TerminalMut,
    TokenNoPanic,
    TerminalNoPanic,
    TokenAnd,
    TerminalAnd,
    TokenAndAnd,
    TerminalAndAnd,
    TokenOr,
    TerminalOr,
    TokenOrOr,
    TerminalOrOr,
    TokenXor,
    TerminalXor,
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
    TokenMod,
    TerminalMod,
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
    TokenQuestionMark,
    TerminalQuestionMark,
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
    TokenHash,
    TerminalHash,
    SyntaxFile,
    TokenSingleLineComment,
    TokenWhitespace,
    TokenNewline,
    TokenMissing,
    TokenSkipped,
}
impl SyntaxKind {
    pub fn is_token(&self) -> bool {
        matches!(
            *self,
            SyntaxKind::TokenIdentifier
                | SyntaxKind::TokenLiteralNumber
                | SyntaxKind::TokenShortString
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
                | SyntaxKind::TokenOf
                | SyntaxKind::TokenLet
                | SyntaxKind::TokenReturn
                | SyntaxKind::TokenMatch
                | SyntaxKind::TokenIf
                | SyntaxKind::TokenElse
                | SyntaxKind::TokenUse
                | SyntaxKind::TokenImplicits
                | SyntaxKind::TokenRef
                | SyntaxKind::TokenMut
                | SyntaxKind::TokenNoPanic
                | SyntaxKind::TokenAnd
                | SyntaxKind::TokenAndAnd
                | SyntaxKind::TokenOr
                | SyntaxKind::TokenOrOr
                | SyntaxKind::TokenXor
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
                | SyntaxKind::TokenMod
                | SyntaxKind::TokenColon
                | SyntaxKind::TokenColonColon
                | SyntaxKind::TokenComma
                | SyntaxKind::TokenDot
                | SyntaxKind::TokenDotDot
                | SyntaxKind::TokenEq
                | SyntaxKind::TokenSemicolon
                | SyntaxKind::TokenQuestionMark
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
                | SyntaxKind::TokenHash
                | SyntaxKind::TokenSingleLineComment
                | SyntaxKind::TokenWhitespace
                | SyntaxKind::TokenNewline
                | SyntaxKind::TokenMissing
                | SyntaxKind::TokenSkipped
        )
    }
    pub fn is_terminal(&self) -> bool {
        matches!(
            *self,
            SyntaxKind::TerminalIdentifier
                | SyntaxKind::TerminalLiteralNumber
                | SyntaxKind::TerminalShortString
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
                | SyntaxKind::TerminalOf
                | SyntaxKind::TerminalLet
                | SyntaxKind::TerminalReturn
                | SyntaxKind::TerminalMatch
                | SyntaxKind::TerminalIf
                | SyntaxKind::TerminalElse
                | SyntaxKind::TerminalUse
                | SyntaxKind::TerminalImplicits
                | SyntaxKind::TerminalRef
                | SyntaxKind::TerminalMut
                | SyntaxKind::TerminalNoPanic
                | SyntaxKind::TerminalAnd
                | SyntaxKind::TerminalAndAnd
                | SyntaxKind::TerminalOr
                | SyntaxKind::TerminalOrOr
                | SyntaxKind::TerminalXor
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
                | SyntaxKind::TerminalMod
                | SyntaxKind::TerminalColon
                | SyntaxKind::TerminalColonColon
                | SyntaxKind::TerminalComma
                | SyntaxKind::TerminalDot
                | SyntaxKind::TerminalDotDot
                | SyntaxKind::TerminalEq
                | SyntaxKind::TerminalSemicolon
                | SyntaxKind::TerminalQuestionMark
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
                | SyntaxKind::TerminalHash
        )
    }
    pub fn is_keyword_token(&self) -> bool {
        matches!(
            *self,
            SyntaxKind::TokenFalse
                | SyntaxKind::TokenTrue
                | SyntaxKind::TokenExtern
                | SyntaxKind::TokenType
                | SyntaxKind::TokenFunction
                | SyntaxKind::TokenModule
                | SyntaxKind::TokenEnum
                | SyntaxKind::TokenStruct
                | SyntaxKind::TokenTrait
                | SyntaxKind::TokenImpl
                | SyntaxKind::TokenOf
                | SyntaxKind::TokenLet
                | SyntaxKind::TokenReturn
                | SyntaxKind::TokenMatch
                | SyntaxKind::TokenIf
                | SyntaxKind::TokenElse
                | SyntaxKind::TokenUse
                | SyntaxKind::TokenImplicits
                | SyntaxKind::TokenRef
                | SyntaxKind::TokenMut
                | SyntaxKind::TokenNoPanic
        )
    }
    pub fn is_keyword_terminal(&self) -> bool {
        matches!(
            *self,
            SyntaxKind::TerminalFalse
                | SyntaxKind::TerminalTrue
                | SyntaxKind::TerminalExtern
                | SyntaxKind::TerminalType
                | SyntaxKind::TerminalFunction
                | SyntaxKind::TerminalModule
                | SyntaxKind::TerminalEnum
                | SyntaxKind::TerminalStruct
                | SyntaxKind::TerminalTrait
                | SyntaxKind::TerminalImpl
                | SyntaxKind::TerminalOf
                | SyntaxKind::TerminalLet
                | SyntaxKind::TerminalReturn
                | SyntaxKind::TerminalMatch
                | SyntaxKind::TerminalIf
                | SyntaxKind::TerminalElse
                | SyntaxKind::TerminalUse
                | SyntaxKind::TerminalImplicits
                | SyntaxKind::TerminalRef
                | SyntaxKind::TerminalMut
                | SyntaxKind::TerminalNoPanic
        )
    }
}
impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
