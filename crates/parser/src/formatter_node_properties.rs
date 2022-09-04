// Auto-generated code
// TODO(Gil): push the generating code.

// I don't really love the name of this file, but it'll be more reasonable once we export the
// formatter to its own crate.
use syntax::node::db::SyntaxGroup;
use syntax::node::kind::SyntaxKind;
use syntax::node::{SyntaxNode, SyntaxNodeDetails};
use syntax::token::{self, TokenKind};

use crate::formatter::NodePath;

pub trait SyntaxNodeFormat {
    fn force_no_space_before(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    fn force_no_space_after(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    fn should_change_indent(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    fn force_line_break(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    fn should_ignore(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
}
impl SyntaxNodeFormat for SyntaxNode {
    /// The implementation for SyntaxNode should only be called with an internal syntax node.
    /// For tokens see implementation below for token::Token)
    fn force_no_space_before(&self, db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.details(db) {
            SyntaxNodeDetails::Syntax(kind) => match kind {
                SyntaxKind::Terminal
                | SyntaxKind::TriviumSkippedToken
                | SyntaxKind::Trivia
                | SyntaxKind::StructArgExpr
                | SyntaxKind::OptionStructArgExprEmpty
                | SyntaxKind::StructArgSingle
                | SyntaxKind::StructArgTail
                | SyntaxKind::StructArgList
                | SyntaxKind::ArgListBraced
                | SyntaxKind::ExprList
                | SyntaxKind::ExprMissing
                | SyntaxKind::OptionGenericArgsEmpty
                | SyntaxKind::OptionGenericArgsSome
                | SyntaxKind::PathSegment
                | SyntaxKind::ExprPath
                | SyntaxKind::ExprLiteral
                | SyntaxKind::ExprParenthesized
                | SyntaxKind::ExprUnary
                | SyntaxKind::ExprBinary
                | SyntaxKind::ExprTuple
                | SyntaxKind::ExprListParenthesized
                | SyntaxKind::ExprFunctionCall
                | SyntaxKind::ExprStructCtorCall
                | SyntaxKind::ExprBlock
                | SyntaxKind::MatchArm
                | SyntaxKind::MatchArms
                | SyntaxKind::ExprMatch
                | SyntaxKind::TypeClause
                | SyntaxKind::OptionTypeClauseEmpty
                | SyntaxKind::ReturnTypeClause
                | SyntaxKind::OptionReturnTypeClauseEmpty
                | SyntaxKind::StatementList
                | SyntaxKind::StatementMissing
                | SyntaxKind::StatementLet
                | SyntaxKind::OptionSemicolonEmpty
                | SyntaxKind::StatementExpr
                | SyntaxKind::StatementReturn
                | SyntaxKind::Param
                | SyntaxKind::ParamList
                | SyntaxKind::ParamListParenthesized
                | SyntaxKind::ParamListBraced
                | SyntaxKind::FunctionSignature
                | SyntaxKind::ItemList
                | SyntaxKind::ItemModule
                | SyntaxKind::ItemFreeFunction
                | SyntaxKind::ItemExternFunction
                | SyntaxKind::ItemTrait
                | SyntaxKind::ItemImpl
                | SyntaxKind::ItemStruct
                | SyntaxKind::ItemEnum
                | SyntaxKind::ItemUse
                | SyntaxKind::SyntaxFile
                | SyntaxKind::NonOptionTypeClauseMissing
                | SyntaxKind::ItemExternType
                | SyntaxKind::GenericArgList => false,
            },
            SyntaxNodeDetails::Token(_) => {
                unreachable!();
            }
        }
    }
    /// The implementation for SyntaxNode should only be called with an internal syntax node.
    /// For tokens see implementation below for token::Token)
    fn force_no_space_after(&self, db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.details(db) {
            SyntaxNodeDetails::Syntax(kind) => match kind {
                SyntaxKind::Terminal
                | SyntaxKind::TriviumSkippedToken
                | SyntaxKind::Trivia
                | SyntaxKind::StructArgExpr
                | SyntaxKind::OptionStructArgExprEmpty
                | SyntaxKind::StructArgSingle
                | SyntaxKind::StructArgTail
                | SyntaxKind::StructArgList
                | SyntaxKind::ArgListBraced
                | SyntaxKind::ExprList
                | SyntaxKind::ExprMissing
                | SyntaxKind::OptionGenericArgsEmpty
                | SyntaxKind::OptionGenericArgsSome
                | SyntaxKind::PathSegment
                | SyntaxKind::ExprPath
                | SyntaxKind::ExprLiteral
                | SyntaxKind::ExprParenthesized
                | SyntaxKind::ExprUnary
                | SyntaxKind::ExprBinary
                | SyntaxKind::ExprTuple
                | SyntaxKind::ExprListParenthesized
                | SyntaxKind::ExprFunctionCall
                | SyntaxKind::ExprStructCtorCall
                | SyntaxKind::ExprBlock
                | SyntaxKind::MatchArm
                | SyntaxKind::MatchArms
                | SyntaxKind::ExprMatch
                | SyntaxKind::TypeClause
                | SyntaxKind::OptionTypeClauseEmpty
                | SyntaxKind::ReturnTypeClause
                | SyntaxKind::OptionReturnTypeClauseEmpty
                | SyntaxKind::StatementList
                | SyntaxKind::StatementMissing
                | SyntaxKind::StatementLet
                | SyntaxKind::OptionSemicolonEmpty
                | SyntaxKind::StatementExpr
                | SyntaxKind::StatementReturn
                | SyntaxKind::Param
                | SyntaxKind::ParamList
                | SyntaxKind::ParamListParenthesized
                | SyntaxKind::ParamListBraced
                | SyntaxKind::FunctionSignature
                | SyntaxKind::ItemList
                | SyntaxKind::ItemModule
                | SyntaxKind::ItemFreeFunction
                | SyntaxKind::ItemExternFunction
                | SyntaxKind::ItemTrait
                | SyntaxKind::ItemImpl
                | SyntaxKind::ItemStruct
                | SyntaxKind::ItemEnum
                | SyntaxKind::ItemUse
                | SyntaxKind::SyntaxFile
                | SyntaxKind::NonOptionTypeClauseMissing
                | SyntaxKind::ItemExternType
                | SyntaxKind::GenericArgList => false,
            },
            SyntaxNodeDetails::Token(_) => {
                unreachable!();
            }
        }
    }
    /// The implementation for SyntaxNode should only be called with an internal syntax node.
    /// For tokens see implementation below for token::Token)
    fn should_change_indent(&self, db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.details(db) {
            SyntaxNodeDetails::Syntax(kind) => match kind {
                SyntaxKind::StatementList => true,
                SyntaxKind::Terminal
                | SyntaxKind::TriviumSkippedToken
                | SyntaxKind::Trivia
                | SyntaxKind::StructArgExpr
                | SyntaxKind::OptionStructArgExprEmpty
                | SyntaxKind::StructArgSingle
                | SyntaxKind::StructArgTail
                | SyntaxKind::StructArgList
                | SyntaxKind::ArgListBraced
                | SyntaxKind::ExprList
                | SyntaxKind::ExprMissing
                | SyntaxKind::OptionGenericArgsEmpty
                | SyntaxKind::OptionGenericArgsSome
                | SyntaxKind::PathSegment
                | SyntaxKind::ExprPath
                | SyntaxKind::ExprLiteral
                | SyntaxKind::ExprParenthesized
                | SyntaxKind::ExprUnary
                | SyntaxKind::ExprBinary
                | SyntaxKind::ExprTuple
                | SyntaxKind::ExprListParenthesized
                | SyntaxKind::ExprFunctionCall
                | SyntaxKind::ExprStructCtorCall
                | SyntaxKind::ExprBlock
                | SyntaxKind::MatchArm
                | SyntaxKind::MatchArms
                | SyntaxKind::ExprMatch
                | SyntaxKind::TypeClause
                | SyntaxKind::OptionTypeClauseEmpty
                | SyntaxKind::ReturnTypeClause
                | SyntaxKind::OptionReturnTypeClauseEmpty
                | SyntaxKind::StatementMissing
                | SyntaxKind::StatementLet
                | SyntaxKind::OptionSemicolonEmpty
                | SyntaxKind::StatementExpr
                | SyntaxKind::StatementReturn
                | SyntaxKind::Param
                | SyntaxKind::ParamList
                | SyntaxKind::ParamListParenthesized
                | SyntaxKind::ParamListBraced
                | SyntaxKind::FunctionSignature
                | SyntaxKind::ItemList
                | SyntaxKind::ItemModule
                | SyntaxKind::ItemFreeFunction
                | SyntaxKind::ItemExternFunction
                | SyntaxKind::ItemTrait
                | SyntaxKind::ItemImpl
                | SyntaxKind::ItemStruct
                | SyntaxKind::ItemEnum
                | SyntaxKind::ItemUse
                | SyntaxKind::SyntaxFile
                | SyntaxKind::NonOptionTypeClauseMissing
                | SyntaxKind::ItemExternType
                | SyntaxKind::GenericArgList => false,
            },
            SyntaxNodeDetails::Token(_) => {
                unreachable!();
            }
        }
    }
    /// The implementation for SyntaxNode should only be called with an internal syntax node.
    /// For tokens see implementation below for token::Token)
    fn force_line_break(&self, db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.details(db) {
            SyntaxNodeDetails::Syntax(kind) => match kind {
                SyntaxKind::StatementLet
                | SyntaxKind::StatementExpr
                | SyntaxKind::StatementReturn
                | SyntaxKind::ItemFreeFunction
                | SyntaxKind::ItemExternFunction
                | SyntaxKind::ItemTrait
                | SyntaxKind::ItemImpl
                | SyntaxKind::ItemStruct
                | SyntaxKind::ItemEnum
                | SyntaxKind::ItemUse => true,
                SyntaxKind::Terminal
                | SyntaxKind::TriviumSkippedToken
                | SyntaxKind::Trivia
                | SyntaxKind::StructArgExpr
                | SyntaxKind::OptionStructArgExprEmpty
                | SyntaxKind::StructArgSingle
                | SyntaxKind::StructArgTail
                | SyntaxKind::StructArgList
                | SyntaxKind::ArgListBraced
                | SyntaxKind::ExprList
                | SyntaxKind::ExprMissing
                | SyntaxKind::OptionGenericArgsEmpty
                | SyntaxKind::OptionGenericArgsSome
                | SyntaxKind::PathSegment
                | SyntaxKind::ExprPath
                | SyntaxKind::ExprLiteral
                | SyntaxKind::ExprParenthesized
                | SyntaxKind::ExprUnary
                | SyntaxKind::ExprBinary
                | SyntaxKind::ExprTuple
                | SyntaxKind::ExprListParenthesized
                | SyntaxKind::ExprFunctionCall
                | SyntaxKind::ExprStructCtorCall
                | SyntaxKind::ExprBlock
                | SyntaxKind::MatchArm
                | SyntaxKind::MatchArms
                | SyntaxKind::ExprMatch
                | SyntaxKind::TypeClause
                | SyntaxKind::OptionTypeClauseEmpty
                | SyntaxKind::ReturnTypeClause
                | SyntaxKind::OptionReturnTypeClauseEmpty
                | SyntaxKind::StatementList
                | SyntaxKind::StatementMissing
                | SyntaxKind::OptionSemicolonEmpty
                | SyntaxKind::Param
                | SyntaxKind::ParamList
                | SyntaxKind::ParamListParenthesized
                | SyntaxKind::ParamListBraced
                | SyntaxKind::FunctionSignature
                | SyntaxKind::ItemList
                | SyntaxKind::ItemModule
                | SyntaxKind::SyntaxFile
                | SyntaxKind::NonOptionTypeClauseMissing
                | SyntaxKind::ItemExternType
                | SyntaxKind::GenericArgList => false,
            },
            SyntaxNodeDetails::Token(_) => {
                unreachable!();
            }
        }
    }
    /// The implementation for SyntaxNode should only be called with an internal syntax node.
    /// For tokens see implementation below for token::Token)
    fn should_ignore(&self, db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.details(db) {
            SyntaxNodeDetails::Syntax(kind) => match kind {
                SyntaxKind::Terminal
                | SyntaxKind::TriviumSkippedToken
                | SyntaxKind::Trivia
                | SyntaxKind::StructArgExpr
                | SyntaxKind::OptionStructArgExprEmpty
                | SyntaxKind::StructArgSingle
                | SyntaxKind::StructArgTail
                | SyntaxKind::StructArgList
                | SyntaxKind::ArgListBraced
                | SyntaxKind::ExprList
                | SyntaxKind::ExprMissing
                | SyntaxKind::OptionGenericArgsEmpty
                | SyntaxKind::OptionGenericArgsSome
                | SyntaxKind::PathSegment
                | SyntaxKind::ExprPath
                | SyntaxKind::ExprLiteral
                | SyntaxKind::ExprParenthesized
                | SyntaxKind::ExprUnary
                | SyntaxKind::ExprBinary
                | SyntaxKind::ExprTuple
                | SyntaxKind::ExprListParenthesized
                | SyntaxKind::ExprFunctionCall
                | SyntaxKind::ExprStructCtorCall
                | SyntaxKind::ExprBlock
                | SyntaxKind::MatchArm
                | SyntaxKind::MatchArms
                | SyntaxKind::ExprMatch
                | SyntaxKind::TypeClause
                | SyntaxKind::OptionTypeClauseEmpty
                | SyntaxKind::ReturnTypeClause
                | SyntaxKind::OptionReturnTypeClauseEmpty
                | SyntaxKind::StatementList
                | SyntaxKind::StatementMissing
                | SyntaxKind::StatementLet
                | SyntaxKind::OptionSemicolonEmpty
                | SyntaxKind::StatementExpr
                | SyntaxKind::StatementReturn
                | SyntaxKind::Param
                | SyntaxKind::ParamList
                | SyntaxKind::ParamListParenthesized
                | SyntaxKind::ParamListBraced
                | SyntaxKind::FunctionSignature
                | SyntaxKind::ItemList
                | SyntaxKind::ItemModule
                | SyntaxKind::ItemFreeFunction
                | SyntaxKind::ItemExternFunction
                | SyntaxKind::ItemTrait
                | SyntaxKind::ItemImpl
                | SyntaxKind::ItemStruct
                | SyntaxKind::ItemEnum
                | SyntaxKind::ItemUse
                | SyntaxKind::SyntaxFile
                | SyntaxKind::NonOptionTypeClauseMissing
                | SyntaxKind::ItemExternType
                | SyntaxKind::GenericArgList => false,
            },
            SyntaxNodeDetails::Token(_) => {
                unreachable!();
            }
        }
    }
}
impl SyntaxNodeFormat for token::Token {
    fn force_no_space_before(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.kind {
            TokenKind::Colon
            | TokenKind::ColonColon
            | TokenKind::Comma
            | TokenKind::Semicolon
            | TokenKind::RParen => true,
            TokenKind::Identifier
            | TokenKind::LiteralNumber
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Function
            | TokenKind::Module
            | TokenKind::Struct
            | TokenKind::Let
            | TokenKind::Return
            | TokenKind::Match
            | TokenKind::And
            | TokenKind::AndAnd
            | TokenKind::OrOr
            | TokenKind::EqEq
            | TokenKind::Neq
            | TokenKind::GE
            | TokenKind::GT
            | TokenKind::LE
            | TokenKind::LT
            | TokenKind::Not
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Dot
            | TokenKind::DotDot
            | TokenKind::Eq
            | TokenKind::Underscore
            | TokenKind::LBrace
            | TokenKind::RBrace
            | TokenKind::LBrack
            | TokenKind::RBrack
            | TokenKind::LParen
            | TokenKind::Arrow
            | TokenKind::MatchArrow
            | TokenKind::SingleLineComment
            | TokenKind::Whitespace
            | TokenKind::Newline
            | TokenKind::Missing
            | TokenKind::EndOfFile
            | TokenKind::BadCharacters
            | TokenKind::Extern
            | TokenKind::Type
            | TokenKind::Use => false,
        }
    }
    fn force_no_space_after(&self, _db: &dyn SyntaxGroup, node_path: &NodePath) -> bool {
        match self.kind {
            TokenKind::ColonColon | TokenKind::LParen => true,
            TokenKind::Identifier
            | TokenKind::LiteralNumber
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Function
            | TokenKind::Module
            | TokenKind::Struct
            | TokenKind::Let
            | TokenKind::Return
            | TokenKind::Match
            | TokenKind::And
            | TokenKind::AndAnd
            | TokenKind::OrOr
            | TokenKind::EqEq
            | TokenKind::Neq
            | TokenKind::GE
            | TokenKind::GT
            | TokenKind::LE
            | TokenKind::LT
            | TokenKind::Not
            | TokenKind::Plus
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Colon
            | TokenKind::Comma
            | TokenKind::Dot
            | TokenKind::DotDot
            | TokenKind::Eq
            | TokenKind::Semicolon
            | TokenKind::Underscore
            | TokenKind::LBrace
            | TokenKind::RBrace
            | TokenKind::LBrack
            | TokenKind::RBrack
            | TokenKind::RParen
            | TokenKind::Arrow
            | TokenKind::MatchArrow
            | TokenKind::SingleLineComment
            | TokenKind::Whitespace
            | TokenKind::Newline
            | TokenKind::Missing
            | TokenKind::EndOfFile
            | TokenKind::BadCharacters
            | TokenKind::Extern
            | TokenKind::Type
            | TokenKind::Use => false,
            TokenKind::Minus => node_path.is_parent_of_kind(SyntaxKind::ExprUnary),
        }
    }
    fn should_change_indent(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.kind {
            TokenKind::Identifier
            | TokenKind::LiteralNumber
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Function
            | TokenKind::Module
            | TokenKind::Struct
            | TokenKind::Let
            | TokenKind::Return
            | TokenKind::Match
            | TokenKind::And
            | TokenKind::AndAnd
            | TokenKind::OrOr
            | TokenKind::EqEq
            | TokenKind::Neq
            | TokenKind::GE
            | TokenKind::GT
            | TokenKind::LE
            | TokenKind::LT
            | TokenKind::Not
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Colon
            | TokenKind::ColonColon
            | TokenKind::Comma
            | TokenKind::Dot
            | TokenKind::DotDot
            | TokenKind::Eq
            | TokenKind::Semicolon
            | TokenKind::Underscore
            | TokenKind::LBrace
            | TokenKind::RBrace
            | TokenKind::LBrack
            | TokenKind::RBrack
            | TokenKind::LParen
            | TokenKind::RParen
            | TokenKind::Arrow
            | TokenKind::MatchArrow
            | TokenKind::SingleLineComment
            | TokenKind::Whitespace
            | TokenKind::Newline
            | TokenKind::Missing
            | TokenKind::EndOfFile
            | TokenKind::BadCharacters
            | TokenKind::Extern
            | TokenKind::Type
            | TokenKind::Use => false,
        }
    }
    fn force_line_break(&self, _db: &dyn SyntaxGroup, node_path: &NodePath) -> bool {
        match self.kind {
            TokenKind::Identifier
            | TokenKind::LiteralNumber
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Function
            | TokenKind::Module
            | TokenKind::Struct
            | TokenKind::Let
            | TokenKind::Return
            | TokenKind::Match
            | TokenKind::And
            | TokenKind::AndAnd
            | TokenKind::OrOr
            | TokenKind::EqEq
            | TokenKind::Neq
            | TokenKind::GE
            | TokenKind::GT
            | TokenKind::LE
            | TokenKind::LT
            | TokenKind::Not
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Colon
            | TokenKind::ColonColon
            | TokenKind::Comma
            | TokenKind::Dot
            | TokenKind::DotDot
            | TokenKind::Eq
            | TokenKind::Semicolon
            | TokenKind::Underscore
            | TokenKind::RBrace
            | TokenKind::LBrack
            | TokenKind::RBrack
            | TokenKind::LParen
            | TokenKind::RParen
            | TokenKind::Arrow
            | TokenKind::MatchArrow
            | TokenKind::Whitespace
            | TokenKind::Newline
            | TokenKind::Missing
            | TokenKind::EndOfFile
            | TokenKind::BadCharacters
            | TokenKind::Extern
            | TokenKind::Type
            | TokenKind::Use => false,
            TokenKind::LBrace => node_path.is_parent_of_kind(SyntaxKind::ExprBlock),
            TokenKind::SingleLineComment => node_path.is_leading_trivia,
        }
    }
    fn should_ignore(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.kind {
            // We ignore whitespaces and newlines as those are injected by the formatter.
            TokenKind::Whitespace
            | TokenKind::Newline
            | TokenKind::Missing
            | TokenKind::BadCharacters => true,
            TokenKind::Identifier
            | TokenKind::LiteralNumber
            | TokenKind::False
            | TokenKind::True
            | TokenKind::Function
            | TokenKind::Module
            | TokenKind::Struct
            | TokenKind::Let
            | TokenKind::Return
            | TokenKind::Match
            | TokenKind::And
            | TokenKind::AndAnd
            | TokenKind::OrOr
            | TokenKind::EqEq
            | TokenKind::Neq
            | TokenKind::GE
            | TokenKind::GT
            | TokenKind::LE
            | TokenKind::LT
            | TokenKind::Not
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Colon
            | TokenKind::ColonColon
            | TokenKind::Comma
            | TokenKind::Dot
            | TokenKind::DotDot
            | TokenKind::Eq
            | TokenKind::Semicolon
            | TokenKind::Underscore
            | TokenKind::LBrace
            | TokenKind::RBrace
            | TokenKind::LBrack
            | TokenKind::RBrack
            | TokenKind::LParen
            | TokenKind::RParen
            | TokenKind::Arrow
            | TokenKind::MatchArrow
            | TokenKind::SingleLineComment
            | TokenKind::EndOfFile
            | TokenKind::Extern
            | TokenKind::Type
            | TokenKind::Use => false,
        }
    }
}
