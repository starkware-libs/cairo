// Autogenerated file.
// TODO(Gil): push the generating code and point to it from here.

use syntax::node::db::SyntaxGroup;
use syntax::node::kind::SyntaxKind;
use syntax::node::SyntaxNode;

use crate::formatter::SyntaxNodeFormat;

fn parent_kind(db: &dyn SyntaxGroup, syntax_node: &SyntaxNode) -> Option<SyntaxKind> {
    Some(syntax_node.parent()?.kind(db))
}
fn parent_parent_kind(db: &dyn SyntaxGroup, syntax_node: &SyntaxNode) -> Option<SyntaxKind> {
    Some(syntax_node.parent()?.parent()?.kind(db))
}

impl SyntaxNodeFormat for SyntaxNode {
    fn force_no_space_before(&self, db: &dyn SyntaxGroup) -> bool {
        // TODO(yg): add more exhaustiveness protection? Here and elsewhere.
        match self.kind(db) {
            SyntaxKind::TokenDot
            | SyntaxKind::TokenColon
            | SyntaxKind::TokenColonColon
            | SyntaxKind::TokenComma
            | SyntaxKind::TokenSemicolon
            | SyntaxKind::TokenRParen => true,
            SyntaxKind::TokenLT | SyntaxKind::TokenGT
                if matches!(
                    parent_parent_kind(db, self),
                    Some(SyntaxKind::PathSegmentWithGenericArgs) | Some(SyntaxKind::GenericArgs)
                ) =>
            {
                true
            }
            _ => false,
        }
    }

    fn force_no_space_after(&self, db: &dyn SyntaxGroup) -> bool {
        match self.kind(db) {
            SyntaxKind::TokenDot | SyntaxKind::TokenColonColon | SyntaxKind::TokenLParen => true,
            SyntaxKind::ExprPath | SyntaxKind::TerminalIdentifier
                if matches!(
                    parent_kind(db, self),
                    Some(SyntaxKind::ItemFreeFunction)
                        | Some(SyntaxKind::ItemExternFunction)
                        | Some(SyntaxKind::ExprFunctionCall)
                ) =>
            {
                true
            }
            SyntaxKind::TokenMinus => {
                matches!(parent_parent_kind(db, self), Some(SyntaxKind::ExprUnary))
            }
            SyntaxKind::TokenLT
                if matches!(
                    parent_parent_kind(db, self),
                    Some(SyntaxKind::PathSegmentWithGenericArgs) | Some(SyntaxKind::GenericArgs)
                ) =>
            {
                true
            }
            _ => false,
        }
    }

    fn should_change_indent(&self, db: &dyn SyntaxGroup) -> bool {
        matches!(
            self.kind(db),
            SyntaxKind::StatementList
                | SyntaxKind::MatchArms
                | SyntaxKind::ExprList
                | SyntaxKind::StructArgList
                | SyntaxKind::ParamList
                | SyntaxKind::GenericParamList
                | SyntaxKind::GenericArgList
        )
    }

    fn force_line_break(&self, db: &dyn SyntaxGroup) -> bool {
        match self.kind(db) {
            SyntaxKind::StatementLet
            | SyntaxKind::StatementExpr
            | SyntaxKind::StatementReturn
            | SyntaxKind::ItemFreeFunction
            | SyntaxKind::ItemExternFunction
            | SyntaxKind::ItemExternType
            | SyntaxKind::ItemTrait
            | SyntaxKind::ItemImpl
            | SyntaxKind::ItemStruct
            | SyntaxKind::ItemEnum
            | SyntaxKind::ItemUse => true,
            SyntaxKind::TerminalComma
                if matches!(parent_kind(db, self), Some(SyntaxKind::MatchArms)) =>
            {
                true
            }
            SyntaxKind::TerminalLBrace => {
                matches!(
                    parent_kind(db, self),
                    Some(SyntaxKind::ExprBlock)
                        | Some(SyntaxKind::ExprMatch)
                        | Some(SyntaxKind::ItemStruct)
                        | Some(SyntaxKind::ItemEnum)
                )
            }
            _ => false,
        }
    }

    fn allow_newline_after(&self, db: &dyn SyntaxGroup) -> bool {
        match self.kind(db) {
            SyntaxKind::TerminalLParen => true,
            SyntaxKind::TerminalComma
                if matches!(
                    parent_kind(db, self),
                    Some(SyntaxKind::ParamList)
                        | Some(SyntaxKind::ExprList)
                        | Some(SyntaxKind::StructArgList)
                ) =>
            {
                true
            }
            _ => false,
        }
    }

    fn allowed_empty_between(&self, db: &dyn SyntaxGroup) -> usize {
        match self.kind(db) {
            SyntaxKind::ItemList => 2,
            SyntaxKind::StatementList => 1,
            _ => 0,
        }
    }
}
