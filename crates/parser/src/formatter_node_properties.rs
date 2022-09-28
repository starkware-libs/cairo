use syntax::node::db::SyntaxGroup;
use syntax::node::kind::SyntaxKind;
use syntax::node::SyntaxNode;

use crate::formatter::{BreakPointProperties, SyntaxNodeFormat};

fn parent_kind(db: &dyn SyntaxGroup, syntax_node: &SyntaxNode) -> Option<SyntaxKind> {
    Some(syntax_node.parent()?.kind(db))
}
fn parent_parent_kind(db: &dyn SyntaxGroup, syntax_node: &SyntaxNode) -> Option<SyntaxKind> {
    Some(syntax_node.parent()?.parent()?.kind(db))
}

impl SyntaxNodeFormat for SyntaxNode {
    fn force_no_space_before(&self, db: &dyn SyntaxGroup) -> bool {
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
                    Some(
                        SyntaxKind::PathSegmentWithGenericArgs
                            | SyntaxKind::GenericArgs
                            | SyntaxKind::WrappedGenericParamList
                    )
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
                    Some(
                        SyntaxKind::ItemFreeFunction
                            | SyntaxKind::ItemExternFunction
                            | SyntaxKind::ExprFunctionCall
                    )
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
                    Some(
                        SyntaxKind::PathSegmentWithGenericArgs
                            | SyntaxKind::GenericArgs
                            | SyntaxKind::WrappedGenericParamList
                    )
                ) =>
            {
                true
            }
            SyntaxKind::TokenGT
                if matches!(
                    parent_parent_kind(db, self),
                    Some(SyntaxKind::WrappedGenericParamList)
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
            | SyntaxKind::ItemModule
            | SyntaxKind::ItemUse => true,
            SyntaxKind::TerminalComma
                if matches!(parent_kind(db, self), Some(SyntaxKind::MatchArms)) =>
            {
                true
            }
            SyntaxKind::TerminalLBrace => {
                matches!(
                    parent_kind(db, self),
                    Some(SyntaxKind::ExprBlock | SyntaxKind::ExprMatch | SyntaxKind::ItemEnum)
                )
            }
            _ => false,
        }
    }

    fn allow_newline_after(&self, _db: &dyn SyntaxGroup) -> bool {
        false
    }

    fn allowed_empty_between(&self, db: &dyn SyntaxGroup) -> usize {
        match self.kind(db) {
            SyntaxKind::ItemList => 2,
            SyntaxKind::StatementList => 1,
            _ => 0,
        }
    }

    fn add_break_point_before(&self, db: &dyn SyntaxGroup) -> bool {
        matches!(
            self.kind(db),
            SyntaxKind::TokenPlus
                | SyntaxKind::TokenMinus
                | SyntaxKind::TokenMul
                | SyntaxKind::TokenDiv
        )
    }
    fn add_break_point_after(&self, _db: &dyn SyntaxGroup) -> bool {
        false
    }
    fn is_breakable_list(&self, db: &dyn SyntaxGroup) -> bool {
        matches!(
            self.kind(db),
            SyntaxKind::StructArgList | SyntaxKind::ParamList | SyntaxKind::ExprList
        )
    }
    fn protected_breaking_node(&self, db: &dyn SyntaxGroup) -> bool {
        matches!(
            self.kind(db),
            SyntaxKind::ExprParenthesized
                | SyntaxKind::ExprListParenthesized
                | SyntaxKind::StructArgList
                | SyntaxKind::ParamList
        )
    }

    // TODO(gil): as for now dangling == !is_list, consider removing dangling,
    // i.e. if it is not list it is dangling
    fn get_break_point_properties(&self, db: &dyn SyntaxGroup) -> BreakPointProperties {
        match self.kind(db) {
            SyntaxKind::ExprList => {
                BreakPointProperties { precedence: 10, dangling: false, is_list: true }
            }
            SyntaxKind::StructArgList => {
                BreakPointProperties { precedence: 11, dangling: false, is_list: true }
            }
            SyntaxKind::ParamList => {
                BreakPointProperties { precedence: 12, dangling: false, is_list: true }
            }
            SyntaxKind::TokenPlus => {
                BreakPointProperties { precedence: 100, dangling: true, is_list: false }
            }
            SyntaxKind::TokenMinus => {
                BreakPointProperties { precedence: 100, dangling: true, is_list: false }
            }
            SyntaxKind::TokenMul => {
                BreakPointProperties { precedence: 101, dangling: true, is_list: false }
            }
            SyntaxKind::TokenDiv => {
                BreakPointProperties { precedence: 101, dangling: true, is_list: false }
            }
            _ => unreachable!(),
        }
    }
}
