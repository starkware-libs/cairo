// Autogenerated file.
// TODO(Gil): push the generating code and point to it from here.

use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::utils::{grandparent_kind, parent_kind};
use cairo_lang_syntax::node::SyntaxNode;

use crate::formatter::{
    BreakLinePointIndentation, BreakLinePointProperties, SyntaxNodeFormat, WrappingBreakLinePoints,
};

impl SyntaxNodeFormat for SyntaxNode {
    fn force_no_space_before(&self, db: &dyn SyntaxGroup) -> bool {
        match self.kind(db) {
            SyntaxKind::TokenDot
            | SyntaxKind::TokenColonColon
            | SyntaxKind::TokenComma
            | SyntaxKind::TokenSemicolon
            | SyntaxKind::TokenQuestionMark
            | SyntaxKind::TokenRParen
            | SyntaxKind::TokenRBrack => true,
            SyntaxKind::TokenLParen
                if matches!(
                    grandparent_kind(db, self),
                    Some(SyntaxKind::FunctionSignature | SyntaxKind::AttributeArgs)
                ) =>
            {
                true
            }
            SyntaxKind::TokenColon
                if grandparent_kind(db, self) != Some(SyntaxKind::ArgFieldInitShorthand) =>
            {
                true
            }
            SyntaxKind::TokenLBrack
                if matches!(grandparent_kind(db, self), Some(SyntaxKind::Attribute)) =>
            {
                true
            }
            SyntaxKind::TokenLT | SyntaxKind::TokenGT
                if matches!(
                    grandparent_kind(db, self),
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
            SyntaxKind::TokenDot
            | SyntaxKind::TokenNot
            | SyntaxKind::TokenColonColon
            | SyntaxKind::TokenLParen
            | SyntaxKind::TokenLBrack
            | SyntaxKind::TokenImplicits => true,
            SyntaxKind::ExprPath | SyntaxKind::TerminalIdentifier
                if matches!(
                    parent_kind(db, self),
                    Some(
                        SyntaxKind::ItemFreeFunction
                            | SyntaxKind::ItemExternFunction
                            | SyntaxKind::ExprFunctionCall
                            | SyntaxKind::PatternEnum
                            | SyntaxKind::PatternStruct
                    )
                ) =>
            {
                true
            }
            SyntaxKind::TokenMinus => {
                matches!(grandparent_kind(db, self), Some(SyntaxKind::ExprUnary))
            }
            SyntaxKind::TokenLT
                if matches!(
                    grandparent_kind(db, self),
                    Some(
                        SyntaxKind::PathSegmentWithGenericArgs
                            | SyntaxKind::GenericArgs
                            | SyntaxKind::WrappedGenericParamList
                    )
                ) =>
            {
                true
            }
            SyntaxKind::TokenColon
                if grandparent_kind(db, self) == Some(SyntaxKind::ArgFieldInitShorthand) =>
            {
                true
            }
            _ => false,
        }
    }

    fn should_change_indent(&self, db: &dyn SyntaxGroup) -> bool {
        if matches!(parent_kind(db, self), Some(SyntaxKind::SyntaxFile)) {
            return false;
        }
        matches!(
            self.kind(db),
            SyntaxKind::StatementList
                | SyntaxKind::MatchArms
                | SyntaxKind::ArgList
                | SyntaxKind::ExprList
                | SyntaxKind::StructArgList
                | SyntaxKind::ParamList
                | SyntaxKind::GenericParamList
                | SyntaxKind::GenericArgList
                | SyntaxKind::ItemList
        )
    }

    fn force_line_break(&self, db: &dyn SyntaxGroup) -> bool {
        match self.kind(db) {
            SyntaxKind::StatementLet
            | SyntaxKind::StatementExpr
            | SyntaxKind::StatementReturn
            | SyntaxKind::ItemConstant
            | SyntaxKind::ItemFreeFunction
            | SyntaxKind::ItemExternFunction
            | SyntaxKind::ItemExternType
            | SyntaxKind::ItemTrait
            | SyntaxKind::ItemImpl
            | SyntaxKind::ItemStruct
            | SyntaxKind::Attribute
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
                    Some(
                        SyntaxKind::ExprBlock
                            | SyntaxKind::ExprMatch
                            | SyntaxKind::ModuleBody
                            | SyntaxKind::TraitBody
                    )
                )
            }
            _ => false,
        }
    }

    // TODO(gil): consider removing this function as it is no longer used.
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
    // TODO(Gil): Add all protected zones and break points when the formatter is stable.
    fn is_protected_breaking_node(&self, db: &dyn SyntaxGroup) -> bool {
        matches!(
            self.kind(db),
            SyntaxKind::ExprParenthesized
                | SyntaxKind::ArgList
                | SyntaxKind::ExprList
                | SyntaxKind::MatchArms
                | SyntaxKind::StructArgList
                | SyntaxKind::PatternStructParamList
                | SyntaxKind::PatternList
                | SyntaxKind::ParamList
                | SyntaxKind::ImplicitsList
                | SyntaxKind::MemberList
                | SyntaxKind::AttributeArgList
                | SyntaxKind::GenericArgList
                | SyntaxKind::GenericParamList
                | SyntaxKind::ArgListParenthesized
        )
    }
    fn get_wrapping_break_line_point_properties(
        &self,
        db: &dyn SyntaxGroup,
    ) -> WrappingBreakLinePoints {
        match self.kind(db) {
            SyntaxKind::ParamList
            | SyntaxKind::StructArgList
            | SyntaxKind::ExprList
            | SyntaxKind::ArgList => WrappingBreakLinePoints {
                leading: Some(BreakLinePointProperties {
                    precedence: 0,
                    break_indentation: BreakLinePointIndentation::IndentedWithTail,
                }),
                trailing: Some(BreakLinePointProperties {
                    precedence: 0,
                    break_indentation: BreakLinePointIndentation::IndentedWithTail,
                }),
            },
            SyntaxKind::TerminalComma => WrappingBreakLinePoints {
                leading: None,
                trailing: Some(BreakLinePointProperties {
                    precedence: 0,
                    break_indentation: BreakLinePointIndentation::NotIndented,
                }),
            },
            SyntaxKind::TerminalPlus | SyntaxKind::TerminalMinus => WrappingBreakLinePoints {
                leading: Some(BreakLinePointProperties {
                    precedence: 1,
                    break_indentation: BreakLinePointIndentation::Indented,
                }),
                trailing: None,
            },
            SyntaxKind::TerminalMul | SyntaxKind::TerminalDiv => WrappingBreakLinePoints {
                leading: Some(BreakLinePointProperties {
                    precedence: 2,
                    break_indentation: BreakLinePointIndentation::Indented,
                }),
                trailing: None,
            },
            _ => WrappingBreakLinePoints { leading: None, trailing: None },
        }
    }
}
