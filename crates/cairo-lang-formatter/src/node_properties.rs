use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::utils::{grandparent_kind, parent_kind};
use cairo_lang_syntax::node::SyntaxNode;

use crate::formatter_impl::{
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
            | SyntaxKind::TokenRBrack
            | SyntaxKind::TokenLBrack
            | SyntaxKind::TokenSingleLineComment => true,
            SyntaxKind::TokenNot
                if matches!(grandparent_kind(db, self), Some(SyntaxKind::ExprInlineMacro)) =>
            {
                true
            }
            SyntaxKind::TokenLParen
                if matches!(grandparent_kind(db, self), Some(SyntaxKind::FunctionSignature)) =>
            {
                true
            }
            SyntaxKind::TokenLBrace
                if matches!(parent_kind(db, self), Some(SyntaxKind::UsePathList)) =>
            {
                true
            }
            SyntaxKind::TokenColon
                if grandparent_kind(db, self) != Some(SyntaxKind::ArgClauseFieldInitShorthand) =>
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
            | SyntaxKind::TokenBitNot
            | SyntaxKind::TokenAt
            | SyntaxKind::TokenColonColon
            | SyntaxKind::TokenLParen
            | SyntaxKind::TokenLBrack
            | SyntaxKind::TokenLBrace
            | SyntaxKind::TokenImplicits => true,
            SyntaxKind::ExprPath | SyntaxKind::TerminalIdentifier
                if matches!(
                    parent_kind(db, self),
                    Some(
                        SyntaxKind::FunctionWithBody
                            | SyntaxKind::ItemExternFunction
                            | SyntaxKind::ExprFunctionCall
                            | SyntaxKind::PatternEnum
                            | SyntaxKind::PatternStruct
                            | SyntaxKind::Attribute
                    )
                ) =>
            {
                true
            }
            SyntaxKind::TokenMinus | SyntaxKind::TokenMul => {
                matches!(
                    grandparent_kind(db, self),
                    Some(SyntaxKind::ExprUnary | SyntaxKind::LiteralNumber)
                )
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
                if grandparent_kind(db, self) == Some(SyntaxKind::ArgClauseFieldInitShorthand) =>
            {
                true
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
            SyntaxKind::ItemList | SyntaxKind::ImplItemList | SyntaxKind::TraitItemList => 2,
            SyntaxKind::StatementList => 1,
            _ => 0,
        }
    }
    // TODO(Gil): Add all protected zones and break points when the formatter is stable.
    fn get_protected_zone_precedence(&self, db: &dyn SyntaxGroup) -> Option<usize> {
        match parent_kind(db, self) {
            // TODO(Gil): protected zone preferences should be local for each syntax node.
            Some(
                SyntaxKind::ItemList
                | SyntaxKind::ImplItemList
                | SyntaxKind::TraitItemList
                | SyntaxKind::StatementList,
            ) => Some(0),
            Some(SyntaxKind::FunctionWithBody) => match self.kind(db) {
                SyntaxKind::AttributeList => Some(1),
                SyntaxKind::ExprBlock => Some(2),
                SyntaxKind::FunctionDeclaration => Some(3),
                _ => None,
            },
            Some(SyntaxKind::ItemExternFunction) => match self.kind(db) {
                SyntaxKind::AttributeList => Some(1),
                SyntaxKind::FunctionDeclaration => Some(2),
                _ => None,
            },
            Some(SyntaxKind::ItemExternType) => match self.kind(db) {
                SyntaxKind::AttributeList => Some(1),
                SyntaxKind::WrappedGenericParamList => Some(2),
                _ => None,
            },
            Some(SyntaxKind::ItemTypeAlias) => match self.kind(db) {
                SyntaxKind::AttributeList => Some(1),
                SyntaxKind::WrappedGenericParamList => Some(2),
                _ => None,
            },
            Some(SyntaxKind::FunctionDeclaration) => match self.kind(db) {
                SyntaxKind::FunctionSignature => Some(1),
                SyntaxKind::WrappedGenericParamList => Some(2),
                _ => None,
            },
            Some(SyntaxKind::ItemTrait) => match self.kind(db) {
                SyntaxKind::AttributeList => Some(1),
                SyntaxKind::TraitBody => Some(2),
                SyntaxKind::WrappedGenericParamList => Some(3),
                _ => None,
            },
            Some(SyntaxKind::ItemEnum) => match self.kind(db) {
                SyntaxKind::AttributeList => Some(1),
                SyntaxKind::MemberList => Some(2),
                SyntaxKind::WrappedGenericParamList => Some(3),
                _ => None,
            },
            Some(SyntaxKind::ItemStruct) => match self.kind(db) {
                SyntaxKind::AttributeList => Some(1),
                SyntaxKind::MemberList => Some(2),
                SyntaxKind::WrappedGenericParamList => Some(3),
                _ => None,
            },
            Some(SyntaxKind::ItemImpl) => match self.kind(db) {
                SyntaxKind::AttributeList => Some(1),
                SyntaxKind::ImplBody => Some(2),
                SyntaxKind::WrappedGenericParamList => Some(3),
                SyntaxKind::ExprPath => Some(4),
                _ => None,
            },
            Some(SyntaxKind::ItemImplAlias) => match self.kind(db) {
                SyntaxKind::AttributeList => Some(1),
                SyntaxKind::WrappedGenericParamList => Some(2),
                SyntaxKind::ExprPath => Some(3),
                _ => None,
            },
            Some(SyntaxKind::ExprIf) => match self.kind(db) {
                SyntaxKind::ExprBlock => Some(1),
                SyntaxKind::ExprBinary
                | SyntaxKind::ExprErrorPropagate
                | SyntaxKind::ExprFieldInitShorthand
                | SyntaxKind::ExprFunctionCall
                | SyntaxKind::ExprIf
                | SyntaxKind::ExprList
                | SyntaxKind::ExprMatch
                | SyntaxKind::ExprMissing
                | SyntaxKind::ExprParenthesized
                | SyntaxKind::ExprPath
                | SyntaxKind::ExprStructCtorCall
                | SyntaxKind::ExprTuple
                | SyntaxKind::ExprUnary => Some(2),
                SyntaxKind::ElseClause => Some(3),
                _ => None,
            },
            Some(SyntaxKind::ExprMatch) => match self.kind(db) {
                SyntaxKind::MatchArms => Some(1),
                SyntaxKind::ExprBinary
                | SyntaxKind::ExprBlock
                | SyntaxKind::ExprErrorPropagate
                | SyntaxKind::ExprFieldInitShorthand
                | SyntaxKind::ExprFunctionCall
                | SyntaxKind::ExprIf
                | SyntaxKind::ExprList
                | SyntaxKind::ExprMatch
                | SyntaxKind::ExprMissing
                | SyntaxKind::ExprParenthesized
                | SyntaxKind::ExprPath
                | SyntaxKind::ExprStructCtorCall
                | SyntaxKind::ExprTuple
                | SyntaxKind::ExprUnary => Some(10),
                _ => None,
            },
            Some(SyntaxKind::StatementLet) => match self.kind(db) {
                SyntaxKind::ExprBinary
                | SyntaxKind::ExprBlock
                | SyntaxKind::ExprErrorPropagate
                | SyntaxKind::ExprFieldInitShorthand
                | SyntaxKind::ExprFunctionCall
                | SyntaxKind::ExprIf
                | SyntaxKind::ExprList
                | SyntaxKind::ExprMatch
                | SyntaxKind::ExprMissing
                | SyntaxKind::ExprParenthesized
                | SyntaxKind::ExprPath
                | SyntaxKind::ExprStructCtorCall
                | SyntaxKind::ExprTuple
                | SyntaxKind::ExprUnary => Some(1),
                SyntaxKind::TerminalEq => Some(10),
                SyntaxKind::PatternEnum | SyntaxKind::PatternTuple | SyntaxKind::PatternStruct => {
                    Some(11)
                }
                SyntaxKind::TypeClause => Some(12),
                _ => None,
            },
            _ => match self.kind(db) {
                SyntaxKind::ExprParenthesized
                | SyntaxKind::ExprList
                | SyntaxKind::ExprBlock
                | SyntaxKind::ExprTuple
                | SyntaxKind::PatternTuple
                | SyntaxKind::ModuleBody
                | SyntaxKind::MatchArms
                | SyntaxKind::MatchArm
                | SyntaxKind::StructArgList
                | SyntaxKind::PatternStructParamList
                | SyntaxKind::PatternList
                | SyntaxKind::ParamList
                | SyntaxKind::ImplicitsList
                | SyntaxKind::ImplicitsClause
                | SyntaxKind::MemberList
                | SyntaxKind::ArgList
                | SyntaxKind::Arg
                | SyntaxKind::GenericArgList
                | SyntaxKind::GenericParamList
                | SyntaxKind::ArgListParenthesized
                | SyntaxKind::StatementList
                | SyntaxKind::ItemList
                | SyntaxKind::TraitItemList
                | SyntaxKind::ImplItemList
                | SyntaxKind::UsePathMulti
                | SyntaxKind::ItemEnum => Some(5),
                _ => None,
            },
        }
    }
    fn get_wrapping_break_line_point_properties(
        &self,
        db: &dyn SyntaxGroup,
    ) -> WrappingBreakLinePoints {
        match parent_kind(db, self) {
            Some(SyntaxKind::ItemList) => WrappingBreakLinePoints {
                leading: None,
                trailing: Some(BreakLinePointProperties::new(
                    1,
                    BreakLinePointIndentation::NotIndented,
                    false,
                    false,
                )),
            },
            Some(SyntaxKind::StatementList) => WrappingBreakLinePoints {
                leading: None,
                trailing: Some(BreakLinePointProperties::new(
                    10,
                    BreakLinePointIndentation::NotIndented,
                    false,
                    false,
                )),
            },
            Some(SyntaxKind::TraitItemList) | Some(SyntaxKind::ImplItemList) => {
                WrappingBreakLinePoints {
                    leading: None,
                    trailing: Some(BreakLinePointProperties::new(
                        12,
                        BreakLinePointIndentation::NotIndented,
                        false,
                        false,
                    )),
                }
            }
            Some(SyntaxKind::ModuleBody) if self.kind(db) == SyntaxKind::ItemList => {
                WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        14,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    )),
                    trailing: Some(BreakLinePointProperties::new(
                        14,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    )),
                }
            }
            Some(SyntaxKind::AttributeList) => WrappingBreakLinePoints {
                leading: None,
                trailing: Some(BreakLinePointProperties::new(
                    20,
                    BreakLinePointIndentation::NotIndented,
                    false,
                    false,
                )),
            },
            _ => match self.kind(db) {
                SyntaxKind::ParamList
                | SyntaxKind::ExprList
                | SyntaxKind::ImplicitsList
                | SyntaxKind::PatternList => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        2,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    )),
                    trailing: Some(BreakLinePointProperties::new(
                        2,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    )),
                },
                SyntaxKind::StructArgList => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        true,
                    )),
                    trailing: Some(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        true,
                    )),
                },
                SyntaxKind::UsePathList => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    )),
                    trailing: Some(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    )),
                },
                SyntaxKind::MemberList => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    )),
                    trailing: Some(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    )),
                },
                SyntaxKind::ArgList => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    )),
                    trailing: Some(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    )),
                },
                SyntaxKind::StatementList => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        4,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    )),
                    trailing: Some(BreakLinePointProperties::new(
                        4,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    )),
                },
                SyntaxKind::TraitItemList | SyntaxKind::ImplItemList => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        5,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    )),
                    trailing: Some(BreakLinePointProperties::new(
                        5,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    )),
                },

                SyntaxKind::MatchArms => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        11,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    )),
                    trailing: Some(BreakLinePointProperties::new(
                        11,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    )),
                },
                SyntaxKind::GenericParamList => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        6,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    )),
                    trailing: Some(BreakLinePointProperties::new(
                        6,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    )),
                },
                SyntaxKind::TerminalComma
                    if matches!(
                        parent_kind(db, self),
                        Some(SyntaxKind::ImplicitsList)
                            | Some(SyntaxKind::ParamList)
                            | Some(SyntaxKind::PatternStructParamList)
                            | Some(SyntaxKind::PatternList)
                            | Some(SyntaxKind::StructArgList)
                            | Some(SyntaxKind::ArgList)
                            | Some(SyntaxKind::ExprList)
                            | Some(SyntaxKind::GenericArgList)
                            | Some(SyntaxKind::GenericParamList)
                    ) =>
                {
                    WrappingBreakLinePoints {
                        leading: None,
                        trailing: Some(BreakLinePointProperties::new(
                            5,
                            BreakLinePointIndentation::NotIndented,
                            true,
                            true,
                        )),
                    }
                }
                SyntaxKind::TerminalComma
                    if matches!(
                        parent_kind(db, self),
                        Some(SyntaxKind::MemberList) | Some(SyntaxKind::MatchArms)
                    ) =>
                {
                    WrappingBreakLinePoints {
                        leading: None,
                        trailing: Some(BreakLinePointProperties::new(
                            6,
                            BreakLinePointIndentation::NotIndented,
                            false,
                            true,
                        )),
                    }
                }
                SyntaxKind::TerminalComma
                    if matches!(parent_kind(db, self), Some(SyntaxKind::UsePathList)) =>
                {
                    let mut trailing = BreakLinePointProperties::new(
                        6,
                        BreakLinePointIndentation::NotIndented,
                        true,
                        true,
                    );
                    trailing.set_single_breakpoint();
                    WrappingBreakLinePoints { leading: None, trailing: Some(trailing) }
                }
                SyntaxKind::TerminalPlus => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        7,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    )),
                    trailing: None,
                },
                SyntaxKind::TerminalMinus
                    if !matches!(
                        parent_kind(db, self),
                        Some(SyntaxKind::ExprUnary | SyntaxKind::LiteralNumber)
                    ) =>
                {
                    WrappingBreakLinePoints {
                        leading: Some(BreakLinePointProperties::new(
                            7,
                            BreakLinePointIndentation::Indented,
                            true,
                            true,
                        )),
                        trailing: None,
                    }
                }
                SyntaxKind::TerminalMul if parent_kind(db, self) != Some(SyntaxKind::ExprUnary) => {
                    WrappingBreakLinePoints {
                        leading: Some(BreakLinePointProperties::new(
                            9,
                            BreakLinePointIndentation::Indented,
                            true,
                            true,
                        )),
                        trailing: None,
                    }
                }
                SyntaxKind::TerminalDiv => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        9,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    )),
                    trailing: None,
                },
                SyntaxKind::TerminalAndAnd => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        10,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    )),
                    trailing: None,
                },
                SyntaxKind::TerminalOrOr => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        11,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    )),
                    trailing: None,
                },
                SyntaxKind::TerminalAnd => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        12,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    )),
                    trailing: None,
                },
                SyntaxKind::TerminalOr => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        13,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    )),
                    trailing: None,
                },
                SyntaxKind::TerminalXor => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        14,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    )),
                    trailing: None,
                },
                SyntaxKind::TerminalDot => WrappingBreakLinePoints {
                    leading: Some(BreakLinePointProperties::new(
                        15,
                        BreakLinePointIndentation::Indented,
                        true,
                        false,
                    )),
                    trailing: None,
                },
                SyntaxKind::TokenEq
                | SyntaxKind::TokenPlusEq
                | SyntaxKind::TokenMinusEq
                | SyntaxKind::TokenMulEq
                | SyntaxKind::TokenDivEq
                | SyntaxKind::TokenModEq => WrappingBreakLinePoints {
                    leading: None,
                    trailing: Some(BreakLinePointProperties::new(
                        16,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    )),
                },
                _ => WrappingBreakLinePoints { leading: None, trailing: None },
            },
        }
    }

    fn should_skip_terminal(&self, db: &dyn SyntaxGroup) -> bool {
        if self.kind(db) == SyntaxKind::TerminalColonColon
            && parent_kind(db, self) == Some(SyntaxKind::PathSegmentWithGenericArgs)
        {
            let path_node = self.parent().unwrap().parent().unwrap();
            matches!(
                parent_kind(db, &path_node),
                Some(SyntaxKind::ItemImpl)
                    | Some(SyntaxKind::GenericParamImpl)
                    | Some(SyntaxKind::GenericArgExpr)
            )
        } else {
            false
        }
    }
}
