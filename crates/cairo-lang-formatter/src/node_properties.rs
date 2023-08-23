use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::utils::{grandparent_kind, parent_kind};
use cairo_lang_syntax::node::SyntaxNode;

use crate::formatter_impl::{
    BreakLinePointIndentation, BreakLinePointProperties, BreakLinePointsPositions, SyntaxNodeFormat,
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
                            | SyntaxKind::PatternStruct
                            | SyntaxKind::Attribute
                    )
                ) =>
            {
                true
            }
            SyntaxKind::ExprPath
                if matches!(parent_kind(db, self), Some(SyntaxKind::PatternEnum))
                    && self
                        .parent()
                        .unwrap()
                        .children(db)
                        .any(|c| c.kind(db) == SyntaxKind::PatternEnumInnerPattern) =>
            {
                true
            }
            SyntaxKind::TokenMinus | SyntaxKind::TokenMul => {
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
                SyntaxKind::VariantList => Some(2),
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
                | SyntaxKind::ExprListParenthesized
                | SyntaxKind::ArgListBraced
                | SyntaxKind::ArgListBracketed
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
                | SyntaxKind::ExprListParenthesized
                | SyntaxKind::ArgListBraced
                | SyntaxKind::ArgListBracketed
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
                | SyntaxKind::ExprListParenthesized
                | SyntaxKind::ArgListBraced
                | SyntaxKind::ArgListBracketed
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
                | SyntaxKind::ExprListParenthesized
                | SyntaxKind::ArgListBraced
                | SyntaxKind::ArgListBracketed
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
                | SyntaxKind::VariantList
                | SyntaxKind::ArgList
                | SyntaxKind::Arg
                | SyntaxKind::GenericArgList
                | SyntaxKind::GenericParamList
                | SyntaxKind::ArgListParenthesized
                | SyntaxKind::StructArgListBraced
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
    ) -> BreakLinePointsPositions {
        match parent_kind(db, self) {
            Some(SyntaxKind::ItemList) => {
                BreakLinePointsPositions::Trailing(BreakLinePointProperties::new(
                    1,
                    BreakLinePointIndentation::NotIndented,
                    false,
                    false,
                ))
            }
            Some(SyntaxKind::StatementList) => {
                BreakLinePointsPositions::Trailing(BreakLinePointProperties::new(
                    10,
                    BreakLinePointIndentation::NotIndented,
                    false,
                    false,
                ))
            }
            Some(SyntaxKind::TraitItemList) | Some(SyntaxKind::ImplItemList) => {
                BreakLinePointsPositions::Trailing(BreakLinePointProperties::new(
                    12,
                    BreakLinePointIndentation::NotIndented,
                    false,
                    false,
                ))
            }
            Some(SyntaxKind::ModuleBody) if self.kind(db) == SyntaxKind::ItemList => {
                BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                    14,
                    BreakLinePointIndentation::IndentedWithTail,
                    false,
                    true,
                ))
            }
            Some(SyntaxKind::AttributeList) => {
                BreakLinePointsPositions::Trailing(BreakLinePointProperties::new(
                    20,
                    BreakLinePointIndentation::NotIndented,
                    false,
                    false,
                ))
            }
            _ => match self.kind(db) {
                SyntaxKind::ParamList
                | SyntaxKind::ExprList
                | SyntaxKind::ImplicitsList
                | SyntaxKind::PatternList => {
                    BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                        2,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    ))
                }
                SyntaxKind::StructArgList => {
                    BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        true,
                    ))
                }
                SyntaxKind::UsePathList => {
                    BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    ))
                }
                SyntaxKind::MemberList | SyntaxKind::VariantList => {
                    BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    ))
                }
                SyntaxKind::ArgList => {
                    BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    ))
                }
                SyntaxKind::StatementList => {
                    BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                        4,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    ))
                }

                SyntaxKind::TraitItemList | SyntaxKind::ImplItemList => {
                    BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                        5,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    ))
                }
                SyntaxKind::MatchArms => {
                    BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                        11,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    ))
                }
                SyntaxKind::GenericParamList => {
                    BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                        6,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    ))
                }
                SyntaxKind::TerminalPlus => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        7,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalMinus
                    if parent_kind(db, self) != Some(SyntaxKind::ExprUnary) =>
                {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        7,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalMul if parent_kind(db, self) != Some(SyntaxKind::ExprUnary) => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        9,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalDiv => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        9,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalAndAnd => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        10,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalOrOr => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        11,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalAnd => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        12,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalOr => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        13,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalXor => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        14,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalDot => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        15,
                        BreakLinePointIndentation::Indented,
                        true,
                        false,
                    ))
                }
                SyntaxKind::TokenEq
                | SyntaxKind::TokenPlusEq
                | SyntaxKind::TokenMinusEq
                | SyntaxKind::TokenMulEq
                | SyntaxKind::TokenDivEq
                | SyntaxKind::TokenModEq => {
                    BreakLinePointsPositions::Trailing(BreakLinePointProperties::new(
                        16,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                _ => BreakLinePointsPositions::None,
            },
        }
    }
    fn get_internal_break_line_point_properties(
        &self,
        db: &dyn SyntaxGroup,
    ) -> BreakLinePointsPositions {
        match self.kind(db) {
            SyntaxKind::ImplicitsList
            | SyntaxKind::ParamList
            | SyntaxKind::PatternList
            | SyntaxKind::PatternStructParamList
            | SyntaxKind::StructArgList
            | SyntaxKind::ArgList
            | SyntaxKind::ExprList
            | SyntaxKind::GenericArgList
            | SyntaxKind::GenericParamList => BreakLinePointsPositions::List {
                properties: BreakLinePointProperties::new(
                    5,
                    BreakLinePointIndentation::NotIndented,
                    true,
                    true,
                ),
                breaking_frequency: 2,
            },
            SyntaxKind::MatchArms | SyntaxKind::MemberList | SyntaxKind::VariantList => {
                BreakLinePointsPositions::List {
                    properties: BreakLinePointProperties::new(
                        6,
                        BreakLinePointIndentation::NotIndented,
                        false,
                        true,
                    ),
                    breaking_frequency: 2,
                }
            }
            SyntaxKind::UsePathList => {
                let mut properties = BreakLinePointProperties::new(
                    6,
                    BreakLinePointIndentation::NotIndented,
                    true,
                    true,
                );
                properties.set_single_breakpoint();
                BreakLinePointsPositions::List { properties, breaking_frequency: 2 }
            }
            _ => BreakLinePointsPositions::None,
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
                    | Some(SyntaxKind::GenericArgValueExpr)
            )
        } else {
            false
        }
    }
}
