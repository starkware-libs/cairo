use cairo_lang_syntax::node::ast::MaybeModuleBody;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::utils::{grandparent_kind, parent_kind};
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode, ast};

use crate::formatter_impl::{
    BreakLinePointIndentation, BreakLinePointProperties, BreakLinePointsPositions, SortKind,
    SyntaxNodeFormat,
};
use crate::{CollectionsBreakingBehavior, FormatterConfig};

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
            | SyntaxKind::TokenSingleLineComment => true,
            SyntaxKind::TokenNot
                if matches!(
                    grandparent_kind(db, self),
                    Some(SyntaxKind::ExprInlineMacro | SyntaxKind::ItemInlineMacro)
                ) =>
            {
                true
            }
            SyntaxKind::TokenLParen
                if matches!(grandparent_kind(db, self), Some(SyntaxKind::FunctionSignature))
                    | matches!(
                        grandparent_kind(db, self),
                        Some(SyntaxKind::VisibilityPubArgumentClause)
                    ) =>
            {
                true
            }
            SyntaxKind::TokenLBrace
                if matches!(parent_kind(db, self), Some(SyntaxKind::UsePathList)) =>
            {
                true
            }
            SyntaxKind::TokenOr => {
                matches!(grandparent_kind(db, self), Some(SyntaxKind::ExprClosure))
            }
            SyntaxKind::TokenOrOr => {
                matches!(parent_kind(db, self), Some(SyntaxKind::ExprClosure))
            }
            SyntaxKind::TokenLBrack
                if !matches!(
                    grandparent_kind(db, self),
                    Some(SyntaxKind::ExprFixedSizeArray | SyntaxKind::PatternFixedSizeArray)
                ) =>
            {
                true
            }
            SyntaxKind::TokenColon
                if grandparent_kind(db, self) != Some(SyntaxKind::ArgClauseFieldInitShorthand) =>
            {
                true
            }
            SyntaxKind::TokenPlus
                if grandparent_kind(db, self) == Some(SyntaxKind::GenericParamImplAnonymous) =>
            {
                true
            }
            SyntaxKind::TokenMinus
                if grandparent_kind(db, self) == Some(SyntaxKind::GenericParamNegativeImpl) =>
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
            SyntaxKind::ParamList
                if parent_kind(db, self) == Some(SyntaxKind::ClosureParamWrapperNAry) =>
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
            | SyntaxKind::TokenImplicits => true,
            SyntaxKind::TerminalDotDot | SyntaxKind::TerminalDotDotEq
                if matches!(parent_kind(db, self), Some(SyntaxKind::ExprBinary)) =>
            {
                true
            }
            SyntaxKind::TokenLBrace => !matches!(
                grandparent_kind(db, self),
                Some(SyntaxKind::PatternStruct | SyntaxKind::ExprStructCtorCall)
            ),
            SyntaxKind::TokenOr => {
                matches!(grandparent_kind(db, self), Some(SyntaxKind::ExprClosure))
            }
            SyntaxKind::TokenOrOr => {
                matches!(parent_kind(db, self), Some(SyntaxKind::ExprClosure))
            }
            SyntaxKind::ExprPath | SyntaxKind::TerminalIdentifier
                if matches!(
                    parent_kind(db, self),
                    Some(
                        SyntaxKind::FunctionWithBody
                            | SyntaxKind::ItemExternFunction
                            | SyntaxKind::ExprFunctionCall
                            | SyntaxKind::Attribute
                    )
                ) =>
            {
                true
            }

            SyntaxKind::ExprPath
                if matches!(parent_kind(db, self), Some(SyntaxKind::PatternEnum))
                    && db
                        .get_children(self.parent().unwrap())
                        .iter()
                        .any(|c| c.kind(db) == SyntaxKind::PatternEnumInnerPattern) =>
            {
                true
            }
            SyntaxKind::TokenMinus
                if grandparent_kind(db, self) == Some(SyntaxKind::GenericParamNegativeImpl) =>
            {
                true
            }
            SyntaxKind::TokenMinus | SyntaxKind::TokenMul => {
                matches!(grandparent_kind(db, self), Some(SyntaxKind::ExprUnary))
            }
            SyntaxKind::TokenPlus
                if grandparent_kind(db, self) == Some(SyntaxKind::GenericParamImplAnonymous) =>
            {
                true
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
            SyntaxKind::TokenDotDot | SyntaxKind::TokenDotDotEq
                if grandparent_kind(db, self) == Some(SyntaxKind::StructArgTail) =>
            {
                true
            }
            SyntaxKind::ParamList
                if parent_kind(db, self) == Some(SyntaxKind::ClosureParamWrapperNAry) =>
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
            SyntaxKind::ModuleItemList | SyntaxKind::ImplItemList | SyntaxKind::TraitItemList => 2,
            SyntaxKind::StatementList => 1,
            _ => 0,
        }
    }
    // TODO(Gil): Add all protected zones and break points when the formatter is stable.
    fn get_protected_zone_precedence(&self, db: &dyn SyntaxGroup) -> Option<usize> {
        match parent_kind(db, self) {
            // TODO(Gil): protected zone preferences should be local for each syntax node.
            Some(
                SyntaxKind::ModuleItemList
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
            Some(SyntaxKind::ExprWhile) => match self.kind(db) {
                SyntaxKind::ExprBlock => Some(1),
                SyntaxKind::ConditionExpr | SyntaxKind::ConditionLet => Some(2),
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
                | SyntaxKind::ExprUnary => Some(3),
                _ => None,
            },

            Some(SyntaxKind::ExprClosure) => match self.kind(db) {
                SyntaxKind::ClosureParamWrapperNAry => Some(3),
                SyntaxKind::ReturnTypeClause => Some(2),
                SyntaxKind::ExprBlock => Some(1),
                _ => None,
            },

            Some(SyntaxKind::ExprIf) => match self.kind(db) {
                SyntaxKind::ExprBlock => Some(1),
                SyntaxKind::ConditionExpr | SyntaxKind::ConditionLet => Some(2),
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
            Some(SyntaxKind::ExprFor) => match self.kind(db) {
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
                | SyntaxKind::ExprUnary
                | SyntaxKind::ExprInlineMacro => Some(2),
                SyntaxKind::PatternEnum
                | SyntaxKind::PatternTuple
                | SyntaxKind::PatternStruct
                | SyntaxKind::PatternFixedSizeArray => Some(10),
                _ => None,
            },
            Some(SyntaxKind::StatementLet) => {
                let let_statement = ast::StatementLet::from_syntax_node(db, self.parent().unwrap());
                let pattern = let_statement.pattern(db).as_syntax_node();

                if pattern.kind(db) == SyntaxKind::PatternStruct {
                    // Calculate the number of descendants for the pattern (LHS) and RHS of the
                    // `let` statement. The `pattern_count` represents the total
                    // number of nested nodes in the pattern, while `rhs_count`
                    // is limited to at most `pattern_count + 1` descendants.
                    let pattern_count = pattern.descendants(db).count();

                    // Limiting `rhs_count` ensures that we don't traverse deeply nested structures
                    // unnecessarily. If the RHS has more descendants than
                    // `pattern_count`, we can conclude that the RHS is more
                    // complex without fully iterating over all descendants.
                    let rhs_count = let_statement
                        .rhs(db)
                        .as_syntax_node()
                        .descendants(db)
                        .take(pattern_count + 1)
                        .count();

                    if pattern_count > rhs_count { Some(9) } else { Some(11) }
                } else {
                    match self.kind(db) {
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
                        SyntaxKind::PatternEnum
                        | SyntaxKind::PatternTuple
                        | SyntaxKind::PatternFixedSizeArray => Some(11),
                        SyntaxKind::TypeClause => Some(12),
                        _ => None,
                    }
                }
            }
            Some(SyntaxKind::ItemConstant) => match self.kind(db) {
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
                SyntaxKind::PatternEnum
                | SyntaxKind::PatternTuple
                | SyntaxKind::PatternStruct
                | SyntaxKind::PatternFixedSizeArray => Some(11),
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
                | SyntaxKind::ModuleItemList
                | SyntaxKind::TraitItemList
                | SyntaxKind::ImplItemList
                | SyntaxKind::UsePathMulti
                | SyntaxKind::ItemEnum
                | SyntaxKind::PatternFixedSizeArray
                | SyntaxKind::ExprFixedSizeArray => Some(5),
                _ => None,
            },
        }
    }
    fn get_wrapping_break_line_point_properties(
        &self,
        db: &dyn SyntaxGroup,
    ) -> BreakLinePointsPositions {
        // TODO(Gil): Make it easier to order the break points precedence.
        match parent_kind(db, self) {
            Some(SyntaxKind::ModuleItemList) if self.kind(db) != SyntaxKind::ItemHeaderDoc => {
                BreakLinePointsPositions::Trailing(BreakLinePointProperties::new(
                    1,
                    BreakLinePointIndentation::NotIndented,
                    false,
                    false,
                ))
            }
            Some(SyntaxKind::StatementList) => {
                BreakLinePointsPositions::Trailing(BreakLinePointProperties::new(
                    11,
                    BreakLinePointIndentation::NotIndented,
                    is_statement_list_break_point_optional(db, &self.parent().unwrap()),
                    false,
                ))
            }
            Some(SyntaxKind::TraitItemList) | Some(SyntaxKind::ImplItemList) => {
                BreakLinePointsPositions::Trailing(BreakLinePointProperties::new(
                    13,
                    BreakLinePointIndentation::NotIndented,
                    false,
                    false,
                ))
            }
            Some(SyntaxKind::ModuleBody) if self.kind(db) == SyntaxKind::ModuleItemList => {
                BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                    15,
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
                SyntaxKind::PatternStructParamList => {
                    let leading_break_point = BreakLinePointProperties::new(
                        2,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        true,
                    );
                    let mut trailing_break_point = leading_break_point.clone();
                    trailing_break_point.set_comma_if_broken();
                    BreakLinePointsPositions::Both {
                        leading: leading_break_point,
                        trailing: trailing_break_point,
                    }
                }
                SyntaxKind::ExprList | SyntaxKind::PatternList => {
                    let leading_break_point = BreakLinePointProperties::new(
                        2,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    );
                    let mut trailing_break_point = leading_break_point.clone();
                    if db.get_children(self.clone()).len() > 2 {
                        trailing_break_point.set_comma_if_broken();
                    }
                    BreakLinePointsPositions::Both {
                        leading: leading_break_point,
                        trailing: trailing_break_point,
                    }
                }
                SyntaxKind::ImplicitsList => {
                    let leading_break_point = BreakLinePointProperties::new(
                        2,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    );
                    let mut trailing_break_point = leading_break_point.clone();
                    trailing_break_point.set_comma_if_broken();
                    BreakLinePointsPositions::Both {
                        leading: leading_break_point,
                        trailing: trailing_break_point,
                    }
                }
                SyntaxKind::ParamList => {
                    let leading_break_point = BreakLinePointProperties::new(
                        2,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    );
                    let mut trailing_break_point = leading_break_point.clone();
                    trailing_break_point.set_comma_if_broken();
                    BreakLinePointsPositions::Both {
                        leading: leading_break_point,
                        trailing: trailing_break_point,
                    }
                }
                SyntaxKind::StructArgList => {
                    let leading_break_point = BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        true,
                    );
                    let mut trailing_break_point = leading_break_point.clone();
                    trailing_break_point.set_comma_if_broken();
                    BreakLinePointsPositions::Both {
                        leading: leading_break_point,
                        trailing: trailing_break_point,
                    }
                }
                SyntaxKind::UsePathList => {
                    let leading_break_point = BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    );
                    let mut trailing_break_point = leading_break_point.clone();
                    trailing_break_point.set_comma_if_broken();
                    BreakLinePointsPositions::Both {
                        leading: leading_break_point,
                        trailing: trailing_break_point,
                    }
                }
                SyntaxKind::MemberList | SyntaxKind::VariantList => {
                    let leading_break_point = BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    );
                    let mut trailing_break_point = leading_break_point.clone();
                    trailing_break_point.set_comma_if_broken();
                    BreakLinePointsPositions::Both {
                        leading: leading_break_point,
                        trailing: trailing_break_point,
                    }
                }
                SyntaxKind::ArgList => {
                    let leading_break_point = BreakLinePointProperties::new(
                        3,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    );
                    let mut trailing_break_point = leading_break_point.clone();
                    trailing_break_point.set_comma_if_broken();
                    BreakLinePointsPositions::Both {
                        leading: leading_break_point,
                        trailing: trailing_break_point,
                    }
                }
                SyntaxKind::StatementList => {
                    BreakLinePointsPositions::new_symmetric(BreakLinePointProperties::new(
                        4,
                        BreakLinePointIndentation::IndentedWithTail,
                        is_statement_list_break_point_optional(db, self),
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
                    let leading_break_point = BreakLinePointProperties::new(
                        12,
                        BreakLinePointIndentation::IndentedWithTail,
                        false,
                        true,
                    );
                    let mut trailing_break_point = leading_break_point.clone();
                    trailing_break_point.set_comma_if_broken();
                    BreakLinePointsPositions::Both {
                        leading: leading_break_point,
                        trailing: trailing_break_point,
                    }
                }
                SyntaxKind::GenericParamList => {
                    let leading_break_point = BreakLinePointProperties::new(
                        6,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    );
                    let mut trailing_break_point = leading_break_point.clone();
                    trailing_break_point.set_comma_if_broken();
                    BreakLinePointsPositions::Both {
                        leading: leading_break_point,
                        trailing: trailing_break_point,
                    }
                }
                SyntaxKind::GenericArgList => {
                    let leading_break_point = BreakLinePointProperties::new(
                        21,
                        BreakLinePointIndentation::IndentedWithTail,
                        true,
                        false,
                    );
                    let mut trailing_break_point = leading_break_point.clone();
                    trailing_break_point.set_comma_if_broken();
                    BreakLinePointsPositions::Both {
                        leading: leading_break_point,
                        trailing: trailing_break_point,
                    }
                }
                SyntaxKind::TerminalPlus
                    if !matches!(
                        parent_kind(db, self),
                        Some(SyntaxKind::GenericParamImplAnonymous)
                    ) =>
                {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        8,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalMinus
                    if !matches!(
                        parent_kind(db, self),
                        Some(SyntaxKind::ExprUnary | SyntaxKind::GenericParamNegativeImpl)
                    ) =>
                {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        8,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalMul
                    if !matches!(
                        parent_kind(db, self),
                        Some(SyntaxKind::ExprUnary | SyntaxKind::UsePathStar)
                    ) =>
                {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        10,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalDiv => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        10,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalAndAnd => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        11,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalOrOr
                    if !matches!(parent_kind(db, self), Some(SyntaxKind::ExprClosure)) =>
                {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        12,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalAnd => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        13,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalOr
                    if !matches!(
                        parent_kind(db, self),
                        Some(SyntaxKind::PatternListOr | SyntaxKind::ClosureParamWrapperNAry)
                    ) =>
                {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        14,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalXor => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        15,
                        BreakLinePointIndentation::Indented,
                        true,
                        true,
                    ))
                }
                SyntaxKind::TerminalDotDot | SyntaxKind::TerminalDotDotEq
                    if matches!(parent_kind(db, self), Some(SyntaxKind::ExprBinary)) =>
                {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        7,
                        BreakLinePointIndentation::Indented,
                        true,
                        false,
                    ))
                }
                SyntaxKind::TerminalDot => {
                    BreakLinePointsPositions::Leading(BreakLinePointProperties::new(
                        16,
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
                        17,
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
        config: &FormatterConfig,
    ) -> BreakLinePointsPositions {
        match self.kind(db) {
            SyntaxKind::ImplicitsList
            | SyntaxKind::PatternList
            | SyntaxKind::PatternStructParamList
            | SyntaxKind::StructArgList
            | SyntaxKind::GenericArgList
            | SyntaxKind::GenericParamList
            | SyntaxKind::ParamList
            | SyntaxKind::ArgList => BreakLinePointsPositions::List {
                properties: BreakLinePointProperties::new(
                    5,
                    BreakLinePointIndentation::NotIndented,
                    true,
                    true,
                ),
                breaking_frequency: 2,
            },
            SyntaxKind::ExprList => {
                let mut properties = BreakLinePointProperties::new(
                    5,
                    BreakLinePointIndentation::NotIndented,
                    true,
                    true,
                );

                if let Some(parent_kind) = parent_kind(db, self) {
                    match parent_kind {
                        SyntaxKind::ExprListParenthesized => match config.tuple_breaking_behavior {
                            CollectionsBreakingBehavior::SingleBreakPoint => {
                                properties.set_single_breakpoint();
                            }
                            CollectionsBreakingBehavior::LineByLine => {
                                properties.set_line_by_line();
                            }
                        },
                        SyntaxKind::ExprFixedSizeArray => {
                            match config.fixed_array_breaking_behavior {
                                CollectionsBreakingBehavior::SingleBreakPoint => {
                                    properties.set_single_breakpoint();
                                }
                                CollectionsBreakingBehavior::LineByLine => {
                                    properties.set_line_by_line();
                                }
                            }
                        }
                        _ => {
                            properties.set_line_by_line();
                        }
                    }
                }

                BreakLinePointsPositions::List { properties, breaking_frequency: 2 }
            }
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
            SyntaxKind::PatternListOr | SyntaxKind::UsePathList => {
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
        let is_last = |node: &SyntaxNode, siblings: &[SyntaxNode]| siblings.last() == Some(node);
        // Check for TerminalComma with specific conditions on list types and position.
        if self.kind(db) == SyntaxKind::TerminalComma
            && matches!(
                parent_kind(db, self),
                Some(
                    SyntaxKind::ExprList
                        | SyntaxKind::PatternList
                        | SyntaxKind::ArgList
                        | SyntaxKind::ParamList
                        | SyntaxKind::ImplicitsList
                        | SyntaxKind::MemberList
                        | SyntaxKind::VariantList
                        | SyntaxKind::UsePathList
                        | SyntaxKind::GenericArgList
                        | SyntaxKind::GenericParamList
                        | SyntaxKind::MatchArms
                        | SyntaxKind::StructArgList
                        | SyntaxKind::PatternStructParamList
                )
            )
        {
            let parent_node = self.parent().unwrap();
            let children = db.get_children(parent_node);
            // Check if it's an ExprList or PatternList with len > 2, or any other list type.
            let is_expr_or_pattern_list = matches!(
                parent_kind(db, self),
                Some(SyntaxKind::ExprList | SyntaxKind::PatternList)
            );
            if (!is_expr_or_pattern_list || children.len() > 2)
            // Ensure that this node is the last element in the list.
            && is_last(self, &children)
            {
                return true;
            }
        }
        if self.kind(db) == SyntaxKind::TerminalEmpty {
            return true;
        }
        if self.kind(db) == SyntaxKind::TerminalSemicolon
            && parent_kind(db, self) == Some(SyntaxKind::StatementExpr)
        {
            let statement_node = self.parent().unwrap();
            let statements_node = statement_node.parent().unwrap();
            // Checking if not the last statement, as `;` may be there to prevent the block from
            // returning the value of the current block.
            let not_last = !is_last(&statement_node, &db.get_children(statements_node));
            let children = db.get_children(statement_node);
            if not_last
                && matches!(
                    children[1].kind(db),
                    SyntaxKind::ExprBlock
                        | SyntaxKind::ExprIf
                        | SyntaxKind::ExprMatch
                        | SyntaxKind::ExprLoop
                        | SyntaxKind::ExprWhile
                        | SyntaxKind::ExprFor
                )
            {
                return true;
            }
        }
        if self.kind(db) == SyntaxKind::TerminalColonColon
            && parent_kind(db, self) == Some(SyntaxKind::PathSegmentWithGenericArgs)
        {
            let path_segment_node = self.parent().unwrap();
            let path_node = path_segment_node.parent().unwrap();
            if !is_last(&path_segment_node, &db.get_children(path_node.clone())) {
                false
            } else {
                matches!(
                    parent_kind(db, &path_node),
                    Some(
                        SyntaxKind::GenericArgValueExpr
                            | SyntaxKind::GenericParamImplAnonymous
                            | SyntaxKind::GenericParamImplNamed
                            | SyntaxKind::ItemImpl
                            | SyntaxKind::ReturnTypeClause
                            | SyntaxKind::TypeClause
                    )
                )
            }
        } else {
            false
        }
    }

    // Merge the `as_sort_kind` method here
    fn as_sort_kind(&self, db: &dyn SyntaxGroup) -> SortKind {
        match self.kind(db) {
            SyntaxKind::ItemModule => {
                let item_module = ast::ItemModule::from_syntax_node(db, self.clone());
                if matches!(item_module.body(db), MaybeModuleBody::None(_)) {
                    SortKind::Module
                } else {
                    SortKind::Immovable
                }
            }
            SyntaxKind::ItemUse => SortKind::UseItem,
            _ => SortKind::Immovable,
        }
    }
}

/// For statement lists, returns if we want these as a single line.
fn is_statement_list_break_point_optional(db: &dyn SyntaxGroup, node: &SyntaxNode) -> bool {
    // Currently, we only want single line blocks for match arms or generic args, with a single
    // statement, with no single line comments.
    matches!(
        grandparent_kind(db, node),
        Some(SyntaxKind::MatchArm | SyntaxKind::GenericArgValueExpr)
    ) && db.get_children(node.clone()).len() == 1
        && node.descendants(db).all(|d| {
            d.kind(db) != SyntaxKind::Trivia
                || ast::Trivia::from_syntax_node(db, d)
                    .elements(db)
                    .iter()
                    .all(|t| !matches!(t, ast::Trivium::SingleLineComment(_)))
        })
}
