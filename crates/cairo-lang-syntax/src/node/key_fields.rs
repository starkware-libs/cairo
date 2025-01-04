// Autogenerated file. To regenerate, please run `cargo run --bin generate-syntax`.

use super::ids::GreenId;
use super::kind::SyntaxKind;

/// Gets the vector of children ids that are the indexing key for this SyntaxKind.

///

/// Each SyntaxKind has some children that are defined in the spec to be its indexing key

/// for its stable pointer. See [super::stable_ptr].

pub fn get_key_fields(kind: SyntaxKind, children: &[GreenId]) -> Vec<GreenId> {
    match kind {
        SyntaxKind::Trivia => vec![],
        SyntaxKind::ExprList => vec![],
        SyntaxKind::Arg => {
            vec![]
        }
        SyntaxKind::ArgClauseNamed => {
            vec![]
        }
        SyntaxKind::ArgClauseUnnamed => {
            vec![]
        }
        SyntaxKind::ArgClauseFieldInitShorthand => {
            vec![]
        }
        SyntaxKind::ExprFieldInitShorthand => {
            vec![]
        }
        SyntaxKind::ArgList => vec![],
        SyntaxKind::ExprMissing => {
            vec![]
        }
        SyntaxKind::PathSegmentSimple => {
            vec![]
        }
        SyntaxKind::OptionTerminalColonColonEmpty => {
            vec![]
        }
        SyntaxKind::PathSegmentWithGenericArgs => {
            vec![]
        }
        SyntaxKind::ExprPath => vec![],
        SyntaxKind::ExprParenthesized => {
            vec![]
        }
        SyntaxKind::ExprUnary => {
            vec![]
        }
        SyntaxKind::ExprBinary => {
            vec![]
        }
        SyntaxKind::ExprListParenthesized => {
            vec![]
        }
        SyntaxKind::ExprFunctionCall => {
            vec![]
        }
        SyntaxKind::ArgListParenthesized => {
            vec![]
        }
        SyntaxKind::OptionArgListParenthesizedEmpty => {
            vec![]
        }
        SyntaxKind::ExprStructCtorCall => {
            vec![]
        }
        SyntaxKind::StructArgListBraced => {
            vec![]
        }
        SyntaxKind::ExprBlock => {
            vec![]
        }
        SyntaxKind::ExprMatch => {
            vec![]
        }
        SyntaxKind::MatchArms => vec![],
        SyntaxKind::MatchArm => {
            vec![]
        }
        SyntaxKind::ExprIf => {
            vec![]
        }
        SyntaxKind::ConditionLet => {
            vec![]
        }
        SyntaxKind::ConditionExpr => {
            vec![]
        }
        SyntaxKind::ExprLoop => {
            vec![]
        }
        SyntaxKind::ExprWhile => {
            vec![]
        }
        SyntaxKind::ExprFor => {
            vec![/* pattern */ children[1], /* identifier */ children[2]]
        }
        SyntaxKind::ElseClause => {
            vec![]
        }
        SyntaxKind::OptionElseClauseEmpty => {
            vec![]
        }
        SyntaxKind::ExprErrorPropagate => {
            vec![]
        }
        SyntaxKind::ExprIndexed => {
            vec![]
        }
        SyntaxKind::ExprInlineMacro => {
            vec![]
        }
        SyntaxKind::ExprFixedSizeArray => {
            vec![]
        }
        SyntaxKind::FixedSizeArraySize => {
            vec![]
        }
        SyntaxKind::OptionFixedSizeArraySizeEmpty => {
            vec![]
        }
        SyntaxKind::ExprClosure => {
            vec![]
        }
        SyntaxKind::ClosureParamWrapperNAry => {
            vec![]
        }
        SyntaxKind::StructArgExpr => {
            vec![]
        }
        SyntaxKind::OptionStructArgExprEmpty => {
            vec![]
        }
        SyntaxKind::StructArgSingle => {
            vec![/* identifier */ children[0]]
        }
        SyntaxKind::StructArgTail => {
            vec![]
        }
        SyntaxKind::StructArgList => vec![],
        SyntaxKind::ArgListBraced => {
            vec![]
        }
        SyntaxKind::ArgListBracketed => {
            vec![]
        }
        SyntaxKind::WrappedArgListMissing => {
            vec![]
        }
        SyntaxKind::PatternIdentifier => {
            vec![/* name */ children[1]]
        }
        SyntaxKind::PatternStruct => {
            vec![]
        }
        SyntaxKind::PatternStructParamList => vec![],
        SyntaxKind::PatternTuple => {
            vec![]
        }
        SyntaxKind::PatternFixedSizeArray => {
            vec![]
        }
        SyntaxKind::PatternList => vec![],
        SyntaxKind::PatternListOr => vec![],
        SyntaxKind::PatternStructParamWithExpr => {
            vec![]
        }
        SyntaxKind::PatternEnum => {
            vec![]
        }
        SyntaxKind::PatternEnumInnerPattern => {
            vec![]
        }
        SyntaxKind::OptionPatternEnumInnerPatternEmpty => {
            vec![]
        }
        SyntaxKind::TypeClause => {
            vec![]
        }
        SyntaxKind::OptionTypeClauseEmpty => {
            vec![]
        }
        SyntaxKind::ReturnTypeClause => {
            vec![]
        }
        SyntaxKind::OptionReturnTypeClauseEmpty => {
            vec![]
        }
        SyntaxKind::StatementList => vec![],
        SyntaxKind::StatementMissing => {
            vec![]
        }
        SyntaxKind::StatementLet => {
            vec![/* pattern */ children[2]]
        }
        SyntaxKind::OptionTerminalSemicolonEmpty => {
            vec![]
        }
        SyntaxKind::StatementExpr => {
            vec![]
        }
        SyntaxKind::StatementContinue => {
            vec![]
        }
        SyntaxKind::ExprClause => {
            vec![]
        }
        SyntaxKind::OptionExprClauseEmpty => {
            vec![]
        }
        SyntaxKind::StatementReturn => {
            vec![]
        }
        SyntaxKind::StatementBreak => {
            vec![]
        }
        SyntaxKind::StatementItem => {
            vec![]
        }
        SyntaxKind::Param => {
            vec![/* name */ children[1]]
        }
        SyntaxKind::ModifierList => vec![],
        SyntaxKind::ParamList => vec![],
        SyntaxKind::ImplicitsClause => {
            vec![]
        }
        SyntaxKind::ImplicitsList => vec![],
        SyntaxKind::OptionImplicitsClauseEmpty => {
            vec![]
        }
        SyntaxKind::OptionTerminalNoPanicEmpty => {
            vec![]
        }
        SyntaxKind::FunctionSignature => {
            vec![]
        }
        SyntaxKind::Member => {
            vec![/* name */ children[2]]
        }
        SyntaxKind::MemberList => vec![],
        SyntaxKind::Variant => {
            vec![/* name */ children[1]]
        }
        SyntaxKind::VariantList => vec![],
        SyntaxKind::ModuleItemList => vec![],
        SyntaxKind::ModuleItemMissing => {
            vec![]
        }
        SyntaxKind::Attribute => {
            vec![]
        }
        SyntaxKind::AttributeList => vec![],
        SyntaxKind::VisibilityDefault => {
            vec![]
        }
        SyntaxKind::VisibilityPubArgumentClause => {
            vec![]
        }
        SyntaxKind::OptionVisibilityPubArgumentClauseEmpty => {
            vec![]
        }
        SyntaxKind::VisibilityPub => {
            vec![]
        }
        SyntaxKind::ItemModule => {
            vec![/* name */ children[3]]
        }
        SyntaxKind::ModuleBody => {
            vec![]
        }
        SyntaxKind::FunctionDeclaration => {
            vec![/* name */ children[1]]
        }
        SyntaxKind::ItemConstant => {
            vec![/* name */ children[3]]
        }
        SyntaxKind::FunctionWithBody => {
            vec![/* declaration */ children[2]]
        }
        SyntaxKind::ItemExternFunction => {
            vec![/* declaration */ children[3]]
        }
        SyntaxKind::ItemExternType => {
            vec![/* name */ children[4]]
        }
        SyntaxKind::ItemTrait => {
            vec![/* name */ children[3]]
        }
        SyntaxKind::TraitBody => {
            vec![]
        }
        SyntaxKind::TraitItemList => vec![],
        SyntaxKind::TraitItemMissing => {
            vec![]
        }
        SyntaxKind::TraitItemFunction => {
            vec![/* declaration */ children[1]]
        }
        SyntaxKind::TraitItemType => {
            vec![/* name */ children[2]]
        }
        SyntaxKind::TraitItemConstant => {
            vec![/* name */ children[2]]
        }
        SyntaxKind::TraitItemImpl => {
            vec![/* name */ children[2]]
        }
        SyntaxKind::ItemImpl => {
            vec![/* name */ children[3]]
        }
        SyntaxKind::ItemInlineMacro => {
            vec![]
        }
        SyntaxKind::ItemHeaderDoc => {
            vec![]
        }
        SyntaxKind::ImplBody => {
            vec![]
        }
        SyntaxKind::ImplItemList => vec![],
        SyntaxKind::ImplItemMissing => {
            vec![]
        }
        SyntaxKind::ItemImplAlias => {
            vec![/* name */ children[3]]
        }
        SyntaxKind::ItemStruct => {
            vec![/* name */ children[3]]
        }
        SyntaxKind::ItemEnum => {
            vec![/* name */ children[3]]
        }
        SyntaxKind::ItemTypeAlias => {
            vec![/* name */ children[3]]
        }
        SyntaxKind::ItemUse => {
            vec![/* use_path */ children[3]]
        }
        SyntaxKind::UsePathLeaf => {
            vec![/* ident */ children[0], /* alias_clause */ children[1]]
        }
        SyntaxKind::UsePathSingle => {
            vec![]
        }
        SyntaxKind::UsePathMulti => {
            vec![]
        }
        SyntaxKind::UsePathStar => {
            vec![]
        }
        SyntaxKind::UsePathList => vec![],
        SyntaxKind::AliasClause => {
            vec![/* alias */ children[1]]
        }
        SyntaxKind::OptionAliasClauseEmpty => {
            vec![]
        }
        SyntaxKind::GenericArgNamed => {
            vec![]
        }
        SyntaxKind::GenericArgUnnamed => {
            vec![]
        }
        SyntaxKind::GenericArgValueExpr => {
            vec![]
        }
        SyntaxKind::GenericArgs => {
            vec![]
        }
        SyntaxKind::GenericArgList => vec![],
        SyntaxKind::AssociatedItemConstraint => {
            vec![]
        }
        SyntaxKind::AssociatedItemConstraints => {
            vec![]
        }
        SyntaxKind::AssociatedItemConstraintList => vec![],
        SyntaxKind::OptionAssociatedItemConstraintsEmpty => {
            vec![]
        }
        SyntaxKind::OptionWrappedGenericParamListEmpty => {
            vec![]
        }
        SyntaxKind::WrappedGenericParamList => {
            vec![]
        }
        SyntaxKind::GenericParamList => vec![],
        SyntaxKind::GenericParamType => {
            vec![/* name */ children[0]]
        }
        SyntaxKind::GenericParamConst => {
            vec![/* name */ children[1]]
        }
        SyntaxKind::GenericParamImplNamed => {
            vec![/* name */ children[1]]
        }
        SyntaxKind::GenericParamImplAnonymous => {
            vec![]
        }
        SyntaxKind::GenericParamNegativeImpl => {
            vec![]
        }
        SyntaxKind::TriviumSkippedNode => {
            vec![]
        }
        SyntaxKind::TokenIdentifier => vec![],
        SyntaxKind::TerminalIdentifier => {
            vec![]
        }
        SyntaxKind::TokenLiteralNumber => vec![],
        SyntaxKind::TerminalLiteralNumber => {
            vec![]
        }
        SyntaxKind::TokenShortString => vec![],
        SyntaxKind::TerminalShortString => {
            vec![]
        }
        SyntaxKind::TokenString => vec![],
        SyntaxKind::TerminalString => {
            vec![]
        }
        SyntaxKind::TokenAs => vec![],
        SyntaxKind::TerminalAs => {
            vec![]
        }
        SyntaxKind::TokenConst => vec![],
        SyntaxKind::TerminalConst => {
            vec![]
        }
        SyntaxKind::TokenElse => vec![],
        SyntaxKind::TerminalElse => {
            vec![]
        }
        SyntaxKind::TokenEnum => vec![],
        SyntaxKind::TerminalEnum => {
            vec![]
        }
        SyntaxKind::TokenExtern => vec![],
        SyntaxKind::TerminalExtern => {
            vec![]
        }
        SyntaxKind::TokenFalse => vec![],
        SyntaxKind::TerminalFalse => {
            vec![]
        }
        SyntaxKind::TokenFunction => vec![],
        SyntaxKind::TerminalFunction => {
            vec![]
        }
        SyntaxKind::TokenIf => vec![],
        SyntaxKind::TerminalIf => {
            vec![]
        }
        SyntaxKind::TokenWhile => vec![],
        SyntaxKind::TerminalWhile => {
            vec![]
        }
        SyntaxKind::TokenFor => vec![],
        SyntaxKind::TerminalFor => {
            vec![]
        }
        SyntaxKind::TokenLoop => vec![],
        SyntaxKind::TerminalLoop => {
            vec![]
        }
        SyntaxKind::TokenImpl => vec![],
        SyntaxKind::TerminalImpl => {
            vec![]
        }
        SyntaxKind::TokenImplicits => vec![],
        SyntaxKind::TerminalImplicits => {
            vec![]
        }
        SyntaxKind::TokenLet => vec![],
        SyntaxKind::TerminalLet => {
            vec![]
        }
        SyntaxKind::TokenMatch => vec![],
        SyntaxKind::TerminalMatch => {
            vec![]
        }
        SyntaxKind::TokenModule => vec![],
        SyntaxKind::TerminalModule => {
            vec![]
        }
        SyntaxKind::TokenMut => vec![],
        SyntaxKind::TerminalMut => {
            vec![]
        }
        SyntaxKind::TokenNoPanic => vec![],
        SyntaxKind::TerminalNoPanic => {
            vec![]
        }
        SyntaxKind::TokenOf => vec![],
        SyntaxKind::TerminalOf => {
            vec![]
        }
        SyntaxKind::TokenRef => vec![],
        SyntaxKind::TerminalRef => {
            vec![]
        }
        SyntaxKind::TokenContinue => vec![],
        SyntaxKind::TerminalContinue => {
            vec![]
        }
        SyntaxKind::TokenReturn => vec![],
        SyntaxKind::TerminalReturn => {
            vec![]
        }
        SyntaxKind::TokenBreak => vec![],
        SyntaxKind::TerminalBreak => {
            vec![]
        }
        SyntaxKind::TokenStruct => vec![],
        SyntaxKind::TerminalStruct => {
            vec![]
        }
        SyntaxKind::TokenTrait => vec![],
        SyntaxKind::TerminalTrait => {
            vec![]
        }
        SyntaxKind::TokenTrue => vec![],
        SyntaxKind::TerminalTrue => {
            vec![]
        }
        SyntaxKind::TokenType => vec![],
        SyntaxKind::TerminalType => {
            vec![]
        }
        SyntaxKind::TokenUse => vec![],
        SyntaxKind::TerminalUse => {
            vec![]
        }
        SyntaxKind::TokenPub => vec![],
        SyntaxKind::TerminalPub => {
            vec![]
        }
        SyntaxKind::TokenAnd => vec![],
        SyntaxKind::TerminalAnd => {
            vec![]
        }
        SyntaxKind::TokenAndAnd => vec![],
        SyntaxKind::TerminalAndAnd => {
            vec![]
        }
        SyntaxKind::TokenArrow => vec![],
        SyntaxKind::TerminalArrow => {
            vec![]
        }
        SyntaxKind::TokenAt => vec![],
        SyntaxKind::TerminalAt => {
            vec![]
        }
        SyntaxKind::TokenBadCharacters => vec![],
        SyntaxKind::TerminalBadCharacters => {
            vec![]
        }
        SyntaxKind::TokenColon => vec![],
        SyntaxKind::TerminalColon => {
            vec![]
        }
        SyntaxKind::TokenColonColon => vec![],
        SyntaxKind::TerminalColonColon => {
            vec![]
        }
        SyntaxKind::TokenComma => vec![],
        SyntaxKind::TerminalComma => {
            vec![]
        }
        SyntaxKind::TokenDiv => vec![],
        SyntaxKind::TerminalDiv => {
            vec![]
        }
        SyntaxKind::TokenDivEq => vec![],
        SyntaxKind::TerminalDivEq => {
            vec![]
        }
        SyntaxKind::TokenDot => vec![],
        SyntaxKind::TerminalDot => {
            vec![]
        }
        SyntaxKind::TokenDotDot => vec![],
        SyntaxKind::TerminalDotDot => {
            vec![]
        }
        SyntaxKind::TokenDotDotEq => vec![],
        SyntaxKind::TerminalDotDotEq => {
            vec![]
        }
        SyntaxKind::TokenEndOfFile => vec![],
        SyntaxKind::TerminalEndOfFile => {
            vec![]
        }
        SyntaxKind::TokenEq => vec![],
        SyntaxKind::TerminalEq => {
            vec![]
        }
        SyntaxKind::TokenEqEq => vec![],
        SyntaxKind::TerminalEqEq => {
            vec![]
        }
        SyntaxKind::TokenGE => vec![],
        SyntaxKind::TerminalGE => {
            vec![]
        }
        SyntaxKind::TokenGT => vec![],
        SyntaxKind::TerminalGT => {
            vec![]
        }
        SyntaxKind::TokenHash => vec![],
        SyntaxKind::TerminalHash => {
            vec![]
        }
        SyntaxKind::TokenLBrace => vec![],
        SyntaxKind::TerminalLBrace => {
            vec![]
        }
        SyntaxKind::TokenLBrack => vec![],
        SyntaxKind::TerminalLBrack => {
            vec![]
        }
        SyntaxKind::TokenLE => vec![],
        SyntaxKind::TerminalLE => {
            vec![]
        }
        SyntaxKind::TokenLParen => vec![],
        SyntaxKind::TerminalLParen => {
            vec![]
        }
        SyntaxKind::TokenLT => vec![],
        SyntaxKind::TerminalLT => {
            vec![]
        }
        SyntaxKind::TokenMatchArrow => vec![],
        SyntaxKind::TerminalMatchArrow => {
            vec![]
        }
        SyntaxKind::TokenMinus => vec![],
        SyntaxKind::TerminalMinus => {
            vec![]
        }
        SyntaxKind::TokenMinusEq => vec![],
        SyntaxKind::TerminalMinusEq => {
            vec![]
        }
        SyntaxKind::TokenMod => vec![],
        SyntaxKind::TerminalMod => {
            vec![]
        }
        SyntaxKind::TokenModEq => vec![],
        SyntaxKind::TerminalModEq => {
            vec![]
        }
        SyntaxKind::TokenMul => vec![],
        SyntaxKind::TerminalMul => {
            vec![]
        }
        SyntaxKind::TokenMulEq => vec![],
        SyntaxKind::TerminalMulEq => {
            vec![]
        }
        SyntaxKind::TokenNeq => vec![],
        SyntaxKind::TerminalNeq => {
            vec![]
        }
        SyntaxKind::TokenNot => vec![],
        SyntaxKind::TerminalNot => {
            vec![]
        }
        SyntaxKind::TokenBitNot => vec![],
        SyntaxKind::TerminalBitNot => {
            vec![]
        }
        SyntaxKind::TokenOr => vec![],
        SyntaxKind::TerminalOr => {
            vec![]
        }
        SyntaxKind::TokenOrOr => vec![],
        SyntaxKind::TerminalOrOr => {
            vec![]
        }
        SyntaxKind::TokenPlus => vec![],
        SyntaxKind::TerminalPlus => {
            vec![]
        }
        SyntaxKind::TokenPlusEq => vec![],
        SyntaxKind::TerminalPlusEq => {
            vec![]
        }
        SyntaxKind::TokenQuestionMark => vec![],
        SyntaxKind::TerminalQuestionMark => {
            vec![]
        }
        SyntaxKind::TokenRBrace => vec![],
        SyntaxKind::TerminalRBrace => {
            vec![]
        }
        SyntaxKind::TokenRBrack => vec![],
        SyntaxKind::TerminalRBrack => {
            vec![]
        }
        SyntaxKind::TokenRParen => vec![],
        SyntaxKind::TerminalRParen => {
            vec![]
        }
        SyntaxKind::TokenSemicolon => vec![],
        SyntaxKind::TerminalSemicolon => {
            vec![]
        }
        SyntaxKind::TokenUnderscore => vec![],
        SyntaxKind::TerminalUnderscore => {
            vec![]
        }
        SyntaxKind::TokenXor => vec![],
        SyntaxKind::TerminalXor => {
            vec![]
        }
        SyntaxKind::SyntaxFile => {
            vec![]
        }
        SyntaxKind::TokenEmpty => vec![],
        SyntaxKind::TerminalEmpty => {
            vec![]
        }
        SyntaxKind::TokenSingleLineComment => vec![],
        SyntaxKind::TokenSingleLineInnerComment => vec![],
        SyntaxKind::TokenSingleLineDocComment => vec![],
        SyntaxKind::TokenWhitespace => vec![],
        SyntaxKind::TokenNewline => vec![],
        SyntaxKind::TokenMissing => vec![],
        SyntaxKind::TokenSkipped => vec![],
    }
}
