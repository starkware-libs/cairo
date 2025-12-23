use cairo_lang_diagnostics::{DiagnosticEntry, error_code};
use cairo_lang_filesystem::ids::{FileId, SpanInFile};
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_syntax::node::kind::SyntaxKind;
use salsa::Database;

#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub struct ParserDiagnostic<'a> {
    pub file_id: FileId<'a>,
    pub span: TextSpan,
    pub kind: ParserDiagnosticKind,
}
impl<'a> ParserDiagnostic<'a> {
    /// Converts a `SyntaxKind` to its corresponding operator string.
    fn kind_to_string(&self, kind: SyntaxKind) -> String {
        format!(
            "'{}'",
            match kind {
                SyntaxKind::TerminalAnd => "&",
                SyntaxKind::TerminalAndAnd => "&&",
                SyntaxKind::TerminalArrow => "->",
                SyntaxKind::TerminalAs => "as",
                SyntaxKind::TerminalAt => "@",
                SyntaxKind::TerminalBitNot => "~",
                SyntaxKind::TerminalBreak => "break",
                SyntaxKind::TerminalColon => ":",
                SyntaxKind::TerminalColonColon => "::",
                SyntaxKind::TerminalComma => ",",
                SyntaxKind::TerminalConst => "const",
                SyntaxKind::TerminalContinue => "continue",
                SyntaxKind::TerminalDiv => "/",
                SyntaxKind::TerminalDivEq => "/=",
                SyntaxKind::TerminalDot => ".",
                SyntaxKind::TerminalDotDot => "..",
                SyntaxKind::TerminalDotDotEq => "..=",
                SyntaxKind::TerminalElse => "else",
                SyntaxKind::TerminalEnum => "enum",
                SyntaxKind::TerminalEq => "=",
                SyntaxKind::TerminalEqEq => "==",
                SyntaxKind::TerminalExtern => "extern",
                SyntaxKind::TerminalFalse => "false",
                SyntaxKind::TerminalFor => "for",
                SyntaxKind::TerminalFunction => "fn",
                SyntaxKind::TerminalGE => ">=",
                SyntaxKind::TerminalGT => ">",
                SyntaxKind::TerminalHash => "#",
                SyntaxKind::TerminalIf => "if",
                SyntaxKind::TerminalImpl => "impl",
                SyntaxKind::TerminalImplicits => "implicits",
                SyntaxKind::TerminalLBrace => "{",
                SyntaxKind::TerminalLBrack => "[",
                SyntaxKind::TerminalLE => "<=",
                SyntaxKind::TerminalLParen => "(",
                SyntaxKind::TerminalLT => "<",
                SyntaxKind::TerminalLet => "let",
                SyntaxKind::TerminalLoop => "loop",
                SyntaxKind::TerminalMatch => "match",
                SyntaxKind::TerminalMatchArrow => "=>",
                SyntaxKind::TerminalMinus => "-",
                SyntaxKind::TerminalMinusEq => "-=",
                SyntaxKind::TerminalMod => "%",
                SyntaxKind::TerminalModEq => "%=",
                SyntaxKind::TerminalModule => "mod",
                SyntaxKind::TerminalMul => "*",
                SyntaxKind::TerminalMulEq => "*=",
                SyntaxKind::TerminalMut => "mut",
                SyntaxKind::TerminalNeq => "!=",
                SyntaxKind::TerminalNoPanic => "nopanic",
                SyntaxKind::TerminalNot => "!",
                SyntaxKind::TerminalOf => "of",
                SyntaxKind::TerminalOr => "|",
                SyntaxKind::TerminalOrOr => "||",
                SyntaxKind::TerminalPlus => "+",
                SyntaxKind::TerminalPlusEq => "+=",
                SyntaxKind::TerminalPub => "pub",
                SyntaxKind::TerminalQuestionMark => "?",
                SyntaxKind::TerminalRBrace => "}",
                SyntaxKind::TerminalRBrack => "]",
                SyntaxKind::TerminalRParen => ")",
                SyntaxKind::TerminalRef => "ref",
                SyntaxKind::TerminalReturn => "return",
                SyntaxKind::TerminalSemicolon => ";",
                SyntaxKind::TerminalStruct => "struct",
                SyntaxKind::TerminalTrait => "trait",
                SyntaxKind::TerminalTrue => "true",
                SyntaxKind::TerminalType => "type",
                SyntaxKind::TerminalUnderscore => "_",
                SyntaxKind::TerminalUse => "use",
                SyntaxKind::TerminalWhile => "while",
                SyntaxKind::TerminalXor => "^",
                _ => return format!("{kind:?}"),
            }
        )
    }
}
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParserDiagnosticKind {
    // TODO(spapini): Add tokens from the recovery set to the message.
    SkippedElement { element_name: String },
    MissingToken(SyntaxKind),
    MissingExpression,
    MissingPathSegment,
    MissingTypeClause,
    MissingTypeExpression,
    MissingWrappedArgList,
    MissingPattern,
    MissingMacroRuleParamKind,
    InvalidParamKindInMacroExpansion,
    InvalidParamKindInMacroRule,
    ExpectedInToken,
    ItemInlineMacroWithoutBang { identifier: String, bracket_type: SyntaxKind },
    ReservedIdentifier { identifier: String },
    UnderscoreNotAllowedAsIdentifier,
    MissingLiteralSuffix,
    InvalidNumericLiteralValue,
    IllegalStringEscaping,
    ShortStringMustBeAscii,
    StringMustBeAscii,
    UnterminatedShortString,
    UnterminatedString,
    VisibilityWithoutItem,
    AttributesWithoutItem,
    AttributesWithoutTraitItem,
    AttributesWithoutImplItem,
    AttributesWithoutStatement,
    DisallowedTrailingSeparatorOr,
    ConsecutiveMathOperators { first_op: SyntaxKind, second_op: SyntaxKind },
    ExpectedSemicolonOrBody,
    LowPrecedenceOperatorInIfLet { op: SyntaxKind },
}

impl<'a> DiagnosticEntry<'a> for ParserDiagnostic<'a> {
    type Kind = ParserDiagnosticKind;
    fn format(&self, _db: &'a dyn Database) -> String {
        match &self.kind {
            Self::Kind::InvalidParamKindInMacroExpansion => {
                "Parameter kinds are not allowed in macro expansion.".to_string()
            }
            Self::Kind::InvalidParamKindInMacroRule => {
                "Macro parameter must have a kind.".to_string()
            }
            Self::Kind::SkippedElement { element_name } => {
                format!("Skipped tokens. Expected: {element_name}.")
            }
            Self::Kind::MissingToken(kind) => {
                format!("Missing token {}.", self.kind_to_string(*kind))
            }
            Self::Kind::MissingExpression => "Missing tokens. Expected an expression.".to_string(),
            Self::Kind::MissingPathSegment => {
                "Missing tokens. Expected a path segment.".to_string()
            }
            Self::Kind::MissingTypeClause => {
                "Unexpected token, expected ':' followed by a type.".to_string()
            }
            Self::Kind::MissingTypeExpression => {
                "Missing tokens. Expected a type expression.".to_string()
            }
            Self::Kind::MissingWrappedArgList => "Missing tokens. Expected an argument list \
                                                  wrapped in either parentheses, brackets, or \
                                                  braces."
                .to_string(),
            Self::Kind::MissingPattern => "Missing tokens. Expected a pattern.".to_string(),
            Self::Kind::MissingMacroRuleParamKind => {
                "Missing tokens. Expected a macro rule parameter kind.".to_string()
            }
            Self::Kind::ExpectedInToken => "Missing identifier token, expected 'in'.".to_string(),
            Self::Kind::ItemInlineMacroWithoutBang { identifier, bracket_type } => {
                let (left, right) = match bracket_type {
                    SyntaxKind::TerminalLParen => ("(", ")"),
                    SyntaxKind::TerminalLBrack => ("[", "]"),
                    SyntaxKind::TerminalLBrace => ("{", "}"),
                    _ => ("", ""),
                };
                format!(
                    "Expected a '!' after the identifier '{identifier}' to start an inline macro.
Did you mean to write `{identifier}!{left}...{right}'?",
                )
            }
            Self::Kind::ReservedIdentifier { identifier } => {
                format!("'{identifier}' is a reserved identifier.")
            }
            Self::Kind::UnderscoreNotAllowedAsIdentifier => {
                "An underscore ('_') is not allowed as an identifier in this context.".to_string()
            }
            Self::Kind::MissingLiteralSuffix => "Missing literal suffix.".to_string(),
            Self::Kind::InvalidNumericLiteralValue => "Literal is not a valid number.".to_string(),
            Self::Kind::IllegalStringEscaping => "Invalid string escaping.".to_string(),
            Self::Kind::ShortStringMustBeAscii => {
                "Short string literals can only include ASCII characters.".into()
            }
            Self::Kind::StringMustBeAscii => {
                "String literals can only include ASCII characters.".into()
            }
            Self::Kind::UnterminatedShortString => "Unterminated short string literal.".into(),
            Self::Kind::UnterminatedString => "Unterminated string literal.".into(),
            Self::Kind::VisibilityWithoutItem => {
                "Missing tokens. Expected an item after visibility.".to_string()
            }
            Self::Kind::AttributesWithoutItem => {
                "Missing tokens. Expected an item after attributes.".to_string()
            }
            Self::Kind::AttributesWithoutTraitItem => {
                "Missing tokens. Expected a trait item after attributes.".to_string()
            }
            Self::Kind::AttributesWithoutImplItem => {
                "Missing tokens. Expected an impl item after attributes.".to_string()
            }
            Self::Kind::AttributesWithoutStatement => {
                "Missing tokens. Expected a statement after attributes.".to_string()
            }
            Self::Kind::DisallowedTrailingSeparatorOr => {
                "A trailing `|` is not allowed in an or-pattern.".to_string()
            }
            Self::Kind::ConsecutiveMathOperators { first_op, second_op } => {
                format!(
                    "Consecutive comparison operators are not allowed: {} followed by {}",
                    self.kind_to_string(*first_op),
                    self.kind_to_string(*second_op)
                )
            }
            Self::Kind::ExpectedSemicolonOrBody => "Expected either ';' or '{' after module name. \
                                                    Use ';' for an external module declaration or \
                                                    '{' for a module with a body."
                .to_string(),
            Self::Kind::LowPrecedenceOperatorInIfLet { op } => {
                format!(
                    "Operator {} is not allowed in let chains. Consider wrapping the expression \
                     in parentheses.",
                    self.kind_to_string(*op)
                )
            }
        }
    }

    fn location(&self, _db: &'a dyn Database) -> SpanInFile<'a> {
        SpanInFile { file_id: self.file_id, span: self.span }
    }

    fn error_code(&self) -> Option<cairo_lang_diagnostics::ErrorCode> {
        Some(match &self.kind {
            Self::Kind::SkippedElement { .. } => error_code!(E1000),
            Self::Kind::MissingToken(_) => error_code!(E1001),
            Self::Kind::MissingExpression => error_code!(E1002),
            Self::Kind::MissingPathSegment => error_code!(E1003),
            Self::Kind::MissingTypeClause => error_code!(E1004),
            Self::Kind::MissingTypeExpression => error_code!(E1005),
            Self::Kind::MissingWrappedArgList => error_code!(E1006),
            Self::Kind::MissingPattern => error_code!(E1007),
            Self::Kind::MissingMacroRuleParamKind => error_code!(E1008),
            Self::Kind::InvalidParamKindInMacroExpansion => error_code!(E1009),
            Self::Kind::InvalidParamKindInMacroRule => error_code!(E1010),
            Self::Kind::ExpectedInToken => error_code!(E1011),
            Self::Kind::ItemInlineMacroWithoutBang { .. } => error_code!(E1012),
            Self::Kind::ReservedIdentifier { .. } => error_code!(E1013),
            Self::Kind::UnderscoreNotAllowedAsIdentifier => error_code!(E1014),
            Self::Kind::MissingLiteralSuffix => error_code!(E1015),
            Self::Kind::InvalidNumericLiteralValue => error_code!(E1016),
            Self::Kind::IllegalStringEscaping => error_code!(E1017),
            Self::Kind::ShortStringMustBeAscii => error_code!(E1018),
            Self::Kind::StringMustBeAscii => error_code!(E1019),
            Self::Kind::UnterminatedShortString => error_code!(E1020),
            Self::Kind::UnterminatedString => error_code!(E1021),
            Self::Kind::VisibilityWithoutItem => error_code!(E1022),
            Self::Kind::AttributesWithoutItem => error_code!(E1023),
            Self::Kind::AttributesWithoutTraitItem => error_code!(E1024),
            Self::Kind::AttributesWithoutImplItem => error_code!(E1025),
            Self::Kind::AttributesWithoutStatement => error_code!(E1026),
            Self::Kind::DisallowedTrailingSeparatorOr => error_code!(E1027),
            Self::Kind::ConsecutiveMathOperators { .. } => error_code!(E1028),
            Self::Kind::ExpectedSemicolonOrBody => error_code!(E1029),
            Self::Kind::LowPrecedenceOperatorInIfLet { .. } => error_code!(E1030),
        })
    }
}
