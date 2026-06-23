use cairo_lang_diagnostics::{DiagnosticEntry, error_code};
use cairo_lang_filesystem::ids::{FileId, SpanInFile};
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_syntax::node::kind::LexemeKind;
use salsa::Database;

#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub struct ParserDiagnostic<'a> {
    pub file_id: FileId<'a>,
    pub span: TextSpan,
    pub kind: ParserDiagnosticKind,
}
impl<'a> ParserDiagnostic<'a> {
    /// Converts a `LexemeKind` to its corresponding operator string.
    fn kind_to_string(&self, kind: LexemeKind) -> String {
        format!(
            "'{}'",
            match kind {
                LexemeKind::And => "&",
                LexemeKind::AndAnd => "&&",
                LexemeKind::Arrow => "->",
                LexemeKind::As => "as",
                LexemeKind::At => "@",
                LexemeKind::BitNot => "~",
                LexemeKind::Break => "break",
                LexemeKind::Colon => ":",
                LexemeKind::ColonColon => "::",
                LexemeKind::Comma => ",",
                LexemeKind::Const => "const",
                LexemeKind::Continue => "continue",
                LexemeKind::Div => "/",
                LexemeKind::DivEq => "/=",
                LexemeKind::Dot => ".",
                LexemeKind::DotDot => "..",
                LexemeKind::DotDotEq => "..=",
                LexemeKind::Else => "else",
                LexemeKind::Enum => "enum",
                LexemeKind::Eq => "=",
                LexemeKind::EqEq => "==",
                LexemeKind::Extern => "extern",
                LexemeKind::False => "false",
                LexemeKind::For => "for",
                LexemeKind::Function => "fn",
                LexemeKind::GE => ">=",
                LexemeKind::GT => ">",
                LexemeKind::Hash => "#",
                LexemeKind::If => "if",
                LexemeKind::Impl => "impl",
                LexemeKind::Implicits => "implicits",
                LexemeKind::LBrace => "{",
                LexemeKind::LBrack => "[",
                LexemeKind::LE => "<=",
                LexemeKind::LParen => "(",
                LexemeKind::LT => "<",
                LexemeKind::Let => "let",
                LexemeKind::Loop => "loop",
                LexemeKind::Match => "match",
                LexemeKind::MatchArrow => "=>",
                LexemeKind::Minus => "-",
                LexemeKind::MinusEq => "-=",
                LexemeKind::Mod => "%",
                LexemeKind::ModEq => "%=",
                LexemeKind::Module => "mod",
                LexemeKind::Mul => "*",
                LexemeKind::MulEq => "*=",
                LexemeKind::Mut => "mut",
                LexemeKind::Neq => "!=",
                LexemeKind::NoPanic => "nopanic",
                LexemeKind::Not => "!",
                LexemeKind::Of => "of",
                LexemeKind::Or => "|",
                LexemeKind::OrOr => "||",
                LexemeKind::Plus => "+",
                LexemeKind::PlusEq => "+=",
                LexemeKind::Pub => "pub",
                LexemeKind::QuestionMark => "?",
                LexemeKind::RBrace => "}",
                LexemeKind::RBrack => "]",
                LexemeKind::RParen => ")",
                LexemeKind::Ref => "ref",
                LexemeKind::Return => "return",
                LexemeKind::Semicolon => ";",
                LexemeKind::Struct => "struct",
                LexemeKind::Trait => "trait",
                LexemeKind::True => "true",
                LexemeKind::Type => "type",
                LexemeKind::Underscore => "_",
                LexemeKind::Use => "use",
                LexemeKind::While => "while",
                LexemeKind::Xor => "^",
                _ => return format!("{kind:?}"),
            }
        )
    }
}
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParserDiagnosticKind {
    // TODO(spapini): Add tokens from the recovery set to the message.
    SkippedElement { element_name: String },
    MissingToken(LexemeKind),
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
    ItemInlineMacroWithoutBang { identifier: String, bracket_type: LexemeKind },
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
    ConsecutiveMathOperators { first_op: LexemeKind, second_op: LexemeKind },
    ExpectedSemicolonOrBody,
    LowPrecedenceOperatorInIfLet { op: LexemeKind },
    MissingMacroRepetitionOperator,
}

impl<'a> DiagnosticEntry<'a> for ParserDiagnostic<'a> {
    fn format(&self, _db: &'a dyn Database) -> String {
        match &self.kind {
            ParserDiagnosticKind::InvalidParamKindInMacroExpansion => {
                "Parameter kinds are not allowed in macro expansion.".to_string()
            }
            ParserDiagnosticKind::InvalidParamKindInMacroRule => {
                "Macro parameter must have a kind.".to_string()
            }
            ParserDiagnosticKind::SkippedElement { element_name } => {
                format!("Skipped tokens. Expected: {element_name}.")
            }
            ParserDiagnosticKind::MissingToken(kind) => {
                format!("Missing token {}.", self.kind_to_string(*kind))
            }
            ParserDiagnosticKind::MissingExpression => {
                "Missing tokens. Expected an expression.".to_string()
            }
            ParserDiagnosticKind::MissingPathSegment => {
                "Missing tokens. Expected a path segment.".to_string()
            }
            ParserDiagnosticKind::MissingTypeClause => {
                "Unexpected token, expected ':' followed by a type.".to_string()
            }
            ParserDiagnosticKind::MissingTypeExpression => {
                "Missing tokens. Expected a type expression.".to_string()
            }
            ParserDiagnosticKind::MissingWrappedArgList => "Missing tokens. Expected an argument \
                                                            list wrapped in either parentheses, \
                                                            brackets, or braces."
                .to_string(),
            ParserDiagnosticKind::MissingPattern => {
                "Missing tokens. Expected a pattern.".to_string()
            }
            ParserDiagnosticKind::MissingMacroRuleParamKind => {
                "Missing tokens. Expected a macro rule parameter kind.".to_string()
            }
            ParserDiagnosticKind::ExpectedInToken => {
                "Missing identifier token, expected 'in'.".to_string()
            }
            ParserDiagnosticKind::ItemInlineMacroWithoutBang { identifier, bracket_type } => {
                let (left, right) = match bracket_type {
                    LexemeKind::LParen => ("(", ")"),
                    LexemeKind::LBrack => ("[", "]"),
                    LexemeKind::LBrace => ("{", "}"),
                    _ => ("", ""),
                };
                format!(
                    "Expected a '!' after the identifier '{identifier}' to start an inline macro.
Did you mean to write `{identifier}!{left}...{right}'?",
                )
            }
            ParserDiagnosticKind::ReservedIdentifier { identifier } => {
                format!("'{identifier}' is a reserved identifier.")
            }
            ParserDiagnosticKind::UnderscoreNotAllowedAsIdentifier => {
                "An underscore ('_') is not allowed as an identifier in this context.".to_string()
            }
            ParserDiagnosticKind::MissingLiteralSuffix => "Missing literal suffix.".to_string(),
            ParserDiagnosticKind::InvalidNumericLiteralValue => {
                "Literal is not a valid number.".to_string()
            }
            ParserDiagnosticKind::IllegalStringEscaping => "Invalid string escaping.".to_string(),
            ParserDiagnosticKind::ShortStringMustBeAscii => {
                "Short string literals can only include ASCII characters.".into()
            }
            ParserDiagnosticKind::StringMustBeAscii => {
                "String literals can only include ASCII characters.".into()
            }
            ParserDiagnosticKind::UnterminatedShortString => {
                "Unterminated short string literal.".into()
            }
            ParserDiagnosticKind::UnterminatedString => "Unterminated string literal.".into(),
            ParserDiagnosticKind::VisibilityWithoutItem => {
                "Missing tokens. Expected an item after visibility.".to_string()
            }
            ParserDiagnosticKind::AttributesWithoutItem => {
                "Missing tokens. Expected an item after attributes.".to_string()
            }
            ParserDiagnosticKind::AttributesWithoutTraitItem => {
                "Missing tokens. Expected a trait item after attributes.".to_string()
            }
            ParserDiagnosticKind::AttributesWithoutImplItem => {
                "Missing tokens. Expected an impl item after attributes.".to_string()
            }
            ParserDiagnosticKind::AttributesWithoutStatement => {
                "Missing tokens. Expected a statement after attributes.".to_string()
            }
            ParserDiagnosticKind::DisallowedTrailingSeparatorOr => {
                "A trailing `|` is not allowed in an or-pattern.".to_string()
            }
            ParserDiagnosticKind::ConsecutiveMathOperators { first_op, second_op } => {
                format!(
                    "Consecutive comparison operators are not allowed: {} followed by {}",
                    self.kind_to_string(*first_op),
                    self.kind_to_string(*second_op)
                )
            }
            ParserDiagnosticKind::ExpectedSemicolonOrBody => {
                "Expected either ';' or '{' after module name. Use ';' for an external module \
                 declaration or '{' for a module with a body."
                    .to_string()
            }
            ParserDiagnosticKind::LowPrecedenceOperatorInIfLet { op } => {
                format!(
                    "Operator {} is not allowed in let chains. Consider wrapping the expression \
                     in parentheses.",
                    self.kind_to_string(*op)
                )
            }
            ParserDiagnosticKind::MissingMacroRepetitionOperator => {
                "Missing macro repetition operator. Expected `?`, `+`, or `*` after `$(...)`."
                    .to_string()
            }
        }
    }

    fn location(&self, _db: &'a dyn Database) -> SpanInFile<'a> {
        SpanInFile { file_id: self.file_id, span: self.span }
    }

    fn error_code(&self) -> Option<cairo_lang_diagnostics::ErrorCode> {
        Some(match &self.kind {
            ParserDiagnosticKind::SkippedElement { .. } => error_code!(E1000),
            ParserDiagnosticKind::MissingToken(_) => error_code!(E1001),
            ParserDiagnosticKind::MissingExpression => error_code!(E1002),
            ParserDiagnosticKind::MissingPathSegment => error_code!(E1003),
            ParserDiagnosticKind::MissingTypeClause => error_code!(E1004),
            ParserDiagnosticKind::MissingTypeExpression => error_code!(E1005),
            ParserDiagnosticKind::MissingWrappedArgList => error_code!(E1006),
            ParserDiagnosticKind::MissingPattern => error_code!(E1007),
            ParserDiagnosticKind::MissingMacroRuleParamKind => error_code!(E1008),
            ParserDiagnosticKind::InvalidParamKindInMacroExpansion => error_code!(E1009),
            ParserDiagnosticKind::InvalidParamKindInMacroRule => error_code!(E1010),
            ParserDiagnosticKind::ExpectedInToken => error_code!(E1011),
            ParserDiagnosticKind::ItemInlineMacroWithoutBang { .. } => error_code!(E1012),
            ParserDiagnosticKind::ReservedIdentifier { .. } => error_code!(E1013),
            ParserDiagnosticKind::UnderscoreNotAllowedAsIdentifier => error_code!(E1014),
            ParserDiagnosticKind::MissingLiteralSuffix => error_code!(E1015),
            ParserDiagnosticKind::InvalidNumericLiteralValue => error_code!(E1016),
            ParserDiagnosticKind::IllegalStringEscaping => error_code!(E1017),
            ParserDiagnosticKind::ShortStringMustBeAscii => error_code!(E1018),
            ParserDiagnosticKind::StringMustBeAscii => error_code!(E1019),
            ParserDiagnosticKind::UnterminatedShortString => error_code!(E1020),
            ParserDiagnosticKind::UnterminatedString => error_code!(E1021),
            ParserDiagnosticKind::VisibilityWithoutItem => error_code!(E1022),
            ParserDiagnosticKind::AttributesWithoutItem => error_code!(E1023),
            ParserDiagnosticKind::AttributesWithoutTraitItem => error_code!(E1024),
            ParserDiagnosticKind::AttributesWithoutImplItem => error_code!(E1025),
            ParserDiagnosticKind::AttributesWithoutStatement => error_code!(E1026),
            ParserDiagnosticKind::DisallowedTrailingSeparatorOr => error_code!(E1027),
            ParserDiagnosticKind::ConsecutiveMathOperators { .. } => error_code!(E1028),
            ParserDiagnosticKind::ExpectedSemicolonOrBody => error_code!(E1029),
            ParserDiagnosticKind::LowPrecedenceOperatorInIfLet { .. } => error_code!(E1030),
            ParserDiagnosticKind::MissingMacroRepetitionOperator => error_code!(E1031),
        })
    }

    fn is_same_kind(&self, other: &Self) -> bool {
        other.kind == self.kind
    }
}
