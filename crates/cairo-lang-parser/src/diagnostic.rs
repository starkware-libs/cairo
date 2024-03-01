use cairo_lang_diagnostics::{error_code, DiagnosticEntry, ErrorCode};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_syntax::node::kind::SyntaxKind;
use smol_str::SmolStr;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ParserDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
    pub kind: ParserDiagnosticKind,
}
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParserDiagnosticKind {
    // TODO(spapini): Add tokens from the recovery set to the message.
    SkippedElement { element_name: SmolStr },
    MissingToken(SyntaxKind),
    MissingExpression,
    MissingPathSegment,
    MissingTypeClause,
    MissingTypeExpression,
    MissingWrappedArgList,
    MissingPatteren,
    ItemInlineMacroWithoutBang { identifier: SmolStr, bracket_type: SyntaxKind },
    ReservedIdentifier { identifier: SmolStr },
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
}
impl DiagnosticEntry for ParserDiagnostic {
    type DbType = dyn FilesGroup;

    fn format(&self, _db: &dyn FilesGroup) -> String {
        match &self.kind {
            ParserDiagnosticKind::SkippedElement { element_name } => {
                format!("Skipped tokens. Expected: {element_name}.")
            }
            ParserDiagnosticKind::MissingToken(kind) => {
                format!("Missing token {kind:?}.")
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
            ParserDiagnosticKind::MissingPatteren => {
                "Missing tokens. Expected a pattern.".to_string()
            }
            ParserDiagnosticKind::ItemInlineMacroWithoutBang { identifier, bracket_type } => {
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
        }
    }

    fn location(&self, _db: &dyn FilesGroup) -> cairo_lang_diagnostics::DiagnosticLocation {
        cairo_lang_diagnostics::DiagnosticLocation { file_id: self.file_id, span: self.span }
    }

    fn error_code(&self) -> Option<ErrorCode> {
        Some(match &self.kind {
            ParserDiagnosticKind::SkippedElement { .. } => error_code!(E0002),
            ParserDiagnosticKind::MissingToken(_) => error_code!(E0001),
            ParserDiagnosticKind::MissingExpression => error_code!(E0001),
            ParserDiagnosticKind::MissingPathSegment => error_code!(E0001),
            ParserDiagnosticKind::MissingTypeClause => error_code!(E0001),
            ParserDiagnosticKind::MissingTypeExpression => error_code!(E0001),
            ParserDiagnosticKind::MissingWrappedArgList => error_code!(E0001),
            ParserDiagnosticKind::MissingPatteren => error_code!(E0001),
            ParserDiagnosticKind::ItemInlineMacroWithoutBang { .. } => error_code!(E0001),
            ParserDiagnosticKind::ReservedIdentifier { .. } => error_code!(E0003),
            ParserDiagnosticKind::UnderscoreNotAllowedAsIdentifier => error_code!(E0004),
            ParserDiagnosticKind::MissingLiteralSuffix => error_code!(E0005),
            ParserDiagnosticKind::InvalidNumericLiteralValue => error_code!(E0006),
            ParserDiagnosticKind::IllegalStringEscaping => error_code!(E0007),
            ParserDiagnosticKind::ShortStringMustBeAscii => error_code!(E0008),
            ParserDiagnosticKind::StringMustBeAscii => error_code!(E0008),
            ParserDiagnosticKind::UnterminatedShortString => error_code!(E0001),
            ParserDiagnosticKind::UnterminatedString => error_code!(E0001),
            ParserDiagnosticKind::VisibilityWithoutItem => error_code!(E0001),
            ParserDiagnosticKind::AttributesWithoutItem => error_code!(E0001),
            ParserDiagnosticKind::AttributesWithoutTraitItem => error_code!(E0001),
            ParserDiagnosticKind::AttributesWithoutImplItem => error_code!(E0001),
            ParserDiagnosticKind::AttributesWithoutStatement => error_code!(E0001),
            ParserDiagnosticKind::DisallowedTrailingSeparatorOr => error_code!(E0001),
        })
    }
}
