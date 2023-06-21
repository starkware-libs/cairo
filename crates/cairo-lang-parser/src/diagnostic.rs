use cairo_lang_diagnostics::DiagnosticEntry;
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
    ReservedIdentifier { identifier: SmolStr },
    UnderscoreNotAllowedAsIdentifier,
    MissingLiteralSuffix,
    InvalidNumericLiteralValue,
    IllegalStringEscaping,
    ShortStringMustBeAscii,
    StringMustBeAscii,
    UnterminatedShortString,
    UnterminatedString,
    AttributesWithoutItem,
    AttributesWithoutTraitItem,
    AttributesWithoutImplItem,
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
                "Short strings can only include ASCII characters.".into()
            }
            ParserDiagnosticKind::StringMustBeAscii => {
                "Strings can only include ASCII characters.".into()
            }
            ParserDiagnosticKind::UnterminatedShortString => {
                "Unterminated short string literal.".into()
            }
            ParserDiagnosticKind::UnterminatedString => "Unterminated string literal.".into(),
            ParserDiagnosticKind::AttributesWithoutItem => {
                "Missing tokens. Expected an item after attributes.".to_string()
            }
            ParserDiagnosticKind::AttributesWithoutTraitItem => {
                "Missing tokens. Expected a trait item after attributes.".to_string()
            }
            ParserDiagnosticKind::AttributesWithoutImplItem => {
                "Missing tokens. Expected an impl item after attributes.".to_string()
            }
        }
    }

    fn location(&self, _db: &dyn FilesGroup) -> cairo_lang_diagnostics::DiagnosticLocation {
        cairo_lang_diagnostics::DiagnosticLocation { file_id: self.file_id, span: self.span }
    }
}
