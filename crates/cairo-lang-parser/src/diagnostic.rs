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
    Missing { kind: ParserDiagnosticKindMissing, parsing_context: SmolStr },
    ReservedIdentifier { identifier: SmolStr },
    UnderscoreNotAllowedAsIdentifier,
    InvalidNumericLiteralValue,
    IllegalStringEscaping,
    ShortStringMustBeAscii,
    StringMustBeAscii,
    UnterminatedShortString,
    UnterminatedString,
}
impl DiagnosticEntry for ParserDiagnostic {
    type DbType = dyn FilesGroup;

    fn format(&self, _db: &dyn FilesGroup) -> String {
        match &self.kind {
            ParserDiagnosticKind::SkippedElement { element_name } => {
                format!("Skipped tokens. Expected: {element_name}.")
            }
            ParserDiagnosticKind::Missing { kind, parsing_context } => {
                format!("{} while parsing {parsing_context}.", kind.error_message())
            }
            ParserDiagnosticKind::ReservedIdentifier { identifier } => {
                format!("'{identifier}' is a reserved identifier.")
            }
            ParserDiagnosticKind::UnderscoreNotAllowedAsIdentifier => {
                "An underscore ('_') is not allowed as an identifier in this context.".to_string()
            }
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
        }
    }

    fn location(&self, _db: &dyn FilesGroup) -> cairo_lang_diagnostics::DiagnosticLocation {
        cairo_lang_diagnostics::DiagnosticLocation { file_id: self.file_id, span: self.span }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParserDiagnosticKindMissing {
    MissingToken(SyntaxKind),
    MissingExpression,
    MissingPathSegment,
    MissingTypeClause,
    MissingTypeExpression,
    MissingLiteralSuffix,
    AttributesWithoutItem,
    AttributesWithoutTraitItem,
    AttributesWithoutImplItem,
}
impl ParserDiagnosticKindMissing {
    fn error_message(&self) -> String {
        match &self {
            ParserDiagnosticKindMissing::MissingToken(kind) => {
                format!("Missing token {kind:?}")
            }
            ParserDiagnosticKindMissing::MissingExpression => {
                "Missing tokens. Expected an expression".to_string()
            }
            ParserDiagnosticKindMissing::MissingPathSegment => {
                "Missing tokens. Expected a path segment".to_string()
            }
            ParserDiagnosticKindMissing::MissingTypeClause => {
                "Missing tokens. Expected a type clause (':' followed by a type)".to_string()
            }
            ParserDiagnosticKindMissing::MissingTypeExpression => {
                "Missing tokens. Expected a type expression".to_string()
            }
            ParserDiagnosticKindMissing::MissingLiteralSuffix => {
                "Missing tokens. Expected a literal suffix".to_string()
            }
            ParserDiagnosticKindMissing::AttributesWithoutItem => {
                "Missing tokens. Expected an item after attributes".to_string()
            }
            ParserDiagnosticKindMissing::AttributesWithoutTraitItem => {
                "Missing tokens. Expected a trait item after attributes".to_string()
            }
            ParserDiagnosticKindMissing::AttributesWithoutImplItem => {
                "Missing tokens. Expected an impl item after attributes".to_string()
            }
        }
    }
}
