use diagnostics::DiagnosticEntry;
use filesystem::db::FilesGroup;
use filesystem::ids::FileId;
use filesystem::span::TextSpan;
use syntax::node::kind::SyntaxKind;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ParserDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
    pub kind: ParserDiagnosticKind,
}
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParserDiagnosticKind {
    // TODO(spapini): Add tokens from the recovery set to the message.
    SkippedElement { element_name: &'static str },
    MissingToken(SyntaxKind),
    MissingExpression,
    MissingPathSegment,
    MissingTypeClause,
    MissingTypeExpression,
}
impl DiagnosticEntry for ParserDiagnostic {
    type DbType = dyn FilesGroup;

    fn format(&self, _db: &dyn FilesGroup) -> String {
        match self.kind {
            // TODO(yuval): replace line breaks with "\n".
            ParserDiagnosticKind::SkippedElement { element_name } => {
                format!("Skipped tokens. Expected element: {element_name}.")
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
                "Missing tokens. Expected a type clause.".to_string()
            }
            ParserDiagnosticKind::MissingTypeExpression => {
                "Missing tokens. Expected a type expression.".to_string()
            }
        }
    }

    fn location(&self, _db: &dyn FilesGroup) -> diagnostics::DiagnosticLocation {
        diagnostics::DiagnosticLocation { file_id: self.file_id, span: self.span }
    }
}
