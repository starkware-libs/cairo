use diagnostics::DiagnosticEntry;
use filesystem::db::FilesGroup;
use filesystem::ids::FileId;
use filesystem::span::TextSpan;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ParserDiagnostic {
    pub file_id: FileId,
    pub span: TextSpan,
    pub kind: ParserDiagnosticKind,
}
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParserDiagnosticKind {
    SkippedTokens,
    MissingExpression,
    MissingBlock,
    MissingPath,
}
impl DiagnosticEntry for ParserDiagnostic {
    type DbType = dyn FilesGroup;

    fn format(&self, _db: &dyn FilesGroup) -> String {
        match self.kind {
            // TODO(yuval): replace line breaks with "\n".
            ParserDiagnosticKind::SkippedTokens => "Skipped tokens.".to_string(),
            ParserDiagnosticKind::MissingExpression => {
                "Missing tokens: 'missing expression'.".to_string()
            }
            ParserDiagnosticKind::MissingBlock => "Missing tokens: 'missing block'.".to_string(),
            ParserDiagnosticKind::MissingPath => "Missing tokens: 'missing path'.".to_string(),
        }
    }

    fn location(&self, _db: &dyn FilesGroup) -> diagnostics::DiagnosticLocation {
        diagnostics::DiagnosticLocation { file_id: self.file_id, span: self.span }
    }
}
