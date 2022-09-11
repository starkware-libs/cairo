#[cfg(test)]
#[path = "diagnostic_test.rs"]
mod test;

use defs::ids::ModuleId;
use diagnostics::{DiagnosticEntry, DiagnosticLocation};
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;

use crate::db::SemanticGroup;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SemanticDiagnostic {
    pub module_id: ModuleId,
    pub stable_ptr: SyntaxStablePtrId,
    pub kind: SemanticDiagnosticKind,
}
impl DiagnosticEntry for SemanticDiagnostic {
    type DbType = dyn SemanticGroup;

    fn format(&self, _db: &Self::DbType) -> String {
        match self.kind {
            SemanticDiagnosticKind::UnknownBinaryOperator => "Unknown binary operator",
            SemanticDiagnosticKind::UnknownFunction => "Unknown function",
        }
        .into()
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        let file_id = db.module_file(self.module_id).expect("Module in diagnostic does not exist");
        let syntax_node = db
            .file_syntax(file_id)
            // There may be syntax errors in the file, which we can safely ignore here.
            .expect("File for diagnostic not found")
            .as_syntax_node()
            .lookup_ptr(db.as_syntax_group(), self.stable_ptr);
        DiagnosticLocation { file_id, span: syntax_node.span(db.as_syntax_group()) }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SemanticDiagnosticKind {
    UnknownBinaryOperator,
    UnknownFunction,
}
