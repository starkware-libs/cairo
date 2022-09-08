use defs::ids::ModuleId;
use diagnostics::{DiagnosticEntry, DiagnosticLocation};
use parser::ParserDiagnostic;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;

use crate::db::SemanticGroup;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Diagnostic {
    Semantic(SemanticDiagnostic),
    Parser(ParserDiagnostic),
}
impl DiagnosticEntry for Diagnostic {
    type DbType = dyn SemanticGroup;

    fn format(&self, db: &Self::DbType) -> String {
        match self {
            Diagnostic::Semantic(diagnostic) => diagnostic.format(db),
            Diagnostic::Parser(diagnostic) => diagnostic.format(db.as_files_group()),
        }
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        match self {
            Diagnostic::Semantic(diagnostic) => diagnostic.location(db),
            Diagnostic::Parser(diagnostic) => diagnostic.location(db.as_files_group()),
        }
    }
}
impl From<ParserDiagnostic> for Diagnostic {
    fn from(diagnostic: ParserDiagnostic) -> Self {
        Self::Parser(diagnostic)
    }
}
impl From<SemanticDiagnostic> for Diagnostic {
    fn from(diagnostic: SemanticDiagnostic) -> Self {
        Self::Semantic(diagnostic)
    }
}

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
            .value
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
