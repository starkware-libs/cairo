use diagnostics::DiagnosticLocation;
use filesystem::span::TextSpan;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;

use crate::db::DefsGroup;
use crate::ids::ModuleFileId;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StableLocation {
    pub module_file_id: ModuleFileId,
    stable_ptr: SyntaxStablePtrId,
}
impl StableLocation {
    pub fn new(module_file_id: ModuleFileId, stable_ptr: SyntaxStablePtrId) -> Self {
        Self { module_file_id, stable_ptr }
    }

    pub fn from_ast<TNode: TypedSyntaxNode>(module_file_id: ModuleFileId, node: &TNode) -> Self {
        Self { module_file_id, stable_ptr: node.as_syntax_node().stable_ptr() }
    }

    /// Returns the [DiagnosticLocation] that corresponds to the [StableLocation].
    pub fn diagnostic_location(&self, db: &(dyn DefsGroup + 'static)) -> DiagnosticLocation {
        let file_id =
            db.module_file(self.module_file_id).expect("Module in diagnostic does not exist");
        let syntax_node = db
            .file_syntax(file_id)
            .expect("File for diagnostic not found")
            .as_syntax_node()
            .lookup_ptr(db.upcast(), self.stable_ptr);
        DiagnosticLocation { file_id, span: syntax_node.span_without_trivia(db.upcast()) }
    }

    /// Returns the [DiagnosticLocation] that corresponds to the [StableLocation].
    pub fn diagnostic_location_until(
        &self,
        db: &(dyn DefsGroup + 'static),
        until_stable_ptr: SyntaxStablePtrId,
    ) -> DiagnosticLocation {
        let syntax_db = db.upcast();
        let file_id =
            db.module_file(self.module_file_id).expect("Module in diagnostic does not exist");
        let root_node =
            db.file_syntax(file_id).expect("File for diagnostic not found").as_syntax_node();
        let start =
            root_node.lookup_ptr(syntax_db, self.stable_ptr).span_start_without_trivia(syntax_db);
        let end =
            root_node.lookup_ptr(syntax_db, until_stable_ptr).span_end_without_trivia(syntax_db);
        DiagnosticLocation { file_id, span: TextSpan { start, end } }
    }
}
