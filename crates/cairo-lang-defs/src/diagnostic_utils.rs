use cairo_lang_diagnostics::DiagnosticLocation;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};

use crate::db::DefsGroup;

/// A stable location of a real, concrete syntax.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct StableLocation {
    pub file_id: FileId,
    pub stable_ptr: SyntaxStablePtrId,
}
impl StableLocation {
    pub fn new(file_id: FileId, stable_ptr: SyntaxStablePtrId) -> Self {
        Self { file_id, stable_ptr }
    }

    pub fn from_ast<TNode: TypedSyntaxNode>(file_id: FileId, node: &TNode) -> Self {
        Self { file_id, stable_ptr: node.as_syntax_node().stable_ptr() }
    }

    /// Returns the [SyntaxNode] that corresponds to the [StableLocation].
    pub fn syntax_node(&self, db: &dyn DefsGroup) -> SyntaxNode {
        db.file_syntax(self.file_id)
            .expect("File for diagnostic not found")
            .lookup_ptr(db.upcast(), self.stable_ptr)
    }

    /// Returns the [DiagnosticLocation] that corresponds to the [StableLocation].
    pub fn diagnostic_location(&self, db: &dyn DefsGroup) -> DiagnosticLocation {
        let syntax_node = db
            .file_syntax(self.file_id)
            .expect("File for diagnostic not found")
            .lookup_ptr(db.upcast(), self.stable_ptr);
        DiagnosticLocation {
            file_id: self.file_id,
            span: syntax_node.span_without_trivia(db.upcast()),
        }
    }

    /// Returns the [DiagnosticLocation] that corresponds to the [StableLocation].
    pub fn diagnostic_location_until(
        &self,
        db: &dyn DefsGroup,
        until_stable_ptr: SyntaxStablePtrId,
    ) -> DiagnosticLocation {
        let syntax_db = db.upcast();
        let root_node = db.file_syntax(self.file_id).expect("File for diagnostic not found");
        let start =
            root_node.lookup_ptr(syntax_db, self.stable_ptr).span_start_without_trivia(syntax_db);
        let end =
            root_node.lookup_ptr(syntax_db, until_stable_ptr).span_end_without_trivia(syntax_db);
        DiagnosticLocation { file_id: self.file_id, span: TextSpan { start, end } }
    }
}
