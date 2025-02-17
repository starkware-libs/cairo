use std::fmt;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::DiagnosticLocation;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};

use crate::db::DefsGroup;

/// A stable location of a real, concrete syntax.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct StableLocation(SyntaxStablePtrId);
impl StableLocation {
    pub fn new(stable_ptr: SyntaxStablePtrId) -> Self {
        Self(stable_ptr)
    }

    pub fn file_id(&self, db: &dyn DefsGroup) -> FileId {
        self.0.file_id(db.upcast())
    }

    pub fn from_ast<TNode: TypedSyntaxNode>(node: &TNode) -> Self {
        Self(node.as_syntax_node().stable_ptr())
    }

    /// Returns the [SyntaxNode] that corresponds to the [StableLocation].
    pub fn syntax_node(&self, db: &dyn DefsGroup) -> SyntaxNode {
        self.0.lookup(db.upcast())
    }

    /// Returns the [SyntaxStablePtrId] of the [StableLocation].
    pub fn stable_ptr(&self) -> SyntaxStablePtrId {
        self.0
    }

    /// Returns the [DiagnosticLocation] that corresponds to the [StableLocation].
    pub fn diagnostic_location(&self, db: &dyn DefsGroup) -> DiagnosticLocation {
        let syntax_node = self.syntax_node(db);
        DiagnosticLocation {
            file_id: self.file_id(db),
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
        let start = self.0.lookup(syntax_db).span_start_without_trivia(syntax_db);
        let end = until_stable_ptr.lookup(syntax_db).span_end_without_trivia(syntax_db);
        DiagnosticLocation { file_id: self.0.file_id(syntax_db), span: TextSpan { start, end } }
    }
}

impl DebugWithDb<dyn DefsGroup> for StableLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn DefsGroup) -> fmt::Result {
        let diag_location = self.diagnostic_location(db);
        diag_location.fmt_location(f, db.upcast())
    }
}
