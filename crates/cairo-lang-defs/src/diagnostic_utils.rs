use std::fmt;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::DiagnosticLocation;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::{TextSpan, TextWidth};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};

use crate::db::DefsGroup;

/// A stable location of a real, concrete syntax.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub struct StableLocation<'db> {
    stable_ptr: SyntaxStablePtrId<'db>,
    /// An optional inner span of the stable location. Useful for diagnostics caused by inline
    /// macros, see [crate::plugin::PluginDiagnostic] for more information. The tuple is (offset,
    /// width).
    inner_span: Option<(TextWidth, TextWidth)>,
}

impl<'db> StableLocation<'db> {
    pub fn new(stable_ptr: SyntaxStablePtrId<'db>) -> Self {
        Self { stable_ptr, inner_span: None }
    }

    pub fn with_inner_span(
        stable_ptr: SyntaxStablePtrId<'db>,
        inner_span: (TextWidth, TextWidth),
    ) -> Self {
        Self { stable_ptr, inner_span: Some(inner_span) }
    }

    pub fn file_id(&self, db: &'db dyn DefsGroup) -> FileId<'db> {
        self.stable_ptr.file_id(db)
    }

    pub fn from_ast<TNode: TypedSyntaxNode<'db>>(db: &'db dyn SyntaxGroup, node: &TNode) -> Self {
        Self::new(node.as_syntax_node().stable_ptr(db))
    }

    /// Returns the [SyntaxNode] that corresponds to the [StableLocation].
    pub fn syntax_node(&self, db: &'db dyn DefsGroup) -> SyntaxNode<'db> {
        self.stable_ptr.lookup(db)
    }

    /// Returns the [SyntaxStablePtrId] of the [StableLocation].
    pub fn stable_ptr(&self) -> SyntaxStablePtrId<'db> {
        self.stable_ptr
    }

    /// Returns the [DiagnosticLocation] that corresponds to the [StableLocation].
    pub fn diagnostic_location(&self, db: &'db dyn DefsGroup) -> DiagnosticLocation<'db> {
        match self.inner_span {
            Some((start, width)) => {
                let start = self.syntax_node(db).offset(db).add_width(start);
                let end = start.add_width(width);
                DiagnosticLocation { file_id: self.file_id(db), span: TextSpan { start, end } }
            }
            None => {
                let syntax_node = self.syntax_node(db);
                DiagnosticLocation {
                    file_id: self.file_id(db),
                    span: syntax_node.span_without_trivia(db),
                }
            }
        }
    }

    /// Returns the [DiagnosticLocation] that corresponds to the [StableLocation].
    pub fn diagnostic_location_until(
        &self,
        db: &'db dyn DefsGroup,
        until_stable_ptr: SyntaxStablePtrId<'db>,
    ) -> DiagnosticLocation<'db> {
        let start = self.stable_ptr.lookup(db).span_start_without_trivia(db);
        let end = until_stable_ptr.lookup(db).span_end_without_trivia(db);
        DiagnosticLocation { file_id: self.stable_ptr.file_id(db), span: TextSpan { start, end } }
    }
}

impl<'a> DebugWithDb<dyn DefsGroup> for StableLocation<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn DefsGroup) -> fmt::Result {
        let diag_location = self.diagnostic_location(db);
        diag_location.fmt_location(f, db)
    }
}
