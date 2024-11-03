use std::fmt;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::DiagnosticLocation;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::{TextSpan, TextWidth};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};

use crate::db::DefsGroup;

/// A stable location of a real, concrete syntax.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct StableLocation {
    stable_ptr: SyntaxStablePtrId,
    /// An optional inner span of the stable location. Useful for diagnostics caused by inline
    /// macros, see [crate::plugin::PluginDiagnostic] for more information. The tuple is (offset,
    /// width).
    inner_span: Option<(TextWidth, TextWidth)>,
}
impl StableLocation {
    pub fn new(stable_ptr: SyntaxStablePtrId) -> Self {
        Self { stable_ptr, inner_span: None }
    }

    pub fn with_inner_span(
        stable_ptr: SyntaxStablePtrId,
        inner_span: (TextWidth, TextWidth),
    ) -> Self {
        Self { stable_ptr, inner_span: Some(inner_span) }
    }

    pub fn file_id(&self, db: &dyn DefsGroup) -> FileId {
        self.stable_ptr.file_id(db.upcast())
    }

    pub fn from_ast<TNode: TypedSyntaxNode>(node: &TNode) -> Self {
        Self::new(node.as_syntax_node().stable_ptr())
    }

    /// Returns the [SyntaxNode] that corresponds to the [StableLocation].
    pub fn syntax_node(&self, db: &dyn DefsGroup) -> SyntaxNode {
        self.stable_ptr.lookup(db.upcast())
    }

    /// Returns the [DiagnosticLocation] that corresponds to the [StableLocation].
    pub fn diagnostic_location(&self, db: &dyn DefsGroup) -> DiagnosticLocation {
        if self.inner_span.is_some() {
            println!("Diagnostic location: {:?}", self.inner_span);
        }
        match self.inner_span {
            Some((start, width)) => {
                let start = self.syntax_node(db).offset().add_width(start);
                let end = start.add_width(width);
                DiagnosticLocation { file_id: self.file_id(db), span: TextSpan { start, end } }
            }
            None => {
                let syntax_node = self.syntax_node(db);
                DiagnosticLocation {
                    file_id: self.file_id(db),
                    span: syntax_node.span_without_trivia(db.upcast()),
                }
            }
        }
    }

    /// Returns the [DiagnosticLocation] that corresponds to the [StableLocation].
    pub fn diagnostic_location_until(
        &self,
        db: &dyn DefsGroup,
        until_stable_ptr: SyntaxStablePtrId,
    ) -> DiagnosticLocation {
        if self.inner_span.is_some() {
            println!("Diagnostic location until: {:?}", self.inner_span);
        }
        let syntax_db = db.upcast();
        let start = self.stable_ptr.lookup(syntax_db).span_start_without_trivia(syntax_db);
        let end = until_stable_ptr.lookup(syntax_db).span_end_without_trivia(syntax_db);
        DiagnosticLocation {
            file_id: self.stable_ptr.file_id(syntax_db),
            span: TextSpan { start, end },
        }
    }
}

impl DebugWithDb<dyn DefsGroup> for StableLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn DefsGroup) -> fmt::Result {
        let diag_location = self.diagnostic_location(db);
        diag_location.fmt_location(f, db.upcast())
    }
}
