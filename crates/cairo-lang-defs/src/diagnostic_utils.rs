use std::fmt;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_filesystem::ids::{FileId, SpanInFile};
use cairo_lang_filesystem::span::{TextSpan, TextWidth};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use salsa::Database;

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

    pub fn file_id(&self, db: &'db dyn Database) -> FileId<'db> {
        self.stable_ptr.file_id(db)
    }

    pub fn from_ast<TNode: TypedSyntaxNode<'db>>(db: &'db dyn Database, node: &TNode) -> Self {
        Self::new(node.as_syntax_node().stable_ptr(db))
    }

    /// Returns the [SyntaxNode] that corresponds to the [StableLocation].
    pub fn syntax_node(&self, db: &'db dyn Database) -> SyntaxNode<'db> {
        self.stable_ptr.lookup(db)
    }

    /// Returns the [SyntaxStablePtrId] of the [StableLocation].
    pub fn stable_ptr(&self) -> SyntaxStablePtrId<'db> {
        self.stable_ptr
    }

    /// Returns the [SpanInFile] that corresponds to the [StableLocation].
    pub fn span_in_file(&self, db: &'db dyn Database) -> SpanInFile<'db> {
        match self.inner_span {
            Some((start, width)) => {
                let start = self.syntax_node(db).offset(db).add_width(start);
                SpanInFile {
                    file_id: self.file_id(db),
                    span: TextSpan::new_with_width(start, width),
                }
            }
            None => {
                let syntax_node = self.syntax_node(db);
                SpanInFile { file_id: self.file_id(db), span: syntax_node.span_without_trivia(db) }
            }
        }
    }

    /// Returns the [SpanInFile] that starts at the [StableLocation], and ends at the given
    /// [SyntaxStablePtrId].
    pub fn span_in_file_until(
        &self,
        db: &'db dyn Database,
        until_stable_ptr: SyntaxStablePtrId<'db>,
    ) -> SpanInFile<'db> {
        let start = self.stable_ptr.lookup(db).span_start_without_trivia(db);
        let end = until_stable_ptr.lookup(db).span_end_without_trivia(db);
        SpanInFile { file_id: self.stable_ptr.file_id(db), span: TextSpan::new(start, end) }
    }
}

impl<'db> DebugWithDb<'db> for StableLocation<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &'db dyn Database) -> fmt::Result {
        let diag_location = self.span_in_file(db);
        diag_location.fmt_location(f, db)
    }
}
