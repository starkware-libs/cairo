use cairo_lang_filesystem::ids::{FileId, SpanInFile};
use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_utils::define_short_id;
use salsa::Database;

use super::SyntaxNode;
use super::green::GreenNode;
use super::kind::SyntaxKind;

define_short_id!(GreenId, GreenNode<'db>);
impl<'a> GreenId<'a> {
    /// Returns the width of the node of this green id.
    pub fn width(&self, db: &dyn Database) -> TextWidth {
        match &self.long(db).details {
            super::green::GreenNodeDetails::Token(text) => TextWidth::from_str(text.long(db)),
            super::green::GreenNodeDetails::Node { width, .. } => *width,
        }
    }
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct SyntaxStablePtrId<'a>(pub SyntaxNode<'a>);

impl<'a> SyntaxStablePtrId<'a> {
    /// Lookups a syntax node using a stable syntax pointer.
    /// Should only be called on the root from which the stable pointer was generated.
    pub fn lookup(&self, _db: &'a dyn Database) -> SyntaxNode<'a> {
        self.0
    }
    pub fn file_id(&self, db: &'a dyn Database) -> FileId<'a> {
        self.0.file_id(db)
    }
    /// Returns the stable pointer of the parent of this stable pointer.
    /// Assumes that the parent exists (that is, `self` is not the root). Panics otherwise.
    pub fn parent<'r: 'a>(&self, db: &'r dyn Database) -> SyntaxStablePtrId<'a> {
        SyntaxStablePtrId(self.0.parent(db).unwrap())
    }
    /// Returns the stable pointer of the `n`th parent of this stable pointer.
    /// n = 0: returns itself.
    /// n = 1: return the parent.
    /// n = 2: return the grandparent.
    /// And so on...
    /// Assumes that the `n`th parent exists. Panics otherwise.
    pub fn nth_parent<'r: 'a>(&self, db: &'r dyn Database, n: usize) -> SyntaxStablePtrId<'a> {
        SyntaxStablePtrId(self.0.nth_parent(db, n))
    }
    /// Returns the kind of this stable pointer.
    /// Assumes that `self` is not the root. Panics otherwise.
    pub fn kind(&self, db: &'a dyn Database) -> SyntaxKind {
        self.0.kind(db)
    }
    /// Returns the span in file of this stable pointer without trivia.
    pub fn span_in_file(&self, db: &'a dyn Database) -> SpanInFile<'a> {
        SpanInFile { file_id: self.file_id(db), span: self.lookup(db).span_without_trivia(db) }
    }
}
