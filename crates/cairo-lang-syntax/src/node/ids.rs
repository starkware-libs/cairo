use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_utils::define_short_id;
use salsa::Database;

use super::SyntaxNode;
use super::green::GreenNode;
use super::kind::SyntaxKind;
use crate::node::stable_ptr::SyntaxStablePtr;

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

define_short_id!(SyntaxStablePtrId, SyntaxStablePtr<'db>);
impl<'a> SyntaxStablePtrId<'a> {
    /// Lookups a syntax node using a stable syntax pointer.
    /// Should only be called on the root from which the stable pointer was generated.
    pub fn lookup(&self, db: &'a dyn Database) -> SyntaxNode<'a> {
        let ptr = self.long(db).clone();
        match ptr {
            SyntaxStablePtr::Root(file_id, green) => SyntaxNode::new_root(db, file_id, green),
            SyntaxStablePtr::Child { parent, .. } => {
                let parent = parent.lookup(db);
                for child in parent.get_children(db).iter() {
                    if child.stable_ptr(db) == *self {
                        return *child;
                    }
                }
                unreachable!();
            }
        }
    }
    pub fn file_id(&self, db: &'a dyn Database) -> FileId<'a> {
        let ptr = self.long(db);
        match ptr {
            SyntaxStablePtr::Root(file_id, _) => *file_id,
            SyntaxStablePtr::Child { parent, .. } => parent.file_id(db),
        }
    }
    /// Returns the stable pointer of the parent of this stable pointer.
    /// Assumes that the parent exists (that is, `self` is not the root). Panics otherwise.
    pub fn parent<'r: 'a>(&self, db: &'r dyn Database) -> SyntaxStablePtrId<'a> {
        let SyntaxStablePtr::Child { parent, .. } = self.long(db).clone() else { panic!() };
        parent
    }
    /// Returns the stable pointer of the `n`th parent of this stable pointer.
    /// n = 0: returns itself.
    /// n = 1: return the parent.
    /// n = 2: return the grand parent.
    /// And so on...
    /// Assumes that the `n`th parent exists. Panics otherwise.
    pub fn nth_parent<'r: 'a>(&self, db: &'r dyn Database, n: usize) -> SyntaxStablePtrId<'a> {
        let mut ptr = *self;
        for _ in 0..n {
            ptr = ptr.parent(db);
        }
        ptr
    }
    /// Returns the kind of this stable pointer.
    /// Assumes that `self` is not the root. Panics otherwise.
    pub fn kind(&self, db: &'a dyn Database) -> SyntaxKind {
        let SyntaxStablePtr::Child { kind, .. } = self.long(db).clone() else { panic!() };
        kind
    }
}
