use std::hash::{Hash, Hasher};

use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_proc_macros::HeapSize;
use salsa::Database;

use super::ids::GreenId;
use super::kind::SyntaxKind;

#[derive(Clone, Debug, PartialEq, Eq, salsa::SalsaValue, HeapSize)]
pub enum GreenNodeDetails<'a> {
    Token(SmolStrId<'a>),
    Node { children: Vec<GreenId<'a>>, width: TextWidth },
}
/// Green node. Underlying untyped representation of the syntax tree.
#[derive(Clone, Debug, PartialEq, Eq, salsa::SalsaValue, HeapSize)]
pub struct GreenNode<'a> {
    pub kind: SyntaxKind,
    pub details: GreenNodeDetails<'a>,
}

/// A borrowed [`GreenNode`], used as an interning lookup key so that the children vector is only
/// allocated when the node is not already interned.
#[derive(Copy, Clone, Debug)]
pub struct GreenNodeRef<'a, 'db> {
    pub kind: SyntaxKind,
    pub details: GreenNodeDetailsRef<'a, 'db>,
}

/// A borrowed [`GreenNodeDetails`]. See [`GreenNodeRef`].
#[derive(Copy, Clone, Debug)]
pub enum GreenNodeDetailsRef<'a, 'db> {
    Token(SmolStrId<'db>),
    Node { children: &'a [GreenId<'db>], width: TextWidth },
}

/// Feeds the details into `state`, in a representation shared by the owned and borrowed forms, so
/// that a node and a lookup key for it always hash the same.
fn hash_details<H: Hasher>(
    token: Option<&SmolStrId<'_>>,
    children: &[GreenId<'_>],
    width: TextWidth,
    state: &mut H,
) {
    match token {
        Some(text) => {
            0u8.hash(state);
            text.hash(state);
        }
        None => {
            1u8.hash(state);
            children.hash(state);
            width.hash(state);
        }
    }
}

impl Hash for GreenNode<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        match &self.details {
            GreenNodeDetails::Token(text) => {
                hash_details(Some(text), &[], TextWidth::default(), state)
            }
            GreenNodeDetails::Node { children, width } => {
                hash_details(None, children, *width, state)
            }
        }
    }
}

impl Hash for GreenNodeRef<'_, '_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        match &self.details {
            GreenNodeDetailsRef::Token(text) => {
                hash_details(Some(text), &[], TextWidth::default(), state)
            }
            GreenNodeDetailsRef::Node { children, width } => {
                hash_details(None, children, *width, state)
            }
        }
    }
}

impl<'db> salsa::HashEqLike<GreenNodeRef<'_, 'db>> for GreenNode<'db> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(self, state);
    }
    fn eq(&self, key: &GreenNodeRef<'_, 'db>) -> bool {
        self.kind == key.kind
            && match (&self.details, &key.details) {
                (GreenNodeDetails::Token(text), GreenNodeDetailsRef::Token(key_text)) => {
                    text == key_text
                }
                (
                    GreenNodeDetails::Node { children, width },
                    GreenNodeDetailsRef::Node { children: key_children, width: key_width },
                ) => children == key_children && width == key_width,
                _ => false,
            }
    }
}

impl<'db> salsa::Lookup<GreenNode<'db>> for GreenNodeRef<'_, 'db> {
    fn into_owned(self) -> GreenNode<'db> {
        GreenNode {
            kind: self.kind,
            details: match self.details {
                GreenNodeDetailsRef::Token(text) => GreenNodeDetails::Token(text),
                GreenNodeDetailsRef::Node { children, width } => {
                    GreenNodeDetails::Node { children: children.to_vec(), width }
                }
            },
        }
    }
}

impl<'db> GreenNodeRef<'_, 'db> {
    /// Interns the node, allocating its children only if it was not interned yet.
    pub fn intern(self, db: &'db dyn Database) -> GreenId<'db> {
        GreenId::new(db, self)
    }
}
impl<'a> GreenNode<'a> {
    pub fn width(&self, db: &dyn Database) -> TextWidth {
        match &self.details {
            GreenNodeDetails::Token(text) => TextWidth::from_str(text.long(db)),
            GreenNodeDetails::Node { width, .. } => *width,
        }
    }
    pub fn children(&self) -> &[GreenId<'a>] {
        match &self.details {
            GreenNodeDetails::Token(_text) => &[],
            GreenNodeDetails::Node { children, .. } => children,
        }
    }
}
