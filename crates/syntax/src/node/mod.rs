use core::hash::Hash;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::Arc;
use std::vec;

use smol_str::SmolStr;

use self::db::GreenInterner;
use self::green::GreenNode;
use self::ids::{GreenId, SyntaxStablePtrId};
use self::key_fields::get_key_fields;
use self::kind::SyntaxKind;
use self::stable_ptr::SyntaxStablePtr;
use crate::token;

pub mod ast;
#[cfg(test)]
mod ast_test;
pub mod db;
pub mod element_list;
pub mod green;
pub mod ids;
pub mod key_fields;
pub mod kind;
pub mod stable_ptr;

/// SyntaxNode. Untyped view of the syntax tree. Adds parent() and offset() capabilities.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SyntaxNode(Arc<SyntaxNodeInner>);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct SyntaxNodeInner {
    green: GreenId,
    /// Number of characters from the beginning of the file to the start of the span of this
    /// syntax subtree.
    offset: u32,
    parent: Option<SyntaxNode>,
    stable_ptr: SyntaxStablePtrId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SyntaxNodeDetails {
    Syntax(SyntaxKind),
    Token(token::Token),
}
impl SyntaxNode {
    pub fn new_root(db: &dyn GreenInterner, green: GreenId) -> Self {
        let inner = SyntaxNodeInner {
            green,
            offset: 0,
            parent: None,
            stable_ptr: db.intern_stable_ptr(SyntaxStablePtr::Root),
        };
        Self(Arc::new(inner))
    }
    pub fn details(&self, db: &dyn GreenInterner) -> SyntaxNodeDetails {
        match db.lookup_intern_green(self.0.green) {
            GreenNode::Internal(internal) => SyntaxNodeDetails::Syntax(internal.kind),
            GreenNode::Token(token) => SyntaxNodeDetails::Token(token),
        }
    }
    pub fn offset(&self) -> u32 {
        self.0.offset
    }
    pub fn width(&self, db: &dyn GreenInterner) -> u32 {
        match db.lookup_intern_green(self.0.green) {
            GreenNode::Internal(internal) => internal.width,
            GreenNode::Token(token) => token.width(),
        }
    }
    pub fn children<'db>(&self, db: &'db dyn GreenInterner) -> SyntaxNodeChildIterator<'db> {
        let green_iterator = match db.lookup_intern_green(self.0.green) {
            GreenNode::Internal(internal) => internal.children,
            GreenNode::Token(_) => vec![],
        }
        .into_iter();
        SyntaxNodeChildIterator {
            db,
            parent: self.clone(),
            green_iterator,
            offset: self.0.offset,
            key_map: HashMap::new(),
        }
    }
    pub fn parent(&self) -> Option<SyntaxNode> {
        self.0.parent.as_ref().cloned()
    }

    /// Should only be called on the root.
    pub fn lookup_ptr(&self, db: &dyn GreenInterner, stable_ptr: SyntaxStablePtrId) -> SyntaxNode {
        assert!(self.0.parent.is_none(), "May only be called on the root.");
        let ptr = db.lookup_intern_stable_ptr(stable_ptr);
        match ptr {
            SyntaxStablePtr::Root => self.clone(),
            SyntaxStablePtr::Child { .. } => {
                let parent = self.lookup_ptr(db, stable_ptr);
                for child in parent.children(db) {
                    if child.0.stable_ptr == stable_ptr {
                        return child;
                    }
                }
                panic!("Should not have reached here.");
            }
        }
    }
}
pub struct SyntaxNodeChildIterator<'db> {
    db: &'db dyn GreenInterner,
    parent: SyntaxNode,
    green_iterator: vec::IntoIter<GreenId>,
    offset: u32,
    key_map: HashMap<(SyntaxKind, Vec<GreenId>), usize>,
}
impl<'db> Iterator for SyntaxNodeChildIterator<'db> {
    type Item = SyntaxNode;

    fn next(&mut self) -> Option<Self::Item> {
        let green_id = self.green_iterator.next()?;
        let green = self.db.lookup_intern_green(green_id);
        let (kind, children) = match green.clone() {
            GreenNode::Internal(node) => (node.kind, node.children),
            GreenNode::Token(_) => {
                // TODO(spapini): change this once token is united with syntax.
                (SyntaxKind::Terminal, vec![])
            }
        };
        let key_fields: Vec<GreenId> = get_key_fields(kind, children);
        let index = match self.key_map.entry((kind, key_fields.clone())) {
            Entry::Occupied(mut entry) => entry.insert(entry.get() + 1),
            Entry::Vacant(entry) => *entry.insert(0),
        };
        let stable_ptr = self.db.intern_stable_ptr(SyntaxStablePtr::Child {
            parent: self.parent.0.stable_ptr,
            kind,
            key_fields,
            index,
        });
        // Create the SyntaxNode view for the child.
        let res = SyntaxNode(Arc::new(SyntaxNodeInner {
            green: green_id,
            offset: self.offset,
            parent: Some(self.parent.clone()),
            stable_ptr,
        }));
        self.offset += green.width();
        Some(res)
    }
}
impl<'db> ExactSizeIterator for SyntaxNodeChildIterator<'db> {
    fn len(&self) -> usize {
        self.green_iterator.len()
    }
}

/// Trait for the typed view of the syntax tree. All the internal node implementations are under
/// the ast module.
pub trait TypedSyntaxNode {
    fn missing(db: &dyn GreenInterner) -> GreenId;
    fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self;
    fn as_syntax_node(&self) -> SyntaxNode;
    fn stable_ptr(&self) -> SyntaxStablePtrId {
        self.as_syntax_node().0.stable_ptr
    }
}

// TODO(spapini): Children should be excluded from Eq and Hash of Typed nodes.
/// Typed view for a token. Implements the typed view interface TypedSyntaxNode.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token {
    node: SyntaxNode,
}

impl Token {
    pub fn new_green(db: &dyn GreenInterner, kind: token::TokenKind, text: SmolStr) -> GreenId {
        db.intern_green(GreenNode::Token(token::Token { kind, text }))
    }
    pub fn raw(&self, db: &dyn GreenInterner) -> token::Token {
        let green = db.lookup_intern_green(self.node.0.green);
        if let GreenNode::Token(token) = green {
            return token;
        }
        panic!("Expected a token, got {:?}.", green);
    }
    pub fn kind(&self, db: &dyn GreenInterner) -> token::TokenKind {
        self.raw(db).kind
    }
    pub fn text(&self, db: &dyn GreenInterner) -> SmolStr {
        self.raw(db).text
    }
    pub fn width(&self, db: &dyn GreenInterner) -> u32 {
        self.raw(db).width()
    }
}
impl TypedSyntaxNode for Token {
    fn missing(db: &dyn GreenInterner) -> GreenId {
        db.intern_green(GreenNode::Token(token::Token::missing()))
    }
    fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
        let green = db.lookup_intern_green(node.0.green);
        match green {
            GreenNode::Internal(internal) => {
                panic!("Unexpected SyntaxKind {:?}. Expected a token.", internal.kind);
            }
            GreenNode::Token(_token) => Self { node },
        }
    }

    fn as_syntax_node(&self) -> SyntaxNode {
        self.node.clone()
    }
}

// TODO(spapini): Consider converting into a trait and moving somewhere else.
impl ast::Identifier {
    pub fn text(&self, db: &dyn GreenInterner) -> SmolStr {
        self.terminal(db).token(db).text(db)
    }
}
