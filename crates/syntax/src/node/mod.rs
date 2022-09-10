use core::hash::Hash;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::Arc;
use std::vec;

use filesystem::span::{TextOffset, TextSpan};
use smol_str::SmolStr;
use utils::extract_matches;

use self::db::SyntaxGroup;
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
pub mod helpers;
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
    pub fn new_root(db: &dyn SyntaxGroup, green: GreenId) -> Self {
        let inner = SyntaxNodeInner {
            green,
            offset: 0,
            parent: None,
            stable_ptr: db.intern_stable_ptr(SyntaxStablePtr::Root),
        };
        Self(Arc::new(inner))
    }
    pub fn details(&self, db: &dyn SyntaxGroup) -> SyntaxNodeDetails {
        match db.lookup_intern_green(self.0.green) {
            GreenNode::Internal(internal) => SyntaxNodeDetails::Syntax(internal.kind),
            GreenNode::Token(token) => SyntaxNodeDetails::Token(token),
        }
    }
    pub fn offset(&self) -> TextOffset {
        TextOffset(self.0.offset as usize)
    }
    pub fn width(&self, db: &dyn SyntaxGroup) -> u32 {
        match db.lookup_intern_green(self.0.green) {
            GreenNode::Internal(internal) => internal.width,
            GreenNode::Token(token) => token.width(),
        }
    }
    pub fn span(&self, db: &dyn SyntaxGroup) -> TextSpan {
        let start = self.offset();
        let end = start.add(self.width(db) as usize);
        TextSpan { start, end }
    }
    pub fn children<'db>(&self, db: &'db dyn SyntaxGroup) -> SyntaxNodeChildIterator<'db> {
        let green_iterator = match db.lookup_intern_green(self.0.green) {
            GreenNode::Internal(internal) => internal.children,
            GreenNode::Token(_) => vec![],
        }
        .into_iter();
        SyntaxNodeChildIterator {
            db,
            node: self.clone(),
            green_iterator,
            offset: self.0.offset,
            key_map: HashMap::new(),
        }
    }
    pub fn parent(&self) -> Option<SyntaxNode> {
        self.0.parent.as_ref().cloned()
    }
    pub fn stable_ptr(&self) -> SyntaxStablePtrId {
        self.0.stable_ptr
    }

    /// Lookups a syntax node using a stable syntax pointer.
    /// Should only be called on the root from which the stable pointer was generated.
    pub fn lookup_ptr(&self, db: &dyn SyntaxGroup, stable_ptr: SyntaxStablePtrId) -> SyntaxNode {
        assert!(self.0.parent.is_none(), "May only be called on the root.");
        let ptr = db.lookup_intern_stable_ptr(stable_ptr);
        match ptr {
            SyntaxStablePtr::Root => self.clone(),
            SyntaxStablePtr::Child { parent, .. } => {
                let parent = self.lookup_ptr(db, parent);
                for child in parent.children(db) {
                    if child.stable_ptr() == stable_ptr {
                        return child;
                    }
                }
                unreachable!();
            }
        }
    }
}
pub struct SyntaxNodeChildIterator<'db> {
    db: &'db dyn SyntaxGroup,
    node: SyntaxNode,
    green_iterator: vec::IntoIter<GreenId>,
    /// The current offset in the source file of the start of the child.
    offset: u32,
    /// Mapping from (kind, key_fields) to the number of times this indexing pair has been seen.
    /// This is used to maintain the correct index for creating each StablePtr.
    /// See [`self::key_fields`].
    key_map: HashMap<(SyntaxKind, Vec<GreenId>), usize>,
}
impl<'db> Iterator for SyntaxNodeChildIterator<'db> {
    type Item = SyntaxNode;

    fn next(&mut self) -> Option<Self::Item> {
        let green_id = self.green_iterator.next()?;
        let green = self.db.lookup_intern_green(green_id);
        let width = green.width();
        let (kind, children) = match green {
            GreenNode::Internal(node) => (node.kind, node.children),
            GreenNode::Token(_) => {
                // TODO(spapini): change this once token is united with syntax.
                (SyntaxKind::ExprMissing, vec![])
            }
        };
        let key_fields: Vec<GreenId> = get_key_fields(kind, children);
        let index = match self.key_map.entry((kind, key_fields.clone())) {
            Entry::Occupied(mut entry) => entry.insert(entry.get() + 1),
            Entry::Vacant(entry) => {
                entry.insert(1);
                0
            }
        };
        let stable_ptr = self.db.intern_stable_ptr(SyntaxStablePtr::Child {
            parent: self.node.0.stable_ptr,
            kind,
            key_fields,
            index,
        });
        // Create the SyntaxNode view for the child.
        let res = SyntaxNode(Arc::new(SyntaxNodeInner {
            green: green_id,
            offset: self.offset,
            parent: Some(self.node.clone()),
            stable_ptr,
        }));
        self.offset += width;
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
    type StablePtr;
    fn missing(db: &dyn SyntaxGroup) -> GreenId;
    // TODO(spapini): Make this return an Option, if the kind is wrong.
    fn from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self;
    fn from_ptr(db: &dyn SyntaxGroup, root: &ast::SyntaxFile, node: Self::StablePtr) -> Self;
    fn as_syntax_node(&self) -> SyntaxNode;
    fn stable_ptr(&self) -> Self::StablePtr;
}

// TODO(spapini): Children should be excluded from Eq and Hash of Typed nodes.
/// Typed view for a token. Implements the typed view interface TypedSyntaxNode.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token {
    node: SyntaxNode,
}

impl Token {
    pub fn new_green(db: &dyn SyntaxGroup, kind: token::TokenKind, text: SmolStr) -> GreenId {
        db.intern_green(GreenNode::Token(token::Token { kind, text }))
    }
    pub fn raw(&self, db: &dyn SyntaxGroup) -> token::Token {
        let green = db.lookup_intern_green(self.node.0.green);
        extract_matches!(green, GreenNode::Token, "Expected a token, got {:?}.", green)
    }
    pub fn kind(&self, db: &dyn SyntaxGroup) -> token::TokenKind {
        self.raw(db).kind
    }
    pub fn text(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.raw(db).text
    }
    pub fn width(&self, db: &dyn SyntaxGroup) -> u32 {
        self.raw(db).width()
    }
}
pub struct TokenPtr(SyntaxStablePtrId);
impl TokenPtr {
    #[allow(dead_code)]
    pub fn untyped(&self) -> SyntaxStablePtrId {
        self.0
    }
}
impl TypedSyntaxNode for Token {
    type StablePtr = TokenPtr;
    fn missing(db: &dyn SyntaxGroup) -> GreenId {
        db.intern_green(GreenNode::Token(token::Token::missing()))
    }
    fn from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
        let green = db.lookup_intern_green(node.0.green);
        match green {
            GreenNode::Internal(internal) => {
                panic!("Unexpected SyntaxKind {:?}. Expected a token.", internal.kind);
            }
            GreenNode::Token(_token) => Self { node },
        }
    }
    fn from_ptr(db: &dyn SyntaxGroup, root: &ast::SyntaxFile, ptr: Self::StablePtr) -> Self {
        Self::from_syntax_node(db, root.as_syntax_node().lookup_ptr(db, ptr.0))
    }
    fn as_syntax_node(&self) -> SyntaxNode {
        self.node.clone()
    }
    fn stable_ptr(&self) -> Self::StablePtr {
        TokenPtr(self.node.0.stable_ptr)
    }
}

// TODO(spapini): Consider converting into a trait and moving somewhere else.
impl ast::Terminal {
    pub fn text(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.token(db).text(db)
    }
}
