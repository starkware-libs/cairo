use core::hash::Hash;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::Arc;
use std::vec;

use filesystem::span::{TextOffset, TextSpan};
use smol_str::SmolStr;

use self::ast::TriviaGreen;
use self::db::SyntaxGroup;
use self::green::GreenNode;
use self::ids::{GreenId, SyntaxStablePtrId};
use self::key_fields::get_key_fields;
use self::kind::SyntaxKind;
use self::stable_ptr::SyntaxStablePtr;

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
impl SyntaxNode {
    pub fn new_root(db: &dyn SyntaxGroup, green: ast::SyntaxFileGreen) -> Self {
        let inner = SyntaxNodeInner {
            green: green.0,
            offset: 0,
            parent: None,
            stable_ptr: db.intern_stable_ptr(SyntaxStablePtr::Root),
        };
        Self(Arc::new(inner))
    }
    pub fn offset(&self) -> TextOffset {
        TextOffset(self.0.offset as usize)
    }
    pub fn width(&self, db: &dyn SyntaxGroup) -> u32 {
        self.green_node(db).width()
    }
    pub fn kind(&self, db: &dyn SyntaxGroup) -> SyntaxKind {
        self.green_node(db).kind
    }
    pub fn span(&self, db: &dyn SyntaxGroup) -> TextSpan {
        let start = self.offset();
        let end = start.add(self.width(db) as usize);
        TextSpan { start, end }
    }
    /// Returns the text of the token if this node is a token.
    pub fn text(&self, db: &dyn SyntaxGroup) -> Option<SmolStr> {
        match self.green_node(db).details {
            green::GreenNodeDetails::Token(text) => Some(text),
            green::GreenNodeDetails::Node { .. } => None,
        }
    }
    pub fn green_node(&self, db: &dyn SyntaxGroup) -> GreenNode {
        db.lookup_intern_green(self.0.green)
    }
    pub fn span_without_trivia(&self, db: &dyn SyntaxGroup) -> TextSpan {
        let start = self.span_start_without_trivia(db);
        let end = self.span_end_without_trivia(db);
        TextSpan { start, end }
    }
    pub fn children<'db>(&self, db: &'db dyn SyntaxGroup) -> SyntaxNodeChildIterator<'db> {
        SyntaxNodeChildIterator {
            db,
            node: self.clone(),
            green_iterator: self.green_node(db).children().into_iter(),
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

    /// Gets the inner token from a terminal SyntaxNode. If the given node is not a terminal,
    /// returns None.
    pub fn get_terminal_token(&self, db: &dyn SyntaxGroup) -> Option<SyntaxNode> {
        let green_node = self.green_node(db);
        if !green_node.kind.is_terminal() {
            return None;
        }
        // At this point we know we should have a second child which is the token.
        let token_node = self.children(db).nth(1).unwrap();
        Some(token_node)
    }

    pub fn span_start_without_trivia(&self, db: &dyn SyntaxGroup) -> TextOffset {
        let green_node = self.green_node(db);
        match green_node.details {
            green::GreenNodeDetails::Node { .. } => {
                if let Some(token_node) = self.get_terminal_token(db) {
                    return token_node.offset();
                }
                let children = &mut self.children(db);
                if let Some(child) = children.find(|child| child.width(db) != 0) {
                    child.span_start_without_trivia(db)
                } else {
                    self.offset()
                }
            }
            green::GreenNodeDetails::Token(_) => self.offset(),
        }
    }
    pub fn span_end_without_trivia(&self, db: &dyn SyntaxGroup) -> TextOffset {
        let green_node = self.green_node(db);
        match green_node.details {
            green::GreenNodeDetails::Node { .. } => {
                if let Some(token_node) = self.get_terminal_token(db) {
                    return token_node.span(db).end;
                }
                let children = &mut self.children(db);
                if let Some(child) = children.filter(|child| child.width(db) != 0).last() {
                    child.span_end_without_trivia(db)
                } else {
                    self.span(db).end
                }
            }
            green::GreenNodeDetails::Token(_) => self.span(db).end,
        }
    }

    /// Lookups a syntax node using an offset.
    pub fn lookup_offset(&self, db: &dyn SyntaxGroup, offset: TextOffset) -> SyntaxNode {
        for child in self.children(db) {
            if child.offset().0 + (child.width(db) as usize) > offset.0 {
                return child.lookup_offset(db, offset);
            }
        }
        self.clone()
    }

    /// Returns all the text under the syntax node.
    /// Note that this traverses the syntax tree, and generates a new string, so use responsibly.
    pub fn get_text(self, db: &dyn SyntaxGroup) -> String {
        let mut buffer = String::default();
        self.append_text(db, &mut buffer);
        buffer
    }

    /// Helper for `get_text` to allocate only a single string.
    fn append_text(self, db: &dyn SyntaxGroup, buffer: &mut String) {
        let node = self.green_node(db);
        match node.details {
            green::GreenNodeDetails::Token(text) => {
                buffer.push_str(text.as_str());
            }
            green::GreenNodeDetails::Node { .. } => {
                for child in self.children(db) {
                    child.append_text(db, buffer);
                }
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
        let kind = green.kind;
        let key_fields: Vec<GreenId> = get_key_fields(kind, green.children());
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
    /// The relevant SyntaxKind. None for enums.
    const OPTIONAL_KIND: Option<SyntaxKind>;
    type StablePtr;
    type Green;
    fn missing(db: &dyn SyntaxGroup) -> Self::Green;
    // TODO(spapini): Make this return an Option, if the kind is wrong.
    fn from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self;
    fn from_ptr(db: &dyn SyntaxGroup, root: &ast::SyntaxFile, node: Self::StablePtr) -> Self;
    fn as_syntax_node(&self) -> SyntaxNode;
    fn stable_ptr(&self) -> Self::StablePtr;
}

pub trait Token: TypedSyntaxNode {
    fn new_green(db: &dyn SyntaxGroup, text: SmolStr) -> Self::Green;
    fn text(&self, db: &dyn SyntaxGroup) -> SmolStr;
}

pub trait Terminal: TypedSyntaxNode {
    const KIND: SyntaxKind;
    type TokenType: Token;
    fn new_green(
        db: &dyn SyntaxGroup,
        leading_trivia: TriviaGreen,
        token: <<Self as Terminal>::TokenType as TypedSyntaxNode>::Green,
        trailing_trivia: TriviaGreen,
    ) -> <Self as TypedSyntaxNode>::Green;
    /// Returns the text of the token of this terminal (excluding the trivia).
    fn text(&self, db: &dyn SyntaxGroup) -> SmolStr;
}
