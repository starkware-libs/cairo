use core::hash::Hash;
use std::fmt::Display;
use std::sync::Arc;

use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::{TextOffset, TextPosition, TextSpan, TextWidth};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern, define_short_id, require};
use smol_str::SmolStr;

use self::ast::TriviaGreen;
use self::db::SyntaxGroup;
use self::green::GreenNode;
use self::ids::{GreenId, SyntaxStablePtrId};
use self::kind::SyntaxKind;
use self::stable_ptr::SyntaxStablePtr;
use crate::node::iter::{Preorder, WalkEvent};

pub mod ast;
pub mod db;
pub mod element_list;
pub mod green;
pub mod helpers;
pub mod ids;
pub mod iter;
pub mod key_fields;
pub mod kind;
pub mod stable_ptr;
pub mod with_db;

#[cfg(test)]
mod ast_test;
#[cfg(test)]
mod test_utils;

/// SyntaxNode. Untyped view of the syntax tree. Adds parent() and offset() capabilities.

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SyntaxNodeLongId {
    green: GreenId,
    /// Number of characters from the beginning of the file to the start of the span of this
    /// syntax subtree.
    offset: TextOffset,
    parent: Option<SyntaxNode>,
    stable_ptr: SyntaxStablePtrId,
}
define_short_id!(
    SyntaxNode,
    SyntaxNodeLongId,
    SyntaxGroup,
    lookup_intern_syntax_node,
    intern_syntax_node
);
impl SyntaxNode {
    pub fn new_root(db: &dyn SyntaxGroup, file_id: FileId, green: GreenId) -> Self {
        Self::new_with_inner(
            db,
            green,
            TextOffset::START,
            None,
            SyntaxStablePtr::Root(file_id, green).intern(db),
        )
    }

    pub fn new_root_with_offset(
        db: &dyn SyntaxGroup,
        file_id: FileId,
        green: GreenId,
        initial_offset: Option<TextOffset>,
    ) -> Self {
        Self::new_with_inner(
            db,
            green,
            initial_offset.unwrap_or_default(),
            None,
            SyntaxStablePtr::Root(file_id, green).intern(db),
        )
    }

    pub fn new_with_inner(
        db: &dyn SyntaxGroup,
        green: GreenId,
        offset: TextOffset,
        parent: Option<SyntaxNode>,
        stable_ptr: SyntaxStablePtrId,
    ) -> Self {
        SyntaxNodeLongId { green, offset, parent, stable_ptr }.intern(db)
    }

    pub fn offset(&self, db: &dyn SyntaxGroup) -> TextOffset {
        self.lookup_intern(db).offset
    }
    pub fn width(&self, db: &dyn SyntaxGroup) -> TextWidth {
        self.green_node(db).width()
    }
    pub fn kind(&self, db: &dyn SyntaxGroup) -> SyntaxKind {
        self.green_node(db).kind
    }
    pub fn span(&self, db: &dyn SyntaxGroup) -> TextSpan {
        let start = self.offset(db);
        let end = start.add_width(self.width(db));
        TextSpan { start, end }
    }
    /// Returns the text of the token if this node is a token.
    pub fn text(&self, db: &dyn SyntaxGroup) -> Option<SmolStr> {
        match &self.green_node(db).details {
            green::GreenNodeDetails::Token(text) => Some(text.clone()),
            green::GreenNodeDetails::Node { .. } => None,
        }
    }
    pub fn green_node(&self, db: &dyn SyntaxGroup) -> Arc<GreenNode> {
        self.lookup_intern(db).green.lookup_intern(db)
    }
    pub fn span_without_trivia(&self, db: &dyn SyntaxGroup) -> TextSpan {
        let start = self.span_start_without_trivia(db);
        let end = self.span_end_without_trivia(db);
        TextSpan { start, end }
    }
    pub fn parent(&self, db: &dyn SyntaxGroup) -> Option<SyntaxNode> {
        self.lookup_intern(db).parent.as_ref().cloned()
    }
    pub fn stable_ptr(&self, db: &dyn SyntaxGroup) -> SyntaxStablePtrId {
        self.lookup_intern(db).stable_ptr
    }

    /// Gets the inner token from a terminal SyntaxNode. If the given node is not a terminal,
    /// returns None.
    pub fn get_terminal_token(&self, db: &dyn SyntaxGroup) -> Option<SyntaxNode> {
        let green_node = self.green_node(db);
        require(green_node.kind.is_terminal())?;
        // At this point we know we should have a second child which is the token.
        self.get_children(db).get(1).copied()
    }

    /// Gets the children syntax nodes of the current node.
    pub fn get_children(&self, db: &dyn SyntaxGroup) -> Arc<[SyntaxNode]> {
        db.get_children(*self)
    }

    /// Implementation of [SyntaxNode::get_children].
    fn get_children_impl(&self, db: &dyn SyntaxGroup) -> Vec<SyntaxNode> {
        let self_long_id = self.lookup_intern(db);
        let mut offset = self_long_id.offset;
        let self_green = self_long_id.green.lookup_intern(db);
        let children = self_green.children();
        let mut res: Vec<SyntaxNode> = Vec::with_capacity(children.len());
        let mut key_map = UnorderedHashMap::<_, usize>::default();
        for green_id in children {
            let green = green_id.lookup_intern(db);
            let width = green.width();
            let kind = green.kind;
            let key_fields = key_fields::get_key_fields(kind, green.children());
            let key_count = key_map.entry((kind, key_fields.clone())).or_default();
            let stable_ptr = SyntaxStablePtr::Child {
                parent: self_long_id.stable_ptr,
                kind,
                key_fields,
                index: *key_count,
            }
            .intern(db);
            *key_count += 1;
            // Create the SyntaxNode view for the child.
            res.push(
                SyntaxNodeLongId { green: *green_id, offset, parent: Some(*self), stable_ptr }
                    .intern(db),
            );

            offset = offset.add_width(width);
        }
        res
    }

    pub fn span_start_without_trivia(&self, db: &dyn SyntaxGroup) -> TextOffset {
        let green_node = self.green_node(db);
        match green_node.details {
            green::GreenNodeDetails::Node { .. } => {
                if let Some(token_node) = self.get_terminal_token(db) {
                    return token_node.offset(db);
                }
                let children = self.get_children(db);
                if let Some(child) =
                    children.iter().find(|child| child.width(db) != TextWidth::default())
                {
                    child.span_start_without_trivia(db)
                } else {
                    self.offset(db)
                }
            }
            green::GreenNodeDetails::Token(_) => self.offset(db),
        }
    }
    pub fn span_end_without_trivia(&self, db: &dyn SyntaxGroup) -> TextOffset {
        let green_node = self.green_node(db);
        match green_node.details {
            green::GreenNodeDetails::Node { .. } => {
                if let Some(token_node) = self.get_terminal_token(db) {
                    return token_node.span(db).end;
                }
                if let Some(child) = self
                    .get_children(db)
                    .iter()
                    .filter(|child| child.width(db) != TextWidth::default())
                    .next_back()
                {
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
        for child in self.get_children(db).iter() {
            if child.offset(db).add_width(child.width(db)) > offset {
                return child.lookup_offset(db, offset);
            }
        }
        *self
    }

    /// Lookups a syntax node using a position.
    pub fn lookup_position(&self, db: &dyn SyntaxGroup, position: TextPosition) -> SyntaxNode {
        match position.offset_in_file(db, self.stable_ptr(db).file_id(db)) {
            Some(offset) => self.lookup_offset(db, offset),
            None => *self,
        }
    }

    /// Returns all the text under the syntax node.
    /// Note that this traverses the syntax tree, and generates a new string, so use responsibly.
    pub fn get_text(&self, db: &dyn SyntaxGroup) -> String {
        format!("{}", NodeTextFormatter { node: self, db })
    }

    /// Returns all the text under the syntax node.
    /// It traverses all the syntax tree of the node, but ignores functions and modules.
    /// We ignore those, because if there's some inner functions or modules, we don't want to get
    /// raw text of them. Comments inside them refer themselves directly, not this SyntaxNode.
    pub fn get_text_without_inner_commentable_children(&self, db: &dyn SyntaxGroup) -> String {
        let mut buffer = String::new();

        match &self.green_node(db).as_ref().details {
            green::GreenNodeDetails::Token(text) => buffer.push_str(text),
            green::GreenNodeDetails::Node { .. } => {
                for child in self.get_children(db).iter() {
                    let kind = child.kind(db);

                    // Checks all the items that the inner comment can be bubbled to (implementation
                    // function is also a FunctionWithBody).
                    if !matches!(
                        kind,
                        SyntaxKind::FunctionWithBody
                            | SyntaxKind::ItemModule
                            | SyntaxKind::TraitItemFunction
                    ) {
                        buffer.push_str(&SyntaxNode::get_text_without_inner_commentable_children(
                            child, db,
                        ));
                    }
                }
            }
        }
        buffer
    }

    /// Returns all the text of the item without comments trivia.
    /// It traverses all the syntax tree of the node.
    pub fn get_text_without_all_comment_trivia(&self, db: &dyn SyntaxGroup) -> String {
        let mut buffer = String::new();

        match &self.green_node(db).as_ref().details {
            green::GreenNodeDetails::Token(text) => buffer.push_str(text),
            green::GreenNodeDetails::Node { .. } => {
                for child in self.get_children(db).iter() {
                    if let Some(trivia) = ast::Trivia::cast(db, *child) {
                        trivia.elements(db).iter().for_each(|element| {
                            if !matches!(
                                element,
                                ast::Trivium::SingleLineComment(_)
                                    | ast::Trivium::SingleLineDocComment(_)
                                    | ast::Trivium::SingleLineInnerComment(_)
                            ) {
                                buffer.push_str(
                                    &element
                                        .as_syntax_node()
                                        .get_text_without_all_comment_trivia(db),
                                );
                            }
                        });
                    } else {
                        buffer
                            .push_str(&SyntaxNode::get_text_without_all_comment_trivia(child, db));
                    }
                }
            }
        }
        buffer
    }

    /// Returns all the text under the syntax node, without the outmost trivia (the leading trivia
    /// of the first token and the trailing trivia of the last token).
    ///
    /// Note that this traverses the syntax tree, and generates a new string, so use responsibly.
    pub fn get_text_without_trivia(self, db: &dyn SyntaxGroup) -> String {
        let trimmed_span = self.span_without_trivia(db);

        self.get_text_of_span(db, trimmed_span)
    }

    /// Returns the text under the syntax node, according to the given span.
    ///
    /// `span` is assumed to be contained within the span of self.
    ///
    /// Note that this traverses the syntax tree, and generates a new string, so use responsibly.
    pub fn get_text_of_span(self, db: &dyn SyntaxGroup, span: TextSpan) -> String {
        let orig_span = self.span(db);
        assert!(orig_span.contains(span));
        let full_text = self.get_text(db);

        let span_in_span = TextSpan {
            start: (span.start - orig_span.start).as_offset(),
            end: (span.end - orig_span.start).as_offset(),
        };
        span_in_span.take(&full_text).to_string()
    }

    /// Traverse the subtree rooted at the current node (including the current node) in preorder.
    ///
    /// This is a shortcut for [`Self::preorder`] paired with filtering for [`WalkEvent::Enter`]
    /// events only.
    pub fn descendants<'db>(
        &self,
        db: &'db dyn SyntaxGroup,
    ) -> impl Iterator<Item = SyntaxNode> + 'db {
        self.preorder(db).filter_map(|event| match event {
            WalkEvent::Enter(node) => Some(node),
            WalkEvent::Leave(_) => None,
        })
    }

    /// Traverse the subtree rooted at the current node (including the current node) in preorder,
    /// excluding tokens.
    pub fn preorder<'db>(&self, db: &'db dyn SyntaxGroup) -> Preorder<'db> {
        Preorder::new(*self, db)
    }

    /// Gets all the leaves of the SyntaxTree, where the self node is the root of a tree.
    pub fn tokens<'a>(&'a self, db: &'a dyn SyntaxGroup) -> impl Iterator<Item = Self> + 'a {
        self.preorder(db).filter_map(|event| match event {
            WalkEvent::Enter(node) if node.green_node(db).kind.is_terminal() => Some(node),
            _ => None,
        })
    }

    /// Mirror of [`TypedSyntaxNode::cast`].
    pub fn cast<T: TypedSyntaxNode>(self, db: &dyn SyntaxGroup) -> Option<T> {
        T::cast(db, self)
    }

    /// Creates an iterator that yields ancestors of this syntax node.
    pub fn ancestors<'a>(&self, db: &'a dyn SyntaxGroup) -> impl Iterator<Item = SyntaxNode> + 'a {
        // We aren't reusing `ancestors_with_self` here to avoid cloning this node.
        std::iter::successors(self.parent(db), |n| n.parent(db))
    }

    /// Creates an iterator that yields this syntax node and walks up its ancestors.
    pub fn ancestors_with_self<'a>(
        &self,
        db: &'a dyn SyntaxGroup,
    ) -> impl Iterator<Item = SyntaxNode> + 'a {
        std::iter::successors(Some(*self), |n| n.parent(db))
    }

    /// Checks whether this syntax node is strictly above the given syntax node in the syntax tree.
    pub fn is_ancestor(&self, db: &dyn SyntaxGroup, node: &SyntaxNode) -> bool {
        node.ancestors(db).any(|n| n == *self)
    }

    /// Checks whether this syntax node is strictly under the given syntax node in the syntax tree.
    pub fn is_descendant(&self, db: &dyn SyntaxGroup, node: &SyntaxNode) -> bool {
        node.is_ancestor(db, self)
    }

    /// Checks whether this syntax node is or is above the given syntax node in the syntax tree.
    pub fn is_ancestor_or_self(&self, db: &dyn SyntaxGroup, node: &SyntaxNode) -> bool {
        node.ancestors_with_self(db).any(|n| n == *self)
    }

    /// Checks whether this syntax node is or is under the given syntax node in the syntax tree.
    pub fn is_descendant_or_self(&self, db: &dyn SyntaxGroup, node: &SyntaxNode) -> bool {
        node.is_ancestor_or_self(db, self)
    }

    /// Finds the first ancestor of a given kind.
    pub fn ancestor_of_kind(&self, db: &dyn SyntaxGroup, kind: SyntaxKind) -> Option<SyntaxNode> {
        self.ancestors(db).find(|node| node.kind(db) == kind)
    }

    /// Finds the first ancestor of a given kind and returns it in typed form.
    pub fn ancestor_of_type<T: TypedSyntaxNode>(&self, db: &dyn SyntaxGroup) -> Option<T> {
        self.ancestors(db).find_map(|node| T::cast(db, node))
    }

    /// Finds the parent of a given kind.
    pub fn parent_of_kind(&self, db: &dyn SyntaxGroup, kind: SyntaxKind) -> Option<SyntaxNode> {
        self.parent(db).filter(|node| node.kind(db) == kind)
    }

    /// Finds the parent of a given kind and returns it in typed form.
    pub fn parent_of_type<T: TypedSyntaxNode>(&self, db: &dyn SyntaxGroup) -> Option<T> {
        self.parent(db).and_then(|node| T::cast(db, node))
    }

    /// Finds the first parent of one of the kinds.
    pub fn ancestor_of_kinds(
        &self,
        db: &dyn SyntaxGroup,
        kinds: &[SyntaxKind],
    ) -> Option<SyntaxNode> {
        self.ancestors(db).find(|node| kinds.contains(&node.kind(db)))
    }

    /// Gets the kind of the given node's parent if it exists.
    pub fn parent_kind(&self, db: &dyn SyntaxGroup) -> Option<SyntaxKind> {
        Some(self.parent(db)?.kind(db))
    }

    /// Gets the kind of the given node's grandparent if it exists.
    pub fn grandparent_kind(&self, db: &dyn SyntaxGroup) -> Option<SyntaxKind> {
        Some(self.parent(db)?.parent(db)?.kind(db))
    }

    /// Gets the kind of the given node's grandrandparent if it exists.
    pub fn grandgrandparent_kind(&self, db: &dyn SyntaxGroup) -> Option<SyntaxKind> {
        Some(self.parent(db)?.parent(db)?.parent(db)?.kind(db))
    }
}

/// Trait for the typed view of the syntax tree. All the internal node implementations are under
/// the ast module.
pub trait TypedSyntaxNode: Sized {
    /// The relevant SyntaxKind. None for enums.
    const OPTIONAL_KIND: Option<SyntaxKind>;
    type StablePtr: TypedStablePtr;
    type Green;
    fn missing(db: &dyn SyntaxGroup) -> Self::Green;
    fn from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self;
    fn cast(db: &dyn SyntaxGroup, node: SyntaxNode) -> Option<Self>;
    fn as_syntax_node(&self) -> SyntaxNode;
    fn stable_ptr(&self, db: &dyn SyntaxGroup) -> Self::StablePtr;
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
    /// Casts a syntax node to this terminal type's token and then walks up to return the terminal.
    fn cast_token(db: &dyn SyntaxGroup, node: SyntaxNode) -> Option<Self> {
        if node.kind(db) == Self::TokenType::OPTIONAL_KIND? {
            Some(Self::from_syntax_node(db, node.parent(db)?))
        } else {
            None
        }
    }
}

/// Trait for stable pointers to syntax nodes.
pub trait TypedStablePtr {
    type SyntaxNode: TypedSyntaxNode;
    /// Returns the syntax node pointed to by this stable pointer.
    fn lookup(&self, db: &dyn SyntaxGroup) -> Self::SyntaxNode;
    /// Returns the untyped stable pointer.
    fn untyped(&self) -> SyntaxStablePtrId;
}

/// Wrapper for formatting the text of syntax nodes.
pub struct NodeTextFormatter<'a> {
    /// The node to format.
    pub node: &'a SyntaxNode,
    /// The syntax db.
    pub db: &'a dyn SyntaxGroup,
}
impl Display for NodeTextFormatter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.node.green_node(self.db).as_ref().details {
            green::GreenNodeDetails::Token(text) => write!(f, "{text}")?,
            green::GreenNodeDetails::Node { .. } => {
                for child in self.node.get_children(self.db).iter() {
                    write!(f, "{}", NodeTextFormatter { node: child, db: self.db })?;
                }
            }
        }
        Ok(())
    }
}
