use core::hash::Hash;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, SmolStrId};
use cairo_lang_filesystem::span::{TextOffset, TextPosition, TextSpan, TextWidth};
use cairo_lang_utils::{Intern, define_short_id, require};
use salsa::Database;
use vector_map::VecMap;

use self::ast::TriviaGreen;
use self::green::GreenNode;
use self::ids::{GreenId, SyntaxStablePtrId};
use self::kind::SyntaxKind;
use self::stable_ptr::SyntaxStablePtr;
use crate::node::db::SyntaxGroup;
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
pub struct SyntaxNodeLongId<'a> {
    green: GreenId<'a>,
    /// Number of characters from the beginning of the file to the start of the span of this
    /// syntax subtree.
    offset: TextOffset,
    parent: Option<SyntaxNode<'a>>,
    stable_ptr: SyntaxStablePtrId<'a>,
}
define_short_id!(SyntaxNode, SyntaxNodeLongId<'db>);
impl<'a> SyntaxNode<'a> {
    pub fn new_root(db: &'a dyn Database, file_id: FileId<'a>, green: GreenId<'a>) -> Self {
        Self::new_with_inner(
            db,
            green,
            TextOffset::START,
            None,
            SyntaxStablePtr::Root(file_id, green).intern(db),
        )
    }

    pub fn new_root_with_offset(
        db: &'a dyn Database,
        file_id: FileId<'a>,
        green: GreenId<'a>,
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
        db: &'a dyn Database,
        green: GreenId<'a>,
        offset: TextOffset,
        parent: Option<SyntaxNode<'a>>,
        stable_ptr: SyntaxStablePtrId<'a>,
    ) -> Self {
        SyntaxNodeLongId { green, offset, parent, stable_ptr }.intern(db)
    }

    pub fn offset(&self, db: &dyn Database) -> TextOffset {
        self.long(db).offset
    }
    pub fn width(&self, db: &dyn Database) -> TextWidth {
        self.green_node(db).width(db)
    }
    pub fn kind(&self, db: &dyn Database) -> SyntaxKind {
        self.green_node(db).kind
    }
    pub fn span(&self, db: &dyn Database) -> TextSpan {
        TextSpan::new_with_width(self.offset(db), self.width(db))
    }
    /// Returns the text of the token if this node is a token.
    pub fn text(&self, db: &'a dyn Database) -> Option<SmolStrId<'a>> {
        match &self.green_node(db).details {
            green::GreenNodeDetails::Token(text) => Some(*text),
            green::GreenNodeDetails::Node { .. } => None,
        }
    }

    /// Returns the green node of the syntax node.
    pub fn green_node(&self, db: &'a dyn Database) -> &'a GreenNode<'a> {
        self.long(db).green.long(db)
    }

    /// Returns the span of the syntax node without trivia.
    pub fn span_without_trivia(&self, db: &dyn Database) -> TextSpan {
        let node = self.long(db);
        let green_node = node.green.long(db);
        let (leading, trailing) = both_trivia_width(db, green_node);
        let start = node.offset.add_width(leading);
        let end = node.offset.add_width(green_node.width(db)).sub_width(trailing);
        TextSpan::new(start, end)
    }
    pub fn parent(&self, db: &'a dyn Database) -> Option<SyntaxNode<'a>> {
        self.long(db).parent
    }
    pub fn stable_ptr(&self, db: &'a dyn Database) -> SyntaxStablePtrId<'a> {
        self.long(db).stable_ptr
    }

    /// Gets the inner token from a terminal SyntaxNode. If the given node is not a terminal,
    /// returns None.
    pub fn get_terminal_token(&'a self, db: &'a dyn Database) -> Option<SyntaxNode<'a>> {
        let green_node = self.green_node(db);
        require(green_node.kind.is_terminal())?;
        // At this point we know we should have a second child which is the token.
        self.get_children(db).get(1).copied()
    }

    /// Gets the children syntax nodes of the current node.
    pub fn get_children(&self, db: &'a dyn Database) -> &'a [SyntaxNode<'a>] {
        db.get_children(*self)
    }

    /// Implementation of [SyntaxNode::get_children].
    fn get_children_impl(&self, db: &'a dyn Database) -> Vec<SyntaxNode<'a>> {
        let self_long_id = self.long(db);
        let mut offset = self_long_id.offset;
        let self_green = self_long_id.green.long(db);
        let children = self_green.children();
        let mut res: Vec<SyntaxNode<'_>> = Vec::with_capacity(children.len());
        let mut key_map = VecMap::<_, usize>::new();
        for green_id in children {
            let green = green_id.long(db);
            let width = green.width(db);
            let kind = green.kind;
            let rng = key_fields::key_fields_range(kind);
            let key_fields: &'a [GreenId<'a>] = &green.children()[rng];
            let key_count = key_map.entry((kind, key_fields)).or_insert(0);
            let stable_ptr = SyntaxStablePtr::Child {
                parent: self_long_id.stable_ptr,
                kind,
                key_fields: key_fields.into(),
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

    /// Returns the start of the span of the syntax node without trivia.
    pub fn span_start_without_trivia(&self, db: &dyn Database) -> TextOffset {
        let node = self.long(db);
        let green_node = node.green.long(db);
        let leading = leading_trivia_width(db, green_node);
        node.offset.add_width(leading)
    }

    /// Returns the end of the span of the syntax node without trivia.
    pub fn span_end_without_trivia(&self, db: &dyn Database) -> TextOffset {
        let node = self.long(db);
        let green_node = node.green.long(db);
        let trailing = trailing_trivia_width(db, green_node);
        node.offset.add_width(green_node.width(db)).sub_width(trailing)
    }

    /// Lookups a syntax node using an offset.
    pub fn lookup_offset(&self, db: &'a dyn Database, offset: TextOffset) -> SyntaxNode<'a> {
        for child in self.get_children(db).iter() {
            if child.offset(db).add_width(child.width(db)) > offset {
                return child.lookup_offset(db, offset);
            }
        }
        *self
    }

    /// Lookups a syntax node using a position.
    pub fn lookup_position(&self, db: &'a dyn Database, position: TextPosition) -> SyntaxNode<'a> {
        match position.offset_in_file(db, self.stable_ptr(db).file_id(db)) {
            Some(offset) => self.lookup_offset(db, offset),
            None => *self,
        }
    }

    /// Returns all the text under the syntax node.
    pub fn get_text(&self, db: &'a dyn Database) -> &'a str {
        // A `None` return from reading the file content is only expected in the case of an IO
        // error. Since a SyntaxNode exists and is being processed, we should have already
        // successfully accessed this file earlier, therefore it should never fail.
        let file_content =
            db.file_content(self.stable_ptr(db).file_id(db)).expect("Failed to read file content");

        self.span(db).take(file_content)
    }

    /// Returns all the text under the syntax node.
    /// It traverses all the syntax tree of the node, but ignores functions and modules.
    /// We ignore those, because if there's some inner functions or modules, we don't want to get
    /// raw text of them. Comments inside them refer themselves directly, not this SyntaxNode.
    pub fn get_text_without_inner_commentable_children(&self, db: &dyn Database) -> String {
        let mut buffer = String::new();

        match &self.green_node(db).details {
            green::GreenNodeDetails::Token(text) => buffer.push_str(text.long(db)),
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
    pub fn get_text_without_all_comment_trivia(&self, db: &dyn Database) -> String {
        let mut buffer = String::new();

        match &self.green_node(db).details {
            green::GreenNodeDetails::Token(text) => buffer.push_str(text.long(db)),
            green::GreenNodeDetails::Node { .. } => {
                for child in self.get_children(db).iter() {
                    if let Some(trivia) = ast::Trivia::cast(db, *child) {
                        trivia.elements(db).for_each(|element| {
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
    pub fn get_text_without_trivia(self, db: &'a dyn Database) -> SmolStrId<'a> {
        let file_content =
            db.file_content(self.stable_ptr(db).file_id(db)).expect("Failed to read file content");
        SmolStrId::from(db, self.span_without_trivia(db).take(file_content))
    }

    /// Returns the text under the syntax node, according to the given span.
    ///
    /// `span` is assumed to be contained within the span of self.
    ///
    /// Note that this traverses the syntax tree, and generates a new string, so use responsibly.
    pub fn get_text_of_span(self, db: &'a dyn Database, span: TextSpan) -> &'a str {
        assert!(self.span(db).contains(span));
        let file_content =
            db.file_content(self.stable_ptr(db).file_id(db)).expect("Failed to read file content");
        span.take(file_content)
    }

    /// Traverse the subtree rooted at the current node (including the current node) in preorder.
    ///
    /// This is a shortcut for [`Self::preorder`] paired with filtering for [`WalkEvent::Enter`]
    /// events only.
    pub fn descendants(&self, db: &'a dyn Database) -> impl Iterator<Item = SyntaxNode<'a>> + 'a {
        self.preorder(db).filter_map(|event| match event {
            WalkEvent::Enter(node) => Some(node),
            WalkEvent::Leave(_) => None,
        })
    }

    /// Traverse the subtree rooted at the current node (including the current node) in preorder,
    /// excluding tokens.
    pub fn preorder(&self, db: &'a dyn Database) -> Preorder<'a> {
        Preorder::new(*self, db)
    }

    /// Gets all the leaves of the SyntaxTree, where the self node is the root of a tree.
    pub fn tokens(&self, db: &'a dyn Database) -> impl Iterator<Item = Self> + 'a {
        self.preorder(db).filter_map(|event| match event {
            WalkEvent::Enter(node) if node.green_node(db).kind.is_terminal() => Some(node),
            _ => None,
        })
    }

    /// Mirror of [`TypedSyntaxNode::cast`].
    pub fn cast<T: TypedSyntaxNode<'a>>(self, db: &'a dyn Database) -> Option<T> {
        T::cast(db, self)
    }

    /// Creates an iterator that yields ancestors of this syntax node.
    pub fn ancestors(&self, db: &'a dyn Database) -> impl Iterator<Item = SyntaxNode<'a>> + 'a {
        // We aren't reusing `ancestors_with_self` here to avoid cloning this node.
        std::iter::successors(self.parent(db), |n| n.parent(db))
    }

    /// Creates an iterator that yields this syntax node and walks up its ancestors.
    pub fn ancestors_with_self(
        &self,
        db: &'a dyn Database,
    ) -> impl Iterator<Item = SyntaxNode<'a>> + 'a {
        std::iter::successors(Some(*self), |n| n.parent(db))
    }

    /// Checks whether this syntax node is strictly above the given syntax node in the syntax tree.
    pub fn is_ancestor(&self, db: &dyn Database, node: &SyntaxNode<'_>) -> bool {
        node.ancestors(db).any(|n| n == *self)
    }

    /// Checks whether this syntax node is strictly under the given syntax node in the syntax tree.
    pub fn is_descendant(&self, db: &dyn Database, node: &SyntaxNode<'_>) -> bool {
        node.is_ancestor(db, self)
    }

    /// Checks whether this syntax node is or is above the given syntax node in the syntax tree.
    pub fn is_ancestor_or_self(&self, db: &dyn Database, node: &SyntaxNode<'_>) -> bool {
        node.ancestors_with_self(db).any(|n| n == *self)
    }

    /// Checks whether this syntax node is or is under the given syntax node in the syntax tree.
    pub fn is_descendant_or_self(&self, db: &dyn Database, node: &SyntaxNode<'_>) -> bool {
        node.is_ancestor_or_self(db, self)
    }

    /// Finds the first ancestor of a given kind.
    pub fn ancestor_of_kind(
        &self,
        db: &'a dyn Database,
        kind: SyntaxKind,
    ) -> Option<SyntaxNode<'a>> {
        self.ancestors(db).find(|node| node.kind(db) == kind)
    }

    /// Finds the first ancestor of a given kind and returns it in typed form.
    pub fn ancestor_of_type<T: TypedSyntaxNode<'a>>(&self, db: &'a dyn Database) -> Option<T> {
        self.ancestors(db).find_map(|node| T::cast(db, node))
    }

    /// Finds the parent of a given kind.
    pub fn parent_of_kind(&self, db: &'a dyn Database, kind: SyntaxKind) -> Option<SyntaxNode<'a>> {
        self.parent(db).filter(|node| node.kind(db) == kind)
    }

    /// Finds the parent of a given kind and returns it in typed form.
    pub fn parent_of_type<T: TypedSyntaxNode<'a>>(&self, db: &'a dyn Database) -> Option<T> {
        self.parent(db).and_then(|node| T::cast(db, node))
    }

    /// Finds the first parent of one of the kinds.
    pub fn ancestor_of_kinds(
        &self,
        db: &'a dyn Database,
        kinds: &[SyntaxKind],
    ) -> Option<SyntaxNode<'a>> {
        self.ancestors(db).find(|node| kinds.contains(&node.kind(db)))
    }

    /// Gets the kind of the given node's parent if it exists.
    pub fn parent_kind(&self, db: &dyn Database) -> Option<SyntaxKind> {
        Some(self.parent(db)?.kind(db))
    }

    /// Gets the kind of the given node's grandparent if it exists.
    pub fn grandparent_kind(&self, db: &dyn Database) -> Option<SyntaxKind> {
        Some(self.parent(db)?.parent(db)?.kind(db))
    }

    /// Gets the kind of the given node's grandgrandparent if it exists.
    pub fn grandgrandparent_kind(&self, db: &dyn Database) -> Option<SyntaxKind> {
        Some(self.parent(db)?.parent(db)?.parent(db)?.kind(db))
    }
}

/// Trait for the typed view of the syntax tree. All the internal node implementations are under
/// the ast module.
pub trait TypedSyntaxNode<'a>: Sized {
    /// The relevant SyntaxKind. None for enums.
    const OPTIONAL_KIND: Option<SyntaxKind>;
    type StablePtr: TypedStablePtr<'a>;
    type Green;
    fn missing(db: &'a dyn Database) -> Self::Green;
    fn from_syntax_node(db: &'a dyn Database, node: SyntaxNode<'a>) -> Self;
    fn cast(db: &'a dyn Database, node: SyntaxNode<'a>) -> Option<Self>;
    fn as_syntax_node(&self) -> SyntaxNode<'a>;
    fn stable_ptr(&self, db: &'a dyn Database) -> Self::StablePtr;
}

pub trait Token<'a>: TypedSyntaxNode<'a> {
    fn new_green(db: &'a dyn Database, text: SmolStrId<'a>) -> Self::Green;
    fn text(&self, db: &'a dyn Database) -> SmolStrId<'a>;
}

pub trait Terminal<'a>: TypedSyntaxNode<'a> {
    const KIND: SyntaxKind;
    type TokenType: Token<'a>;
    fn new_green(
        db: &'a dyn Database,
        leading_trivia: TriviaGreen<'a>,
        token: <<Self as Terminal<'a>>::TokenType as TypedSyntaxNode<'a>>::Green,
        trailing_trivia: TriviaGreen<'a>,
    ) -> <Self as TypedSyntaxNode<'a>>::Green;
    /// Returns the text of the token of this terminal (excluding the trivia).
    fn text(&self, db: &'a dyn Database) -> SmolStrId<'a>;
    /// Casts a syntax node to this terminal type's token and then walks up to return the terminal.
    fn cast_token(db: &'a dyn Database, node: SyntaxNode<'a>) -> Option<Self> {
        if node.kind(db) == Self::TokenType::OPTIONAL_KIND? {
            Some(Self::from_syntax_node(db, node.parent(db)?))
        } else {
            None
        }
    }
}

/// Trait for stable pointers to syntax nodes.
pub trait TypedStablePtr<'a> {
    type SyntaxNode: TypedSyntaxNode<'a>;
    /// Returns the syntax node pointed to by this stable pointer.
    fn lookup(&self, db: &'a dyn Database) -> Self::SyntaxNode;
    /// Returns the untyped stable pointer.
    fn untyped(self) -> SyntaxStablePtrId<'a>;
}

/// Returns the width of the leading trivia of the given green node.
fn leading_trivia_width<'a>(db: &'a dyn Database, green: &GreenNode<'a>) -> TextWidth {
    match &green.details {
        green::GreenNodeDetails::Token(_) => TextWidth::default(),
        green::GreenNodeDetails::Node { children, width } => {
            if *width == TextWidth::default() {
                return TextWidth::default();
            }
            if green.kind.is_terminal() {
                return children[0].long(db).width(db);
            }
            let non_empty = find_non_empty_child(db, &mut children.iter())
                .expect("Parent width non-empty - one of the children should be non-empty");
            leading_trivia_width(db, non_empty)
        }
    }
}

/// Returns the width of the trailing trivia of the given green node.
fn trailing_trivia_width<'a>(db: &'a dyn Database, green: &GreenNode<'a>) -> TextWidth {
    match &green.details {
        green::GreenNodeDetails::Token(_) => TextWidth::default(),
        green::GreenNodeDetails::Node { children, width } => {
            if *width == TextWidth::default() {
                return TextWidth::default();
            }
            if green.kind.is_terminal() {
                return children[2].long(db).width(db);
            }
            let non_empty = find_non_empty_child(db, &mut children.iter().rev())
                .expect("Parent width non-empty - one of the children should be non-empty");
            trailing_trivia_width(db, non_empty)
        }
    }
}

/// Returns the width of the leading and trailing trivia of the given green node.
fn both_trivia_width<'a>(db: &'a dyn Database, green: &GreenNode<'a>) -> (TextWidth, TextWidth) {
    match &green.details {
        green::GreenNodeDetails::Token(_) => (TextWidth::default(), TextWidth::default()),
        green::GreenNodeDetails::Node { children, width } => {
            if *width == TextWidth::default() {
                return (TextWidth::default(), TextWidth::default());
            }
            if green.kind.is_terminal() {
                return (children[0].long(db).width(db), children[2].long(db).width(db));
            }
            let mut iter = children.iter();
            let first_non_empty = find_non_empty_child(db, &mut iter)
                .expect("Parent width non-empty - one of the children should be non-empty");
            if let Some(last_non_empty) = find_non_empty_child(db, &mut iter.rev()) {
                (
                    leading_trivia_width(db, first_non_empty),
                    trailing_trivia_width(db, last_non_empty),
                )
            } else {
                both_trivia_width(db, first_non_empty)
            }
        }
    }
}

/// Finds the first non-empty child in the given iterator.
fn find_non_empty_child<'a>(
    db: &'a dyn Database,
    child_iter: &mut impl Iterator<Item = &'a GreenId<'a>>,
) -> Option<&'a GreenNode<'a>> {
    child_iter.find_map(|child| {
        let child = child.long(db);
        (child.width(db) != TextWidth::default()).then_some(child)
    })
}
