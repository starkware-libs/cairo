use core::hash::Hash;
use std::fmt::Display;
use std::sync::Arc;

use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::{TextOffset, TextPosition, TextSpan, TextWidth};
use cairo_lang_utils::{Intern, LookupIntern, require};
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
pub mod utils;
pub mod with_db;

#[cfg(test)]
mod ast_test;
#[cfg(test)]
mod test_utils;

/// SyntaxNode. Untyped view of the syntax tree. Adds parent() and offset() capabilities.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SyntaxNode(Arc<SyntaxNodeInner>);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct SyntaxNodeInner {
    green: GreenId,
    /// Number of characters from the beginning of the file to the start of the span of this
    /// syntax subtree.
    offset: TextOffset,
    parent: Option<SyntaxNode>,
    stable_ptr: SyntaxStablePtrId,
}
impl SyntaxNode {
    pub fn new_root(db: &dyn SyntaxGroup, file_id: FileId, green: GreenId) -> Self {
        let inner = SyntaxNodeInner {
            green,
            offset: TextOffset::START,
            parent: None,
            stable_ptr: SyntaxStablePtr::Root(file_id, green).intern(db),
        };
        Self(Arc::new(inner))
    }

    pub fn new_root_with_offset(
        db: &dyn SyntaxGroup,
        file_id: FileId,
        green: GreenId,
        initial_offset: Option<TextOffset>,
    ) -> Self {
        let inner = SyntaxNodeInner {
            green,
            offset: initial_offset.unwrap_or_default(),
            parent: None,
            stable_ptr: SyntaxStablePtr::Root(file_id, green).intern(db),
        };
        Self(Arc::new(inner))
    }

    pub fn offset(&self) -> TextOffset {
        self.0.offset
    }
    pub fn width(&self, db: &dyn SyntaxGroup) -> TextWidth {
        self.green_node(db).width()
    }
    pub fn kind(&self, db: &dyn SyntaxGroup) -> SyntaxKind {
        self.green_node(db).kind
    }
    pub fn span(&self, db: &dyn SyntaxGroup) -> TextSpan {
        let start = self.offset();
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
        self.0.green.lookup_intern(db)
    }
    pub fn span_without_trivia(&self, db: &dyn SyntaxGroup) -> TextSpan {
        let start = self.span_start_without_trivia(db);
        let end = self.span_end_without_trivia(db);
        TextSpan { start, end }
    }
    pub fn parent(&self) -> Option<SyntaxNode> {
        self.0.parent.as_ref().cloned()
    }
    pub fn stable_ptr(&self) -> SyntaxStablePtrId {
        self.0.stable_ptr
    }

    /// Gets the inner token from a terminal SyntaxNode. If the given node is not a terminal,
    /// returns None.
    pub fn get_terminal_token(&self, db: &dyn SyntaxGroup) -> Option<SyntaxNode> {
        let green_node = self.green_node(db);
        require(green_node.kind.is_terminal())?;
        // At this point we know we should have a second child which is the token.
        let token_node = db.get_children(self.clone())[1].clone();
        Some(token_node)
    }

    pub fn span_start_without_trivia(&self, db: &dyn SyntaxGroup) -> TextOffset {
        let green_node = self.green_node(db);
        match green_node.details {
            green::GreenNodeDetails::Node { .. } => {
                if let Some(token_node) = self.get_terminal_token(db) {
                    return token_node.offset();
                }
                let children = db.get_children(self.clone());
                if let Some(child) =
                    children.iter().find(|child| child.width(db) != TextWidth::default())
                {
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
                let children = &mut db.get_children(self.clone());
                if let Some(child) = children
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
        for child in db.get_children(self.clone()).iter() {
            if child.offset().add_width(child.width(db)) > offset {
                return child.lookup_offset(db, offset);
            }
        }
        self.clone()
    }

    /// Lookups a syntax node using a position.
    pub fn lookup_position(&self, db: &dyn SyntaxGroup, position: TextPosition) -> SyntaxNode {
        match position.offset_in_file(db.upcast(), self.stable_ptr().file_id(db)) {
            Some(offset) => self.lookup_offset(db, offset),
            None => self.clone(),
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
                for child in db.get_children(self.clone()).iter() {
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
                for child in db.get_children(self.clone()).iter() {
                    if let Some(trivia) = ast::Trivia::cast(db, child.clone()) {
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
        Preorder::new(self.clone(), db)
    }

    /// Gets all the leaves of the SyntaxTree, where the self node is the root of a tree.
    pub fn tokens<'a>(&'a self, db: &'a dyn SyntaxGroup) -> impl Iterator<Item = Self> + 'a {
        self.preorder(db).filter_map(|event| match event {
            WalkEvent::Enter(node) if node.green_node(db).kind.is_terminal() => Some(node),
            _ => None,
        })
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
    /// Casts a syntax node to this terminal type's token and then walks up to return the terminal.
    fn cast_token(db: &dyn SyntaxGroup, node: SyntaxNode) -> Option<Self> {
        if node.kind(db) == Self::TokenType::OPTIONAL_KIND? {
            Some(Self::from_syntax_node(db, node.parent()?))
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
                for child in self.db.get_children(self.node.clone()).iter() {
                    write!(f, "{}", NodeTextFormatter { node: child, db: self.db })?;
                }
            }
        }
        Ok(())
    }
}
