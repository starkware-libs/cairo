use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextPosition;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::node::ast::TerminalIdentifier;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::Upcast;

// TODO(mkaput): Make this a real Salsa query group with sensible LRU.
/// Language server-specific extensions to the syntax group of the Cairo compiler.
pub trait LsSyntaxGroup: Upcast<dyn ParserGroup> {
    /// Finds the most specific [`SyntaxNode`] at the given [`TextPosition`] in file.
    fn find_syntax_node_at_position(
        &self,
        file: FileId,
        position: TextPosition,
    ) -> Option<SyntaxNode> {
        let db = self.upcast();
        Some(db.file_syntax(file).to_option()?.lookup_position(db.upcast(), position))
    }

    /// Finds a [`TerminalIdentifier`] at the given [`TextPosition`] in file.
    ///
    /// The lookup for identifiers is slightly more sophisticated than just looking for an arbitrary
    /// syntax node, because identifiers are usually what the user is interested in.
    /// In case when user position is `ident<caret>()`, while regular syntax node lookup would
    /// return the left paren, a much better UX would be to correct the lookup to the identifier.
    /// Such corrections are always valid and deterministic, because grammar-wise it is not possible
    /// to have two identifiers/keywords being glued to each other.
    fn find_identifier_at_position(
        &self,
        file: FileId,
        position: TextPosition,
    ) -> Option<TerminalIdentifier> {
        let db = self.upcast();
        let syntax_db = db.upcast();

        let find = |position: TextPosition| {
            let node = self.find_syntax_node_at_position(file, position)?;
            if node.kind(syntax_db) == SyntaxKind::TokenIdentifier {
                Some(TerminalIdentifier::from_syntax_node(syntax_db, node.parent()?))
            } else {
                None
            }
        };

        find(position).or_else(|| {
            // Try searching one character to the left.
            let col = position.col.checked_sub(1)?;
            find(TextPosition { col, ..position })
        })
    }
<<<<<<< HEAD

    /// Traverse tree in root direction.
    ///
    /// Finds first node with specified kind.
    /// Returns it's respective child that is the ancestor of `node`.
    fn first_ancestor_of_kind_respective_child(
        &self,
        mut node: SyntaxNode,
        kind: SyntaxKind,
    ) -> Option<SyntaxNode> {
        let db = self.upcast();
        let syntax_db = db.upcast();

        while let Some(parent) = node.parent() {
            if parent.kind(syntax_db) == kind {
                return Some(node);
            } else {
                node = parent;
            }
        }
        None
    }

    /// Finds first ancestor of a given kind.
    fn first_ancestor_of_kind(&self, node: SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
        self.first_ancestor_of_kind_respective_child(node, kind).and_then(|node| node.parent())
    }
||||||| 223ca9963
=======

    /// Finds first ancestor of a given kind.
    fn first_ancestor_of_kind(&self, mut node: SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
        let db = self.upcast();
        let syntax_db = db.upcast();

        while let Some(parent) = node.parent() {
            if parent.kind(syntax_db) == kind {
                return Some(parent);
            } else {
                node = parent;
            }
        }
        None
    }
>>>>>>> origin/dev-v2.7.1
}

impl<T> LsSyntaxGroup for T where T: Upcast<dyn ParserGroup> + ?Sized {}
