use super::ids::{GreenId, SyntaxStablePtrId};
use super::kind::SyntaxKind;

/// Stable pointer to a node in the syntax tree.
/// Has enough information to uniquely define a node in the AST, given the tree.
/// Has undefined behavior when used with the wrong tree.
/// This is not a real pointer in the low-level sense, just a representation of the path from the
/// root to the node.
/// Stable means that when the AST is changed, pointers of unchanged items tend to stay the same.
/// For example, if a function is changed, the pointer of an unrelated function in the AST should
/// remain the same, as much as possible.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SyntaxStablePtr {
    /// The root node of the tree.
    Root,
    /// A child node.
    Child {
        /// The parent of the node.
        parent: SyntaxStablePtrId,
        /// The SyntaxKind of the node.
        kind: SyntaxKind,
        /// A list of field values for this node, to index by.
        /// Which fields are used is determined by each SyntaxKind.
        /// For example, a function item might use the name of the function.
        key_fields: Vec<GreenId>,
        /// Chronological index among all nodes with the same (parent, kind, key_fields).
        index: usize,
    },
}
