use super::db::SyntaxGroup;
use super::kind::SyntaxKind;
use super::SyntaxNode;

/// Checks whether the given node has a parent of the given kind.
pub fn is_parent_of_kind(db: &dyn SyntaxGroup, node: &SyntaxNode, kind: SyntaxKind) -> bool {
    let Some(parent) = node.parent() else {
        return false;
    };
    parent.kind(db) == kind
}

/// Checks whether the given node has a grandparent of the given kind.
pub fn is_grandparent_of_kind(db: &dyn SyntaxGroup, node: &SyntaxNode, kind: SyntaxKind) -> bool {
    let Some(parent) = node.parent() else {
        return false;
    };
    let Some(grandparent) = parent.parent() else {
        return false;
    };
    grandparent.kind(db) == kind
}

/// Gets the kind of the parent of the given node, if it exists.
pub fn parent_kind(db: &dyn SyntaxGroup, syntax_node: &SyntaxNode) -> Option<SyntaxKind> {
    Some(syntax_node.parent()?.kind(db))
}

/// Gets the kind of the grandparent of the given node, if it exists.
pub fn grandparent_kind(db: &dyn SyntaxGroup, syntax_node: &SyntaxNode) -> Option<SyntaxKind> {
    Some(syntax_node.parent()?.parent()?.kind(db))
}
