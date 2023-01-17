use super::db::SyntaxGroup;
use super::kind::SyntaxKind;
use super::SyntaxNode;

// TODO(yg): doc.
pub fn is_parent_of_kind(db: &dyn SyntaxGroup, node: &SyntaxNode, kind: SyntaxKind) -> bool {
    let Some(parent) = node.parent() else {
        return false;
    };
    parent.kind(db) == kind
}

// TODO(yg): doc.
pub fn is_grandparent_of_kind(db: &dyn SyntaxGroup, node: &SyntaxNode, kind: SyntaxKind) -> bool {
    let Some(parent) = node.parent() else {
        return false;
    };
    let Some(grandparent) = parent.parent() else {
        return false;
    };
    grandparent.kind(db) == kind
}
