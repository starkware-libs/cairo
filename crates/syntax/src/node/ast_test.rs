use super::kind::SyntaxKind;
use super::{ast::Empty, GreenInterner, SyntaxNode, SyntaxNodeKind};

use super::GreenDatabase;

#[salsa::database(GreenDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

fn traverse(db: &dyn GreenInterner, node: SyntaxNode) -> Vec<(SyntaxNodeKind, u32, u32)> {
    let mut res = vec![(node.kind(db), node.offset(), node.width(db))];
    for c in node.children(db) {
        res.append(&mut traverse(db, c));
    }
    res
}

#[test]
fn test_empty() {
    let db_val = DatabaseImpl::default();
    let db = &db_val;

    // TODO: Use a builder for easier construction of token.
    // Construct green nodes.
    let empty = Empty::new_green(db);

    let root = SyntaxNode::new_root(empty);
    assert_eq!(traverse(db, root), [(SyntaxNodeKind::Syntax(SyntaxKind::Empty), 0, 0),])
}
