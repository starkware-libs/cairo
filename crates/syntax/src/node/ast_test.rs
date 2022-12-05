use db_utils::Upcast;
use filesystem::db::{FilesDatabase, FilesGroup};
use pretty_assertions::assert_eq;
use smol_str::SmolStr;
use test_log::test;

use super::ast::{
    ExprBinary, ExprPath, PathSegmentGreen, PathSegmentSimple, SyntaxFileGreen, TerminalIdentifier,
    TerminalLiteralNumber, TerminalPlus, TokenIdentifier, TokenLiteralNumber, TokenPlus,
    TokenWhitespace, Trivia,
};
use super::db::SyntaxDatabase;
use super::kind::SyntaxKind;
use super::{SyntaxGroup, SyntaxNode, Terminal, Token};

#[salsa::database(SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
impl salsa::Database for DatabaseForTesting {}
impl Upcast<dyn FilesGroup> for DatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}

fn traverse(
    db: &dyn SyntaxGroup,
    node: SyntaxNode,
) -> Vec<(SyntaxKind, Option<SmolStr>, u32, u32)> {
    let mut res = vec![(node.kind(db), node.text(db), node.offset().0 as u32, node.width(db))];
    for child in node.children(db) {
        res.append(&mut traverse(db, child));
    }
    res
}

#[test]
fn test_ast() {
    let db_val = DatabaseForTesting::default();
    let db = &db_val;
    let root = setup(db);

    assert_eq!(
        traverse(db, root),
        [
            (SyntaxKind::ExprBinary, None, 0, 7),
            (SyntaxKind::ExprPath, None, 0, 4),
            (SyntaxKind::PathSegmentSimple, None, 0, 4),
            (SyntaxKind::TerminalIdentifier, None, 0, 4),
            (SyntaxKind::Trivia, None, 0, 0),
            (SyntaxKind::TokenIdentifier, Some("foo".into()), 0, 3),
            (SyntaxKind::Trivia, None, 3, 1),
            (SyntaxKind::TokenWhitespace, Some(" ".into()), 3, 1),
            (SyntaxKind::TerminalPlus, None, 4, 2),
            (SyntaxKind::Trivia, None, 4, 0),
            (SyntaxKind::TokenPlus, Some("+".into()), 4, 1),
            (SyntaxKind::Trivia, None, 5, 1),
            (SyntaxKind::TokenWhitespace, Some(" ".into()), 5, 1),
            (SyntaxKind::TerminalLiteralNumber, None, 6, 1),
            (SyntaxKind::Trivia, None, 6, 0),
            (SyntaxKind::TokenLiteralNumber, Some("5".into()), 6, 1),
            (SyntaxKind::Trivia, None, 7, 0)
        ]
    )
}

#[test]
fn test_stable_ptr() {
    let db_val = DatabaseForTesting::default();
    let db = &db_val;
    let root = setup(db);
    traverse_and_verify_ptr(db, &root, root.clone());
}
fn traverse_and_verify_ptr(db: &dyn SyntaxGroup, root: &SyntaxNode, node: SyntaxNode) {
    let ptr = node.stable_ptr();
    let looked_up_node = root.lookup_ptr(db, ptr);
    assert_eq!(node, looked_up_node);
    for c in node.children(db) {
        traverse_and_verify_ptr(db, root, c);
    }
}

fn setup(db: &DatabaseForTesting) -> SyntaxNode {
    // TODO: Use a builder for easier construction of token.
    // Construct green nodes.
    let token_foo = TokenIdentifier::new_green(db, "foo".into());
    let token_whitespace1 = TokenWhitespace::new_green(db, " ".into());
    let token_plus = TokenPlus::new_green(db, "+".into());
    let token_whitespace2 = TokenWhitespace::new_green(db, " ".into());
    let token5 = TokenLiteralNumber::new_green(db, "5".into());
    assert_eq!(token_whitespace1, token_whitespace2);
    let no_trivia = Trivia::new_green(db, vec![]);
    let triviums = vec![token_whitespace1, token_whitespace2];
    assert_eq!(triviums[0], triviums[1]);
    let terminal_foo = TerminalIdentifier::new_green(
        db,
        no_trivia,
        token_foo,
        Trivia::new_green(db, vec![triviums[0].into()]),
    );
    let terminal_plus = TerminalPlus::new_green(
        db,
        no_trivia,
        token_plus,
        Trivia::new_green(db, vec![triviums[1].into()]),
    );
    let terminal5 = TerminalLiteralNumber::new_green(db, no_trivia, token5, no_trivia);
    let expr = ExprBinary::new_green(
        db,
        ExprPath::new_green(
            db,
            vec![PathSegmentGreen::from(PathSegmentSimple::new_green(db, terminal_foo)).into()],
        )
        .into(),
        terminal_plus.into(),
        terminal5.into(),
    );
    // SyntaxNode::new_root only accepts ast::SyntaxFileGreen, but we only have an expression.
    // This is a hack to crate a green id of "SyntaxFile" from "Expr".
    let root = SyntaxFileGreen(expr.0);
    SyntaxNode::new_root(db, root)
}
