use pretty_assertions::assert_eq;

use super::ast::{
    ExprBinary, ExprLiteral, ExprPath, PathSegmentGreen, PathSegmentIdent, SyntaxFileGreen,
    Terminal, Trivia,
};
use super::db::SyntaxDatabase;
use super::kind::SyntaxKind;
use super::{SyntaxGroup, SyntaxNode, SyntaxNodeDetails};
use crate::{node, token};

#[salsa::database(SyntaxDatabase)]
#[derive(Default)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
impl salsa::Database for DatabaseForTesting {}

fn traverse(db: &dyn SyntaxGroup, node: SyntaxNode) -> Vec<(SyntaxNodeDetails, u32, u32)> {
    let mut res = vec![(node.details(db), node.offset().0 as u32, node.width(db))];
    for c in node.children(db) {
        res.append(&mut traverse(db, c));
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
            (SyntaxNodeDetails::Syntax(SyntaxKind::ExprBinary), 0, 7),
            (SyntaxNodeDetails::Syntax(SyntaxKind::ExprPath), 0, 4),
            (SyntaxNodeDetails::Syntax(SyntaxKind::PathSegmentIdent), 0, 4),
            (SyntaxNodeDetails::Syntax(SyntaxKind::Terminal), 0, 4),
            (SyntaxNodeDetails::Syntax(SyntaxKind::Trivia), 0, 0),
            (
                SyntaxNodeDetails::Token(token::Token {
                    kind: token::TokenKind::Identifier,
                    text: "foo".into()
                }),
                0,
                3
            ),
            (SyntaxNodeDetails::Syntax(SyntaxKind::Trivia), 3, 1),
            (
                SyntaxNodeDetails::Token(token::Token {
                    kind: token::TokenKind::Whitespace,
                    text: " ".into()
                }),
                3,
                1
            ),
            (SyntaxNodeDetails::Syntax(SyntaxKind::Terminal), 4, 2),
            (SyntaxNodeDetails::Syntax(SyntaxKind::Trivia), 4, 0),
            (
                SyntaxNodeDetails::Token(token::Token {
                    kind: token::TokenKind::Plus,
                    text: "+".into()
                }),
                4,
                1
            ),
            (SyntaxNodeDetails::Syntax(SyntaxKind::Trivia), 5, 1),
            (
                SyntaxNodeDetails::Token(token::Token {
                    kind: token::TokenKind::Whitespace,
                    text: " ".into()
                }),
                5,
                1
            ),
            (SyntaxNodeDetails::Syntax(SyntaxKind::ExprLiteral), 6, 1),
            (SyntaxNodeDetails::Syntax(SyntaxKind::Terminal), 6, 1),
            (SyntaxNodeDetails::Syntax(SyntaxKind::Trivia), 6, 0),
            (
                SyntaxNodeDetails::Token(token::Token {
                    kind: token::TokenKind::LiteralNumber,
                    text: "5".into()
                }),
                6,
                1
            ),
            (SyntaxNodeDetails::Syntax(SyntaxKind::Trivia), 7, 0)
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
    let tokens = vec![
        node::Token::new_green(db, token::TokenKind::Identifier, "foo".into()),
        node::Token::new_green(db, token::TokenKind::Whitespace, " ".into()),
        node::Token::new_green(db, token::TokenKind::Plus, "+".into()),
        node::Token::new_green(db, token::TokenKind::Whitespace, " ".into()),
        node::Token::new_green(db, token::TokenKind::LiteralNumber, "5".into()),
    ];
    assert_eq!(tokens[1], tokens[3]);
    let no_trivia = Trivia::new_green(db, vec![]);
    let triviums = vec![tokens[1], tokens[3]];
    assert_eq!(triviums[0], triviums[1]);
    let terminals = vec![
        Terminal::new_green(
            db,
            no_trivia,
            tokens[0],
            Trivia::new_green(db, vec![triviums[0].into()]),
        ),
        Terminal::new_green(
            db,
            no_trivia,
            tokens[2],
            Trivia::new_green(db, vec![triviums[1].into()]),
        ),
        Terminal::new_green(db, no_trivia, tokens[4], no_trivia),
    ];
    let expr = ExprBinary::new_green(
        db,
        ExprPath::new_green(
            db,
            vec![PathSegmentGreen::from(PathSegmentIdent::new_green(db, terminals[0])).into()],
        )
        .into(),
        terminals[1],
        ExprLiteral::new_green(db, terminals[2]).into(),
    );
    // SyntaxNode::new_root only accepts ast::SyntaxFileGreen, but we only have an expression.
    // This is a hack to crate a green id of "SyntaxFile" from "Expr".
    let root = SyntaxFileGreen(expr.0);
    SyntaxNode::new_root(db, root)
}
