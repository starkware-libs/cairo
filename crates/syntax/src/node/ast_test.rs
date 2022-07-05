use super::ast::{
    ExprBinary, ExprLiteral, ExprPath, Identifier, PathSegment, Terminal, Trivia, TriviumWhitespace,
};
use super::kind::SyntaxKind;
use super::{ast::Empty, GreenInterner, SyntaxNode, SyntaxNodeKind};
use crate::{
    node::SyntaxToken,
    token::{Token, TokenKind},
};

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

    let empty = Empty::new_green(db);

    let root = SyntaxNode::new_root(empty);
    assert_eq!(traverse(db, root), [(SyntaxNodeKind::Syntax(SyntaxKind::Empty), 0, 0),])
}

#[test]
fn test_ast() {
    let db_val = DatabaseImpl::default();
    let db = &db_val;

    // TODO: Use a builder for easier construction of token.
    // Construct green nodes.
    let empty = Empty::new_green(db);
    let tokens = vec![
        SyntaxToken::new_green(db, TokenKind::Identifier, "foo".into()),
        SyntaxToken::new_green(db, TokenKind::Whitespace, " ".into()),
        SyntaxToken::new_green(db, TokenKind::Plus, "+".into()),
        SyntaxToken::new_green(db, TokenKind::Whitespace, " ".into()),
        SyntaxToken::new_green(db, TokenKind::LiteralNumber, "5".into()),
    ];
    assert_eq!(tokens[1], tokens[3]);
    let no_trivia = Trivia::new_green(db, vec![]);
    let triviums = vec![
        TriviumWhitespace::new_green(db, tokens[1]),
        TriviumWhitespace::new_green(db, tokens[3]),
    ];
    assert_eq!(triviums[0], triviums[1]);
    let terminals = vec![
        Terminal::new_green(db, no_trivia, tokens[0], Trivia::new_green(db, vec![triviums[0]])),
        Terminal::new_green(db, no_trivia, tokens[2], Trivia::new_green(db, vec![triviums[1]])),
        Terminal::new_green(db, no_trivia, tokens[4], no_trivia),
    ];

    let expr = ExprBinary::new_green(
        db,
        ExprPath::new_green(
            db,
            vec![PathSegment::new_green(db, Identifier::new_green(db, terminals[0]), empty)],
        ),
        terminals[1],
        ExprLiteral::new_green(db, terminals[2]),
    );

    let root = SyntaxNode::new_root(expr);
    assert_eq!(
        traverse(db, root),
        [
            (SyntaxNodeKind::Syntax(SyntaxKind::ExprBinary), 0, 7),
            (SyntaxNodeKind::Syntax(SyntaxKind::ExprPath), 0, 4),
            (SyntaxNodeKind::Syntax(SyntaxKind::PathSegment), 0, 4),
            (SyntaxNodeKind::Syntax(SyntaxKind::Identifier), 0, 4),
            (SyntaxNodeKind::Syntax(SyntaxKind::Terminal), 0, 4),
            (SyntaxNodeKind::Syntax(SyntaxKind::Trivia), 0, 0),
            (
                SyntaxNodeKind::Token(Token { kind: TokenKind::Identifier, text: "foo".into() }),
                0,
                3
            ),
            (SyntaxNodeKind::Syntax(SyntaxKind::Trivia), 3, 1),
            (SyntaxNodeKind::Syntax(SyntaxKind::TriviumWhitespace), 3, 1),
            (SyntaxNodeKind::Token(Token { kind: TokenKind::Whitespace, text: " ".into() }), 3, 1),
            (SyntaxNodeKind::Syntax(SyntaxKind::Empty), 4, 0),
            (SyntaxNodeKind::Syntax(SyntaxKind::Terminal), 4, 2),
            (SyntaxNodeKind::Syntax(SyntaxKind::Trivia), 4, 0),
            (SyntaxNodeKind::Token(Token { kind: TokenKind::Plus, text: "+".into() }), 4, 1),
            (SyntaxNodeKind::Syntax(SyntaxKind::Trivia), 5, 1),
            (SyntaxNodeKind::Syntax(SyntaxKind::TriviumWhitespace), 5, 1),
            (SyntaxNodeKind::Token(Token { kind: TokenKind::Whitespace, text: " ".into() }), 5, 1),
            (SyntaxNodeKind::Syntax(SyntaxKind::ExprLiteral), 6, 1),
            (SyntaxNodeKind::Syntax(SyntaxKind::Terminal), 6, 1),
            (SyntaxNodeKind::Syntax(SyntaxKind::Trivia), 6, 0),
            (
                SyntaxNodeKind::Token(Token { kind: TokenKind::LiteralNumber, text: "5".into() }),
                6,
                1
            ),
            (SyntaxNodeKind::Syntax(SyntaxKind::Trivia), 7, 0)
        ]
    )
}
