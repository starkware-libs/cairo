use std::path::PathBuf;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileLongId;
use cairo_lang_filesystem::span::{TextOffset, TextWidth};
use pretty_assertions::assert_eq;
use test_log::test;

use super::ast::{
    ExprBinary, ExprPath, PathSegmentGreen, PathSegmentSimple, SyntaxFileGreen, TerminalIdentifier,
    TerminalPlus, TokenIdentifier, TokenPlus, TokenWhitespace, Trivia,
};
use super::kind::SyntaxKind;
use super::{SyntaxNode, Terminal, Token};
use crate::node::ast::{LiteralNumber, OptionTerminalMinusEmpty, TerminalNumber, TokenNumber};
use crate::node::test_utils::DatabaseForTesting;

#[test]
fn test_ast() {
    let db_val = DatabaseForTesting::default();
    let db = &db_val;
    let root = setup(db);

    assert_eq!(
        root.descendants(db)
            .map(|node| (node.kind(db), node.token_text(db), node.offset(), node.width(db)))
            .collect::<Vec<_>>(),
        vec![
            (
                SyntaxKind::ExprBinary,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(0)),
                TextWidth::new_for_testing(7)
            ),
            (
                SyntaxKind::ExprPath,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(0)),
                TextWidth::new_for_testing(4)
            ),
            (
                SyntaxKind::PathSegmentSimple,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(0)),
                TextWidth::new_for_testing(4)
            ),
            (
                SyntaxKind::TerminalIdentifier,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(0)),
                TextWidth::new_for_testing(4)
            ),
            (
                SyntaxKind::Trivia,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(0)),
                TextWidth::new_for_testing(0)
            ),
            (
                SyntaxKind::TokenIdentifier,
                Some("foo"),
                TextOffset::default().add_width(TextWidth::new_for_testing(0)),
                TextWidth::new_for_testing(3)
            ),
            (
                SyntaxKind::Trivia,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(3)),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::TokenWhitespace,
                Some(" "),
                TextOffset::default().add_width(TextWidth::new_for_testing(3)),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::TerminalPlus,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(4)),
                TextWidth::new_for_testing(2)
            ),
            (
                SyntaxKind::Trivia,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(4)),
                TextWidth::new_for_testing(0)
            ),
            (
                SyntaxKind::TokenPlus,
                Some("+"),
                TextOffset::default().add_width(TextWidth::new_for_testing(4)),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::Trivia,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(5)),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::TokenWhitespace,
                Some(" "),
                TextOffset::default().add_width(TextWidth::new_for_testing(5)),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::LiteralNumber,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(6)),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::OptionTerminalMinusEmpty,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(6)),
                TextWidth::new_for_testing(0)
            ),
            (
                SyntaxKind::TerminalNumber,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(6)),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::Trivia,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(6)),
                TextWidth::new_for_testing(0)
            ),
            (
                SyntaxKind::TokenNumber,
                Some("5"),
                TextOffset::default().add_width(TextWidth::new_for_testing(6)),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::Trivia,
                None,
                TextOffset::default().add_width(TextWidth::new_for_testing(7)),
                TextWidth::new_for_testing(0)
            )
        ]
    )
}

#[test]
fn test_stable_ptr() {
    let db_val = DatabaseForTesting::default();
    let db = &db_val;
    let root = setup(db);
    for node in root.descendants(db) {
        let ptr = node.stable_ptr();
        let looked_up_node = ptr.lookup(db);
        assert_eq!(node, looked_up_node);
    }
}

fn setup(db: &DatabaseForTesting) -> SyntaxNode {
    // TODO: Use a builder for easier construction of token.
    // Construct green nodes.
    let token_foo = TokenIdentifier::new_green(db, "foo");
    let token_whitespace1 = TokenWhitespace::new_green(db, " ");
    let token_plus = TokenPlus::new_green(db, "+");
    let token_whitespace2 = TokenWhitespace::new_green(db, " ");
    let token5 = TokenNumber::new_green(db, "5");
    assert_eq!(token_whitespace1, token_whitespace2);
    let no_trivia = Trivia::new_green(db, &[]);
    let triviums = vec![token_whitespace1, token_whitespace2];
    assert_eq!(triviums[0], triviums[1]);
    let terminal_foo = TerminalIdentifier::new_green(
        db,
        no_trivia,
        token_foo,
        Trivia::new_green(db, &[triviums[0].into()]),
    );
    let terminal_plus = TerminalPlus::new_green(
        db,
        no_trivia,
        token_plus,
        Trivia::new_green(db, &[triviums[1].into()]),
    );
    let terminal5 = TerminalNumber::new_green(db, no_trivia, token5, no_trivia);
    let number5 =
        LiteralNumber::new_green(db, OptionTerminalMinusEmpty::new_green(db).into(), terminal5);
    let expr = ExprBinary::new_green(
        db,
        ExprPath::new_green(
            db,
            &[PathSegmentGreen::from(PathSegmentSimple::new_green(db, terminal_foo)).into()],
        )
        .into(),
        terminal_plus.into(),
        number5.into(),
    );
    // SyntaxNode::new_root only accepts ast::SyntaxFileGreen, but we only have an expression.
    // This is a hack to crate a green id of "SyntaxFile" from "Expr".
    let root = SyntaxFileGreen(expr.0);
    let file_id = db.intern_file(FileLongId::OnDisk(PathBuf::default()));
    SyntaxNode::new_root(db, file_id, root.0)
}
