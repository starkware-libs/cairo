use std::path::PathBuf;

use cairo_lang_filesystem::ids::{FileLongId, SmolStrId};
use cairo_lang_filesystem::span::{TextOffset, TextWidth};
use cairo_lang_test_utils::test;
use cairo_lang_utils::Intern;
use pretty_assertions::assert_eq;

use super::ast::{
    ExprBinary, ExprPath, PathSegmentGreen, PathSegmentSimple, SyntaxFileGreen, TerminalIdentifier,
    TerminalPlus, TokenIdentifier, TokenPlus, TokenWhitespace, Trivia,
};
use super::kind::SyntaxKind;
use super::{SyntaxNode, Terminal, Token, TypedSyntaxNode};
use crate::node::ast::{
    ExprPathInner, OptionTerminalDollarEmpty, TerminalLiteralNumber, TokenLiteralNumber,
};
use crate::node::test_utils::DatabaseForTesting;

#[test]
fn test_ast() {
    let db_val = DatabaseForTesting::default();
    let db = &db_val;
    let root = setup(db);

    assert_eq!(
        root.descendants(db)
            .map(|node| (
                node.kind(db),
                node.text(db).map(|s| s.long(db).as_str()),
                node.offset(db),
                node.width(db)
            ))
            .collect::<Vec<_>>(),
        vec![
            (SyntaxKind::ExprBinary, None, TextOffset::START, TextWidth::new_for_testing(7)),
            (SyntaxKind::ExprPath, None, TextOffset::START, TextWidth::new_for_testing(4)),
            (
                SyntaxKind::OptionTerminalDollarEmpty,
                None,
                TextOffset::START,
                TextWidth::new_for_testing(0)
            ),
            (SyntaxKind::ExprPathInner, None, TextOffset::START, TextWidth::new_for_testing(4)),
            (SyntaxKind::PathSegmentSimple, None, TextOffset::START, TextWidth::new_for_testing(4)),
            (
                SyntaxKind::TerminalIdentifier,
                None,
                TextOffset::START,
                TextWidth::new_for_testing(4)
            ),
            (SyntaxKind::Trivia, None, TextOffset::START, TextWidth::new_for_testing(0)),
            (
                SyntaxKind::TokenIdentifier,
                Some("foo"),
                TextOffset::START,
                TextWidth::new_for_testing(3)
            ),
            (
                SyntaxKind::Trivia,
                None,
                TextWidth::new_for_testing(3).as_offset(),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::TokenWhitespace,
                Some(" "),
                TextWidth::new_for_testing(3).as_offset(),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::TerminalPlus,
                None,
                TextWidth::new_for_testing(4).as_offset(),
                TextWidth::new_for_testing(2)
            ),
            (
                SyntaxKind::Trivia,
                None,
                TextWidth::new_for_testing(4).as_offset(),
                TextWidth::new_for_testing(0)
            ),
            (
                SyntaxKind::TokenPlus,
                Some("+"),
                TextWidth::new_for_testing(4).as_offset(),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::Trivia,
                None,
                TextWidth::new_for_testing(5).as_offset(),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::TokenWhitespace,
                Some(" "),
                TextWidth::new_for_testing(5).as_offset(),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::TerminalLiteralNumber,
                None,
                TextWidth::new_for_testing(6).as_offset(),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::Trivia,
                None,
                TextWidth::new_for_testing(6).as_offset(),
                TextWidth::new_for_testing(0)
            ),
            (
                SyntaxKind::TokenLiteralNumber,
                Some("5"),
                TextWidth::new_for_testing(6).as_offset(),
                TextWidth::new_for_testing(1)
            ),
            (
                SyntaxKind::Trivia,
                None,
                TextWidth::new_for_testing(7).as_offset(),
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
        let ptr = node.stable_ptr(db);
        let looked_up_node = ptr.lookup(db);
        assert_eq!(node, looked_up_node);
    }
}

fn setup(db: &DatabaseForTesting) -> SyntaxNode<'_> {
    // TODO: Use a builder for easier construction of token.
    // Construct green nodes.
    let token_foo = TokenIdentifier::new_green(db, SmolStrId::from(db, "foo"));
    let token_whitespace1 = TokenWhitespace::new_green(db, SmolStrId::from(db, " "));
    let token_plus = TokenPlus::new_green(db, SmolStrId::from(db, "+"));
    let token_whitespace2 = TokenWhitespace::new_green(db, SmolStrId::from(db, " "));
    let token5 = TokenLiteralNumber::new_green(db, SmolStrId::from(db, "5"));
    assert_eq!(token_whitespace1, token_whitespace2);
    let no_trivia = Trivia::new_green(db, &[]);
    let triviums = [token_whitespace1, token_whitespace2];
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
    let terminal5 = TerminalLiteralNumber::new_green(db, no_trivia, token5, no_trivia);
    let empty_dollar = OptionTerminalDollarEmpty::new_green(db).into();
    let expr = ExprBinary::new_green(
        db,
        ExprPath::new_green(
            db,
            empty_dollar,
            ExprPathInner::new_green(
                db,
                &[PathSegmentGreen::from(PathSegmentSimple::new_green(db, terminal_foo)).into()],
            ),
        )
        .into(),
        terminal_plus.into(),
        terminal5.into(),
    );
    // SyntaxNode::new_detached_root only accepts ast::SyntaxFileGreen, but we only have an
    // expression.
    // This is a hack to create a green id of "SyntaxFile" from "Expr".
    let root = SyntaxFileGreen(expr.0);
    let file_id = FileLongId::OnDisk(PathBuf::default()).intern(db);
    SyntaxNode::new_detached_root(db, file_id, root.0)
}

#[test]
fn test_casts() {
    let db_val = DatabaseForTesting::default();
    let db = &db_val;

    let no_trivia = Trivia::new_green(db, &[]);
    let green_plus_token = TokenPlus::new_green(db, SmolStrId::from(db, "+"));
    let green_terminal_plus = TerminalPlus::new_green(db, no_trivia, green_plus_token, no_trivia);
    let file_id = FileLongId::OnDisk(PathBuf::default()).intern(db);
    let terminal = SyntaxNode::new_detached_root(db, file_id, green_terminal_plus.0);

    assert_eq!(terminal.kind(db), SyntaxKind::TerminalPlus);
    assert!(TerminalIdentifier::cast(db, terminal).is_none());
    assert!(TokenIdentifier::cast(db, terminal).is_none());
    assert!(TokenPlus::cast(db, terminal).is_none());
    let Some(terminal) = TerminalPlus::cast(db, terminal) else {
        panic!("Expected `TerminalPlus::cast` to return a valid terminal.");
    };
    let token = terminal.token(db).as_syntax_node();
    assert!(TerminalIdentifier::cast(db, token).is_none());
    assert!(TokenIdentifier::cast(db, token).is_none());
    assert!(TerminalPlus::cast(db, token).is_none());
    assert!(TokenPlus::cast(db, token).is_some());
}
