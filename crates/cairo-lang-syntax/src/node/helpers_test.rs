use cairo_lang_filesystem::ids::SmolStrId;

use super::GetIdentifier;
use crate::node::ast::{
    ExprPath, ExprPathInner, OptionTerminalDollarEmpty, PathSegmentGreen, PathSegmentSimple,
    TerminalColonColon, TerminalIdentifier, TokenColonColon, TokenIdentifier, Trivia,
};
use crate::node::test_utils::DatabaseForTesting;
use crate::node::{Terminal, Token};

#[test]
fn test_expr_path_identifier() {
    let db_val = DatabaseForTesting::default();
    let db = &db_val;

    let no_trivia = Trivia::new_green(db, &[]);
    let token_foo = TokenIdentifier::new_green(db, SmolStrId::from(db, "foo"));
    let terminal_foo = TerminalIdentifier::new_green(db, no_trivia, token_foo, no_trivia);

    let token_bar = TokenIdentifier::new_green(db, SmolStrId::from(db, "bar"));
    let terminal_bar = TerminalIdentifier::new_green(db, no_trivia, token_bar, no_trivia);

    let token_separator = TokenColonColon::new_green(db, SmolStrId::from(db, "::"));
    let separator = TerminalColonColon::new_green(db, no_trivia, token_separator, no_trivia);

    PathSegmentSimple::new_green(db, terminal_foo);

    let children = &[
        PathSegmentGreen::from(PathSegmentSimple::new_green(db, terminal_foo)).into(),
        separator.into(),
        PathSegmentGreen::from(PathSegmentSimple::new_green(db, terminal_bar)).into(),
    ];
    let empty_dollar = OptionTerminalDollarEmpty::new_green(db).into();
    assert_eq!(
        ExprPath::new_green(db, empty_dollar, ExprPathInner::new_green(db, children))
            .identifier(db)
            .long(db),
        "bar"
    );
}
