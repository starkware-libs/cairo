use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_test_utils::test;
use cairo_lang_utils::Intern;

use super::green::{GreenNode, GreenNodeDetails, GreenNodeDetailsRef, GreenNodeRef};
use super::kind::SyntaxKind;
use super::test_utils::DatabaseForTesting;

/// Interning a node by reference must yield the id of the equal owned node - otherwise structurally
/// identical nodes would silently get distinct ids, as the two forms are both in use.
#[test]
fn test_intern_ref_matches_owned() {
    let db_val = DatabaseForTesting::default();
    let db = &db_val;

    let token = GreenNode {
        kind: SyntaxKind::TokenIdentifier,
        details: GreenNodeDetails::Token(SmolStrId::from(db, "foo")),
    }
    .intern(db);
    let token_by_ref = GreenNodeRef {
        kind: SyntaxKind::TokenIdentifier,
        details: GreenNodeDetailsRef::Token(SmolStrId::from(db, "foo")),
    }
    .intern(db);
    assert_eq!(token, token_by_ref);

    let width = TextWidth::from_str("foo");
    let children = [token, token_by_ref];
    let owned = GreenNode {
        kind: SyntaxKind::ExprPath,
        details: GreenNodeDetails::Node { children: children.to_vec(), width },
    }
    .intern(db);
    let by_ref = GreenNodeRef {
        kind: SyntaxKind::ExprPath,
        details: GreenNodeDetailsRef::Node { children: &children, width },
    }
    .intern(db);
    assert_eq!(owned, by_ref);

    // A different width is a different node, in both forms.
    let other_width = GreenNodeRef {
        kind: SyntaxKind::ExprPath,
        details: GreenNodeDetailsRef::Node { children: &children, width: TextWidth::default() },
    }
    .intern(db);
    assert_ne!(owned, other_width);
}
