use std::sync::Arc;

use filesystem::db::FilesGroup;
use filesystem::ids::{FileLongId, VirtualFile};
use smol_str::SmolStr;
use syntax::node::ast::{ItemList, SyntaxFile, Terminal, Trivia};
use syntax::node::db::{AsSyntaxGroup, SyntaxGroup};
use syntax::node::{SyntaxNode, Token, TypedSyntaxNode};
use syntax::token::TokenKind;

use super::ParserGroup;
use crate::test_utils::ParserDatabaseForTesting;

fn build_empty_file_green_tree(db: &dyn SyntaxGroup) -> SyntaxFile {
    let eof_token = Token::new_green(db, TokenKind::EndOfFile, SmolStr::from(""));
    let eof_terminal = Terminal::new_green(
        db,
        Trivia::new_green(db, vec![]),
        eof_token,
        Trivia::new_green(db, vec![]),
    );
    SyntaxFile::from_syntax_node(
        db,
        SyntaxNode::new_root(
            db,
            SyntaxFile::new_green(db, ItemList::new_green(db, vec![]), eof_terminal),
        ),
    )
}

#[test]
fn test_parser() {
    let db = ParserDatabaseForTesting::default();

    // Parse empty cairo file.
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "file.cairo".into(),
        content: Arc::new("".into()),
    }));
    let syntax_file = db.file_syntax(file_id).expect("Unexpected diagnostics").unwrap();

    let expected_syntax_file = build_empty_file_green_tree(db.as_syntax_group());

    assert_eq!(*syntax_file, expected_syntax_file);
}
