use db_utils::Upcast;
use pretty_assertions::assert_eq;
use smol_str::SmolStr;
use syntax::node::ast::{ItemList, SyntaxFile, TerminalEndOfFile, TokenEndOfFile, Trivia};
use syntax::node::db::SyntaxGroup;
use syntax::node::{SyntaxNode, Terminal, Token, TypedSyntaxNode};
use test_log::test;

use crate::db::ParserGroup;
use crate::test_utils::create_virtual_file;
use crate::utils::SimpleParserDatabase;

fn build_empty_file_green_tree(db: &dyn SyntaxGroup) -> SyntaxFile {
    let eof_token = TokenEndOfFile::new_green(db, SmolStr::from(""));
    let eof_terminal = TerminalEndOfFile::new_green(
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
    let db = SimpleParserDatabase::default();

    // Parse empty cairo file.
    let file_id = create_virtual_file(&db, "file.cairo", "");
    let syntax_file = db.file_syntax(file_id).unwrap();
    let diagnostics = db.file_syntax_diagnostics(file_id);
    assert_eq!(diagnostics.format(&db), "");

    let expected_syntax_file = build_empty_file_green_tree(db.upcast());

    assert_eq!(*syntax_file, expected_syntax_file);
}
