use std::sync::Arc;

use filesystem::db::{FilesDatabase, FilesGroup};
use filesystem::ids::{FileLongId, VirtualFile};
use smol_str::SmolStr;
use syntax::node::ast::{ItemList, SyntaxFile, Terminal, Trivia};
use syntax::node::db::{AsSyntaxGroup, GreenDatabase, SyntaxGroup};
use syntax::node::{SyntaxNode, Token, TypedSyntaxNode};
use syntax::token::TokenKind;

use super::{ParserDatabase, ParserGroup};

#[salsa::database(ParserDatabase, GreenDatabase, FilesDatabase)]
#[derive(Default)]
pub struct TestDatabase {
    storage: salsa::Storage<TestDatabase>,
}
impl salsa::Database for TestDatabase {}
impl AsSyntaxGroup for TestDatabase {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}

fn build_empty_file_green_tree(syntax_group: &dyn SyntaxGroup) -> SyntaxFile {
    let eof_token = Token::new_green(syntax_group, TokenKind::EndOfFile, SmolStr::from(""));
    let eof_terminal = Terminal::new_green(
        syntax_group,
        Trivia::new_green(syntax_group, vec![]),
        eof_token,
        Trivia::new_green(syntax_group, vec![]),
    );
    SyntaxFile::from_syntax_node(
        syntax_group,
        SyntaxNode::new_root(SyntaxFile::new_green(
            syntax_group,
            ItemList::new_green(syntax_group, vec![]),
            eof_terminal,
        )),
    )
}

#[test]
fn test_parser() {
    let db = TestDatabase::default();
    let syntax_group = db.as_syntax_group();

    // Parse empty cairo file.
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "file.cairo".into(),
        content: Arc::new("".into()),
    }));
    let syntax_file = db.file_syntax(file_id).unwrap();

    let expected_syntax_file = build_empty_file_green_tree(syntax_group);

    assert_eq!(*syntax_file, expected_syntax_file);
}
