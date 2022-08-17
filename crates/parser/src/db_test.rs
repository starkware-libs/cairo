use std::sync::Arc;

use db_utils::Upcast;
use filesystem::db::{FilesDatabase, FilesGroup};
use filesystem::ids::{FileLongId, VirtualFile};
use smol_str::SmolStr;
use syntax::node::ast::{ItemList, SyntaxFile, Terminal, Trivia};
use syntax::node::db::{GreenDatabase, GreenInterner};
use syntax::node::{SyntaxNode, Token, TypedSyntaxNode};
use syntax::token::TokenKind;

use super::{ParserDatabase, ParserGroup};

#[salsa::database(ParserDatabase, GreenDatabase, FilesDatabase)]
#[derive(Default)]
pub struct TestDatabase {
    storage: salsa::Storage<TestDatabase>,
}
impl salsa::Database for TestDatabase {}
impl Upcast<dyn GreenInterner> for TestDatabase {
    fn upcast(&self) -> &(dyn GreenInterner + 'static) {
        &*self
    }
}

#[test]
fn test_parser() {
    let db = TestDatabase::default();
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "root.cairo".into(),
        content: Arc::new("".into()),
    }));
    let syntax_file = db.file_syntax(file_id);
    let eof_token = Token::new_green(db.upcast(), TokenKind::EndOfFile, SmolStr::from(""));
    let eof_terminal = Terminal::new_green(
        db.upcast(),
        Trivia::new_green(db.upcast(), vec![]),
        eof_token,
        Trivia::new_green(db.upcast(), vec![]),
    );
    let expected_syntax_file = SyntaxFile::from_syntax_node(
        db.upcast(),
        SyntaxNode::new_root(SyntaxFile::new_green(
            db.upcast(),
            ItemList::new_green(db.upcast(), vec![]),
            eof_terminal,
        )),
    );
    assert_eq!(*syntax_file, expected_syntax_file);
}
