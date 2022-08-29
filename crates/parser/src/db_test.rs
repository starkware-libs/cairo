use std::sync::Arc;

use filesystem::db::{FilesDatabase, FilesGroup};
use filesystem::ids::{FileLongId, VirtualFile};
use smol_str::SmolStr;
use syntax::node::ast::{ItemList, SyntaxFile, Terminal, Trivia};
use syntax::node::db::{AsGreenInterner, GreenDatabase, GreenInterner};
use syntax::node::{SyntaxNode, Token, TypedSyntaxNode};
use syntax::token::TokenKind;

use super::{ParserDatabase, ParserGroup};

#[salsa::database(ParserDatabase, GreenDatabase, FilesDatabase)]
#[derive(Default)]
pub struct TestDatabase {
    storage: salsa::Storage<TestDatabase>,
}
impl salsa::Database for TestDatabase {}
impl AsGreenInterner for TestDatabase {
    fn as_green_interner(&self) -> &(dyn GreenInterner + 'static) {
        self
    }
}

fn build_empty_file_green_tree(green_interner: &dyn GreenInterner) -> SyntaxFile {
    let eof_token = Token::new_green(green_interner, TokenKind::EndOfFile, SmolStr::from(""));
    let eof_terminal = Terminal::new_green(
        green_interner,
        Trivia::new_green(green_interner, vec![]),
        eof_token,
        Trivia::new_green(green_interner, vec![]),
    );
    SyntaxFile::from_syntax_node(
        green_interner,
        SyntaxNode::new_root(SyntaxFile::new_green(
            green_interner,
            ItemList::new_green(green_interner, vec![]),
            eof_terminal,
        )),
    )
}

#[test]
fn test_parser() {
    let db = TestDatabase::default();
    let green_interner = db.as_green_interner();

    // Parse empty cairo file.
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "file.cairo".into(),
        content: Arc::new("".into()),
    }));
    let syntax_file = db.file_syntax(file_id).unwrap();

    let expected_syntax_file = build_empty_file_green_tree(green_interner);

    assert_eq!(*syntax_file, expected_syntax_file);
}
