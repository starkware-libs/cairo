use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::ast::{
    ModuleItemList, SyntaxFile, TerminalEndOfFile, TokenEndOfFile, Trivia,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, Token, TypedSyntaxNode};
use cairo_lang_utils::Upcast;
use pretty_assertions::assert_eq;
use smol_str::SmolStr;
use test_log::test;

use crate::db::ParserGroup;
use crate::test_utils::create_virtual_file;
use crate::utils::SimpleParserDatabase;

fn build_empty_file_green_tree(db: &dyn SyntaxGroup, file_id: FileId) -> SyntaxFile {
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
            file_id,
            SyntaxFile::new_green(db, ModuleItemList::new_green(db, vec![]), eof_terminal).0,
        ),
    )
}

#[test]
fn test_parser() {
    let db = SimpleParserDatabase::default();

    // Parse empty cairo file.
    let file_id = create_virtual_file(&db, "file.cairo", "");
    let syntax_file = db.file_module_syntax(file_id).unwrap();
    let diagnostics = db.file_syntax_diagnostics(file_id);
    assert_eq!(diagnostics.format(&db), "");

    let expected_syntax_file = build_empty_file_green_tree(db.upcast(), file_id);

    assert_eq!(syntax_file, expected_syntax_file);
}
