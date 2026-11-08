use std::path::PathBuf;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{Directory, SmolStrId};
use cairo_lang_filesystem::override_file_content;
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_syntax::node::ast::{
    ModuleItemList, SyntaxFile, SyntaxFileGreen, TerminalEndOfFile, TokenEndOfFile, Trivia,
};
use cairo_lang_syntax::node::{SyntaxNode, Terminal, Token as SyntaxToken, TypedSyntaxNode};
use indoc::indoc;
use pretty_assertions::assert_eq;
use salsa::Database;

use crate::db::ParserGroup;
use crate::printer::print_tree;
use crate::test_utils::{MockToken, MockTokenStream, create_virtual_file};
use crate::utils::{SimpleParserDatabase, get_syntax_root_and_diagnostics_from_file};

fn build_empty_file_green_tree<'a>(db: &'a dyn Database) -> SyntaxFileGreen<'a> {
    let eof_token = TokenEndOfFile::new_green(db, SmolStrId::from(db, ""));
    let eof_terminal = TerminalEndOfFile::new_green(
        db,
        Trivia::new_green(db, &[]),
        eof_token,
        Trivia::new_green(db, &[]),
    );
    SyntaxFile::new_green(db, ModuleItemList::new_green(db, &[]), eof_terminal)
}

#[test]
fn test_parser() {
    let db = SimpleParserDatabase::default();

    // Parse empty Cairo file.
    let file_id = create_virtual_file(&db, "file.cairo", "");
    let syntax_file = db.file_module_syntax(file_id).unwrap();
    let diagnostics = db.file_syntax_diagnostics(file_id);
    assert_eq!(diagnostics.format(&db), "");

    // Compare green trees: the test only asserts the parsed structure, and the parsed root (minted
    // by the parse query) is a distinct node from any standalone one, so node identity wouldn't
    // match anyway.
    let expected_green = build_empty_file_green_tree(&db);
    assert_eq!(syntax_file.as_syntax_node().green_node(&db), expected_green.0.long(&db));
}

#[test]
fn test_parser_shorthand() {
    let db = SimpleParserDatabase::default();
    let (_node, diagnostics) = db.parse_virtual_with_diagnostics("");
    assert_eq!(diagnostics.format(&db), "");
    let _node = db.parse_virtual("").unwrap();
}

#[test]
fn test_token_stream_parser() {
    let filepath: PathBuf =
        [env!("CARGO_MANIFEST_DIR"), "src/parser_test_data/cairo_test_files/short.cairo"]
            .into_iter()
            .collect();
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;

    let (root_node, _) = get_syntax_root_and_diagnostics_from_file(db, filepath);

    let token_stream = MockTokenStream::from_syntax_node(db, root_node);
    let (node_from_token_stream, _) = db.parse_token_stream(&token_stream);

    let original_leaves: Vec<SyntaxNode<'_>> = root_node.tokens(db).collect();

    let token_stream_origin_leaves: Vec<SyntaxNode<'_>> =
        node_from_token_stream.tokens(db).collect();

    assert_eq!(original_leaves.len(), token_stream_origin_leaves.len());
    assert_eq!(
        print_tree(db, &root_node, false, true),
        print_tree(db, &node_from_token_stream, false, true)
    );
}

#[test]
fn test_token_stream_expr_parser() {
    let expr_code = indoc! {r#"
      temp = a
    "#};
    let db = SimpleParserDatabase::default();

    let token_stream = MockTokenStream {
        tokens: vec![MockToken {
            content: expr_code.to_string(),
            span: TextSpan::from_str(expr_code),
        }],
    };

    let (node_from_token_stream, _) = db.parse_token_stream_expr(&token_stream);

    let node_text = node_from_token_stream.get_text(&db);

    assert_eq!(node_text, expr_code);
}

#[test]
fn reparse_reuses_canonical_root_node_id() {
    // A file's root is the *canonical* root, seeded by the file-keyed `file_syntax_data` query (via
    // `SyntaxNode::new_canonical_root`). Editing the file's content therefore reuses the same root
    // node id rather than minting a fresh one — the id stability that downstream salsa early cutoff
    // relies on. A detached, green-keyed root (`new_detached_root`) would instead get a new id on
    // every content change, so this assertion guards that `file_syntax` stays canonical.
    // Each phase is scoped so that no `FileId`/`SyntaxNode` borrow is held across a `&mut` content
    // edit. The node id is encoded in `SyntaxNode`'s `Debug`, captured as an owned string.
    let mut db = SimpleParserDatabase::default();

    {
        let db_ref = &mut db;
        let file_id = Directory::Real("src".into()).file(db_ref, "lib.cairo");
        override_file_content!(db_ref, file_id, Some("fn foo() {}\n".into()));
    }
    let root_id_before = {
        let file_id = Directory::Real("src".into()).file(&db, "lib.cairo");
        format!("{:?}", db.file_syntax(file_id).unwrap())
    };

    {
        let db_ref = &mut db;
        let file_id = Directory::Real("src".into()).file(db_ref, "lib.cairo");
        override_file_content!(db_ref, file_id, Some("fn foo() { let _x = 1; }\n".into()));
    }
    let root_id_after = {
        let file_id = Directory::Real("src".into()).file(&db, "lib.cairo");
        format!("{:?}", db.file_syntax(file_id).unwrap())
    };

    assert_eq!(
        root_id_before, root_id_after,
        "canonical root node id must stay stable across a reparse"
    );
}
