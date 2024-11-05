use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::ast::{
    ModuleItemList, SyntaxFile, TerminalEndOfFile, TokenEndOfFile, Trivia,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, Token as SyntaxToken, TypedSyntaxNode};
use cairo_lang_utils::Upcast;
use indoc::indoc;
use pretty_assertions::assert_eq;
use smol_str::SmolStr;
use test_log::test;

use crate::db::ParserGroup;
use crate::printer::print_tree;
use crate::test_utils::{TextSpan, Token, TokenStream, create_virtual_file};
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

#[test]
fn test_parser_shorthand() {
    let db = SimpleParserDatabase::default();
    let (_node, diagnostics) = db.parse_virtual_with_diagnostics("");
    assert_eq!(diagnostics.format(&db), "");
    let _node = db.parse_virtual("").unwrap();
}

#[test]
fn test_token_stream_parser() {
    let code = include_str!("parser_test_data/cairo_test_files/short.cairo");
    let db = SimpleParserDatabase::default();

    let root_node = db.parse_virtual(code).unwrap();

    let token_stream = TokenStream::from_syntax_node(&db, root_node.clone());

    let (node_from_token_stream, _) = db.parse_token_stream(&token_stream);

    let original_leaves: Vec<SyntaxNode> = root_node.tokens(db.upcast()).collect();

    let token_stream_origin_leaves: Vec<SyntaxNode> =
        node_from_token_stream.tokens(db.upcast()).collect();

    assert_eq!(original_leaves.len(), token_stream_origin_leaves.len());
    assert_eq!(
        print_tree(&db, &root_node, false, true),
        print_tree(&db, &node_from_token_stream, false, true)
    )
}

#[test]
fn test_token_stream_expr_parser() {
    let expr_code = indoc! {r#"
      temp = a
    "#};
    let db = SimpleParserDatabase::default();

    let token_stream = TokenStream {
        tokens: vec![Token {
            content: expr_code.to_string(),
            span: TextSpan { start: 0, end: expr_code.len() },
        }],
    };

    let (node_from_token_stream, _) = db.parse_token_stream_expr(&token_stream);

    let node_text = node_from_token_stream.get_text(db.upcast());

    assert_eq!(node_text, expr_code);
}
