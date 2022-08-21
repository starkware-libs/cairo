use std::fs;

use filesystem::ids::FileId;
use salsa::{InternId, InternKey};
use syntax::node::db::GreenDatabase;
use syntax::node::TypedSyntaxNode;

use crate::parser::Parser;
use crate::printer::print_tree;

#[salsa::database(GreenDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

fn test_source() -> FileId {
    FileId::from_intern_id(InternId::from(100u32))
}

fn read_file(filename: &str) -> String {
    fs::read_to_string(filename)
        .unwrap_or_else(|_| panic!("Something went wrong reading file {}", filename))
}

/// Parse the cairo file, print it, and compare with the expected result.
fn parse_and_compare_tree(
    cairo_filename: &str,
    expected_filename: &str,
    print_colors: bool,
    print_trivia: bool,
) {
    let db_val = DatabaseImpl::default();
    let db = &db_val;

    let contents = read_file(cairo_filename);
    let mut parser = Parser::from_text(db, test_source(), contents.as_str());
    let syntax_root = parser.parse_syntax_file().as_syntax_node();

    let printed_tree = print_tree(db, &syntax_root, print_colors, print_trivia);

    let expected_tree = read_file(expected_filename);
    // println!("{printed_tree}"); // TODO(yg): remove.
    assert_eq!(printed_tree, expected_tree);
}

fn _debug_failure(printed_tree: String, expected_tree: String) {
    println!("{printed_tree}");

    let mut printed_bytes = String::new();
    for c in printed_tree.as_bytes() {
        printed_bytes.push_str(format!("{:02x} ", c).as_str());
    }
    let mut expected_bytes = String::new();
    for c in expected_tree.as_bytes() {
        expected_bytes.push_str(format!("{:02x} ", c).as_str());
    }

    println!("{printed_bytes}");
    println!("{expected_bytes}");
}

#[test]
fn test_short_tree() {
    parse_and_compare_tree(
        // cairo_filename =
        "test_data/cairo_files/short.cairo",
        // expected_result_filename =
        "test_data/expected_results/short_tree",
        // print_colors =
        false,
        // print_trivia =
        false,
    );
}

#[test]
fn test_short_tree_colored() {
    parse_and_compare_tree(
        // cairo_filename =
        "test_data/cairo_files/short.cairo",
        // expected_result_filename =
        "test_data/expected_results/short_tree_colored",
        // print_colors =
        true,
        // print_trivia =
        false,
    );
}

#[test]
fn test1_tree() {
    parse_and_compare_tree(
        // cairo_filename =
        "test_data/cairo_files/test1.cairo",
        // expected_result_filename =
        "test_data/expected_results/test1_tree",
        // print_colors =
        false,
        // print_trivia =
        false,
    );
}

#[test]
fn test1_tree_with_trivia() {
    parse_and_compare_tree(
        // cairo_filename =
        "test_data/cairo_files/test1.cairo",
        // expected_result_filename =
        "test_data/expected_results/test1_tree_with_trivia",
        // print_colors =
        false,
        // print_trivia =
        true,
    );
}

#[test]
fn test2_tree() {
    parse_and_compare_tree(
        // cairo_filename =
        "/home/yuval/workspace/cairo2/crates/parser/test_data/cairo_files/test2.cairo",
        // expected_result_filename =
        "/home/yuval/workspace/cairo2/crates/parser/test_data/expected_results/test2_tree",
        // print_colors =
        false,
        // print_trivia =
        false,
    );
}

#[test]
fn test2_tree_with_trivia() {
    parse_and_compare_tree(
        // cairo_filename =
        "test_data/cairo_files/test2.cairo",
        // expected_result_filename =
        "test_data/expected_results/test2_tree_with_trivia",
        // print_colors =
        false,
        // print_trivia =
        true,
    );
}
