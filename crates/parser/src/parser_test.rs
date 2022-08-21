use std::fs;

use filesystem::ids::FileId;
use salsa::{InternId, InternKey};
use syntax::node::db::GreenDatabase;
use syntax::node::TypedSyntaxNode;
use test_case::test_case;

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
#[test_case("test_data/cairo_files/short.cairo", "test_data/expected_results/short_tree", false, false; "short_tree")]
#[test_case("test_data/cairo_files/short.cairo", "test_data/expected_results/short_tree_colored", true, false; "short_tree_colored")]
#[test_case("test_data/cairo_files/test1.cairo", "test_data/expected_results/test1_tree", false, false; "test1_tree")]
#[test_case("test_data/cairo_files/test1.cairo", "test_data/expected_results/test1_tree_with_trivia", false, true; "test1_tree_with_trivia")]
#[test_case("test_data/cairo_files/test2.cairo", "test_data/expected_results/test2_tree", false, false; "test2_tree")]
#[test_case("test_data/cairo_files/test2.cairo", "test_data/expected_results/test2_tree_with_trivia", false, true; "test2_tree_with_trivia")]
fn parse_and_compare_tree(
    cairo_filename: &str,
    expected_tree_filename: &str,
    print_colors: bool,
    print_trivia: bool,
) {
    let db_val = DatabaseImpl::default();
    let db = &db_val;

    let contents = read_file(cairo_filename);
    let mut parser = Parser::from_text(db, test_source(), contents.as_str());
    let syntax_root = parser.parse_syntax_file().as_syntax_node();

    let printed_tree = print_tree(db, &syntax_root, print_colors, print_trivia);

    let expected_tree = read_file(expected_tree_filename);
    // assert_eq prints a long confusing error on failure, so it's not used here.
    if printed_tree != expected_tree {
        panic!(
            "assertion failed: printed_tree != expected_tree. To debug this, use \
             _debug_failure(). If the printed_tree looks like the right output, you can copy it \
             from running _debug_failure() and paste in the expected tree file. Note to carefully \
             review it and not to blindly paste is there, as this loses the whole point of the \
             test."
        );
    }
}

// TODO(yuval): add a CLI referred from the test failure, to be used after changes to the
// parser/printer.
fn _debug_failure(printed_tree: String, expected_tree: String) {
    println!("Printed tree:\n{printed_tree}");

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
