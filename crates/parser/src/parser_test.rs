use std::fs;

use filesystem::ids::FileId;
use salsa::{InternId, InternKey};
use syntax::node::db::GreenDatabase;
use syntax::node::{SyntaxNode, TypedSyntaxNode};
use test_case::test_case;

use crate::colored_printer::print_colored;
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
    // Make sure the colors are printed, even if the test doesn't run in a terminal.
    colored::control::set_override(true);

    let db_val = DatabaseImpl::default();
    let db = &db_val;

    let syntax_root = get_syntax_root(db, cairo_filename);

    let printed_tree = print_tree(db, &syntax_root, print_colors, print_trivia);

    let expected_tree = read_file(expected_tree_filename);

    compare_printed_and_expected(printed_tree, expected_tree);
}

#[test_case("test_data/cairo_files/colored.cairo","test_data/expected_results/colored", false;"colored")]
#[test_case("test_data/cairo_files/colored.cairo", "test_data/expected_results/colored_verbose", true; "colored_verbose")]
fn parse_and_compare_colored(cairo_filename: &str, expected_tree_filename: &str, verbose: bool) {
    // Make sure the colors are printed, even if the test doesn't run in a terminal.
    colored::control::set_override(true);

    let db_val = DatabaseImpl::default();
    let db = &db_val;

    let syntax_root = get_syntax_root(db, cairo_filename);

    let printed = print_colored(db, &syntax_root, verbose);
    let expected = read_file(expected_tree_filename);

    compare_printed_and_expected(printed, expected);
}

// TODO(yuval): add a CLI referred from the test failure, to be used after changes to the
// parser/printer.
fn _debug_failure(printed: String, expected: String) {
    println!("Printed:\n{printed}");

    let printed_bytes = printed.as_bytes();
    let expected_bytes = expected.as_bytes();

    if printed == expected {
        println!("Printed is as expected.");
        return;
    }
    for (i, printed_byte) in printed_bytes.iter().enumerate() {
        let expected_byte = expected_bytes[i];
        if *printed_byte != expected_byte {
            println!("printed is different than expected! First different byte index: {i}");
            println!("Printed byte: {printed_byte:02x}, Expected byte: {expected_byte:02x}");

            let initial_index = if i < 5000 { 0 } else { i - 5000 };
            let last_50_printed = &printed_bytes[initial_index..(i + 1)];
            let last_50_expected = &expected_bytes[initial_index..(i + 1)];

            _print_bytes("Printed hex:", last_50_printed, true);
            _print_bytes("Expected hex:", last_50_expected, true);
            _print_bytes("Printed raw:", last_50_printed, false);
            _print_bytes("Expected raw:", last_50_expected, false);
            break;
        }
    }
}

fn get_syntax_root(db: &DatabaseImpl, cairo_filename: &str) -> SyntaxNode {
    let contents = read_file(cairo_filename);
    let mut parser = Parser::from_text(db, test_source(), contents.as_str());
    parser.parse_syntax_file().as_syntax_node()
}

fn compare_printed_and_expected(printed: String, expected: String) {
    // assert_eq prints a long confusing error on failure, so it's not used here.
    if printed != expected {
        panic!(
            "assertion failed: printed != expected. To debug this, use _debug_failure(). If \
             `printed` looks like the right output, you can copy it from running _debug_failure() \
             and paste in the expected file. Note to carefully review it and not to blindly paste \
             it there, as this loses the whole point of the test."
        );
    }
}

// `hex`: print hex if true, raw if false.
fn _print_bytes(title: &str, bytes: &[u8], hex: bool) {
    println!("{title}");
    let mut bytes_str = String::new();
    if hex {
        for c in bytes {
            bytes_str.push_str(format!("{c:02x} ").as_str());
        }
    } else {
        for c in bytes {
            bytes_str.push_str(format!("{}", *c as char).as_str());
        }
    }
    println!("{bytes_str}");
}
