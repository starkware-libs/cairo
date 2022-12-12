#![allow(non_upper_case_globals)]
use std::fmt::Write;

use pretty_assertions::assert_eq;
use test_case::test_case;
use utils::ordered_hash_map::OrderedHashMap;

use crate::colored_printer::print_colored;
use crate::parser_test;
use crate::printer::{print_partial_tree, print_tree};
use crate::test_utils::{create_virtual_file, get_diagnostics, read_file};
use crate::utils::{
    get_syntax_root_and_diagnostics, get_syntax_root_and_diagnostics_from_file,
    SimpleParserDatabase,
};

struct ParserTreeTestParams {
    cairo_filename: &'static str,
    expected_output_filename: &'static str,
    print_diagnostics: bool,
    print_colors: bool,
    print_trivia: bool,
}

const TEST_short_tree_uncolored: ParserTreeTestParams = ParserTreeTestParams {
    cairo_filename: "test_data/cairo_files/short.cairo",
    expected_output_filename: "test_data/expected_results/short_tree",
    print_diagnostics: true,
    print_colors: false,
    print_trivia: false,
};
const TEST_short_tree_colored: ParserTreeTestParams = ParserTreeTestParams {
    cairo_filename: "test_data/cairo_files/short.cairo",
    expected_output_filename: "test_data/expected_results/short_tree_colored",
    print_diagnostics: false,
    print_colors: true,
    print_trivia: false,
};
const TEST_test1_tree_no_trivia: ParserTreeTestParams = ParserTreeTestParams {
    cairo_filename: "test_data/cairo_files/test1.cairo",
    expected_output_filename: "test_data/expected_results/test1_tree_no_trivia",
    print_diagnostics: true,
    print_colors: false,
    print_trivia: false,
};
const TEST_test1_tree_with_trivia: ParserTreeTestParams = ParserTreeTestParams {
    cairo_filename: "test_data/cairo_files/test1.cairo",
    expected_output_filename: "test_data/expected_results/test1_tree_with_trivia",
    print_diagnostics: false,
    print_colors: false,
    print_trivia: true,
};
const TEST_test2_tree_no_trivia: ParserTreeTestParams = ParserTreeTestParams {
    cairo_filename: "test_data/cairo_files/test2.cairo",
    expected_output_filename: "test_data/expected_results/test2_tree_no_trivia",
    print_diagnostics: true,
    print_colors: false,
    print_trivia: false,
};
const TEST_test2_tree_with_trivia: ParserTreeTestParams = ParserTreeTestParams {
    cairo_filename: "test_data/cairo_files/test2.cairo",
    expected_output_filename: "test_data/expected_results/test2_tree_with_trivia",
    print_diagnostics: false,
    print_colors: false,
    print_trivia: true,
};
#[cfg(feature = "fix_parser_tests")]
static TREE_TEST_CASES: [&ParserTreeTestParams; 6] = [
    &TEST_short_tree_uncolored,
    &TEST_short_tree_colored,
    &TEST_test1_tree_no_trivia,
    &TEST_test1_tree_with_trivia,
    &TEST_test2_tree_no_trivia,
    &TEST_test2_tree_with_trivia,
];

/// Parse the cairo file, print it, and compare with the expected result.
#[test_case(&TEST_short_tree_uncolored; "short_tree_uncolored")]
#[test_case(&TEST_short_tree_colored; "short_tree_colored")]
#[test_case(&TEST_test1_tree_no_trivia; "test1_tree_no_trivia")]
#[test_case(&TEST_test1_tree_with_trivia; "test1_tree_with_trivia")]
#[test_case(&TEST_test2_tree_no_trivia; "test2_tree_no_trivia")]
#[test_case(&TEST_test2_tree_with_trivia; "test2_tree_with_trivia")]
fn parse_and_compare_tree(test_params: &ParserTreeTestParams) {
    parse_and_compare_tree_maybe_fix(test_params, false);
}

fn parse_and_compare_tree_maybe_fix(test_params: &ParserTreeTestParams, fix: bool) {
    // Make sure the colors are printed, even if the test doesn't run in a terminal.
    colored::control::set_override(true);

    let db_val = SimpleParserDatabase::default();
    let db = &db_val;

    let (syntax_root, diagnostics) =
        get_syntax_root_and_diagnostics_from_file(db, test_params.cairo_filename);
    let diagnostics_str = diagnostics.format(db);

    let mut printed_tree =
        print_tree(db, &syntax_root, test_params.print_colors, test_params.print_trivia);
    if test_params.print_diagnostics {
        write!(printed_tree, "--------------------\n{diagnostics_str}").unwrap();
    }

    let expected_tree = read_file(test_params.expected_output_filename);
    compare_printed_and_expected_maybe_fix(
        printed_tree,
        expected_tree,
        test_params.expected_output_filename,
        fix,
    );
}

struct ParserColoredTestParams {
    cairo_filename: &'static str,
    expected_colored_filename: &'static str,
    verbose: bool,
}

const TEST_colored: ParserColoredTestParams = ParserColoredTestParams {
    cairo_filename: "test_data/cairo_files/colored.cairo",
    expected_colored_filename: "test_data/expected_results/colored",
    verbose: false,
};
const TEST_colored_verbose: ParserColoredTestParams = ParserColoredTestParams {
    cairo_filename: "test_data/cairo_files/colored.cairo",
    expected_colored_filename: "test_data/expected_results/colored_verbose",
    verbose: true,
};
#[cfg(feature = "fix_parser_tests")]
static COLORED_TEST_CASES: [&ParserColoredTestParams; 2] = [&TEST_colored, &TEST_colored_verbose];

#[test_case(&TEST_colored;"colored")]
#[test_case(&TEST_colored_verbose; "colored_verbose")]
fn parse_and_compare_colored(test_params: &ParserColoredTestParams) {
    parse_and_compare_colored_maybe_fix(test_params, false);
}
fn parse_and_compare_colored_maybe_fix(test_params: &ParserColoredTestParams, fix: bool) {
    // Make sure the colors are printed, even if the test doesn't run in a terminal.
    colored::control::set_override(true);

    let db_val = SimpleParserDatabase::default();
    let db = &db_val;

    let (syntax_root, _diagnostics) =
        get_syntax_root_and_diagnostics_from_file(db, test_params.cairo_filename);
    let printed = print_colored(db, &syntax_root, test_params.verbose);
    let expected = read_file(test_params.expected_colored_filename);
    compare_printed_and_expected_maybe_fix(
        printed,
        expected,
        test_params.expected_colored_filename,
        fix,
    );
}

/// Compares the printed output with the expected one. If `fix` is true, overwrites the output file
/// to fix the test.
fn compare_printed_and_expected_maybe_fix(
    printed: String,
    expected: String,
    expected_output_filename: &str,
    fix: bool,
) {
    if printed != expected {
        if fix {
            println!(
                "Test failed, fixing expected output file: {}. Please make sure that the result \
                 is correct.",
                expected_output_filename
            );
            std::fs::write(expected_output_filename, printed)
                .expect("Failed writing to the expected output file");
        } else {
            panic!(
                "assertion failed: printed != expected.\nTo automatically fix this, run:\n  cargo \
                 test -p parser -F fix_parser_tests --tests parser::test::fix_parser_tests -- \
                 --nocapture\nNote to carefully review it and not to blindly paste it there, as \
                 this loses the whole point of the test.\nTo debug this without fixing, use \
                 _debug_failure()."
            );
        }
    }
}

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

// Fixes the parser tests expected output files to the content of the parsed files.
// !!! Use this with caution! Review the changed output files carefully !!!
#[test]
#[cfg(feature = "fix_parser_tests")]
pub fn fix_parser_tests() {
    for test_params in TREE_TEST_CASES {
        parse_and_compare_tree_maybe_fix(test_params, true);
    }
    for test_params in COLORED_TEST_CASES {
        parse_and_compare_colored_maybe_fix(test_params, true);
    }
    println!("All Parser tests should be fixed now!");
}

/// Inputs:
/// - cairo_code
/// - top_level_kind - the highest SyntaxKind that is interesting. All other kinds, if not under it,
///   are ignored.
/// - ignored_kinds: Syntax kinds to ignore when printing. In this context, "ignore" means printing
///   the nodes themselves, but not their children.
/// Outputs:
/// - expected_tree - the printed syntax tree of the given cairo_code, ignoring the irrelevant
///   kinds.
pub fn test_partial_parser_tree(
    db: &mut SimpleParserDatabase,
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    // TODO(yuval): allow pointing to a code in another file.

    let file_id = create_virtual_file(db, "dummy_file.cairo", &inputs["cairo_code"]);
    let (syntax_root, diagnostics) =
        get_syntax_root_and_diagnostics(db, file_id, &inputs["cairo_code"]);
    // TODO(yuval): also test diagnostics
    assert_eq!(diagnostics.format(db), "");

    let ignored_kinds: Vec<&str> = inputs["ignored_kinds"].split('\n').collect();
    OrderedHashMap::from([(
        "expected_tree".into(),
        print_partial_tree(db, &syntax_root, &inputs["top_level_kind"], ignored_kinds),
    )])
}

parser_test!(
    diagnostic_tests,
    [
        "src/parser_test_data/module_diagnostics",
        "src/parser_test_data/exprs",
        "src/parser_test_data/func",
        "src/parser_test_data/if",
        "src/parser_test_data/match",
        "src/parser_test_data/pattern",
        "src/parser_test_data/question_mark",
        "src/parser_test_data/semicolon",
        "src/parser_test_data/reserved_identifier",
        "src/parser_test_data/underscore_not_supported",
    ],
    get_diagnostics
);

parser_test!(
    item_free_function,
    ["src/parser_test_data/item_free_function"],
    test_partial_parser_tree
);
parser_test!(
    function_signature,
    ["src/parser_test_data/function_signature"],
    test_partial_parser_tree
);
parser_test!(function_call, ["src/parser_test_data/function_call"], test_partial_parser_tree);
parser_test!(
    not_isnt_a_binary_operator,
    ["src/parser_test_data/not_isnt_a_binary_operator"],
    test_partial_parser_tree
);
parser_test!(item_trait, ["src/parser_test_data/item_trait"], test_partial_parser_tree);
parser_test!(let_statement, ["src/parser_test_data/let_statement"], test_partial_parser_tree);
parser_test!(if_else, ["src/parser_test_data/if_else"], test_partial_parser_tree);

parser_test!(literal, ["src/parser_test_data/literal"], test_partial_parser_tree);
parser_test!(module, ["src/parser_test_data/module"], test_partial_parser_tree);
