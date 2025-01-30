#![allow(non_upper_case_globals)]

use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::{
    bool_input, get_direct_or_file_content, verify_diagnostics_expectation,
};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::colored_printer::print_colored;
use crate::printer::{print_partial_tree, print_tree};
use crate::test_utils::{create_virtual_file, get_diagnostics};
use crate::utils::{SimpleParserDatabase, get_syntax_root_and_diagnostics};

/// Tests a partial parser tree of a given Cairo code, according to the configuration.
///
/// Inputs:
/// - cairo_code (either directly or from a path, if starting with ">>> file: ").
/// - top_level_kind - the highest SyntaxKind that is interesting. All other kinds, if not under it,
///   are ignored. If empty, the whole tree is printed.
/// - ignored_kinds: Syntax kinds to ignore when printing. In this context, "ignore" means printing
///   the nodes themselves, but not their children.
///
/// Outputs:
/// - expected_tree - the printed syntax tree of the given cairo_code, with/without trivia, ignoring
///   the irrelevant kinds.
pub fn test_partial_parser_tree(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    test_partial_parser_tree_inner(inputs, args, false)
}
pub fn test_partial_parser_tree_with_trivia(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    test_partial_parser_tree_inner(inputs, args, true)
}
fn test_partial_parser_tree_inner(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
    print_trivia: bool,
) -> TestRunnerResult {
    let db = &SimpleParserDatabase::default();
    let (syntax_root, diagnostics) = get_syntax_root_and_diagnostics_from_inputs(db, inputs);
    let error = verify_diagnostics_expectation(args, &diagnostics);

    let ignored_kinds: Vec<&str> = inputs["ignored_kinds"].split('\n').collect();
    TestRunnerResult {
        outputs: OrderedHashMap::from([
            (
                "expected_tree".into(),
                print_partial_tree(
                    db,
                    &syntax_root,
                    &inputs["top_level_kind"],
                    ignored_kinds,
                    print_trivia,
                ),
            ),
            ("expected_diagnostics".into(), diagnostics),
        ]),
        error,
    }
}

/// Tests the full parser tree of a given Cairo code.
pub fn test_full_parser_tree(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &SimpleParserDatabase::default();
    let (syntax_root, diagnostics) = get_syntax_root_and_diagnostics_from_inputs(db, inputs);
    let error = verify_diagnostics_expectation(args, &diagnostics);

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            (
                "expected_tree".into(),
                print_tree(
                    db,
                    &syntax_root,
                    bool_input(&inputs["print_colors"]),
                    bool_input(&inputs["print_trivia"]),
                ),
            ),
            ("expected_diagnostics".into(), diagnostics),
        ]),
        error,
    }
}

/// Tests the colored printing of a given Cairo code.
fn test_colored_parsed_code(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    colored::control::set_override(true);
    let db = &SimpleParserDatabase::default();
    let (syntax_root, _) = get_syntax_root_and_diagnostics_from_inputs(db, inputs);
    use anstream::adapter::strip_str;

    let colored_output = print_colored(db, &syntax_root, bool_input(&inputs["is_verbose"]));
    let striped_output = strip_str(&colored_output).to_string();
    assert_ne!(colored_output, striped_output, "The output is not colored");

    TestRunnerResult {
        outputs: OrderedHashMap::from([("expected".into(), striped_output)]),
        error: None,
    }
}

/// Returns the syntax_root and diagnostics of a file in the db, according to the parser test
/// inputs.
fn get_syntax_root_and_diagnostics_from_inputs(
    db: &SimpleParserDatabase,
    inputs: &OrderedHashMap<String, String>,
) -> (SyntaxNode, String) {
    let (file_path, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
    let file_id = create_virtual_file(db, file_path, &cairo_code);
    let (syntax_root, diagnostics) = get_syntax_root_and_diagnostics(db, file_id, &cairo_code);
    (syntax_root, diagnostics.format(db))
}

cairo_lang_test_utils::test_file_test!(
    full_parser_tree,
    "src/parser_test_data/full_trees",
    {
        short: "short",
        test1: "test1",
        test2: "test2",
        test3: "test3",
    },
    test_full_parser_tree
);
cairo_lang_test_utils::test_file_test!(
    colored_parsed_code,
    "src/parser_test_data/",
    {
        colored: "colored",
    },
    test_colored_parsed_code
);
cairo_lang_test_utils::test_file_test!(
    diagnostics,
    "src/parser_test_data/diagnostics",
    {
        expr_diagnostics: "expr_diagnostics",
        enum_diagnostics: "enum_diagnostics",
        fn_: "fn",
        generic_params: "generic_params",
        if_: "if",
        illegal_string_escapes: "illegal_string_escapes",
        match_: "match",
        module_diagnostics: "module_diagnostics",
        pattern: "pattern",
        question_mark: "question_mark",
        reserved_identifier: "reserved_identifier",
        semicolon: "semicolon",
        underscore_not_supported: "underscore_not_supported",
        unterminated_string: "unterminated_string",
        skipped_tokens: "skipped_tokens",
    },
    get_diagnostics
);
cairo_lang_test_utils::test_file_test!(
    partial_parser_tree,
    "src/parser_test_data/partial_trees",
    {
        constant: "constant",
        enum_: "enum",
        expr: "expr",
        loop_: "loop",
        item_free_function: "item_free_function",
        function_signature: "function_signature",
        function_call: "function_call",
        closure: "closure",
        match_: "match",
        unary_only_operators: "unary_only_operators",
        item_trait: "item_trait",
        item_inline_macro: "item_inline_macro",
        let_statement: "let_statement",
        if_else: "if_else",
        impl_alias: "impl_alias",
        literal: "literal",
        string_literal: "string_literal",
        logical_operator: "logical_operator",
        module: "module",
        op_eq: "op_eq",
        array: "array",
        attrs: "attrs",
        inline_macro: "inline_macro",
        generics: "generics",
        generic_params: "generic_params",
        while_: "while",
        for_: "for",
        range: "range",
        use_: "use",
        type_alias: "type_alias",
},
    test_partial_parser_tree
);
cairo_lang_test_utils::test_file_test!(
    partial_parser_tree_with_trivia,
    "src/parser_test_data/partial_trees_with_trivia",
    {
        comments: "comments",
        path: "path",
        path_compat: "path_compat",
        attribute_errors: "attribute_errors",
    },
    test_partial_parser_tree_with_trivia
);
