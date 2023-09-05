#![allow(non_upper_case_globals)]

use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_test_utils::{bool_input, has_disallowed_diagnostics};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::colored_printer::print_colored;
use crate::printer::{print_partial_tree, print_tree};
use crate::test_utils::{create_virtual_file, get_diagnostics, read_file};
use crate::utils::{get_syntax_root_and_diagnostics, SimpleParserDatabase};

/// Tests a partial parser tree of a given Cairo code, according to the configuration.
/// Inputs:
/// - cairo_code (either directly or from a path, if starting with ">>> file: ").
/// - top_level_kind - the highest SyntaxKind that is interesting. All other kinds, if not under it,
///   are ignored. If empty, the whole tree is printed.
/// - ignored_kinds: Syntax kinds to ignore when printing. In this context, "ignore" means printing
///   the nodes themselves, but not their children.
/// Outputs:
/// - expected_tree - the printed syntax tree of the given cairo_code, with/without trivia, ignoring
///   the irrelevant kinds.
pub fn test_partial_parser_tree(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> Result<OrderedHashMap<String, String>, String> {
    test_partial_parser_tree_inner(inputs, args, false)
}
pub fn test_partial_parser_tree_with_trivia(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> Result<OrderedHashMap<String, String>, String> {
    test_partial_parser_tree_inner(inputs, args, true)
}
fn test_partial_parser_tree_inner(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
    print_trivia: bool,
) -> Result<OrderedHashMap<String, String>, String> {
    let db = &SimpleParserDatabase::default();
    let (syntax_root, diagnostics) = get_syntax_root_and_diagnostics_from_inputs(db, inputs);
    has_disallowed_diagnostics(args, &diagnostics)?;

    let ignored_kinds: Vec<&str> = inputs["ignored_kinds"].split('\n').collect();
    Ok(OrderedHashMap::from([
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
    ]))
}

/// Tests the full parser tree of a given Cairo code.
pub fn test_full_parser_tree(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> Result<OrderedHashMap<String, String>, String> {
    let db = &SimpleParserDatabase::default();
    let (syntax_root, diagnostics) = get_syntax_root_and_diagnostics_from_inputs(db, inputs);
    has_disallowed_diagnostics(args, &diagnostics)?;

    Ok(OrderedHashMap::from([
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
    ]))
}

/// Tests the colored printing of a given Cairo code.
fn test_colored_parsed_code(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> Result<OrderedHashMap<String, String>, String> {
    let db = &SimpleParserDatabase::default();
    let (syntax_root, _) = get_syntax_root_and_diagnostics_from_inputs(db, inputs);
    Ok(OrderedHashMap::from([(
        "expected_colored".into(),
        print_colored(db, &syntax_root, bool_input(&inputs["is_verbose"])),
    )]))
}

/// Returns the syntax_root and diagnostics of a file in the db, according to the parser test
/// inputs.
fn get_syntax_root_and_diagnostics_from_inputs(
    db: &SimpleParserDatabase,
    inputs: &OrderedHashMap<String, String>,
) -> (SyntaxNode, String) {
    let cairo_code = get_cairo_code(inputs);
    let file_id = create_virtual_file(db, "dummy_file.cairo", &cairo_code);
    let (syntax_root, diagnostics) = get_syntax_root_and_diagnostics(db, file_id, &cairo_code);
    (syntax_root, diagnostics.format(db))
}
/// Returns the cairo code to parse from the "cairo_code" input, either directly or from the
/// provided file path.
fn get_cairo_code(inputs: &OrderedHashMap<String, String>) -> String {
    let cairo_code = inputs["cairo_code"].clone();
    if cairo_code.starts_with(">>> file: ") {
        let file_path = cairo_code[10..].to_string();
        return read_file(&file_path);
    }
    cairo_code
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
    diagnostic,
    "src/parser_test_data",
    {
        expr_diagnostics: "expr_diagnostics",
        enum_diagnostics: "enum_diagnostics",
        fn_: "fn",
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
    },
    get_diagnostics
);
cairo_lang_test_utils::test_file_test!(
    partial_parser_tree,
    "src/parser_test_data",
    {
        constant: "constant",
        enum_: "enum",
        expr: "expr",
        loop_: "loop",
        item_free_function: "item_free_function",
        function_signature: "function_signature",
        function_call: "function_call",
        unary_only_operators: "unary_only_operators",
        item_trait: "item_trait",
        item_inline_macro: "item_inline_macro",
        let_statement: "let_statement",
        if_else: "if_else",
        impl_alias: "impl_alias",
        literal: "literal",
        string_literal: "string_literal",
        logical_operator: "logical_operator",
        attribute_errors: "attribute_errors",
        module: "module",
        op_eq: "op_eq",
        array: "array",
        attrs: "attrs",
        inline_macro: "inline_macro",
        generics: "generics",
    },
    test_partial_parser_tree
);
cairo_lang_test_utils::test_file_test!(
    partial_parser_tree_with_trivia,
    "src/parser_test_data",
    {
        path: "path_with_trivia",
        path_compat: "path_with_trivia_compat",
    },
    test_partial_parser_tree_with_trivia
);
