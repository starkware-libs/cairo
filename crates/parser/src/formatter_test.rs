use filesystem::db::FilesDatabase;
use syntax::node::db::SyntaxDatabase;
use test_case::test_case;

use crate::formatter::{get_formatted_file, FormatterConfig};
use crate::test_utils::{get_syntax_root_and_diagnostics, read_file, ParserDatabaseForTesting};

#[salsa::database(SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

// TODO(Gil): Add tests
#[test_case(
    "test_data/cairo_files/formatter_test.cairo",
    "test_data/expected_results/formatter_test.cairo"
)]
fn format_and_compare_file(unformatted_filename: &str, expected_filename: &str) {
    let db_val = ParserDatabaseForTesting::default();
    let db = &db_val;

    let (syntax_root, diagnostics) = get_syntax_root_and_diagnostics(db, unformatted_filename);
    assert!(diagnostics.0.is_empty(), "A parsing error occured while trying to format the code.");
    let config = FormatterConfig::default();
    let formatted_file = get_formatted_file(db, &syntax_root, config);
    let expected_file = read_file(expected_filename);
    assert_eq!(formatted_file, expected_file);
}
