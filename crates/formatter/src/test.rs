use std::fs;

use db_utils::Upcast;
use filesystem::db::{FilesDatabase, FilesGroup};
use parser::utils::{get_syntax_root_and_diagnostics_from_file, SimpleParserDatabase};
use pretty_assertions::assert_eq;
use syntax::node::db::SyntaxDatabase;
use test_case::test_case;

use crate::{get_formatted_file, FormatterConfig};

#[salsa::database(SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}
impl Upcast<dyn FilesGroup> for DatabaseImpl {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}

// TODO(Gil): Add tests
#[test_case("test_data/cairo_files/test1.cairo", "test_data/expected_results/test1.cairo")]
#[test_case(
    "test_data/cairo_files/linebreaking.cairo",
    "test_data/expected_results/linebreaking.cairo"
)]
fn format_and_compare_file(unformatted_filename: &str, expected_filename: &str) {
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;

    let (syntax_root, diagnostics) =
        get_syntax_root_and_diagnostics_from_file(db, unformatted_filename);
    diagnostics.expect("A parsing error occurred while trying to format the code.");
    let config = FormatterConfig::default();
    let formatted_file = get_formatted_file(db, &syntax_root, config);
    let expected_file =
        fs::read_to_string(expected_filename).expect("Expected file does not exists.");
    assert_eq!(formatted_file, expected_file);
}
