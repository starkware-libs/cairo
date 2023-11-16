use std::fs;
use std::path::PathBuf;

use cairo_lang_filesystem::db::{FilesDatabase, FilesGroup};
use cairo_lang_parser::utils::{get_syntax_root_and_diagnostics_from_file, SimpleParserDatabase};
use cairo_lang_syntax::node::db::SyntaxDatabase;
use cairo_lang_utils::Upcast;
use pretty_assertions::assert_eq;
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
#[test_case("test_data/cairo_files/test1.cairo", "test_data/expected_results/test1.cairo", false)]
#[test_case(
    "test_data/cairo_files/linebreaking.cairo",
    "test_data/expected_results/linebreaking.cairo",
    false
)]
#[test_case("test_data/cairo_files/attrs.cairo", "test_data/expected_results/attrs.cairo", false)]
#[test_case(
    "test_data/cairo_files/use_sorting.cairo",
    "test_data/expected_results/use_sorting.cairo",
    true
)]
#[test_case(
    "test_data/cairo_files/fmt_skip.cairo",
    "test_data/expected_results/fmt_skip.cairo",
    false
)]
fn format_and_compare_file(unformatted_filename: &str, expected_filename: &str, use_sorting: bool) {
    let db_val = SimpleParserDatabase::default();
    let db = &db_val;

    let unformatted_filepath: PathBuf =
        [env!("CARGO_MANIFEST_DIR"), unformatted_filename].into_iter().collect();
    let (syntax_root, diagnostics) =
        get_syntax_root_and_diagnostics_from_file(db, unformatted_filepath);
    diagnostics.expect(&format!(
        "There were parsing errors while trying to format the code:\n{}",
        diagnostics.format(db)
    ));

    let config = FormatterConfig { sort_module_level_items: use_sorting, ..Default::default() };

    let formatted_file = get_formatted_file(db, &syntax_root, config);
    let expected_file =
        fs::read_to_string(expected_filename).expect("Expected file does not exists.");
    assert_eq!(formatted_file, expected_file);
}
