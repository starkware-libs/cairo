use std::fs;
use std::path::PathBuf;

use cairo_lang_filesystem::db::{ExternalFiles, FilesDatabase, FilesGroup};
use cairo_lang_parser::utils::{SimpleParserDatabase, get_syntax_root_and_diagnostics_from_file};
use cairo_lang_syntax::node::db::SyntaxDatabase;
use cairo_lang_utils::Upcast;
use pretty_assertions::assert_eq;
use test_case::test_case;

use crate::{FormatterConfig, get_formatted_file};

#[salsa::database(SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}
impl ExternalFiles for DatabaseImpl {}
impl Upcast<dyn FilesGroup> for DatabaseImpl {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}

// TODO(Gil): Add tests
#[test_case(
    "test_data/cairo_files/test1.cairo",
    "test_data/expected_results/test1.cairo",
    false,
    false,
    false,
    false,
    false
)]
#[test_case(
    "test_data/cairo_files/linebreaking.cairo",
    "test_data/expected_results/linebreaking.cairo",
    false,
    false,
    false,
    false,
    false
)]
#[test_case(
    "test_data/cairo_files/attrs.cairo",
    "test_data/expected_results/attrs.cairo",
    false,
    false,
    false,
    false,
    false
)]
#[test_case(
    "test_data/cairo_files/use_sorting.cairo",
    "test_data/expected_results/use_sorting.cairo",
    true,
    false,
    false,
    false,
    false
)]
#[test_case(
    "test_data/cairo_files/fmt_skip.cairo",
    "test_data/expected_results/fmt_skip.cairo",
    false,
    false,
    false,
    false,
    false
)]
#[test_case(
    "test_data/cairo_files/sorted_mod_use.cairo",
    "test_data/expected_results/sorted_mod_use.cairo",
    true,
    false,
    false,
    false,
    false
)]
#[test_case(
    "test_data/cairo_files/sort_inner_use.cairo",
    "test_data/expected_results/sort_inner_use.cairo",
    true,
    false,
    false,
    false,
    false
)]
// TODO (Dean): Move these tests to our main test infrastructure.
#[test_case(
    "test_data/cairo_files/sort_single_line.cairo",
    "test_data/expected_results/sort_single_line.cairo",
    true,
    false,
    false,
    false,
    false
)]
#[test_case(
    "test_data/cairo_files/sort_line_by_line.cairo",
    "test_data/expected_results/sort_line_by_line.cairo",
    true,
    true,
    true,
    false,
    false
)]
#[test_case(
    "test_data/cairo_files/use_merge.cairo",
    "test_data/expected_results/use_merge.cairo",
    true,
    false,
    false,
    true,
    false
)]
#[test_case(
    "test_data/cairo_files/use_merge_with_dup.cairo",
    "test_data/expected_results/use_merge_with_dup.cairo",
    true,
    false,
    false,
    true,
    true
)]
fn format_and_compare_file(
    unformatted_filename: &str,
    expected_filename: &str,
    use_sorting: bool,
    tuple_line_breaking: bool,
    fixed_array_line_breaking: bool,
    merge_use_statements: bool,
    allow_duplicate_uses: bool,
) {
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

    let config = FormatterConfig::default()
        .sort_module_level_items(Some(use_sorting))
        .tuple_breaking_behavior(Some(tuple_line_breaking.into()))
        .fixed_array_breaking_behavior(Some(fixed_array_line_breaking.into()))
        .merge_use_items(Some(merge_use_statements))
        .allow_duplicate_uses(Some(allow_duplicate_uses));

    let formatted_file = get_formatted_file(db, &syntax_root, config);
    let expected_file =
        fs::read_to_string(expected_filename).expect("Expected file does not exist.");
    assert_eq!(formatted_file, expected_file);
}
