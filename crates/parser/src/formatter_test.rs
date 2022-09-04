use std::fs;
use std::path::PathBuf;

use diagnostics::{Diagnostics, WithDiagnostics};
use filesystem::db::{FilesDatabase, FilesGroup};
use filesystem::ids::FileLongId;
use syntax::node::db::SyntaxDatabase;
use syntax::node::{SyntaxNode, TypedSyntaxNode};
use test_case::test_case;

use crate::formatter::{get_formatted_file, FormatterConfig};
use crate::parser::{Parser, ParserDiagnostic};

#[salsa::database(SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

fn read_file(filename: &str) -> String {
    fs::read_to_string(filename)
        .unwrap_or_else(|_| panic!("Something went wrong reading file {}", filename))
}

fn get_syntax_root_and_diagnostics(
    db: &DatabaseImpl,
    cairo_filename: &str,
) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
    let file_id = db.intern_file(FileLongId::OnDisk(PathBuf::from(cairo_filename)));
    let contents = db.file_content(file_id).unwrap();
    let parser = Parser::from_text(db, file_id, contents.as_str());
    let WithDiagnostics { value: syntax_root, diagnostics } = parser.parse_syntax_file();
    (syntax_root.as_syntax_node(), diagnostics)
}

#[test_case(
    "test_data/cairo_files/formatter_test.cairo",
    "test_data/expected_results/formatter_test.cairo"
)]
fn parse_and_compare_tree(unformatted_filename: &str, expected_filename: &str) {
    let db_val = DatabaseImpl::default();
    let db = &db_val;

    let (syntax_root, _) = get_syntax_root_and_diagnostics(db, unformatted_filename);
    let config = FormatterConfig::new();
    let formatted_file = get_formatted_file(db, &syntax_root, config);
    println!("{formatted_file}");
    let expected_file = read_file(expected_filename);
    assert_eq!(formatted_file, expected_file);
}
