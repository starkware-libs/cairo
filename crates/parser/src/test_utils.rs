use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use db_utils::Upcast;
use diagnostics::{Diagnostics, DiagnosticsBuilder};
use filesystem::db::{init_files_group, FilesDatabase, FilesGroup};
use filesystem::ids::{FileId, FileLongId, VirtualFile};
use smol_str::SmolStr;
use syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use syntax::node::{SyntaxNode, TypedSyntaxNode};
use utils::ordered_hash_map::OrderedHashMap;

use crate::db::ParserDatabase;
use crate::parser::Parser;
use crate::ParserDiagnostic;

// Test salsa database.
#[salsa::database(ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct ParserDatabaseForTesting {
    storage: salsa::Storage<ParserDatabaseForTesting>,
}
impl salsa::Database for ParserDatabaseForTesting {}
impl Default for ParserDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}

impl Upcast<dyn SyntaxGroup> for ParserDatabaseForTesting {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}

// TODO(gil): Move these methods into the parser lib.
pub fn get_syntax_root_and_diagnostics_from_file(
    db: &ParserDatabaseForTesting,
    cairo_filename: &str,
) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
    let file_id = FileId::new(db, PathBuf::from(cairo_filename));
    let contents = db.file_content(file_id).unwrap();
    get_syntax_root_and_diagnostics(db, file_id, contents.as_str())
}

pub fn get_syntax_root_and_diagnostics(
    db: &ParserDatabaseForTesting,
    file_id: FileId,
    contents: &str,
) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
    let mut diagnostics = DiagnosticsBuilder::new();
    let syntax_root = Parser::parse_file(db, &mut diagnostics, file_id, contents);
    (syntax_root.as_syntax_node(), diagnostics.build())
}

pub fn read_file(filename: &str) -> String {
    fs::read_to_string(filename)
        .unwrap_or_else(|_| panic!("Something went wrong reading file {}", filename))
}

pub fn get_diagnostics(
    db: &mut ParserDatabaseForTesting,
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    let code = &inputs["cairo_code"];

    let file_id = create_virtual_file(db, "dummy_file.cairo", code);
    let (_, diagnostics) = get_syntax_root_and_diagnostics(db, file_id, code);
    OrderedHashMap::from([("expected_diagnostics".into(), diagnostics.format(db))])
}

// TODO(yuval): stop virtual files for tests anymore. See semantic tests.
/// Creates a virtual file with the given content and returns its ID.
pub fn create_virtual_file(
    db: &ParserDatabaseForTesting,
    file_name: impl Into<SmolStr>,
    content: &str,
) -> FileId {
    db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: file_name.into(),
        content: Arc::new(content.into()),
    }))
}

#[macro_export]
macro_rules! parser_test {
    ($test_name:ident, $filenames:expr, $func:ident) => {
        utils::test_file_test!($test_name, $filenames, ParserDatabaseForTesting, $func);
    };
}
