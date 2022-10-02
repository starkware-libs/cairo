use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use db_utils::Upcast;
use diagnostics::{Diagnostics, DiagnosticsBuilder};
use filesystem::db::{init_files_group, FilesDatabase, FilesGroup};
use filesystem::ids::{FileId, FileLongId, VirtualFile};
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

// TODO(gil): Move this method into the parser lib.
pub fn get_syntax_root_and_diagnostics(
    db: &ParserDatabaseForTesting,
    cairo_filename: &str,
) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
    let file_id = FileId::new(db, PathBuf::from(cairo_filename));
    let contents = db.file_content(file_id).unwrap();
    let mut diagnostics = DiagnosticsBuilder::new();
    let syntax_root = Parser::parse_file(db, &mut diagnostics, file_id, contents.as_str());
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

    let mut diagnostics = DiagnosticsBuilder::new();
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "dummy_file.cairo".into(),
        content: Arc::new(code.into()),
    }));
    Parser::parse_file(db, &mut diagnostics, file_id, code);
    OrderedHashMap::from([("expected_diagnostics".into(), diagnostics.build().format(db))])
}

#[macro_export]
macro_rules! parser_test {
    ($test_name:ident, $filenames:expr, $func:ident) => {
        utils::test_file_test!($test_name, $filenames, ParserDatabaseForTesting, $func);
    };
}
