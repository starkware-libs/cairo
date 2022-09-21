use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use db_utils::Upcast;
use diagnostics::Diagnostics;
use filesystem::db::{init_files_group, FilesDatabase, FilesGroup};
use filesystem::ids::{FileLongId, VirtualFile};
use syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use syntax::node::{SyntaxNode, TypedSyntaxNode};

use crate::db::ParserDatabase;
use crate::parser::Parser;
use crate::{diagnostic, ParserDiagnostic};

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

pub fn get_syntax_root_and_diagnostics(
    db: &ParserDatabaseForTesting,
    cairo_filename: &str,
) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
    let file_id = db.intern_file(FileLongId::OnDisk(PathBuf::from(cairo_filename)));
    let contents = db.file_content(file_id).unwrap();
    let mut diagnostics = Diagnostics::new();
    let syntax_root = Parser::parse_file(db, &mut diagnostics, file_id, contents.as_str());
    (syntax_root.as_syntax_node(), diagnostics)
}

pub fn read_file(filename: &str) -> String {
    fs::read_to_string(filename)
        .unwrap_or_else(|_| panic!("Something went wrong reading file {}", filename))
}

pub fn get_diagnostics(
    db: &ParserDatabaseForTesting,
    code: &str,
) -> diagnostics::Diagnostics<diagnostic::ParserDiagnostic> {
    let mut diagnostics = Diagnostics::new();
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "dummy_file.cairo".into(),
        content: Arc::new(code.into()),
    }));
    Parser::parse_file(db, &mut diagnostics, file_id, code);
    diagnostics
}

/// Creates a test for a given function that reads test files.
/// filenames - a vector of tests files the test will apply to.
/// db - the salsa DB to use for the test.
/// func - the function to be applied on the test params to generate the tested result.
/// params - the function parameters. For functions specialized here the parameters can be omitted.
#[macro_export]
macro_rules! diagnostics_test {
    ($test_name:ident, $filenames:expr, $db:expr, $func:expr, $($param:expr),*) => {
        #[test]
        fn $test_name() -> Result<(), std::io::Error> {
            let db = $db;
            for filename in $filenames {
                let tests = utils::parse_test_file::parse_test_file(
                    std::path::Path::new(filename)
                )?;
                for (name, test) in tests {
                    let diagnostics = $func(
                        &db,
                        $(&test[$param],)*
                    ).format(&db);
                    pretty_assertions::assert_eq!(
                        diagnostics.trim(), test["Expected Result"], "\"{name}\" failed."
                    );
                }
            }
            Ok(())
        }
    };

    ($test_name:ident, $filenames:expr, $db:expr, get_diagnostics) => {
        diagnostics_test!($test_name, $filenames, $db, get_diagnostics, "Code");
    };
}
