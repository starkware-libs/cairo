use std::path::PathBuf;
use std::sync::Arc;

use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use filesystem::db::{AsFilesGroup, FilesDatabase, FilesGroup};
use filesystem::ids::{CrateLongId, FileLongId, ModuleId};
use parser::db::ParserDatabase;
use semantic::corelib::core_config;
use semantic::db::SemanticDatabase;
use sierra_generator::db::{SierraGenDatabase, SierraGenGroup};
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};
use test_case::test_case;

#[salsa::database(
    SemanticDatabase,
    DefsDatabase,
    ParserDatabase,
    SyntaxDatabase,
    FilesDatabase,
    SierraGenDatabase
)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}
impl AsDefsGroup for DatabaseImpl {
    fn as_defs_group(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}
impl AsSyntaxGroup for DatabaseImpl {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl AsFilesGroup for DatabaseImpl {
    fn as_files_group(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}

fn setup(cairo_file: &str) -> (DatabaseImpl, ModuleId) {
    let mut db_val = DatabaseImpl::default();
    let db = &mut db_val;
    let dir = env!("CARGO_MANIFEST_DIR");
    // Pop the "/tests" suffix.
    let mut path = PathBuf::from(dir).parent().unwrap().to_owned();
    path.push("examples");
    path.push(cairo_file);

    let file_id = db.intern_file(FileLongId::OnDisk(path));
    let crate_id = db.intern_crate(CrateLongId("mock_crate".into()));
    db.set_project_config(core_config(db).with_crate(crate_id, file_id));
    let module_id = ModuleId::CrateRoot(crate_id);
    (db_val, module_id)
}

fn compile_to_sierra(cairo_file: &str) -> Arc<sierra::program::Program> {
    let (db, module_id) = setup(cairo_file);
    db.get_program_code(module_id).expect("Creating sierra code should succeed").unwrap()
}

#[test_case("fib.cairo")]
fn cairo_to_sierra(cairo_file: &str) {
    compile_to_sierra(cairo_file);
}

#[ignore = "Compilation to sierra is yet to be a final product"]
#[test_case("fib.cairo")]
fn cairo_to_casm(cairo_file: &str) {
    let sierra_program = compile_to_sierra(cairo_file);
    sierra_to_casm::compiler::compile(&sierra_program).unwrap();
}
