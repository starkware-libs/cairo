use db_utils::Upcast;
use defs::db::{DefsDatabase, DefsGroup};
use defs::ids::ModuleId;
use filesystem::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup};
use lowering::db::{LoweringDatabase, LoweringGroup};
use parser::db::ParserDatabase;
use semantic::db::{SemanticDatabase, SemanticGroup};
use semantic::test_utils::setup_test_crate;
use syntax::node::db::{SyntaxDatabase, SyntaxGroup};

use crate::db::{SierraGenDatabase, SierraGenGroup};
use crate::replace_ids::replace_sierra_ids_in_program;

#[salsa::database(
    DefsDatabase,
    FilesDatabase,
    LoweringDatabase,
    ParserDatabase,
    SemanticDatabase,
    SierraGenDatabase,
    SyntaxDatabase
)]
pub struct SierraGenDatabaseForTesting {
    storage: salsa::Storage<SierraGenDatabaseForTesting>,
}
impl salsa::Database for SierraGenDatabaseForTesting {}
impl Default for SierraGenDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}
impl AsFilesGroupMut for SierraGenDatabaseForTesting {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl Upcast<dyn DefsGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn defs::db::DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn semantic::db::SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn LoweringGroup> for SierraGenDatabaseForTesting {
    fn upcast(&self) -> &(dyn lowering::db::LoweringGroup + 'static) {
        self
    }
}

/// Compiles 'content' to sierra and replaces the sierra ids to make it readable.
pub fn checked_compile_to_sierra(content: &str) -> sierra::program::Program {
    let mut db_val = SierraGenDatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = setup_test_crate(db, content);

    let module_id = ModuleId::CrateRoot(crate_id);
    db.module_semantic_diagnostics(module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected semantic diagnostics");
    db.module_lowering_diagnostics(module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected lowering diagnostics.");
    db.module_sierra_diagnostics(module_id)
        .expect_with_db(db, "Unexpected Sierra generation diagnostics.");

    let program = db.crate_sierra_program(crate_id).unwrap();
    replace_sierra_ids_in_program(db, &program)
}
