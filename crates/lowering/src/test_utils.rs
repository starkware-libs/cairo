use db_utils::Upcast;
use defs::db::{DefsDatabase, DefsGroup};
use filesystem::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup};
use parser::db::ParserDatabase;
use semantic::db::{SemanticDatabase, SemanticGroup};
use syntax::node::db::{SyntaxDatabase, SyntaxGroup};

use crate::db::{LoweringDatabase, LoweringGroup};

#[salsa::database(
    LoweringDatabase,
    SemanticDatabase,
    DefsDatabase,
    ParserDatabase,
    SyntaxDatabase,
    FilesDatabase
)]
pub struct LoweringDatabaseForTesting {
    storage: salsa::Storage<LoweringDatabaseForTesting>,
}
impl salsa::Database for LoweringDatabaseForTesting {}
impl Default for LoweringDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}
impl AsFilesGroupMut for LoweringDatabaseForTesting {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl Upcast<dyn DefsGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn LoweringGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn LoweringGroup + 'static) {
        self
    }
}
