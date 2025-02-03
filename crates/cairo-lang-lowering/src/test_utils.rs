use std::sync::{LazyLock, Mutex};

use cairo_lang_defs::db::{DefsDatabase, DefsGroup, init_defs_group, try_ext_as_virtual_impl};
use cairo_lang_filesystem::db::{
    AsFilesGroupMut, ExternalFiles, FilesDatabase, FilesGroup, init_dev_corelib, init_files_group,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::ids::VirtualFile;
use cairo_lang_parser::db::{ParserDatabase, ParserGroup};
use cairo_lang_semantic::db::{
    PluginSuiteInput, SemanticDatabase, SemanticGroup, init_semantic_group,
};
use cairo_lang_semantic::inline_macros::get_default_plugin_suite;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_utils::Upcast;

use crate::db::{LoweringDatabase, LoweringGroup, init_lowering_group};
use crate::utils::InliningStrategy;

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
impl ExternalFiles for LoweringDatabaseForTesting {
    fn try_ext_as_virtual(&self, external_id: salsa::InternId) -> Option<VirtualFile> {
        try_ext_as_virtual_impl(self.upcast(), external_id)
    }
}
impl salsa::ParallelDatabase for LoweringDatabaseForTesting {
    fn snapshot(&self) -> salsa::Snapshot<LoweringDatabaseForTesting> {
        salsa::Snapshot::new(LoweringDatabaseForTesting { storage: self.storage.snapshot() })
    }
}
impl LoweringDatabaseForTesting {
    pub fn new() -> Self {
        let mut res = LoweringDatabaseForTesting { storage: Default::default() };
        init_files_group(&mut res);
        init_defs_group(&mut res);
        init_semantic_group(&mut res);

        let suite = res.intern_plugin_suite(get_default_plugin_suite());
        res.set_default_plugins_from_suite(suite);

        let corelib_path = detect_corelib().expect("Corelib not found in default location.");
        init_dev_corelib(&mut res, corelib_path);
        init_lowering_group(&mut res, InliningStrategy::Default);
        res
    }

    /// Snapshots the db for read only.
    pub fn snapshot(&self) -> LoweringDatabaseForTesting {
        LoweringDatabaseForTesting { storage: self.storage.snapshot() }
    }
}

pub static SHARED_DB: LazyLock<Mutex<LoweringDatabaseForTesting>> =
    LazyLock::new(|| Mutex::new(LoweringDatabaseForTesting::new()));
impl Default for LoweringDatabaseForTesting {
    fn default() -> Self {
        SHARED_DB.lock().unwrap().snapshot()
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
impl Upcast<dyn ParserGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn ParserGroup + 'static) {
        self
    }
}
