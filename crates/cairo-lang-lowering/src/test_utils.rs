use std::sync::Mutex;

use cairo_lang_defs::db::{DefsDatabase, DefsGroup};
use cairo_lang_filesystem::db::{
    init_dev_corelib, init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_parser::db::ParserDatabase;
use cairo_lang_semantic::db::{SemanticDatabase, SemanticGroup};
use cairo_lang_semantic::inline_macros::get_default_plugin_suite;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_utils::Upcast;
use once_cell::sync::Lazy;

use crate::db::{init_lowering_group, LoweringDatabase, LoweringGroup};

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
impl salsa::ParallelDatabase for LoweringDatabaseForTesting {
    fn snapshot(&self) -> salsa::Snapshot<LoweringDatabaseForTesting> {
        salsa::Snapshot::new(LoweringDatabaseForTesting { storage: self.storage.snapshot() })
    }
}
impl LoweringDatabaseForTesting {
    /// Snapshots the db for read only.
    pub fn snapshot(&self) -> LoweringDatabaseForTesting {
        LoweringDatabaseForTesting { storage: self.storage.snapshot() }
    }
}
pub static SHARED_DB: Lazy<Mutex<LoweringDatabaseForTesting>> = Lazy::new(|| {
    let mut res = LoweringDatabaseForTesting { storage: Default::default() };
    init_files_group(&mut res);
    let suite = get_default_plugin_suite();
    res.set_macro_plugins(suite.plugins);
    res.set_inline_macro_plugins(suite.inline_macro_plugins.into());
    res.set_analyzer_plugins(suite.analyzer_plugins);

    let corelib_path = detect_corelib().expect("Corelib not found in default location.");
    init_dev_corelib(&mut res, corelib_path);
    init_lowering_group(&mut res);
    Mutex::new(res)
});
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
