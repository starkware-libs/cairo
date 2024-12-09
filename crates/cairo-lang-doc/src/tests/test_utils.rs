use anyhow::{Result, anyhow};
use cairo_lang_defs::db::{DefsDatabase, DefsGroup, init_defs_group};
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::{
    AsFilesGroupMut, CrateConfiguration, ExternalFiles, FilesDatabase, FilesGroup, FilesGroupEx,
    init_dev_corelib, init_files_group,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::ids::{CrateId, Directory, FileLongId};
use cairo_lang_parser::db::{ParserDatabase, ParserGroup};
use cairo_lang_semantic::db::{
    PluginSuiteInput, SemanticDatabase, SemanticGroup, init_semantic_group,
};
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_utils::{Intern, Upcast};

use crate::db::{DocDatabase, DocGroup};

#[salsa::database(
    ParserDatabase,
    SemanticDatabase,
    DocDatabase,
    DefsDatabase,
    SyntaxDatabase,
    FilesDatabase
)]
pub struct TestDatabase {
    storage: salsa::Storage<TestDatabase>,
}

impl salsa::Database for TestDatabase {}
impl ExternalFiles for TestDatabase {}

impl Default for TestDatabase {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_defs_group(&mut res);
        init_semantic_group(&mut res);

        let plugin_suite = res.intern_plugin_suite(PluginSuite::default());
        res.set_default_plugins_from_suite(plugin_suite);

        res
    }
}

impl TestDatabase {
    pub fn new() -> Result<Self> {
        let mut db = Self::default();
        let path =
            detect_corelib().ok_or_else(|| anyhow!("Failed to find development corelib."))?;
        init_dev_corelib(&mut db, path);
        Ok(db)
    }
}
impl AsFilesGroupMut for TestDatabase {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn DocGroup> for TestDatabase {
    fn upcast(&self) -> &(dyn DocGroup + 'static) {
        self
    }
}
impl Upcast<dyn DefsGroup> for TestDatabase {
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for TestDatabase {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn ParserGroup> for TestDatabase {
    fn upcast(&self) -> &(dyn ParserGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for TestDatabase {
    fn upcast(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for TestDatabase {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}

pub fn setup_test_module<T: DefsGroup + AsFilesGroupMut + ?Sized>(
    db: &mut T,
    content: &str,
) -> CrateId {
    let crate_id = CrateId::plain(db, "test");
    let directory = Directory::Real("src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(directory)));
    let file = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
    db.as_files_group_mut().override_file_content(file, Some(content.into()));
    let syntax_diagnostics = db.file_syntax_diagnostics(file).format(Upcast::upcast(db));
    assert_eq!(syntax_diagnostics, "");
    crate_id
}

pub fn set_file_content(db: &mut TestDatabase, path: &str, content: &str) {
    let file_id = FileLongId::OnDisk(path.into()).intern(db);
    db.as_files_group_mut().override_file_content(file_id, Some(content.into()));
}
