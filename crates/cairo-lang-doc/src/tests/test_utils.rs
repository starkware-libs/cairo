use anyhow::{Result, anyhow};
use cairo_lang_defs::db::{DefsGroup, init_defs_group};
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::{
    CrateConfiguration, ExternalFiles, FilesGroup, FilesGroupEx, init_dev_corelib, init_files_group,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::ids::{CrateId, Directory, FileLongId};
use cairo_lang_filesystem::{override_file_content, set_crate_config};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::{Elongate, PluginSuiteInput, SemanticGroup, init_semantic_group};
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_utils::{Intern, Upcast};

use crate::db::DocGroup;

#[salsa::db]
#[derive(Clone)]
pub struct TestDatabase {
    storage: salsa::Storage<TestDatabase>,
}
#[salsa::db]
impl salsa::Database for TestDatabase {}
impl ExternalFiles for TestDatabase {}

impl Default for TestDatabase {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_defs_group(&mut res);
        init_semantic_group(&mut res);

        let plugin_suite = PluginSuite::default();
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

impl Elongate for TestDatabase {
    fn elongate(&self) -> &dyn SemanticGroup {
        self
    }
}

impl<'db> Upcast<'db, dyn DocGroup> for TestDatabase {
    fn upcast(&self) -> &dyn DocGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn DefsGroup> for TestDatabase {
    fn upcast(&self) -> &dyn DefsGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn FilesGroup> for TestDatabase {
    fn upcast(&self) -> &dyn FilesGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn ParserGroup> for TestDatabase {
    fn upcast(&self) -> &dyn ParserGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn SemanticGroup> for TestDatabase {
    fn upcast(&self) -> &dyn SemanticGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn SyntaxGroup> for TestDatabase {
    fn upcast(&self) -> &dyn SyntaxGroup {
        self
    }
}

pub fn setup_test_module(db: &mut dyn DefsGroup, content: &str) {
    let crate_id = test_crate_id(db);
    let directory = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(directory)));
    let crate_id = test_crate_id(db);
    let file = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
    override_file_content!(db, file, Some(content.into()));
    let crate_id = test_crate_id(db);
    let file = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
    let syntax_diagnostics = db.file_syntax_diagnostics(file).format(db);
    assert_eq!(syntax_diagnostics, "");
}

pub fn test_crate_id<'db>(db: &'db dyn DefsGroup) -> CrateId<'db> {
    CrateId::plain(db, "test")
}

pub fn setup_test_module_without_syntax_diagnostics(db: &mut dyn DefsGroup, content: &str) {
    let crate_id = test_crate_id(db);
    let directory = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(directory)));
    let crate_id = test_crate_id(db);
    let file = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
    override_file_content!(db, file, Some(content.into()));
}

pub fn set_file_content(db: &mut TestDatabase, path: &str, content: &str) {
    let file_id = FileLongId::OnDisk(path.into()).intern(db);
    override_file_content!(db, file_id, Some(content.into()));
}
