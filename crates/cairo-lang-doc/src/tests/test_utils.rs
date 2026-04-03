use anyhow::{Result, anyhow};
use cairo_lang_defs::db::{DefsGroup, init_defs_group};
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::{
    CrateConfiguration, FilesGroup, GranularFileContentStorage, GranularFileContentView,
    init_dev_corelib, init_files_group, new_granular_file_content_storage,
    register_files_group_view, set_editor_file_content_for_input,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::ids::{CrateId, Directory, FileLongId, SmolStrId};
use cairo_lang_filesystem::set_crate_config;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::{PluginSuiteInput, init_semantic_group};
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_utils::Intern;
use salsa::Database;

#[salsa::db]
#[derive(Clone)]
pub struct TestDatabase {
    storage: salsa::Storage<TestDatabase>,
    granular_file_contents: GranularFileContentStorage,
}
#[salsa::db]
impl salsa::Database for TestDatabase {}
impl GranularFileContentView for TestDatabase {
    fn granular_file_content_storage(&self) -> Option<&GranularFileContentStorage> {
        Some(&self.granular_file_contents)
    }
}

impl Default for TestDatabase {
    fn default() -> Self {
        let mut res = Self {
            storage: Default::default(),
            granular_file_contents: new_granular_file_content_storage(),
        };
        register_files_group_view(&res);
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

pub fn setup_test_module(db: &mut dyn Database, content: &str) {
    let crate_id = test_crate_id(db);
    let directory = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(directory)));
    let file_input = {
        let crate_id = test_crate_id(db);
        let file = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
        db.file_input(file).clone()
    };
    set_editor_file_content_for_input(db, file_input, Some(content.into()));
    let crate_id = test_crate_id(db);
    let file = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
    let syntax_diagnostics = db.file_syntax_diagnostics(file).format(db);
    assert_eq!(syntax_diagnostics, "");
}

pub fn test_crate_id<'db>(db: &'db dyn Database) -> CrateId<'db> {
    CrateId::plain(db, SmolStrId::from(db, "test"))
}

pub fn setup_test_module_without_syntax_diagnostics(db: &mut dyn Database, content: &str) {
    let crate_id = test_crate_id(db);
    let directory = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(directory)));
    let file_input = {
        let crate_id = test_crate_id(db);
        let file = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
        db.file_input(file).clone()
    };
    set_editor_file_content_for_input(db, file_input, Some(content.into()));
}

pub fn set_file_content(db: &mut TestDatabase, path: &str, content: &str) {
    let file_input = {
        let file_id = FileLongId::OnDisk(path.into()).intern(db);
        db.file_input(file_id).clone()
    };
    set_editor_file_content_for_input(db, file_input, Some(content.into()));
}
