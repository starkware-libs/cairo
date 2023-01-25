use std::sync::Arc;

use cairo_lang_defs::db::{DefsDatabase, DefsGroup, HasMacroPlugins};
use cairo_lang_defs::plugin::MacroPlugin;
use cairo_lang_filesystem::db::{
    init_dev_corelib, init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup, FilesGroupEx,
    CORELIB_CRATE_NAME,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::ids::CrateLongId;
use cairo_lang_lowering::db::{init_lowering_group, LoweringDatabase, LoweringGroup};
use cairo_lang_parser::db::ParserDatabase;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_project::ProjectConfig;
use cairo_lang_semantic::corelib::get_core_ty_by_name;
use cairo_lang_semantic::db::{SemanticDatabase, SemanticGroup, SemanticGroupEx};
use cairo_lang_semantic::plugin::SemanticPlugin;
use cairo_lang_sierra_generator::db::SierraGenDatabase;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_utils::Upcast;
use {cairo_lang_defs as defs, cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::project::update_crate_roots_from_project_config;

#[salsa::database(
    DefsDatabase,
    FilesDatabase,
    LoweringDatabase,
    ParserDatabase,
    SemanticDatabase,
    SierraGenDatabase,
    SyntaxDatabase
)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}
impl salsa::Database for RootDatabase {}
impl RootDatabase {
    pub fn new(plugins: Vec<Arc<dyn SemanticPlugin>>) -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_lowering_group(&mut res);
        res.set_semantic_plugins(plugins);
        res
    }

    pub fn empty() -> Self {
        Self::new(Vec::new())
    }

    pub fn builder() -> RootDatabaseBuilder {
        RootDatabaseBuilder::default()
    }
}

impl Default for RootDatabase {
    fn default() -> Self {
        // TODO(spapini): Consider taking from config.
        Self::new(get_default_plugins())
    }
}

#[derive(Default)]
pub struct RootDatabaseBuilder {
    db: RootDatabase,
}

impl RootDatabaseBuilder {
    pub fn empty() -> Self {
        Self { db: RootDatabase::empty() }
    }

    pub fn with_plugins(&mut self, plugins: Vec<Arc<dyn SemanticPlugin>>) -> &mut Self {
        self.db.set_semantic_plugins(plugins);
        self
    }

    pub fn with_dev_corelib(&mut self) -> Option<&mut Self> {
        if let Some(path) = detect_corelib() {
            init_dev_corelib(&mut self.db, path);
            Some(self)
        } else {
            None
        }
    }

    pub fn with_project_config(&mut self, config: ProjectConfig) -> &mut Self {
        update_crate_roots_from_project_config(&mut self.db, config.clone());

        if let Some(corelib) = config.corelib {
            let core_crate = self.db.intern_crate(CrateLongId(CORELIB_CRATE_NAME.into()));
            self.db.set_crate_root(core_crate, Some(corelib));
        }

        self
    }

    pub fn with_implicit_precedence(&mut self, precedence: Vec<&str>) -> &mut Self {
        self.db.set_implicit_precedence(Arc::new(
            precedence
                .iter()
                .map(|name| get_core_ty_by_name(&self.db, name.into(), vec![]))
                .collect::<Vec<_>>(),
        ));
        self
    }

    pub fn build(self) -> RootDatabase {
        self.db
    }
}

impl AsFilesGroupMut for RootDatabase {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl Upcast<dyn DefsGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn defs::db::DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn semantic::db::SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn LoweringGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn lowering::db::LoweringGroup + 'static) {
        self
    }
}
impl HasMacroPlugins for RootDatabase {
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>> {
        self.get_macro_plugins()
    }
}
