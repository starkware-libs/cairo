use std::sync::Arc;

use anyhow::{anyhow, Result};
use cairo_lang_defs::db::{DefsDatabase, DefsGroup, HasMacroPlugins};
use cairo_lang_defs::plugin::MacroPlugin;
use cairo_lang_filesystem::cfg::CfgSet;
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

    /// Snapshots the db for read only.
    pub fn snapshot(&self) -> RootDatabase {
        RootDatabase { storage: self.storage.snapshot() }
    }
}

impl Default for RootDatabase {
    fn default() -> Self {
        // TODO(spapini): Consider taking from config.
        Self::new(get_default_plugins())
    }
}

#[derive(Clone, Debug, Default)]
pub struct RootDatabaseBuilder {
    plugins: Option<Vec<Arc<dyn SemanticPlugin>>>,
    detect_corelib: bool,
    project_config: Option<Box<ProjectConfig>>,
    implicit_precedence: Option<Vec<String>>,
    cfg_set: Option<CfgSet>,
}

impl RootDatabaseBuilder {
    pub fn empty() -> Self {
        Default::default()
    }

    pub fn with_plugins(&mut self, plugins: Vec<Arc<dyn SemanticPlugin>>) -> &mut Self {
        self.plugins = Some(plugins);
        self
    }

    pub fn detect_corelib(&mut self) -> &mut Self {
        self.detect_corelib = true;
        self
    }

    pub fn with_project_config(&mut self, config: ProjectConfig) -> &mut Self {
        self.project_config = Some(Box::new(config));
        self
    }

    pub fn with_implicit_precedence(&mut self, precedence: &[impl ToString]) -> &mut Self {
        self.implicit_precedence = Some(precedence.iter().map(ToString::to_string).collect());
        self
    }

    pub fn with_cfg(&mut self, cfg_set: impl Into<CfgSet>) -> &mut Self {
        self.cfg_set = Some(cfg_set.into());
        self
    }

    pub fn build(&mut self) -> Result<RootDatabase> {
        // NOTE: Order of operations matters here!
        //   Errors if something is not OK are very subtle, mostly this results in missing
        //   identifier diagnostics, or panics regarding lack of corelib items.

        let mut db = RootDatabase::default();

        if let Some(cfg_set) = &self.cfg_set {
            db.use_cfg(cfg_set);
        }

        if self.detect_corelib {
            let path =
                detect_corelib().ok_or_else(|| anyhow!("Failed to find development corelib."))?;
            init_dev_corelib(&mut db, path);
        }

        if let Some(config) = self.project_config.clone() {
            update_crate_roots_from_project_config(&mut db, *config.clone());

            if let Some(corelib) = config.corelib {
                let core_crate = db.intern_crate(CrateLongId(CORELIB_CRATE_NAME.into()));
                db.set_crate_root(core_crate, Some(corelib));
            }
        }

        if let Some(precedence) = self.implicit_precedence.clone() {
            db.set_implicit_precedence(Arc::new(
                precedence
                    .into_iter()
                    .map(|name| get_core_ty_by_name(&db, name.into(), vec![]))
                    .collect::<Vec<_>>(),
            ));
        }

        if let Some(plugins) = self.plugins.clone() {
            db.set_semantic_plugins(plugins);
        }

        Ok(db)
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
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn LoweringGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn LoweringGroup + 'static) {
        self
    }
}
impl HasMacroPlugins for RootDatabase {
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>> {
        self.get_macro_plugins()
    }
}
