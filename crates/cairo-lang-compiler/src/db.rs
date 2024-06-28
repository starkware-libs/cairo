use std::sync::Arc;

use anyhow::{anyhow, Result};
use cairo_lang_defs::db::{DefsDatabase, DefsGroup};
use cairo_lang_defs::plugin::{InlineMacroExprPlugin, MacroPlugin};
use cairo_lang_filesystem::cfg::CfgSet;
use cairo_lang_filesystem::db::{
    init_dev_corelib, init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup, FilesGroupEx,
    CORELIB_CRATE_NAME,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagId;
use cairo_lang_lowering::db::{init_lowering_group, LoweringDatabase, LoweringGroup};
use cairo_lang_parser::db::{ParserDatabase, ParserGroup};
use cairo_lang_project::ProjectConfig;
use cairo_lang_semantic::db::{SemanticDatabase, SemanticGroup};
use cairo_lang_semantic::inline_macros::get_default_plugin_suite;
use cairo_lang_semantic::plugin::{AnalyzerPlugin, PluginSuite};
use cairo_lang_sierra_generator::db::SierraGenDatabase;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::Upcast;

use crate::project::{update_crate_root, update_crate_roots_from_project_config};

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
impl salsa::ParallelDatabase for RootDatabase {
    fn snapshot(&self) -> salsa::Snapshot<RootDatabase> {
        salsa::Snapshot::new(RootDatabase { storage: self.storage.snapshot() })
    }
}
impl RootDatabase {
    fn new(
        plugins: Vec<Arc<dyn MacroPlugin>>,
        inline_macro_plugins: OrderedHashMap<String, Arc<dyn InlineMacroExprPlugin>>,
        analyzer_plugins: Vec<Arc<dyn AnalyzerPlugin>>,
        disable_inlining: bool,
    ) -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_lowering_group(&mut res, disable_inlining);
        res.set_macro_plugins(plugins);
        res.set_inline_macro_plugins(inline_macro_plugins.into());
        res.set_analyzer_plugins(analyzer_plugins);
        res
    }

    pub fn empty() -> Self {
        Self::builder().clear_plugins().build().unwrap()
    }

    pub fn builder() -> RootDatabaseBuilder {
        RootDatabaseBuilder::new()
    }

    /// Snapshots the db for read only.
    pub fn snapshot(&self) -> RootDatabase {
        RootDatabase { storage: self.storage.snapshot() }
    }
}

impl Default for RootDatabase {
    fn default() -> Self {
        Self::builder().build().unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct RootDatabaseBuilder {
    plugin_suite: PluginSuite,
    detect_corelib: bool,
    auto_withdraw_gas: bool,
    project_config: Option<Box<ProjectConfig>>,
    cfg_set: Option<CfgSet>,
    disable_inlining: bool,
}

impl RootDatabaseBuilder {
    fn new() -> Self {
        Self {
            plugin_suite: get_default_plugin_suite(),
            detect_corelib: false,
            auto_withdraw_gas: true,
            project_config: None,
            cfg_set: None,
            disable_inlining: false,
        }
    }

    pub fn with_plugin_suite(&mut self, suite: PluginSuite) -> &mut Self {
        self.plugin_suite.add(suite);
        self
    }

    pub fn clear_plugins(&mut self) -> &mut Self {
        self.plugin_suite = get_default_plugin_suite();
        self
    }

    pub fn disable_inlining(&mut self) -> &mut Self {
        self.disable_inlining = true;
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

    pub fn with_cfg(&mut self, cfg_set: impl Into<CfgSet>) -> &mut Self {
        self.cfg_set = Some(cfg_set.into());
        self
    }

    pub fn skip_auto_withdraw_gas(&mut self) -> &mut Self {
        self.auto_withdraw_gas = false;
        self
    }

    pub fn build(&mut self) -> Result<RootDatabase> {
        // NOTE: Order of operations matters here!
        //   Errors if something is not OK are very subtle, mostly this results in missing
        //   identifier diagnostics, or panics regarding lack of corelib items.

        let mut db = RootDatabase::new(
            self.plugin_suite.plugins.clone(),
            self.plugin_suite.inline_macro_plugins.clone(),
            self.plugin_suite.analyzer_plugins.clone(),
            self.disable_inlining,
        );

        if let Some(cfg_set) = &self.cfg_set {
            db.use_cfg(cfg_set);
        }

        if self.detect_corelib {
            let path =
                detect_corelib().ok_or_else(|| anyhow!("Failed to find development corelib."))?;
            init_dev_corelib(&mut db, path)
        }

        let add_withdraw_gas_flag_id = FlagId::new(db.upcast(), "add_withdraw_gas");
        db.set_flag(
            add_withdraw_gas_flag_id,
            Some(Arc::new(Flag::AddWithdrawGas(self.auto_withdraw_gas))),
        );

        if let Some(config) = &self.project_config {
            update_crate_roots_from_project_config(&mut db, config.as_ref());
            if let Some(corelib) = &config.corelib {
                update_crate_root(&mut db, config, CORELIB_CRATE_NAME.into(), corelib.clone());
            }
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
impl Upcast<dyn ParserGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn ParserGroup + 'static) {
        self
    }
}
