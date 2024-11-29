use std::sync::Arc;

use cairo_lang_defs::db::{DefsDatabase, DefsGroup, try_ext_as_virtual_impl};
use cairo_lang_doc::db::DocDatabase;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::db::{
    AsFilesGroupMut, ExternalFiles, FilesDatabase, FilesGroup, init_files_group,
};
use cairo_lang_filesystem::ids::VirtualFile;
use cairo_lang_lowering::db::{LoweringDatabase, LoweringGroup, init_lowering_group};
use cairo_lang_lowering::utils::InliningStrategy;
use cairo_lang_parser::db::{ParserDatabase, ParserGroup};
use cairo_lang_semantic::db::{SemanticDatabase, SemanticGroup};
use cairo_lang_semantic::inline_macros::get_default_plugin_suite;
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_starknet::starknet_plugin_suite;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_test_plugin::test_plugin_suite;
use cairo_lang_utils::Upcast;

pub use self::semantic::*;
pub use self::swapper::*;
pub use self::syntax::*;
use super::proc_macros::db::{ProcMacroDatabase, init_proc_macro_group};
use crate::Tricks;

mod semantic;
mod swapper;
mod syntax;

/// The Cairo compiler Salsa database tailored for language server usage.
#[salsa::database(
    DefsDatabase,
    FilesDatabase,
    LoweringDatabase,
    ParserDatabase,
    SemanticDatabase,
    SyntaxDatabase,
    DocDatabase,
    ProcMacroDatabase
)]
pub struct AnalysisDatabase {
    storage: salsa::Storage<Self>,
}

impl AnalysisDatabase {
    /// Creates a new instance of the database.
    pub fn new(tricks: &Tricks) -> Self {
        let mut db = Self { storage: Default::default() };

        init_files_group(&mut db);
        init_lowering_group(&mut db, InliningStrategy::Default);
        // proc-macro-server can be restarted many times but we want to keep these data across
        // multiple server starts, so init it once per database, not per server.
        init_proc_macro_group(&mut db);

        db.set_cfg_set(Self::initial_cfg_set().into());

        let plugin_suite = [
            get_default_plugin_suite(),
            starknet_plugin_suite(),
            // TODO(#6551): Test plugin should be only enabled for crates that indeed need it.
            //   Same for other plugins.
            //   Right now this code will conflict with procmacros.
            test_plugin_suite(),
        ]
        .into_iter()
        .chain(tricks.extra_plugin_suites.iter().flat_map(|f| f()))
        .fold(PluginSuite::default(), |mut acc, suite| {
            acc.add(suite);
            acc
        });
        db.apply_plugin_suite(plugin_suite);

        db
    }

    /// Returns the [`CfgSet`] that should be assumed in the initial database state
    /// and in [`CfgSet`] for workspace members.
    /// This enables code fragments tagged with `#[cfg(test)]` and `#[cfg(target: 'test')]` to be
    /// included in analysis by Language Server.
    pub(crate) fn initial_cfg_set() -> CfgSet {
        CfgSet::from_iter([Cfg::name("test"), Cfg::kv("target", "test")])
    }

    /// Returns the [`CfgSet`] that should be assumed for dependencies.
    /// This enables code fragments tagged with `#[cfg(target: 'test')]` to be
    /// included in analysis by Language Server.
    pub(crate) fn initial_cfg_set_for_deps() -> CfgSet {
        CfgSet::from_iter([Cfg::kv("target", "test")])
    }

    /// Shortcut for settings compiler plugins from a [`PluginSuite`].
    fn apply_plugin_suite(&mut self, plugin_suite: PluginSuite) {
        self.set_macro_plugins(plugin_suite.plugins);
        self.set_inline_macro_plugins(plugin_suite.inline_macro_plugins.into());
        self.set_analyzer_plugins(plugin_suite.analyzer_plugins);
    }

    /// Updates the plugin list in the database.
    ///
    /// This function modifies the database by removing plugins specified in `plugins_to_remove`,
    /// adding plugins from `plugins_to_add`, and retaining all other existing plugins. Plugins are
    /// considered identical based on their [`Arc`] reference.
    pub fn replace_plugin_suite(
        &mut self,
        plugins_to_remove: Option<PluginSuite>,
        plugins_to_add: PluginSuite,
    ) {
        let mut macro_plugins = self.macro_plugins();
        let mut analyzer_plugins = self.analyzer_plugins();
        let mut inline_macro_plugins = Arc::unwrap_or_clone(self.inline_macro_plugins());

        if let Some(plugins_to_remove) = plugins_to_remove {
            macro_plugins.retain(|plugin| {
                plugins_to_remove
                    .plugins
                    .iter()
                    .all(|previous_plugin| !Arc::ptr_eq(previous_plugin, plugin))
            });
            analyzer_plugins.retain(|plugin| {
                plugins_to_remove
                    .analyzer_plugins
                    .iter()
                    .all(|previous_plugin| !Arc::ptr_eq(previous_plugin, plugin))
            });
            inline_macro_plugins.retain(|_, plugin| {
                plugins_to_remove
                    .inline_macro_plugins
                    .iter()
                    .all(|(_, previous_plugin)| !Arc::ptr_eq(previous_plugin, plugin))
            });
        }

        macro_plugins.extend(plugins_to_add.plugins);
        analyzer_plugins.extend(plugins_to_add.analyzer_plugins);
        inline_macro_plugins.extend(plugins_to_add.inline_macro_plugins);

        self.set_macro_plugins(macro_plugins);
        self.set_analyzer_plugins(analyzer_plugins);
        self.set_inline_macro_plugins(Arc::new(inline_macro_plugins));
    }
}

impl salsa::Database for AnalysisDatabase {}
impl ExternalFiles for AnalysisDatabase {
    fn try_ext_as_virtual(&self, external_id: salsa::InternId) -> Option<VirtualFile> {
        try_ext_as_virtual_impl(self.upcast(), external_id)
    }
}

impl salsa::ParallelDatabase for AnalysisDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(AnalysisDatabase { storage: self.storage.snapshot() })
    }
}

impl AsFilesGroupMut for AnalysisDatabase {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}

impl Upcast<dyn FilesGroup> for AnalysisDatabase {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}

impl Upcast<dyn SyntaxGroup> for AnalysisDatabase {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}

impl Upcast<dyn DefsGroup> for AnalysisDatabase {
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}

impl Upcast<dyn SemanticGroup> for AnalysisDatabase {
    fn upcast(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}

impl Upcast<dyn LoweringGroup> for AnalysisDatabase {
    fn upcast(&self) -> &(dyn LoweringGroup + 'static) {
        self
    }
}

impl Upcast<dyn ParserGroup> for AnalysisDatabase {
    fn upcast(&self) -> &(dyn ParserGroup + 'static) {
        self
    }
}
