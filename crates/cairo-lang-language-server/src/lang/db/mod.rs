use cairo_lang_defs::db::{DefsDatabase, DefsGroup};
use cairo_lang_doc::db::DocDatabase;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::db::{
    init_files_group, AsFilesGroupMut, ExternalFiles, FilesDatabase, FilesGroup,
};
use cairo_lang_lowering::db::{init_lowering_group, LoweringDatabase, LoweringGroup};
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
pub use self::syntax::*;
use crate::Tricks;

mod semantic;
mod syntax;

/// The Cairo compiler Salsa database tailored for language server usage.
#[salsa::database(
    DefsDatabase,
    FilesDatabase,
    LoweringDatabase,
    ParserDatabase,
    SemanticDatabase,
    SyntaxDatabase,
    DocDatabase
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

        db.set_cfg_set(Self::initial_cfg_set().into());

        let plugin_suite =
            [get_default_plugin_suite(), starknet_plugin_suite(), test_plugin_suite()]
                .into_iter()
                .chain(tricks.extra_plugin_suites.iter().flat_map(|f| f()))
                .fold(PluginSuite::default(), |mut acc, suite| {
                    acc.add(suite);
                    acc
                });
        db.apply_plugin_suite(plugin_suite);

        db
    }

    /// Returns the [`CfgSet`] that should be assumed in the initial database state.
    fn initial_cfg_set() -> CfgSet {
        CfgSet::from_iter([Cfg::name("test"), Cfg::kv("target", "test")])
    }

    /// Shortcut for settings compiler plugins from a [`PluginSuite`].
    fn apply_plugin_suite(&mut self, plugin_suite: PluginSuite) {
        self.set_macro_plugins(plugin_suite.plugins);
        self.set_inline_macro_plugins(plugin_suite.inline_macro_plugins.into());
        self.set_analyzer_plugins(plugin_suite.analyzer_plugins);
    }
}

impl salsa::Database for AnalysisDatabase {}
impl ExternalFiles for AnalysisDatabase {}

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
