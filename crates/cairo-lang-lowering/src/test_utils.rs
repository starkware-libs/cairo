use std::sync::{LazyLock, Mutex};

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::{
    InlineMacroPluginOverrideStorage, InlineMacroPluginOverrideView, MacroPluginOverrideStorage,
    MacroPluginOverrideView, init_defs_group, init_external_files,
    new_inline_macro_plugin_override_storage, new_macro_plugin_override_storage,
    register_inline_macro_plugin_override_view, register_macro_plugin_override_view,
};
use cairo_lang_filesystem::db::{
    CrateConfigStorage, CrateConfigView, FileContentView, init_dev_corelib, init_files_group,
    new_crate_config_storage, register_crate_config_view, register_files_group_view,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::flag::{Flag, FlagsGroup};
use cairo_lang_filesystem::ids::FlagLongId;
use cairo_lang_semantic::db::{
    AnalyzerPluginOverrideStorage, AnalyzerPluginOverrideView, PluginSuiteInput,
    init_semantic_group, new_analyzer_plugin_override_storage,
    register_analyzer_plugin_override_view,
};
use cairo_lang_semantic::inline_macros::get_default_plugin_suite;
use salsa::Database;

use crate::Lowered;
use crate::db::init_lowering_group;
use crate::fmt::LoweredFormatter;
use crate::optimizations::config::Optimizations;
use crate::utils::InliningStrategy;

#[salsa::db]
#[derive(Clone)]
pub struct LoweringDatabaseForTesting {
    storage: salsa::Storage<LoweringDatabaseForTesting>,
    crate_configs: CrateConfigStorage,
    macro_plugin_overrides: MacroPluginOverrideStorage,
    inline_macro_plugin_overrides: InlineMacroPluginOverrideStorage,
    analyzer_plugin_overrides: AnalyzerPluginOverrideStorage,
}
#[salsa::db]
impl salsa::Database for LoweringDatabaseForTesting {}
impl FileContentView for LoweringDatabaseForTesting {}
impl CrateConfigView for LoweringDatabaseForTesting {
    fn crate_config_storage(&self) -> Option<&CrateConfigStorage> {
        Some(&self.crate_configs)
    }
}
impl MacroPluginOverrideView for LoweringDatabaseForTesting {
    fn macro_plugin_override_storage(&self) -> Option<&MacroPluginOverrideStorage> {
        Some(&self.macro_plugin_overrides)
    }
}
impl InlineMacroPluginOverrideView for LoweringDatabaseForTesting {
    fn inline_macro_plugin_override_storage(&self) -> Option<&InlineMacroPluginOverrideStorage> {
        Some(&self.inline_macro_plugin_overrides)
    }
}
impl AnalyzerPluginOverrideView for LoweringDatabaseForTesting {
    fn analyzer_plugin_override_storage(&self) -> Option<&AnalyzerPluginOverrideStorage> {
        Some(&self.analyzer_plugin_overrides)
    }
}

impl LoweringDatabaseForTesting {
    pub fn new() -> Self {
        let mut res = LoweringDatabaseForTesting {
            storage: Default::default(),
            crate_configs: new_crate_config_storage(),
            macro_plugin_overrides: new_macro_plugin_override_storage(),
            inline_macro_plugin_overrides: new_inline_macro_plugin_override_storage(),
            analyzer_plugin_overrides: new_analyzer_plugin_override_storage(),
        };
        register_files_group_view(&res);
        register_crate_config_view(&res);
        register_macro_plugin_override_view(&res);
        register_inline_macro_plugin_override_view(&res);
        register_analyzer_plugin_override_view(&res);
        init_external_files(&mut res);
        init_files_group(&mut res);
        init_defs_group(&mut res);
        init_semantic_group(&mut res);

        res.set_default_plugins_from_suite(get_default_plugin_suite());

        let corelib_path = detect_corelib().expect("Corelib not found in default location.");
        init_dev_corelib(&mut res, corelib_path);
        init_lowering_group(
            &mut res,
            Optimizations::enabled_with_default_movable_functions(InliningStrategy::Default),
            None,
        );
        res
    }

    /// Snapshots the db.
    pub fn snapshot(&self) -> LoweringDatabaseForTesting {
        self.clone()
    }
}

pub static SHARED_DB: LazyLock<Mutex<LoweringDatabaseForTesting>> =
    LazyLock::new(|| Mutex::new(LoweringDatabaseForTesting::new()));
pub static SHARED_DB_FUTURE_SIERRA: LazyLock<Mutex<LoweringDatabaseForTesting>> =
    LazyLock::new(|| {
        let mut db = LoweringDatabaseForTesting::new();
        db.set_flag(FlagLongId(Flag::FUTURE_SIERRA.into()), Some(Flag::FutureSierra(true)));
        Mutex::new(db)
    });
impl LoweringDatabaseForTesting {
    pub fn with_future_sierra() -> Self {
        SHARED_DB_FUTURE_SIERRA.lock().unwrap().snapshot()
    }
}
impl Default for LoweringDatabaseForTesting {
    fn default() -> Self {
        SHARED_DB.lock().unwrap().snapshot()
    }
}

/// Helper for formatting a lowered representation for tests.
pub fn formatted_lowered(db: &dyn Database, lowered: Option<&Lowered<'_>>) -> String {
    match lowered {
        Some(lowered) => {
            let lowered_formatter = LoweredFormatter::new(db, &lowered.variables);
            format!("{:?}", lowered.debug(&lowered_formatter))
        }
        None => "<Failed lowering function - run with RUST_LOG=warn (or less) to see diagnostics>"
            .to_string(),
    }
}
