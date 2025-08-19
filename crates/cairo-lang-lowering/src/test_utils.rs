use std::sync::{LazyLock, Mutex};

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::{DefsGroup, init_defs_group, init_external_files};
use cairo_lang_filesystem::db::{FilesGroup, init_dev_corelib, init_files_group};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::{Elongate, PluginSuiteInput, SemanticGroup, init_semantic_group};
use cairo_lang_semantic::inline_macros::get_default_plugin_suite;
use cairo_lang_utils::Upcast;

use crate::Lowered;
use crate::db::{LoweringGroup, UseApproxCodeSizeEstimator, init_lowering_group};
use crate::fmt::LoweredFormatter;
use crate::utils::InliningStrategy;

impl UseApproxCodeSizeEstimator for LoweringDatabaseForTesting {}

#[salsa::db]
#[derive(Clone)]
pub struct LoweringDatabaseForTesting {
    storage: salsa::Storage<LoweringDatabaseForTesting>,
}
#[salsa::db]
impl salsa::Database for LoweringDatabaseForTesting {}

impl LoweringDatabaseForTesting {
    pub fn new() -> Self {
        let mut res = LoweringDatabaseForTesting { storage: Default::default() };
        init_external_files(&mut res);
        init_files_group(&mut res);
        init_defs_group(&mut res);
        init_semantic_group(&mut res);

        res.set_default_plugins_from_suite(get_default_plugin_suite());

        let corelib_path = detect_corelib().expect("Corelib not found in default location.");
        init_dev_corelib(&mut res, corelib_path);
        init_lowering_group(&mut res, InliningStrategy::Default);
        res
    }

    /// Snapshots the db.
    pub fn snapshot(&self) -> LoweringDatabaseForTesting {
        self.clone()
    }
}

pub static SHARED_DB: LazyLock<Mutex<LoweringDatabaseForTesting>> =
    LazyLock::new(|| Mutex::new(LoweringDatabaseForTesting::new()));
impl Default for LoweringDatabaseForTesting {
    fn default() -> Self {
        SHARED_DB.lock().unwrap().snapshot()
    }
}

impl Elongate for LoweringDatabaseForTesting {
    fn elongate(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}

impl<'db> Upcast<'db, dyn FilesGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &dyn FilesGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn DefsGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &dyn DefsGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn SemanticGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &dyn SemanticGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn LoweringGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &dyn LoweringGroup {
        self
    }
}
impl<'db> Upcast<'db, dyn ParserGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &dyn ParserGroup {
        self
    }
}

/// Helper for formatting a lowered representation for tests.
pub fn formatted_lowered(db: &dyn LoweringGroup, lowered: Option<&Lowered<'_>>) -> String {
    match lowered {
        Some(lowered) => {
            let lowered_formatter = LoweredFormatter::new(db, &lowered.variables);
            format!("{:?}", lowered.debug(&lowered_formatter))
        }
        None => "<Failed lowering function - run with RUST_LOG=warn (or less) to see diagnostics>"
            .to_string(),
    }
}
