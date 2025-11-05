use std::sync::{LazyLock, Mutex};

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::{init_defs_group, init_external_files};
use cairo_lang_filesystem::db::{init_dev_corelib, init_files_group};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_semantic::db::{PluginSuiteInput, init_semantic_group};
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
