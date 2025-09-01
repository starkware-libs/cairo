use std::sync::{LazyLock, Mutex};

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_executable_plugin::executable_plugin_suite;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};
use cairo_lang_test_utils::{get_direct_or_file_content, verify_diagnostics_expectation};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::compile;

/// Salsa database configured to find the corelib, when reused by different tests should be able to
/// use the cached queries that rely on the corelib's code, which vastly reduces the tests runtime.
pub static SHARED_DB: LazyLock<Mutex<RootDatabase>> = LazyLock::new(|| {
    Mutex::new(
        RootDatabase::builder()
            .skip_auto_withdraw_gas()
            .with_cfg(CfgSet::from_iter([Cfg::kv("gas", "disabled")]))
            .detect_corelib()
            .with_default_plugin_suite(executable_plugin_suite())
            .build()
            .unwrap(),
    )
});

#[derive(Default)]
struct CompileExecutableTestRunner {}

impl TestFileRunner for CompileExecutableTestRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let db = SHARED_DB.lock().unwrap().snapshot();
        let (_, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
        let (test_module, semantic_diagnostics) = setup_test_module(&db, &cairo_code).split();
        let result = compile::compile_executable_in_prepared_db(
            &db,
            None,
            vec![test_module.crate_id],
            DiagnosticsReporter::stderr().with_crates(&[test_module
                .crate_id
                .long(&db)
                .clone()
                .into_crate_input(&db)]),
            Default::default(),
        )
        .map(|compiled| compiled.compiled_function.to_string())
        .unwrap_or_else(|e| e.to_string());
        let error = verify_diagnostics_expectation(args, &semantic_diagnostics);
        TestRunnerResult {
            outputs: OrderedHashMap::from([
                ("result".into(), result),
                ("expected_diagnostics".into(), semantic_diagnostics),
            ]),
            error,
        }
    }
}

cairo_lang_test_utils::test_file_test_with_runner!(
    compile_executable,
    "src/compile_test_data",
    {
        basic: "basic",
    },
    CompileExecutableTestRunner
);
