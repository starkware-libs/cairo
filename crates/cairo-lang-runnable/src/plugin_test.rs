use std::sync::{LazyLock, Mutex};

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_plugins::test_utils::expand_module_text;
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};
use cairo_lang_test_utils::{get_direct_or_file_content, verify_diagnostics_expectation};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

/// Salsa database configured to find the corelib, when reused by different tests should be able to
/// use the cached queries that rely on the corelib's code, which vastly reduces the tests runtime.
pub static SHARED_DB: LazyLock<Mutex<RootDatabase>> = LazyLock::new(|| {
    Mutex::new(
        RootDatabase::builder()
            .detect_corelib()
            .with_plugin_suite(std::mem::take(
                PluginSuite::default().add_plugin::<super::RunnablePlugin>(),
            ))
            .build()
            .unwrap(),
    )
});

#[derive(Default)]
struct ExpandRunnableTestRunner {}

// TODO(Gil): Make this test expand inline macros as well.
impl TestFileRunner for ExpandRunnableTestRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let db = SHARED_DB.lock().unwrap().snapshot();
        let (_, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
        let (test_module, semantic_diagnostics) = setup_test_module(&db, &cairo_code).split();
        let result = expand_module_text(&db, test_module.module_id, &mut vec![]);
        let error = verify_diagnostics_expectation(args, &semantic_diagnostics);
        TestRunnerResult {
            outputs: OrderedHashMap::from([
                ("generated_cairo_code".into(), result),
                ("expected_diagnostics".into(), semantic_diagnostics),
            ]),
            error,
        }
    }
}

cairo_lang_test_utils::test_file_test_with_runner!(
    expand_runnable,
    "src/test_data",
    {
        diagnostics: "diagnostics",
        expansion: "expansion",
    },
    ExpandRunnableTestRunner
);
