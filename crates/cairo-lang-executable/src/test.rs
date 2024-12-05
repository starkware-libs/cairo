use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_plugins::test_utils::expand_module_text;
use cairo_lang_semantic::db::PluginSuiteInput;
use cairo_lang_semantic::inline_macros::get_default_plugin_suite;
use cairo_lang_semantic::test_utils::TestModule;
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};
use cairo_lang_test_utils::{get_direct_or_file_content, verify_diagnostics_expectation};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::compile;
use crate::plugin::executable_plugin_suite;

#[derive(Default)]
struct ExpandExecutableTestRunner {}

impl TestFileRunner for ExpandExecutableTestRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let mut db =
            RootDatabase::builder().skip_auto_withdraw_gas().detect_corelib().build().unwrap();
        db.set_plugins_from_suite(get_default_plugin_suite() + executable_plugin_suite());

        let (_, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
        let (test_module, semantic_diagnostics) = TestModule::builder(&db, &cairo_code, None)
            .build_and_check_for_diagnostics(&db)
            .split();
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
    expand_executable,
    "src/plugin_test_data",
    {
        diagnostics: "diagnostics",
        expansion: "expansion",
    },
    ExpandExecutableTestRunner
);

#[derive(Default)]
struct CompileExecutableTestRunner {}

impl TestFileRunner for CompileExecutableTestRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let mut db =
            RootDatabase::builder().skip_auto_withdraw_gas().detect_corelib().build().unwrap();
        db.set_plugins_from_suite(get_default_plugin_suite() + executable_plugin_suite());

        let (_, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
        let (test_module, semantic_diagnostics) = TestModule::builder(&db, &cairo_code, None)
            .build_and_check_for_diagnostics(&db)
            .split();
        let result = compile::compile_executable_in_prepared_db(
            &db,
            None,
            vec![test_module.crate_id],
            DiagnosticsReporter::stderr(),
        )
        .unwrap();
        let error = verify_diagnostics_expectation(args, &semantic_diagnostics);
        TestRunnerResult {
            outputs: OrderedHashMap::from([
                ("generated_casm_code".into(), result.to_string()),
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
