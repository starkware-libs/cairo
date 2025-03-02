use std::sync::{LazyLock, Mutex};

use cairo_lang_casm::hints::Hint;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_plugins::test_utils::expand_module_text;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};
use cairo_lang_test_utils::{get_direct_or_file_content, verify_diagnostics_expectation};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_vm::types::program;
use itertools::Itertools;
use serde_json::json;

use crate::compile;
use crate::plugin::executable_plugin_suite;

/// Salsa database configured to find the corelib, when reused by different tests should be able to
/// use the cached queries that rely on the corelib's code, which vastly reduces the tests runtime.
pub static SHARED_DB: LazyLock<Mutex<RootDatabase>> = LazyLock::new(|| {
    Mutex::new(
        RootDatabase::builder()
            .skip_auto_withdraw_gas()
            .with_cfg(CfgSet::from_iter([Cfg::kv("gas", "disabled"), Cfg::kv("m31", "enabled")]))
            .detect_corelib()
            .with_default_plugin_suite(executable_plugin_suite())
            .build()
            .unwrap(),
    )
});

#[derive(Default)]
struct ExpandExecutableTestRunner {}

impl TestFileRunner for ExpandExecutableTestRunner {
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

fn format_hints(hint: &Hint) -> String {
    let data = json!({
        "accessible_scopes": [],
        "code": hint,
        "flow_tracking_data": {
            "ap_tracking": {
                "group": 0,
                "offset": 1
            },
            "reference_ids": {}
        }
    });

    format!("{}", data)
}

impl TestFileRunner for CompileExecutableTestRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let db = SHARED_DB.lock().unwrap().snapshot();
        let (_, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
        let (test_module, semantic_diagnostics) = setup_test_module(&db, &cairo_code).split();
        let result = match compile::compile_executable_in_prepared_db(
            &db,
            None,
            vec![test_module.crate_id],
            DiagnosticsReporter::stderr().with_crates(&[test_module.crate_id]),
            Default::default(),
        ) {
            Err(e) => e.to_string(),
            Ok(r) => {
                let mut s = format!("{}\n\n", r.program);
                let (bytecode, hints) =
                    r.program.assemble_m31(&r.wrapper.header, &r.wrapper.footer);
               
                for x in &bytecode.iter().chunks(4) {
                    s += "[";
                    let mut first = true;
                    for y in x {
                        if !first {
                            s += ", ";
                        }
                        first = false;
                        s.push_str(&format!("\"{:#x}\"", y));
                    }
                    s += "],\n";
                }

                for (pc, hints_at_pc) in &hints {
                    s.push_str(&format!("\"{:x}\": [\n", pc >> 2));

                    for h in hints_at_pc {
                        s.push_str(&format_hints(h));
                    }

                    s.push_str("\n],");
                }

                s
            }
        };
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
