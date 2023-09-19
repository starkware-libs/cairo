use cairo_lang_compiler::diagnostics::get_diagnostics_as_string;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::test_utils::SHARED_DB;

#[derive(Default)]
struct ExpandContractTestRunner {}

impl TestFileRunner for ExpandContractTestRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let db = SHARED_DB.lock().unwrap().snapshot();
        let (test_module, _semantic_diagnostics) =
            setup_test_module(&db, inputs["cairo_code"].as_str()).split();

        let mut module_ids = vec![test_module.module_id];
        module_ids.extend(
            db.module_submodules_ids(test_module.module_id)
                .unwrap_or_default()
                .iter()
                .copied()
                .map(ModuleId::Submodule),
        );
        let mut files = vec![];
        for module_id in module_ids {
            for file in db.module_files(module_id).unwrap_or_default().iter().copied() {
                if files.contains(&file) {
                    continue;
                }
                files.push(file);
            }
        }
        let mut file_contents = vec![];

        for file in files {
            file_contents.push(format!("{}:", file.file_name(&db)));
            file_contents.push(db.file_content(file).unwrap().as_ref().clone());
        }

        let diagnostics = get_diagnostics_as_string(&db, &[test_module.crate_id]);
        let error = verify_diagnostics_expectation(args, &diagnostics);

        TestRunnerResult {
            outputs: OrderedHashMap::from([
                ("generated_cairo_code".into(), file_contents.join("\n\n")),
                ("expected_diagnostics".into(), diagnostics),
            ]),
            error,
        }
    }
}

cairo_lang_test_utils::test_file_test_with_runner!(
    expand_contract,
    "src/plugin/plugin_test_data/contracts",
    {
        diagnostics: "diagnostics",
        contract: "contract",
        events: "events",
        embedded_impl: "embedded_impl",
        raw_output: "raw_output",
        storage: "storage",
        hello_starknet: "hello_starknet",
        dispatcher: "dispatcher",
        user_defined_types: "user_defined_types",
        l1_handler: "l1_handler",
        no_body: "no_body",
        external_event: "external_event",
        with_component: "with_component",
        with_component_diagnostics: "with_component_diagnostics",
        with_ownable: "with_ownable",
        upgradable_counter: "upgradable_counter",
        interfaces: "interfaces",
    },
    ExpandContractTestRunner
);

cairo_lang_test_utils::test_file_test_with_runner!(
    expand_component,
    "src/plugin/plugin_test_data/components",
    {
        component: "component",
        diagnostics: "diagnostics",
        no_body: "no_body",
        no_storage: "no_storage",
    },
    ExpandContractTestRunner
);
