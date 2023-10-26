use std::path::PathBuf;

use cairo_lang_compiler::diagnostics::get_diagnostics_as_string;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileLongId;
use cairo_lang_plugins::test_utils::expand_module_text;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};
use cairo_lang_test_utils::{get_direct_or_file_content, verify_diagnostics_expectation};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::test_utils::{SHARED_DB, SHARED_DB_WITH_CONTRACTS};

#[derive(Default)]
struct ExpandContractTestRunner {}

// TODO(Gil): Make this test expand inline macros as well.
impl TestFileRunner for ExpandContractTestRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let db = SHARED_DB.lock().unwrap().snapshot();
        let (_, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
        let (test_module, _semantic_diagnostics) = setup_test_module(&db, &cairo_code).split();

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
        dispatcher: "dispatcher",
        user_defined_types: "user_defined_types",
        l1_handler: "l1_handler",
        no_body: "no_body",
        external_event: "external_event",
        with_component: "with_component",
        with_component_diagnostics: "with_component_diagnostics",
        interfaces: "interfaces",
    },
    ExpandContractTestRunner
);

cairo_lang_test_utils::test_file_test_with_runner!(
    expand_component,
    "src/plugin/plugin_test_data/components",
    {
        component: "component",
        embeddable_as: "embeddable_as",
        diagnostics: "diagnostics",
        no_body: "no_body",
        no_storage: "no_storage",
    },
    ExpandContractTestRunner
);

#[derive(Default)]
struct ExpandContractFromCrateTestRunner {}

impl TestFileRunner for ExpandContractFromCrateTestRunner {
    /// Inits the database with the contracts crate, and expands a specific contract file.
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        _args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let db = SHARED_DB_WITH_CONTRACTS.lock().unwrap().snapshot();
        let contract_file_id =
            db.intern_file(FileLongId::OnDisk(PathBuf::from(inputs["contract_file_name"].clone())));
        let contract_module_ids = db.file_modules(contract_file_id).unwrap();
        let mut diagnostic_items = vec![];
        let result = contract_module_ids
            .iter()
            .map(|module_id| expand_module_text(&db, *module_id, &mut diagnostic_items))
            .collect::<Vec<_>>()
            .join("\n");
        let joined_diagnostics = diagnostic_items.join("\n");
        let error = verify_diagnostics_expectation(_args, &joined_diagnostics);
        TestRunnerResult {
            outputs: OrderedHashMap::from([
                ("generated_cairo_code".into(), result),
                ("expected_diagnostics".into(), joined_diagnostics),
            ]),
            error,
        }
    }
}

// TODO(Gil): Move all contracts to the test crate and use this runner.
cairo_lang_test_utils::test_file_test_with_runner!(
    expand_contract_from_crate,
    "src/plugin/plugin_test_data/contracts",
    {
        hello_starknet: "hello_starknet",
        with_ownable: "with_ownable",
        with_erc20: "with_erc20",
        upgradable_counter: "upgradable_counter",
        ownable_erc20: "ownable_erc20",
        mintable: "mintable",
        multi_component: "multi_component",
    },
    ExpandContractFromCrateTestRunner
);
