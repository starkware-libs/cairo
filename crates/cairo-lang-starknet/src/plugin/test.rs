use std::path::PathBuf;

use cairo_lang_compiler::diagnostics::get_diagnostics_as_string;
use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_diagnostics::DiagnosticLocation;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileLongId;
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_plugins::test_utils::expand_module_text;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};
use cairo_lang_test_utils::{get_direct_or_file_content, verify_diagnostics_expectation};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, Upcast};

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
        if let Ok(submodules_ids) = db.module_submodules_ids(test_module.module_id) {
            module_ids.extend(submodules_ids.iter().copied().map(ModuleId::Submodule));
        }
        let mut files = vec![];
        for module_files in
            module_ids.into_iter().filter_map(|module_id| db.module_files(module_id).ok())
        {
            for file in module_files.iter().copied() {
                if !files.contains(&file) {
                    files.push(file);
                }
            }
        }
        let mut file_contents = vec![];

        for file_id in files {
            let content = db.file_content(file_id).unwrap();
            let content_location =
                DiagnosticLocation { file_id, span: TextSpan::from_str(&content) };
            let original_location = content_location.user_location(db.upcast());
            let origin = (content_location != original_location)
                .then(|| format!("{:?}\n", original_location.debug(db.upcast())))
                .unwrap_or_default();
            let file_name = file_id.file_name(&db);
            file_contents.push(format!("{origin}{file_name}:\n\n{content}"));
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
        new_storage_interface: "new_storage_interface",
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
            FileLongId::OnDisk(PathBuf::from(inputs["contract_file_name"].clone())).intern(&db);
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
        with_ownable_mini: "with_ownable_mini",
        with_erc20_mini: "with_erc20_mini",
        with_erc20: "with_erc20",
        upgradable_counter: "upgradable_counter",
        ownable_erc20: "ownable_erc20",
        mintable: "mintable",
        multi_component: "multi_component",
        storage_accesses: "storage_accesses",
    },
    ExpandContractFromCrateTestRunner
);
