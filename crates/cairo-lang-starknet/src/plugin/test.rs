use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::get_diagnostics_as_string;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_test_utils::parse_test_file::TestFileRunner;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::plugin::StarkNetPlugin;

struct ExpandContractTestRunner {
    db: RootDatabase,
}

impl Default for ExpandContractTestRunner {
    fn default() -> Self {
        Self {
            db: RootDatabase::builder()
                .detect_corelib()
                .with_semantic_plugin(Arc::new(StarkNetPlugin::default()))
                .build()
                .unwrap(),
        }
    }
}

impl TestFileRunner for ExpandContractTestRunner {
    fn run(&mut self, inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
        let (test_module, _semantic_diagnostics) =
            setup_test_module(&mut self.db, inputs["cairo_code"].as_str()).split();

        let mut module_ids = vec![test_module.module_id];
        module_ids.extend(
            self.db
                .module_submodules_ids(test_module.module_id)
                .unwrap_or_default()
                .into_iter()
                .map(ModuleId::Submodule),
        );
        let mut files = vec![];
        for module_id in module_ids {
            for file in self.db.module_files(module_id).unwrap_or_default() {
                if files.contains(&file) {
                    continue;
                }
                files.push(file);
            }
        }
        let mut file_contents = vec![];

        for file in files {
            file_contents.push(format!("{}:", file.file_name(&self.db)));
            file_contents.push(self.db.file_content(file).unwrap().as_ref().clone());
        }

        OrderedHashMap::from([
            ("generated_cairo_code".into(), file_contents.join("\n\n")),
            ("expected_diagnostics".into(), get_diagnostics_as_string(&mut self.db)),
        ])
    }
}

cairo_lang_test_utils::test_file_test_with_runner!(
    expand_contract,
    "src/plugin/plugin_test_data",
    {
        diagnostics: "diagnostics",
        contract: "contract",
        events: "events",
        raw_output: "raw_output",
        storage: "storage",
        hello_starknet: "hello_starknet",
        dispatcher: "dispatcher",
        user_defined_types: "user_defined_types",
        l1_handler: "l1_handler",
    },
    ExpandContractTestRunner
);
