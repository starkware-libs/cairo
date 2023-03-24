use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::get_diagnostics_as_string;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::plugin::{MacroPlugin, PluginGeneratedFile, PluginResult};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_test_utils::parse_test_file::TestFileRunner;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::StarknetRootDatabaseBuilderEx;
use crate::plugin::StarkNetPlugin;

struct ExpandContractTestRunner {
    db: RootDatabase,
}
impl Default for ExpandContractTestRunner {
    fn default() -> Self {
        Self { db: RootDatabase::builder().detect_corelib().with_starknet().build().unwrap() }
    }
}
impl TestFileRunner for ExpandContractTestRunner {
    fn run(&mut self, inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
        let (test_module, _semantic_diagnostics) =
            setup_test_module(&mut self.db, inputs["cairo_code"].as_str()).split();

        let file_id = self.db.module_main_file(test_module.module_id).unwrap();
        let syntax_file = self.db.file_syntax(file_id).unwrap();

        let mut generated_items: Vec<String> = Vec::new();

        for item in syntax_file.items(&self.db).elements(&self.db).into_iter() {
            let PluginResult { code, diagnostics: _, remove_original_item } =
                StarkNetPlugin.generate_code(&self.db, item.clone());

            let content = match code {
                Some(PluginGeneratedFile { content, .. }) => content,
                None => continue,
            };
            if !remove_original_item {
                generated_items.push(item.as_syntax_node().get_text(&self.db));
            }
            generated_items.push(content);
        }

        OrderedHashMap::from([
            ("generated_cairo_code".into(), generated_items.join("\n")),
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
        raw_output: "raw_output",
        storage: "storage",
        hello_starknet: "hello_starknet",
        dispatcher: "dispatcher",
        user_defined_types: "user_defined_types",
    },
    ExpandContractTestRunner
);
