use std::default::Default;
use std::sync::Arc;

use cairo_lang_defs::db::{DefsGroup, init_defs_group, try_ext_as_virtual_impl};
use cairo_lang_defs::ids::{MacroPluginLongId, ModuleId};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_filesystem::cfg::CfgSet;
use cairo_lang_filesystem::db::{
    CrateConfiguration, ExternalFiles, FilesGroup, FilesGroupEx, init_files_group,
};
use cairo_lang_filesystem::ids::{
    CodeMapping, CodeOrigin, CrateId, Directory, FileLongId, VirtualFile,
};
use cairo_lang_filesystem::override_file_content;
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, Upcast};
use itertools::chain;

use crate::get_base_plugins;
use crate::test_utils::expand_module_text;

cairo_lang_test_utils::test_file_test!(
    expand_plugin,
    "src/test_data",
    {
        compile_error: "compile_error",
        config: "config",
        derive: "derive",
        generate_trait: "generate_trait",
        panicable: "panicable",
        external_attributes_validation: "external_attributes_validation",
    },
    test_expand_plugin
);

cairo_lang_test_utils::test_file_test!(
    expand_general_plugin,
    "src/test_data",
    {
        general: "general",
    },
    test_general_plugin
);

#[salsa::db]
#[derive(Clone)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
#[salsa::db]
impl salsa::Database for DatabaseForTesting {}
impl ExternalFiles for DatabaseForTesting {
    fn try_ext_as_virtual(&self, external_id: salsa::Id) -> Option<VirtualFile> {
        try_ext_as_virtual_impl(self.upcast(), external_id)
    }
}
impl Default for DatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_defs_group(&mut res);
        res.set_default_macro_plugins_input(
            get_base_plugins().into_iter().map(MacroPluginLongId).collect(),
        );
        res
    }
}
impl Upcast<dyn DefsGroup> for DatabaseForTesting {
    fn upcast(&self) -> &dyn DefsGroup {
        self
    }
}
impl Upcast<dyn FilesGroup> for DatabaseForTesting {
    fn upcast(&self) -> &dyn FilesGroup {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for DatabaseForTesting {
    fn upcast(&self) -> &dyn SyntaxGroup {
        self
    }
}

/// Tests expansion of given code, with only the default plugins.
pub fn test_expand_plugin(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    test_expand_plugin_inner(inputs, args, &[])
}

/// Tests expansion of given code, with the default plugins plus a dedicated plugin.
fn test_general_plugin(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    test_expand_plugin_inner(inputs, args, &[Arc::new(DoubleIndirectionPlugin)])
}

/// Tests expansion of given code, with the default plugins plus the given extra plugins.
pub fn test_expand_plugin_inner(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
    extra_plugins: &[Arc<dyn MacroPlugin>],
) -> TestRunnerResult {
    let mut db = DatabaseForTesting::default();

    let extra_plugins = extra_plugins.iter().cloned().map(MacroPluginLongId);

    let default_plugins = db.default_macro_plugins_input();
    let plugins = chain!(default_plugins.iter().cloned(), extra_plugins).collect::<Arc<[_]>>();
    db.set_default_macro_plugins_input(plugins);

    let cfg_set: Option<CfgSet> =
        inputs.get("cfg").map(|s| serde_json::from_str(s.as_str()).unwrap());
    if let Some(cfg_set) = cfg_set {
        db.set_cfg_set(Arc::new(cfg_set));
    }

    let cairo_code = &inputs["cairo_code"];

    let crate_id = CrateId::plain(&db, "test");
    let root = Directory::Real("test_src".into());
    cairo_lang_filesystem::set_crate_config!(
        db,
        crate_id,
        Some(CrateConfiguration::default_for_root(root))
    );

    // Main module file.
    let file_id = FileLongId::OnDisk("test_src/lib.cairo".into()).intern(&db);
    override_file_content!(db, file_id, Some(format!("{cairo_code}\n").into()));

    let crate_id = CrateId::plain(&db, "test");
    let mut diagnostic_items = vec![];
    let expanded_module =
        expand_module_text(&db, ModuleId::CrateRoot(crate_id), &mut diagnostic_items);
    let joined_diagnostics = diagnostic_items.join("\n");
    let error = verify_diagnostics_expectation(args, &joined_diagnostics);

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("expanded_cairo_code".into(), expanded_module),
            ("expected_diagnostics".into(), joined_diagnostics),
        ]),
        error,
    }
}

#[derive(Debug)]
struct DoubleIndirectionPlugin;
impl MacroPlugin for DoubleIndirectionPlugin {
    fn generate_code<'db>(
        &self,
        db: &'db dyn SyntaxGroup,
        item_ast: ast::ModuleItem<'db>,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult<'db> {
        let node = item_ast.as_syntax_node();
        let orig_span = node.span(db);
        let code_mappings = |content: &str| {
            vec![CodeMapping {
                span: TextSpan::from_str(content),
                origin: CodeOrigin::Start(orig_span.start),
            }]
        };
        match item_ast {
            ast::ModuleItem::Struct(struct_ast) => {
                if struct_ast.has_attr(db, "first") {
                    let content = "#[second] struct A {}\n".to_string();
                    PluginResult {
                        code: Some(PluginGeneratedFile {
                            name: "virt1".into(),
                            code_mappings: code_mappings(content.as_str()),
                            content,
                            aux_data: None,
                            diagnostics_note: Some("first note".to_string()),
                            is_unhygienic: false,
                        }),
                        ..PluginResult::default()
                    }
                } else if struct_ast.has_attr(db, "second") {
                    let content = "struct B {}\n".to_string();
                    PluginResult {
                        code: Some(PluginGeneratedFile {
                            name: "virt2".into(),
                            code_mappings: code_mappings(content.as_str()),
                            content,
                            aux_data: None,
                            diagnostics_note: Some("second note".to_string()),
                            is_unhygienic: false,
                        }),
                        ..PluginResult::default()
                    }
                } else {
                    PluginResult {
                        diagnostics: vec![PluginDiagnostic::error(
                            struct_ast.stable_ptr(db),
                            "Double indirection diagnostic".to_string(),
                        )],
                        ..PluginResult::default()
                    }
                }
            }
            _ => PluginResult::default(),
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec!["first".to_string(), "second".to_string()]
    }
}
