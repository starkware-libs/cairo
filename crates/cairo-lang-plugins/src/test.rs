use std::sync::Arc;

use cairo_lang_defs::db::{DefsDatabase, DefsGroup};
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::plugin::{MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult};
use cairo_lang_filesystem::cfg::CfgSet;
use cairo_lang_filesystem::db::{
    init_files_group, AsFilesGroupMut, CrateConfiguration, FilesDatabase, FilesGroup, FilesGroupEx,
};
use cairo_lang_filesystem::ids::{CrateLongId, Directory, FileLongId};
use cairo_lang_parser::db::ParserDatabase;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::Upcast;

use crate::get_base_plugins;
use crate::test_utils::expand_module_text;

cairo_lang_test_utils::test_file_test!(
    expand_plugin,
    "src/test_data",
    {
        config: "config",
        derive: "derive",
        generate_trait: "generate_trait",
        panicable: "panicable",
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

#[salsa::database(DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
impl salsa::Database for DatabaseForTesting {}
impl Default for DatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res.set_macro_plugins(get_base_plugins());
        res
    }
}
impl AsFilesGroupMut for DatabaseForTesting {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn DefsGroup> for DatabaseForTesting {
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for DatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for DatabaseForTesting {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
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
    let db = &mut DatabaseForTesting::default();
    let mut plugins = db.macro_plugins();
    plugins.extend_from_slice(extra_plugins);
    db.set_macro_plugins(plugins);

    let cfg_set: Option<CfgSet> =
        inputs.get("cfg").map(|s| serde_json::from_str(s.as_str()).unwrap());
    if let Some(cfg_set) = cfg_set {
        db.set_cfg_set(Arc::new(cfg_set));
    }

    let cairo_code = &inputs["cairo_code"];

    let crate_id = db.intern_crate(CrateLongId::Real("test".into()));
    let root = Directory::Real("test_src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    let file_id = db.intern_file(FileLongId::OnDisk("test_src/lib.cairo".into()));
    db.as_files_group_mut()
        .override_file_content(file_id, Some(Arc::new(format!("{cairo_code}\n"))));

    let mut diagnostic_items = vec![];
    let expanded_module =
        expand_module_text(db, ModuleId::CrateRoot(crate_id), &mut diagnostic_items);
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
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Struct(struct_ast) => {
                if struct_ast.has_attr(db, "first") {
                    PluginResult {
                        code: Some(PluginGeneratedFile {
                            name: "virt1".into(),
                            content: "#[second] struct A {}\n".to_string(),
                            code_mappings: Default::default(),
                            aux_data: None,
                        }),
                        ..PluginResult::default()
                    }
                } else if struct_ast.has_attr(db, "second") {
                    PluginResult {
                        code: Some(PluginGeneratedFile {
                            name: "virt2".into(),
                            content: "struct B {}\n".to_string(),
                            code_mappings: Default::default(),
                            aux_data: None,
                        }),
                        ..PluginResult::default()
                    }
                } else {
                    PluginResult {
                        diagnostics: vec![PluginDiagnostic::error(
                            struct_ast.stable_ptr().untyped(),
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
