use std::sync::Arc;

use cairo_lang_defs::db::{DefsDatabase, DefsGroup};
use cairo_lang_defs::ids::{LanguageElementId, ModuleId, ModuleItemId};
use cairo_lang_diagnostics::{format_diagnostics, DiagnosticLocation};
use cairo_lang_filesystem::cfg::CfgSet;
use cairo_lang_filesystem::db::{
    init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup, FilesGroupEx,
};
use cairo_lang_filesystem::ids::{CrateLongId, Directory, FileLongId};
use cairo_lang_parser::db::ParserDatabase;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::Upcast;

use crate::get_default_plugins;

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

#[salsa::database(DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
impl salsa::Database for DatabaseForTesting {}
impl Default for DatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res.set_macro_plugins(get_default_plugins());
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

/// Returns the expanded code for `module_id` after running all plugins and extends `diagnostics`
/// with all the plugins diagnostics.
fn expand_module_text(
    db: &dyn DefsGroup,
    module_id: ModuleId,
    diagnostics: &mut Vec<String>,
) -> String {
    let mut output = String::new();
    let syntax_db = db.upcast();
    // Collect the module diagnostics.
    for (file_id, diag) in db.module_plugin_diagnostics(module_id).unwrap().iter() {
        let syntax_node = diag.stable_ptr.lookup(syntax_db);
        let location = DiagnosticLocation {
            file_id: file_id.file_id(db.upcast()).unwrap(),
            span: syntax_node.span_without_trivia(syntax_db),
        };
        diagnostics.push(format_diagnostics(db.upcast(), &diag.message, location));
    }
    for item_id in db.module_items(module_id).unwrap().iter() {
        if let ModuleItemId::Submodule(item) = item_id {
            let submodule_item = item.stable_ptr(db).lookup(syntax_db);
            if let ast::MaybeModuleBody::Some(body) = submodule_item.body(syntax_db) {
                // Recursively expand inline submodules.
                output.extend([
                    submodule_item.attributes(syntax_db).node.get_text(syntax_db),
                    submodule_item.module_kw(syntax_db).as_syntax_node().get_text(syntax_db),
                    submodule_item.name(syntax_db).as_syntax_node().get_text(syntax_db),
                    body.lbrace(syntax_db).as_syntax_node().get_text(syntax_db),
                    expand_module_text(db, ModuleId::Submodule(*item), diagnostics),
                    body.rbrace(syntax_db).as_syntax_node().get_text(syntax_db),
                ]);
                continue;
            }
        }
        let syntax_item = item_id.untyped_stable_ptr(db);
        // Output other items as is.
        output.push_str(&syntax_item.lookup(syntax_db).get_text(syntax_db));
    }
    output
}

pub fn test_expand_plugin(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut DatabaseForTesting::default();

    let cfg_set: Option<CfgSet> =
        inputs.get("cfg").map(|s| serde_json::from_str(s.as_str()).unwrap());
    if let Some(cfg_set) = cfg_set {
        db.set_cfg_set(Arc::new(cfg_set));
    }

    let cairo_code = &inputs["cairo_code"];

    let crate_id = db.intern_crate(CrateLongId::Real("test".into()));
    let root = Directory::Real("test_src".into());
    db.set_crate_root(crate_id, Some(root));

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
