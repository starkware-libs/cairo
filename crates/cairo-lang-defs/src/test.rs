use std::fmt::Write as _;
use std::sync::Arc;

use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_filesystem::db::{
    AsFilesGroupMut, CrateConfiguration, ExternalFiles, FilesDatabase, FilesGroup, FilesGroupEx,
    init_files_group,
};
use cairo_lang_filesystem::ids::{CrateId, Directory, FileLongId, VirtualFile};
use cairo_lang_parser::db::{ParserDatabase, ParserGroup};
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, ast};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern, Upcast, extract_matches, try_extract_matches};
use indoc::indoc;

use crate::db::{DefsDatabase, DefsGroup, init_defs_group, try_ext_as_virtual_impl};
use crate::ids::{
    FileIndex, GenericParamLongId, MacroPluginLongId, ModuleFileId, ModuleId, ModuleItemId,
    NamedLanguageElementId, SubmoduleLongId,
};
use crate::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};

#[salsa::database(DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
impl salsa::Database for DatabaseForTesting {}
impl ExternalFiles for DatabaseForTesting {
    fn try_ext_as_virtual(&self, external_id: salsa::InternId) -> Option<VirtualFile> {
        try_ext_as_virtual_impl(self.upcast(), external_id)
    }
}
impl Default for DatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_defs_group(&mut res);
        res.set_default_macro_plugins(Arc::new([
            res.intern_macro_plugin(MacroPluginLongId(Arc::new(FooToBarPlugin))),
            res.intern_macro_plugin(MacroPluginLongId(Arc::new(RemoveOrigPlugin))),
            res.intern_macro_plugin(MacroPluginLongId(Arc::new(DummyPlugin))),
        ]));
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

cairo_lang_test_utils::test_file_test!(
    defs,
    "src/test_data",
    {
        generic_item_id: "generic_item_id",
    },
    test_generic_item_id
);
fn test_generic_item_id(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let mut db_val = DatabaseForTesting::default();
    let module_id = setup_test_module(&mut db_val, inputs["module_code"].as_str());

    let module_file_id = ModuleFileId(module_id, FileIndex(0));
    let db = &db_val;
    let file_id = db.module_main_file(module_id).unwrap();
    let node = db.file_syntax(file_id).unwrap();
    let mut output = String::new();

    fn find_generics(
        db: &DatabaseForTesting,
        mut module_file_id: ModuleFileId,
        node: &SyntaxNode,
        output: &mut String,
    ) {
        match node.kind(db) {
            SyntaxKind::ItemModule => {
                let submodule_id =
                    SubmoduleLongId(module_file_id, ast::ItemModulePtr(node.stable_ptr()))
                        .intern(db);
                module_file_id = ModuleFileId(ModuleId::Submodule(submodule_id), FileIndex(0));
            }
            SyntaxKind::GenericParamType
            | SyntaxKind::GenericParamConst
            | SyntaxKind::GenericParamImplNamed
            | SyntaxKind::GenericParamImplAnonymous => {
                let param_id =
                    GenericParamLongId(module_file_id, ast::GenericParamPtr(node.stable_ptr()))
                        .intern(db);
                let generic_item = param_id.generic_item(db);
                writeln!(output, "{:?} -> {:?}", param_id.debug(db), generic_item.debug(db))
                    .unwrap();
            }
            _ => {}
        }
        for child in db.get_children(node.clone()).iter() {
            find_generics(db, module_file_id, child, output);
        }
    }
    find_generics(db, module_file_id, &node, &mut output);

    TestRunnerResult::success(OrderedHashMap::from([("output".into(), output)]))
}

pub fn setup_test_module<T: DefsGroup + AsFilesGroupMut + ?Sized>(
    db: &mut T,
    content: &str,
) -> ModuleId {
    let crate_id = CrateId::plain(db, "test");
    let directory = Directory::Real("src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(directory)));
    let file = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
    db.as_files_group_mut().override_file_content(file, Some(content.into()));
    let syntax_diagnostics = db.file_syntax_diagnostics(file).format(Upcast::upcast(db));
    assert_eq!(syntax_diagnostics, "");
    ModuleId::CrateRoot(crate_id)
}

#[test]
fn test_module_file() {
    let mut db_val = DatabaseForTesting::default();
    let module_id = setup_test_module(
        &mut db_val,
        indoc! {"
            mod mysubmodule;
        "},
    );
    let db = &db_val;
    let item_id =
        extract_matches!(db.module_items(module_id).ok().unwrap()[0], ModuleItemId::Submodule);
    assert_eq!(item_id.name(db), "mysubmodule");

    let submodule_id = ModuleId::Submodule(item_id);
    assert_eq!(
        db.module_main_file(module_id).unwrap().lookup_intern(db),
        FileLongId::OnDisk("src/lib.cairo".into())
    );
    assert_eq!(
        db.module_main_file(submodule_id).unwrap().lookup_intern(db),
        FileLongId::OnDisk("src/mysubmodule.cairo".into())
    );
}

fn set_file_content(db: &mut DatabaseForTesting, path: &str, content: &str) {
    let file_id = FileLongId::OnDisk(path.into()).intern(db);
    db.as_files_group_mut().override_file_content(file_id, Some(content.into()));
}

#[test]
fn test_submodules() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = CrateId::plain(db, "test");
    let root = Directory::Real("src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "mod submod;");
    set_file_content(db, "src/submod.cairo", "mod subsubmod;");
    set_file_content(db, "src/submod/subsubmod.cairo", "fn foo() {}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);
    let submodule_id =
        ModuleId::Submodule(*db.module_submodules_ids(module_id).unwrap().first().unwrap());
    let subsubmodule_id =
        ModuleId::Submodule(*db.module_submodules_ids(submodule_id).unwrap().first().unwrap());

    assert_eq!(
        format!("{:?}", db.module_items(subsubmodule_id).unwrap().debug(db)),
        "[FreeFunctionId(test::submod::subsubmod::foo), ExternTypeId(test::submod::subsubmod::B)]"
    );

    // Test file mappings.
    assert_eq!(
        &db.file_modules(db.module_main_file(module_id).unwrap()).unwrap()[..],
        vec![module_id]
    );
    assert_eq!(
        &db.file_modules(db.module_main_file(submodule_id).unwrap()).unwrap()[..],
        vec![submodule_id]
    );
    assert_eq!(
        &db.file_modules(db.module_main_file(subsubmodule_id).unwrap()).unwrap()[..],
        vec![subsubmodule_id]
    );
}

#[derive(Debug)]
struct DummyPlugin;
impl MacroPlugin for DummyPlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        match item_ast {
            ast::ModuleItem::Struct(struct_ast) => {
                let remove_original_item = struct_ast.has_attr(db, "remove_original");
                PluginResult {
                    code: Some(PluginGeneratedFile {
                        name: "virt".into(),
                        content: format!("fn f(x:{}){{}}", struct_ast.name(db).text(db)),
                        code_mappings: Default::default(),
                        aux_data: None,
                        diagnostics_note: Default::default(),
                    }),
                    diagnostics: vec![],
                    remove_original_item,
                }
            }
            ast::ModuleItem::FreeFunction(free_function_ast) => PluginResult {
                code: Some(PluginGeneratedFile {
                    name: "virt2".into(),
                    content: "extern type B;".into(),
                    code_mappings: Default::default(),
                    aux_data: None,
                    diagnostics_note: Default::default(),
                }),
                diagnostics: vec![PluginDiagnostic::error(&free_function_ast, "bla".into())],
                remove_original_item: false,
            },
            _ => PluginResult::default(),
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec!["remove_original".to_string()]
    }
}

#[test]
fn test_plugin() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = CrateId::plain(db, "test");
    let root = Directory::Real("src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "struct A{}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original struct still exists.
    // 2. The expected items were generated.
    assert_eq!(
        format!("{:?}", db.module_items(module_id).unwrap().debug(db)),
        "[StructId(test::A), FreeFunctionId(test::f), ExternTypeId(test::B)]"
    );
}

#[test]
fn test_plugin_remove_original() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = CrateId::plain(db, "test");
    let root = Directory::Real("src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "#[remove_original] struct A{}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original struct was removed.
    // 2. The expected items were generated.
    assert_eq!(
        format!("{:?}", db.module_items(module_id).unwrap().debug(db)),
        "[FreeFunctionId(test::f), ExternTypeId(test::B)]"
    );
}

/// If the original item is a function that is marked with #[remove_orig], only removes it, without
/// generating any new code.
#[derive(Debug)]
struct RemoveOrigPlugin;
impl MacroPlugin for RemoveOrigPlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        let Some(free_function_ast) = try_extract_matches!(item_ast, ast::ModuleItem::FreeFunction)
        else {
            return PluginResult::default();
        };
        if !free_function_ast.has_attr(db, "remove_orig") {
            return PluginResult::default();
        }
        PluginResult { code: None, diagnostics: vec![], remove_original_item: true }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec!["remove_orig".to_string()]
    }
}

/// Changes a function 'foo' to 'bar' if annotated with #[foo_to_bar]. Doesn't remove the original
/// item.
#[derive(Debug)]
struct FooToBarPlugin;
impl MacroPlugin for FooToBarPlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        let Some(free_function_ast) = try_extract_matches!(item_ast, ast::ModuleItem::FreeFunction)
        else {
            return PluginResult::default();
        };
        if free_function_ast.declaration(db).name(db).text(db) != "foo" {
            return PluginResult::default();
        }
        if !free_function_ast.has_attr(db, "foo_to_bar") {
            return PluginResult::default();
        }

        PluginResult {
            code: Some(PluginGeneratedFile {
                name: "virt".into(),
                content: "fn bar() {}".to_string(),
                code_mappings: vec![],
                aux_data: None,
                diagnostics_note: Default::default(),
            }),
            diagnostics: vec![],
            remove_original_item: false,
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec!["foo_to_bar".to_string()]
    }
}

#[test]
fn test_foo_to_bar() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = CrateId::plain(db, "test");
    let root = Directory::Real("src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "#[foo_to_bar] fn foo() {}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original function remained.
    // 2. The expected items were generated.
    assert_eq!(
        format!("{:?}", db.module_items(module_id).unwrap().debug(db)),
        "[FreeFunctionId(test::foo), FreeFunctionId(test::bar), ExternTypeId(test::B), \
         ExternTypeId(test::B)]"
    );
}

// Verify that if the first plugin removed the original item, the second item doesn't act on the
// original item.
#[test]
fn test_first_plugin_removes() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = CrateId::plain(db, "test");
    let root = Directory::Real("src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "#[remove_orig] fn foo() {}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original function was removed.
    // 2. No 'B' was generated by DummyPlugin.
    // Note RemoveOrigPlugin is before DummyPlugin in the plugins order. RemoveOrigPlugin already
    // acted on 'foo', so DummyPlugin shouldn't.
    assert_eq!(format!("{:?}", db.module_items(module_id).unwrap().debug(db)), "[]");
}

// Verify that if the first plugin generates new code, the later plugins don't act on the
// original // item.
#[test]
fn test_first_plugin_generates() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = CrateId::plain(db, "test");
    let root = Directory::Real("src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "#[foo_to_bar] #[remove_orig] fn foo() {}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. 'bar' was generated by FooToBarPlugin.
    // 2. the original function was removed.
    assert_eq!(
        format!("{:?}", db.module_items(module_id).unwrap().debug(db)),
        "[FreeFunctionId(test::bar), ExternTypeId(test::B)]"
    );
}

// Verify that the later plugins do act on items generated by earlier plugins.
#[test]
fn test_plugin_chain() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = CrateId::plain(db, "test");
    let root = Directory::Real("src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "#[foo_to_bar] fn foo() {}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original function remained.
    // 2. 'bar' was generated by FooToBarPlugin.
    // 3. 'B' were generated by DummyPlugin for foo and bar.
    assert_eq!(
        format!("{:?}", db.module_items(module_id).unwrap().debug(db)),
        "[FreeFunctionId(test::foo), FreeFunctionId(test::bar), ExternTypeId(test::B), \
         ExternTypeId(test::B)]"
    )
}

// Test that unknown inline item macros are raising an error.
#[test]
fn test_unknown_item_macro() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = CrateId::plain(db, "test");
    let root = Directory::Real("src".into());
    db.set_crate_config(crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "unknown_item_macro!();");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);
    assert_eq!(
        format!("{:?}", db.module_plugin_diagnostics(module_id).unwrap()),
        "[(ModuleFileId(CrateRoot(CrateId(0)), FileIndex(0)), PluginDiagnostic { stable_ptr: \
         SyntaxStablePtrId(3), message: \"Unknown inline item macro: 'unknown_item_macro'.\", \
         severity: Error })]"
    )
}
