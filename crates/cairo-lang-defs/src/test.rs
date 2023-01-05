use std::sync::Arc;

use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_filesystem::db::{
    init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup, FilesGroupEx,
};
use cairo_lang_filesystem::ids::{CrateLongId, Directory, FileLongId};
use cairo_lang_parser::db::ParserDatabase;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::{extract_matches, try_extract_matches, Upcast};
use indoc::indoc;

use crate::db::{DefsDatabase, DefsGroup, HasMacroPlugins};
use crate::ids::{ModuleId, ModuleItemId};
use crate::plugin::{
    DynGeneratedFileAuxData, GeneratedFileAuxData, MacroPlugin, PluginDiagnostic,
    PluginGeneratedFile, PluginResult,
};

#[salsa::database(DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
    plugins: Vec<Arc<dyn MacroPlugin>>,
}
impl salsa::Database for DatabaseForTesting {}
impl Default for DatabaseForTesting {
    fn default() -> Self {
        let mut res = Self {
            storage: Default::default(),
            plugins: vec![
                Arc::new(FooToBarPlugin {}),
                Arc::new(RemoveOrigPlugin {}),
                Arc::new(DummyPlugin {}),
            ],
        };
        init_files_group(&mut res);
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
impl HasMacroPlugins for DatabaseForTesting {
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>> {
        self.plugins.clone()
    }
}

pub fn setup_test_module<T: DefsGroup + AsFilesGroupMut + ?Sized>(
    db: &mut T,
    content: &str,
) -> ModuleId {
    let crate_id = db.intern_crate(CrateLongId("test".into()));
    let directory = Directory("src".into());
    db.set_crate_root(crate_id, Some(directory));
    let file = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
    db.as_files_group_mut().override_file_content(file, Some(Arc::new(content.to_string())));
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
        db.lookup_intern_file(db.module_main_file(module_id).unwrap()),
        FileLongId::OnDisk("src/lib.cairo".into())
    );
    assert_eq!(
        db.lookup_intern_file(db.module_main_file(submodule_id).unwrap()),
        FileLongId::OnDisk("src/mysubmodule.cairo".into())
    );
}

fn set_file_content(db: &mut DatabaseForTesting, path: &str, content: &str) {
    let file_id = db.intern_file(FileLongId::OnDisk(path.into()));
    db.as_files_group_mut().override_file_content(file_id, Some(Arc::new(content.into())));
}

#[test]
fn test_submodules() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = db.intern_crate(CrateLongId("test".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));

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
    assert_eq!(db.file_modules(db.module_main_file(module_id).unwrap()).unwrap(), vec![module_id]);
    assert_eq!(
        db.file_modules(db.module_main_file(submodule_id).unwrap()).unwrap(),
        vec![submodule_id]
    );
    assert_eq!(
        db.file_modules(db.module_main_file(subsubmodule_id).unwrap()).unwrap(),
        vec![subsubmodule_id]
    );
}

#[derive(Debug)]
struct DummyAuxData;
impl GeneratedFileAuxData for DummyAuxData {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn eq(&self, _other: &dyn GeneratedFileAuxData) -> bool {
        false
    }
}

#[derive(Debug)]
struct DummyPlugin {}
impl MacroPlugin for DummyPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Struct(struct_ast) => {
                let remove_original_item = struct_ast
                    .attributes(db)
                    .elements(db)
                    .iter()
                    .any(|attr| attr.attr(db).text(db) == "remove_original");
                PluginResult {
                    code: Some(PluginGeneratedFile {
                        name: "virt".into(),
                        content: format!("fn f(x:{}){{}}", struct_ast.name(db).text(db)),
                        aux_data: DynGeneratedFileAuxData::new(DummyAuxData),
                    }),
                    diagnostics: vec![],
                    remove_original_item,
                }
            }
            ast::Item::FreeFunction(free_function_ast) => PluginResult {
                code: Some(PluginGeneratedFile {
                    name: "virt2".into(),
                    content: "extern type B;".into(),
                    aux_data: DynGeneratedFileAuxData::new(DummyAuxData),
                }),
                diagnostics: vec![PluginDiagnostic {
                    stable_ptr: free_function_ast.stable_ptr().untyped(),
                    message: "bla".into(),
                }],
                remove_original_item: false,
            },
            _ => PluginResult::default(),
        }
    }
}

#[test]
fn test_plugin() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = db.intern_crate(CrateLongId("test".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "struct A{}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original struct still exists.
    // 2. The expected items were generated.
    assert_eq!(
        format!("{:?}", db.module_items(module_id).unwrap().debug(db)),
        "[FreeFunctionId(test::f), ExternTypeId(test::B), StructId(test::A)]"
    );
}

#[test]
fn test_plugin_remove_original() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = db.intern_crate(CrateLongId("test".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));

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
struct RemoveOrigPlugin {}
impl MacroPlugin for RemoveOrigPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        let Some(free_function_ast) = try_extract_matches!(item_ast, ast::Item::FreeFunction) else { return PluginResult::default(); };
        if !free_function_ast
            .attributes(db)
            .elements(db)
            .iter()
            .any(|attr| attr.attr(db).text(db) == "remove_orig")
        {
            return PluginResult::default();
        }
        PluginResult { code: None, diagnostics: vec![], remove_original_item: true }
    }
}

/// Changes a function 'foo' to 'bar' if annotated with #[foo_to_bar]. Doesn't remove the original
/// item.
#[derive(Debug)]
struct FooToBarPlugin {}
impl MacroPlugin for FooToBarPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        let Some(free_function_ast) = try_extract_matches!(item_ast, ast::Item::FreeFunction) else { return PluginResult::default(); };
        if free_function_ast.declaration(db).name(db).text(db) != "foo" {
            return PluginResult::default();
        }
        if !free_function_ast
            .attributes(db)
            .elements(db)
            .iter()
            .any(|attr| attr.attr(db).text(db) == "foo_to_bar")
        {
            return PluginResult::default();
        }

        PluginResult {
            code: Some(PluginGeneratedFile {
                name: "virt".into(),
                content: "fn bar() {}".to_string(),
                aux_data: DynGeneratedFileAuxData::new(DummyAuxData),
            }),
            diagnostics: vec![],
            remove_original_item: false,
        }
    }
}

#[test]
fn test_foo_to_bar() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = db.intern_crate(CrateLongId("test".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "#[foo_to_bar] fn foo() {}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original function remained.
    // 2. The expected items were generated.
    assert_eq!(
        format!("{:?}", db.module_items(module_id).unwrap().debug(db)),
        "[FreeFunctionId(test::foo), FreeFunctionId(test::bar), ExternTypeId(test::B)]"
    );
}

// Verify that if the first plugin removed the original item, the second item doesn't act on the
// original item.
#[test]
fn test_first_plugin_removes() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = db.intern_crate(CrateLongId("test".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));

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
    let crate_id = db.intern_crate(CrateLongId("test".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "#[foo_to_bar] #[remove_orig] fn foo() {}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. 'bar' was generated by FooToBarPlugin.
    // 2. the original function remained.
    // Note RemoveOrigPlugin is after FooToBarPlugin in the plugins order. FooToBarPlugin already
    // acted on the original 'foo' and thus RemoveOrigPlugin shouldn't act on it.
    assert_eq!(
        format!("{:?}", db.module_items(module_id).unwrap().debug(db)),
        "[FreeFunctionId(test::foo), FreeFunctionId(test::bar), ExternTypeId(test::B)]"
    );
}

// Verify that the later plugins do act on items generated by earlier plugins.
#[test]
fn test_plugin_chain() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = db.intern_crate(CrateLongId("test".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "#[foo_to_bar] fn foo() {}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1.  The original function remained.
    // 2. 'bar' was generated by FooToBarPlugin.
    // 3. 'B' were generated by DummyPlugin.
    assert_eq!(
        format!("{:?}", db.module_items(module_id).unwrap().debug(db)),
        "[FreeFunctionId(test::foo), FreeFunctionId(test::bar), ExternTypeId(test::B)]"
    )
}
