use std::sync::Arc;

use cairo_db_utils::Upcast;
use cairo_debug::debug::DebugWithDb;
use cairo_filesystem::db::{
    init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup, FilesGroupEx,
};
use cairo_filesystem::ids::{CrateLongId, Directory, FileLongId};
use cairo_parser::db::ParserDatabase;
use cairo_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_utils::extract_matches;
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
    plugin: Arc<DummyPlugin>,
}
impl salsa::Database for DatabaseForTesting {}
impl Default for DatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default(), plugin: Arc::new(DummyPlugin {}) };
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
        vec![self.plugin.clone()]
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
fn test_resolve() {
    let mut db_val = DatabaseForTesting::default();
    let module_id = setup_test_module(
        &mut db_val,
        indoc! {"
            fn foo() -> felt { 5 }
            extern fn felt_add(a: felt, b: felt) -> felt nopanic;
        "},
    );
    let db = &db_val;
    assert!(db.module_item_by_name(module_id, "doesnt_exist".into()).unwrap().is_none());
    let felt_add = db.module_item_by_name(module_id, "felt_add".into()).unwrap();
    assert_eq!(format!("{:?}", felt_add.debug(db)), "Some(ExternFunctionId(test::felt_add))");
    match db.module_item_by_name(module_id, "felt_add".into()).unwrap().unwrap() {
        crate::ids::ModuleItemId::ExternFunction(_) => {}
        _ => panic!("Expected an extern function"),
    };
    match db.module_item_by_name(module_id, "foo".into()).unwrap().unwrap() {
        crate::ids::ModuleItemId::FreeFunction(_) => {}
        _ => panic!("Expected a free function"),
    };
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
    let item_id = extract_matches!(
        db.module_item_by_name(module_id, "mysubmodule".into()).unwrap().unwrap(),
        ModuleItemId::Submodule
    );
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
    let submodule_id = ModuleId::Submodule(
        *db.module_data(module_id).unwrap().submodules.iter().next().unwrap().0,
    );
    let subsubmodule_id = ModuleId::Submodule(
        *db.module_data(submodule_id).unwrap().submodules.iter().next().unwrap().0,
    );

    db.module_item_by_name(subsubmodule_id, "foo".into())
        .unwrap()
        .expect("Expected to find foo() in subsubmodule.");

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
struct DummyPlugin {}

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

impl MacroPlugin for DummyPlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: cairo_syntax::node::ast::Item,
    ) -> PluginResult {
        match item_ast {
            ast::Item::Struct(struct_ast) => PluginResult {
                code: Some(PluginGeneratedFile {
                    name: "virt".into(),
                    content: format!("fn foo(x:{}){{}}", struct_ast.name(db).text(db)),
                    aux_data: DynGeneratedFileAuxData::new(DummyAuxData),
                }),
                diagnostics: vec![],
            },
            ast::Item::FreeFunction(item) => PluginResult {
                code: Some(PluginGeneratedFile {
                    name: "virt2".into(),
                    content: "extern type B;".into(),
                    aux_data: DynGeneratedFileAuxData::new(DummyAuxData),
                }),
                diagnostics: vec![PluginDiagnostic {
                    stable_ptr: item.stable_ptr().untyped(),
                    message: "bla".into(),
                }],
            },
            _ => PluginResult { code: None, diagnostics: vec![] },
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

    assert_eq!(
        format!("{:?}", db.module_item_by_name(module_id, "foo".into()).unwrap().debug(db)),
        "Some(FreeFunctionId(test::foo))"
    );

    assert_eq!(
        format!("{:?}", db.module_item_by_name(module_id, "B".into()).unwrap().debug(db)),
        "Some(ExternTypeId(test::B))"
    );
}
