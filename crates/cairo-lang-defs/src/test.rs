use std::fmt::Write as _;
use std::sync::Arc;

use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_filesystem::db::{CrateConfiguration, FilesGroup, init_files_group};
use cairo_lang_filesystem::ids::{CrateId, Directory, FileLongId, SmolStrId};
use cairo_lang_filesystem::{override_file_content, set_crate_config};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, TypedSyntaxNode, ast};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, extract_matches, try_extract_matches};
use indoc::indoc;
use salsa::{Database, Setter};

use crate::db::{DefsGroup, defs_group_input, init_defs_group, init_external_files};
use crate::ids::{
    GenericParamLongId, MacroPluginLongId, ModuleId, ModuleItemId, NamedLanguageElementId,
    SubmoduleLongId,
};
use crate::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};

#[salsa::db]
#[derive(Clone)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
#[salsa::db]
impl salsa::Database for DatabaseForTesting {}

impl Default for DatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_external_files(&mut res);
        init_files_group(&mut res);
        init_defs_group(&mut res);
        defs_group_input(&res).set_default_macro_plugins(&mut res).to(Some(vec![
            MacroPluginLongId(Arc::new(FooToBarPlugin)),
            MacroPluginLongId(Arc::new(RemoveOrigPlugin)),
            MacroPluginLongId(Arc::new(DummyPlugin)),
        ]));
        res
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
    let mut db_val: DatabaseForTesting = DatabaseForTesting::default();
    setup_test_module(&mut db_val, inputs["module_code"].as_str());
    let module_id = ModuleId::CrateRoot(get_crate_id(&db_val));

    let file_id = db_val.module_main_file(module_id).unwrap();
    let file_syntax = db_val.file_module_syntax(file_id).unwrap();
    let mut output = String::new();

    fn find_generics<'db>(
        db: &'db dyn Database,
        mut module_id: ModuleId<'db>,
        node: &SyntaxNode<'db>,
        output: &mut String,
    ) {
        match node.kind(db) {
            SyntaxKind::ItemModule => {
                let submodule_id =
                    SubmoduleLongId(module_id, ast::ItemModulePtr(node.stable_ptr(db))).intern(db);
                module_id = ModuleId::Submodule(submodule_id);
            }
            SyntaxKind::GenericParamType
            | SyntaxKind::GenericParamConst
            | SyntaxKind::GenericParamImplNamed
            | SyntaxKind::GenericParamImplAnonymous => {
                let param_id =
                    GenericParamLongId(module_id, ast::GenericParamPtr(node.stable_ptr(db)))
                        .intern(db);
                let generic_item = param_id.generic_item(db);
                writeln!(output, "{:?} -> {:?}", param_id.debug(db), generic_item.debug(db))
                    .unwrap();
            }
            _ => {}
        }
        for child in node.get_children(db).iter() {
            find_generics(db, module_id, child, output);
        }
    }
    find_generics(&db_val, module_id, &file_syntax.as_syntax_node(), &mut output);

    TestRunnerResult::success(OrderedHashMap::from([("output".into(), output)]))
}

pub fn get_crate_id<'db>(db: &'db dyn Database) -> CrateId<'db> {
    CrateId::plain(db, SmolStrId::from(db, "test"))
}

pub fn setup_test_module(db: &mut dyn Database, content: &str) {
    let crate_id = get_crate_id(db);
    let directory = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(directory)));
    let crate_id = get_crate_id(db);
    let file = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
    override_file_content!(db, file, Some(content.into()));
    let crate_id = get_crate_id(db);
    let file = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
    let syntax_diagnostics = db.file_syntax_diagnostics(file).format(db);
    assert_eq!(syntax_diagnostics, "");
}

#[test]
fn test_module_file() {
    let mut db_val = DatabaseForTesting::default();
    setup_test_module(
        &mut db_val,
        indoc! {"
            mod mysubmodule;
        "},
    );
    let module_id = ModuleId::CrateRoot(get_crate_id(&db_val));
    let db = &db_val;
    let item_id = extract_matches!(
        module_id.module_data(db).ok().unwrap().items(db)[0],
        ModuleItemId::Submodule
    );
    assert_eq!(item_id.name(db).long(db), "mysubmodule");

    let submodule_id = ModuleId::Submodule(item_id);
    assert_eq!(
        db.module_main_file(module_id).unwrap().long(db),
        &FileLongId::OnDisk("src/lib.cairo".into())
    );
    assert_eq!(
        db.module_main_file(submodule_id).unwrap().long(db),
        &FileLongId::OnDisk("src/mysubmodule.cairo".into())
    );
}

macro_rules! set_file_content {
    ($db:expr, $path:expr, $content:expr) => {
        let file_id = FileLongId::OnDisk($path.into()).intern($db);
        override_file_content!($db, file_id, Some($content.into()));
    };
}

#[test]
fn test_submodules() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = get_crate_id(db);
    let root = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content!(db, "src/lib.cairo", "mod submod;");
    set_file_content!(db, "src/submod.cairo", "mod subsubmod;");
    set_file_content!(db, "src/submod/subsubmod.cairo", "fn foo() {}");

    let crate_id = get_crate_id(db);
    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);
    let submodule_id =
        ModuleId::Submodule(*db.module_submodules_ids(module_id).unwrap().first().unwrap());
    let subsubmodule_id =
        ModuleId::Submodule(*db.module_submodules_ids(submodule_id).unwrap().first().unwrap());

    let db_ref: &dyn Database = &*db;
    assert_eq!(
        format!("{:?}", subsubmodule_id.module_data(db).unwrap().items(db).debug(db_ref)),
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
struct DummyPlugin;
impl MacroPlugin for DummyPlugin {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        item_ast: ast::ModuleItem<'db>,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult<'db> {
        match item_ast {
            ast::ModuleItem::Struct(struct_ast) => {
                let remove_original_item = struct_ast.has_attr(db, "remove_original");
                PluginResult {
                    code: Some(PluginGeneratedFile {
                        name: "virt".into(),
                        content: format!("fn f(x:{}){{}}", struct_ast.name(db).text(db).long(db)),
                        code_mappings: Default::default(),
                        aux_data: None,
                        diagnostics_note: Default::default(),
                        is_unhygienic: false,
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
                    is_unhygienic: false,
                }),
                diagnostics: vec![PluginDiagnostic::error(
                    free_function_ast.stable_ptr(db),
                    "bla".into(),
                )],
                remove_original_item: false,
            },
            _ => PluginResult::default(),
        }
    }

    fn declared_attributes<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        vec![SmolStrId::from(db, "remove_original")]
    }
}

#[test]
fn test_plugin() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = get_crate_id(db);
    let root = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content!(db, "src/lib.cairo", "struct A{}");
    let crate_id = get_crate_id(db);
    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original struct still exists.
    // 2. The expected items were generated.
    let db_ref: &dyn Database = &*db;
    assert_eq!(
        format!("{:?}", module_id.module_data(db).unwrap().items(db).debug(db_ref)),
        "[StructId(test::A), FreeFunctionId(test::f), ExternTypeId(test::B)]"
    );
}

#[test]
fn test_plugin_remove_original() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = get_crate_id(db);
    let root = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content!(db, "src/lib.cairo", "#[remove_original] struct A{}");
    let crate_id = get_crate_id(db);

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original struct was removed.
    // 2. The expected items were generated.
    let db_ref: &dyn Database = &*db;
    assert_eq!(
        format!("{:?}", module_id.module_data(db).unwrap().items(db).debug(db_ref)),
        "[FreeFunctionId(test::f), ExternTypeId(test::B)]"
    );
}

/// If the original item is a function that is marked with #[remove_orig], only removes it, without
/// generating any new code.
#[derive(Debug)]
struct RemoveOrigPlugin;
impl MacroPlugin for RemoveOrigPlugin {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        item_ast: ast::ModuleItem<'db>,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult<'db> {
        let Some(free_function_ast) = try_extract_matches!(item_ast, ast::ModuleItem::FreeFunction)
        else {
            return PluginResult::default();
        };
        if !free_function_ast.has_attr(db, "remove_orig") {
            return PluginResult::default();
        }
        PluginResult { code: None, diagnostics: vec![], remove_original_item: true }
    }

    fn declared_attributes<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        vec![SmolStrId::from(db, "remove_orig")]
    }
}

/// Changes a function 'foo' to 'bar' if annotated with #[foo_to_bar]. Doesn't remove the original
/// item.
#[derive(Debug)]
struct FooToBarPlugin;
impl MacroPlugin for FooToBarPlugin {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        item_ast: ast::ModuleItem<'db>,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult<'db> {
        let Some(free_function_ast) = try_extract_matches!(item_ast, ast::ModuleItem::FreeFunction)
        else {
            return PluginResult::default();
        };
        if free_function_ast.declaration(db).name(db).text(db).long(db) != "foo" {
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
                is_unhygienic: false,
            }),
            diagnostics: vec![],
            remove_original_item: false,
        }
    }

    fn declared_attributes<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        vec![SmolStrId::from(db, "foo_to_bar")]
    }
}

#[test]
fn test_foo_to_bar() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = get_crate_id(db);
    let root = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content!(db, "src/lib.cairo", "#[foo_to_bar] fn foo() {}");

    let crate_id = get_crate_id(db);
    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original function remained.
    // 2. The expected items were generated.
    let db_ref: &dyn Database = &*db;
    assert_eq!(
        format!("{:?}", module_id.module_data(db).unwrap().items(db).debug(db_ref)),
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
    let crate_id = get_crate_id(db);
    let root = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content!(db, "src/lib.cairo", "#[remove_orig] fn foo() {}");

    let crate_id = get_crate_id(db);
    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original function was removed.
    // 2. No 'B' was generated by DummyPlugin.
    // Note RemoveOrigPlugin is before DummyPlugin in the plugins order. RemoveOrigPlugin already
    // acted on 'foo', so DummyPlugin shouldn't.
    let db_ref: &dyn Database = &*db;
    assert_eq!(format!("{:?}", module_id.module_data(db).unwrap().items(db).debug(db_ref)), "[]");
}

// Verify that if the first plugin generates new code, the later plugins don't act on the
// original item.
#[test]
fn test_first_plugin_generates() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = get_crate_id(db);
    let root = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content!(db, "src/lib.cairo", "#[foo_to_bar] #[remove_orig] fn foo() {}");

    let crate_id = get_crate_id(db);
    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. 'bar' was generated by FooToBarPlugin.
    // 2. the original function was removed.
    let db_ref: &dyn Database = &*db;
    assert_eq!(
        format!("{:?}", module_id.module_data(db).unwrap().items(db).debug(db_ref)),
        "[FreeFunctionId(test::bar), ExternTypeId(test::B)]"
    );
}

// Verify that the later plugins do act on items generated by earlier plugins.
#[test]
fn test_plugin_chain() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = get_crate_id(db);
    let root = Directory::Real("src".into());
    set_crate_config!(db, crate_id, Some(CrateConfiguration::default_for_root(root)));

    // Main module file.
    set_file_content!(db, "src/lib.cairo", "#[foo_to_bar] fn foo() {}");

    let crate_id = get_crate_id(db);
    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);

    // Verify that:
    // 1. The original function remained.
    // 2. 'bar' was generated by FooToBarPlugin.
    // 3. 'B' were generated by DummyPlugin for foo and bar.
    let db_ref: &dyn Database = &*db;
    assert_eq!(
        format!("{:?}", module_id.module_data(db).unwrap().items(db).debug(db_ref)),
        "[FreeFunctionId(test::foo), FreeFunctionId(test::bar), ExternTypeId(test::B), \
         ExternTypeId(test::B)]"
    )
}
