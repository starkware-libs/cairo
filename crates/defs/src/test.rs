use std::sync::Arc;

use debug::debug::DebugWithDb;
use filesystem::db::{init_files_group, AsFilesGroup, FilesDatabase, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateLongId, Directory, FileLongId};
use indoc::indoc;
use parser::db::ParserDatabase;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};
use utils::extract_matches;

use crate::db::{DefsDatabase, DefsGroup};
use crate::ids::{ModuleId, ModuleItemId};

#[salsa::database(DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
impl salsa::Database for DatabaseForTesting {}
impl Default for DatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}
impl AsFilesGroup for DatabaseForTesting {
    fn as_files_group(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl AsSyntaxGroup for DatabaseForTesting {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}

pub fn setup_test_module<T: DefsGroup + ?Sized>(db: &mut T, content: &str) -> ModuleId {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let directory = Directory("src".into());
    db.set_crate_root(crate_id, Some(directory));
    let file = db.module_file(ModuleId::CrateRoot(crate_id)).unwrap();
    db.as_files_group_mut().override_file_content(file, Some(Arc::new(content.to_string())));
    let syntax_diagnostics = db.file_syntax_diagnostics(file).format(db.as_files_group());
    assert_eq!(syntax_diagnostics, "");
    ModuleId::CrateRoot(crate_id)
}

#[test]
fn test_resolve() {
    let mut db_val = DatabaseForTesting::default();
    let module_id = setup_test_module(
        &mut db_val,
        indoc! {"
            func foo() -> felt { 5 }
            extern func felt_add(a: felt, b: felt) -> felt;
        "},
    );
    let db = &db_val;
    assert!(db.module_item_by_name(module_id, "doesnt_exist".into()).is_none());
    let felt_add = db.module_item_by_name(module_id, "felt_add".into());
    assert_eq!(format!("{:?}", felt_add.debug(db)), "Some(ExternFunctionId(test_crate::felt_add))");
    match db.module_item_by_name(module_id, "felt_add".into()).unwrap() {
        crate::ids::ModuleItemId::ExternFunction(_) => {}
        _ => panic!("Expected an extern function"),
    };
    match db.module_item_by_name(module_id, "foo".into()).unwrap() {
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
        db.module_item_by_name(module_id, "mysubmodule".into()).unwrap(),
        ModuleItemId::Submodule
    );
    let submodule_id = ModuleId::Submodule(item_id);
    assert_eq!(
        db.lookup_intern_file(db.module_file(module_id).unwrap()),
        FileLongId::OnDisk("src/lib.cairo".into())
    );
    assert_eq!(
        db.lookup_intern_file(db.module_file(submodule_id).unwrap()),
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

    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "mod submod;");
    set_file_content(db, "src/submod.cairo", "mod subsubmod;");
    set_file_content(db, "src/submod/subsubmod.cairo", "func foo() {}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);
    let submodule_id = ModuleId::Submodule(
        *db.module_data(module_id).unwrap().submodules.iter().next().unwrap().0,
    );
    let subsubmodule_id = ModuleId::Submodule(
        *db.module_data(submodule_id).unwrap().submodules.iter().next().unwrap().0,
    );

    db.module_item_by_name(subsubmodule_id, "foo".into())
        .expect("Expected to find foo() in subsubmodule.");
}
