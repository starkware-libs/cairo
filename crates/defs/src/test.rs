use debug::debug::DebugWithDb;
use filesystem::db::{init_files_group, AsFilesGroup, FilesDatabase, FilesGroup};
use indoc::indoc;
use parser::db::ParserDatabase;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use crate::db::{DefsDatabase, DefsGroup};
use crate::test_utils::setup_test_module;

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

#[test]
fn test_resolve() {
    let mut db_val = DatabaseForTesting::default();
    let (module_id, diagnostics) = setup_test_module(
        &mut db_val,
        indoc! {"
            func foo() -> felt { 5 }
            extern func felt_add(a: felt, b: felt) -> felt;
        "},
    );
    assert_eq!(diagnostics, "");
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

// TODO(spapini): Test .cairo suffix on submodules.
