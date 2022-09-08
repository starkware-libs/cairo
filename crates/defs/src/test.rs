use std::collections::HashMap;
use std::sync::Arc;

use debug::debug::DebugWithDb;
use filesystem::db::{init_files_group, AsFilesGroup, FilesDatabase, FilesGroup, ProjectConfig};
use filesystem::ids::{CrateLongId, FileLongId};
use indoc::indoc;
use parser::db::ParserDatabase;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use crate::db::{DefsDatabase, DefsGroup};
use crate::ids::ModuleId;

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
}
impl AsSyntaxGroup for DatabaseForTesting {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
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
    assert!(
        db.module_item_by_name(module_id, "doesnt_exist".into())
            .expect("Unexpected error")
            .is_none()
    );
    let felt_add = db.module_item_by_name(module_id, "felt_add".into()).expect("Unexpected error");
    assert_eq!(format!("{:?}", felt_add.debug(db)), "Some(ExternFunctionId(test_crate::felt_add))");
    match db.module_item_by_name(module_id, "felt_add".into()).expect("Unexpected error").unwrap() {
        crate::ids::ModuleItemId::ExternFunction(_) => {}
        _ => panic!("Expected an extern function"),
    };
    match db.module_item_by_name(module_id, "foo".into()).expect("Unexpected error").unwrap() {
        crate::ids::ModuleItemId::FreeFunction(_) => {}
        _ => panic!("Expected a free function"),
    };
}

fn setup_test_module(db: &mut DatabaseForTesting, content: &str) -> ModuleId {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let file_id = db.intern_file(FileLongId::OnDisk("test.cairo".into()));
    db.set_file_overrides(Arc::new(HashMap::from_iter([(file_id, Arc::new(content.to_string()))])));
    db.set_project_config(ProjectConfig {
        crate_roots: [(crate_id, file_id)].into_iter().collect(),
    });
    ModuleId::CrateRoot(crate_id)
}
