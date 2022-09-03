use std::sync::Arc;

use filesystem::db::{FilesDatabase, FilesGroup, ProjectConfig};
use filesystem::ids::{CrateLongId, FileLongId, ModuleId, VirtualFile};
use indoc::indoc;
use parser::db::ParserDatabase;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use crate::db::{DefsDatabase, DefsGroup};

#[salsa::database(DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

impl AsSyntaxGroup for DatabaseImpl {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}

#[test]
fn test_resolve() {
    let mut db_val = DatabaseImpl::default();
    let module_id = setup_test_module(
        &mut db_val,
        indoc! {"
            func foo() -> felt { 5 }
            extern func felt_add(a: felt, b: felt) -> felt;
        "},
    );
    let db = &db_val;
    assert!(
        db.module_resolve_identifier(module_id, "doesnt_exist".into())
            .expect("Unexpected error")
            .is_none()
    );
    match db
        .module_resolve_identifier(module_id, "felt_add".into())
        .expect("Unexpected error")
        .unwrap()
    {
        crate::ids::ModuleItemId::ExternFunction(_) => {}
        _ => panic!("Expected an extern function"),
    };
    match db.module_resolve_identifier(module_id, "foo".into()).expect("Unexpected error").unwrap()
    {
        crate::ids::ModuleItemId::FreeFunction(_) => {}
        _ => panic!("Expected a free function"),
    };
}

fn setup_test_module(db: &mut DatabaseImpl, content: &str) -> ModuleId {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "test.cairo".into(),
        content: Arc::new(content.to_string()),
    }));
    db.set_project_config(ProjectConfig {
        crate_roots: [(crate_id, file_id)].into_iter().collect(),
    });
    ModuleId::CrateRoot(crate_id)
}
