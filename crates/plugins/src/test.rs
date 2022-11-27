use std::sync::Arc;

use db_utils::Upcast;
use debug::DebugWithDb;
use defs::db::{init_defs_group, DefsDatabase, DefsGroup};
use defs::ids::ModuleId;
use filesystem::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateLongId, Directory, FileLongId};
use indoc::indoc;
// use indoc::indoc;
use parser::db::ParserDatabase;
use parser::utils::get_node_text;
use pretty_assertions::assert_eq;
use syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use syntax::node::TypedSyntaxNode;

use crate::derive::DerivePlugin;
use crate::panicable::PanicablePlugin;

#[salsa::database(DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
impl salsa::Database for DatabaseForTesting {}
impl Default for DatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_defs_group(&mut res);
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

fn set_file_content(db: &mut DatabaseForTesting, path: &str, content: &str) {
    let file_id = db.intern_file(FileLongId::OnDisk(path.into()));
    db.as_files_group_mut().override_file_content(file_id, Some(Arc::new(content.into())));
}

#[test]
fn test_derive_plugin() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));
    db.set_macro_plugins(vec![Arc::new(DerivePlugin {})]);

    // Main module file.
    set_file_content(
        db,
        "src/lib.cairo",
        r#"
            #[derive(Copy, Drop)]
            struct A{}
            #[derive(Copy, Drop)]
            struct B{}
        "#,
    );

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);
    let submodule_id = db.module_submodules(module_id).unwrap().pop().unwrap();

    assert_eq!(
        format!("{:?}", db.module_item_by_name(submodule_id, "ACopy".into()).debug(db)),
        "Some(ImplId(test_crate::impls::ACopy))"
    );

    assert_eq!(
        format!("{:?}", db.module_item_by_name(submodule_id, "ADrop".into()).debug(db)),
        "Some(ImplId(test_crate::impls::ADrop))"
    );

    assert_eq!(
        format!("{:?}", db.module_item_by_name(submodule_id, "BCopy".into()).debug(db)),
        "Some(ImplId(test_crate::impls::BCopy))"
    );

    assert_eq!(
        format!("{:?}", db.module_item_by_name(submodule_id, "BDrop".into()).debug(db)),
        "Some(ImplId(test_crate::impls::BDrop))"
    );
}

#[test]
fn test_panicable_plugin() {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));
    db.set_macro_plugins(vec![Arc::new(PanicablePlugin {})]);

    // Main module file.
    set_file_content(
        db,
        "src/lib.cairo",
        indoc! {"
            #[panic_with(1)]
            extern func foo(a: felt, b: other) -> Option::<()> implicits (rc: RangeCheck, gb: GasBuiltin) nopanic;

            #[panic_with(2)]
            extern func bar() -> Option::<felt> nopanic;

            #[panic_with(3)]
            func non_extern(_: some_type) -> Option::<(felt, other)> nopanic {
                (4, 56)
            }
        "},
    );

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);
    let submodule_id = db.module_submodules(module_id).unwrap().pop().unwrap();
    let submodule_syntax = db.module_syntax(submodule_id).unwrap();
    assert_eq!(
        get_node_text(db, &submodule_syntax.as_syntax_node()),
        indoc! {"
        func foo(a: felt, b: other) -> () {
            match super::foo(a, b) {
                Option::Some (v) => {
                    v
                },
                Option::None (v) => {
                    let data = array_new::<felt>();
                    array_append::<felt>(data, 1);
                    panic(data);
                },
            }
        }

        func bar() -> felt {
            match super::bar() {
                Option::Some (v) => {
                    v
                },
                Option::None (v) => {
                    let data = array_new::<felt>();
                    array_append::<felt>(data, 2);
                    panic(data);
                },
            }
        }

        func non_extern(_: some_type) -> (felt, other) {
            match super::non_extern(_) {
                Option::Some (v) => {
                    v
                },
                Option::None (v) => {
                    let data = array_new::<felt>();
                    array_append::<felt>(data, 3);
                    panic(data);
                },
            }
        }
    "}
    );
}
