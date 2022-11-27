use std::sync::Arc;

use db_utils::Upcast;
use defs::db::{init_defs_group, DefsDatabase, DefsGroup, MacroPlugin};
use defs::ids::ModuleId;
use filesystem::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateLongId, Directory, FileLongId};
use indoc::indoc;
use parser::db::ParserDatabase;
use pretty_assertions::assert_eq;
use syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use syntax::node::TypedSyntaxNode;
use test_case::test_case;

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

#[test_case(
    vec![Arc::new(DerivePlugin{})],
    indoc! {"
        #[derive(Copy, Drop)]
        struct A{}

        #[derive(Copy, Drop)]
        struct B{}
    "},
    "impls",
    indoc! {"
        impl ACopy of Copy::<super::A>;
        impl ADrop of Drop::<super::A>;

        impl BCopy of Copy::<super::B>;
        impl BDrop of Drop::<super::B>;
    "};
    "derive"
)]
#[test_case(
    vec![Arc::new(PanicablePlugin{})],
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
    "panicable",
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
    "};
    "panicable"
)]
fn plugin_test(
    plugins: Vec<Arc<dyn MacroPlugin>>,
    content: &str,
    expected_submodule: &str,
    expected_submodule_content: &str,
) {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));
    db.set_macro_plugins(plugins);

    // Main module file.
    set_file_content(db, "src/lib.cairo", content);
    let module_id = ModuleId::CrateRoot(crate_id);
    let submodule_id = db
        .module_submodules(module_id)
        .unwrap()
        .into_iter()
        .find(|submodule_id| {
            if let ModuleId::VirtualSubmodule(id) = submodule_id {
                db.lookup_intern_virtual_submodule(*id).name == expected_submodule
            } else {
                false
            }
        })
        .expect("Didn't find expected submodule.");

    assert_eq!(
        db.module_syntax(submodule_id).unwrap().as_syntax_node().get_text(db),
        expected_submodule_content
    );
}
