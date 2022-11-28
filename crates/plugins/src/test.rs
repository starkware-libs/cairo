use std::sync::Arc;

use db_utils::Upcast;
use defs::db::{init_defs_group, DefsDatabase, DefsGroup, MacroPlugin};
use defs::ids::ModuleId;
use filesystem::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateLongId, Directory, FileLongId};
use indoc::indoc;
use itertools::zip_eq;
use parser::db::ParserDatabase;
use pretty_assertions::assert_eq;
use syntax::node::db::{SyntaxDatabase, SyntaxGroup};
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
    &[
        indoc! {"
            impl ACopy of Copy::<A>;
            impl ADrop of Drop::<A>;
        "},
        indoc! {"
            impl BCopy of Copy::<B>;
            impl BDrop of Drop::<B>;
        "},
    ];
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
    &[
        indoc! {"
            func foo_panicable(a: felt, b: other) -> () {
                match foo(a, b) {
                    Option::Some (v) => {
                        v
                    },
                    Option::None (v) => {
                        let data = array_new::<felt>();
                        array_append::<felt>(data, 1);
                        panic(data);
                        foo_panicable(a, b)
                    },
                }
            }
        "},
        indoc! {"
            func bar_panicable() -> felt {
                match bar() {
                    Option::Some (v) => {
                        v
                    },
                    Option::None (v) => {
                        let data = array_new::<felt>();
                        array_append::<felt>(data, 2);
                        panic(data);
                        bar_panicable()
                    },
                }
            }
        "},
        indoc! {"
            func non_extern_panicable(_: some_type) -> (felt, other) {
                match non_extern(_) {
                    Option::Some (v) => {
                        v
                    },
                    Option::None (v) => {
                        let data = array_new::<felt>();
                        array_append::<felt>(data, 3);
                        panic(data);
                        non_extern_panicable(_)
                    },
                }
            }
        "},
    ];
    "panicable"
)]
fn plugin_test(plugins: Vec<Arc<dyn MacroPlugin>>, content: &str, expected_codes: &[&str]) {
    let mut db_val = DatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root));
    db.set_macro_plugins(plugins);

    // Main module file.
    set_file_content(db, "src/lib.cairo", content);
    let module_id = ModuleId::CrateRoot(crate_id);
    for (file, expected_code) in
        zip_eq(db.module_files(module_id).unwrap().into_iter().skip(1), expected_codes)
    {
        assert_eq!(db.file_content(file).unwrap().as_str(), *expected_code);
    }
}
