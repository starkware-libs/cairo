use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_test_utils::test;
use cairo_lang_utils::Upcast;
use salsa::{Database, Storage};

use crate::debug::DebugWithDb;

// Test database query group.
trait TestGroup: salsa::Database {}

impl TestGroup for DummyDb {}

// Structs.
#[salsa::interned(revisions = usize::MAX)]
struct Dummy {
    id: usize,
}

#[salsa::db]
#[derive(Clone, Default)]
struct DummyDb {
    storage: Storage<Self>,
}

#[salsa::db]
impl Database for DummyDb {}

impl<'db> DebugWithDb<'db> for Dummy<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        write!(f, "Dummy({})", self.id(db))
    }
}

impl<'db> Upcast<'db, dyn TestGroup> for DummyDb {
    fn upcast(&'db self) -> &'db dyn TestGroup {
        self
    }
}

impl<'db> Upcast<'db, ()> for dyn TestGroup {
    fn upcast(&'db self) -> &'db () {
        &()
    }
}

impl<'db> Upcast<'db, dyn Database> for dyn TestGroup {
    fn upcast(&'db self) -> &'db dyn Database {
        self
    }
}

#[derive(DebugWithDb)]
#[debug_db(dyn TestGroup)]
struct ComplexStruct<'db> {
    a: Option<usize>,
    b: Dummy<'db>,
    #[hide_field_debug_with_db]
    c: usize,
    #[hide_field_debug_with_db]
    d: usize,
}

#[test]
fn test_debug() {
    let db = DummyDb::default();
    let db_ref: &dyn TestGroup = &db;
    let a = ComplexStruct { a: Some(5), b: Dummy::new(db_ref, 6), c: 7, d: 8 };
    assert_eq!(format!("{:?}", a.debug(db_ref)), "ComplexStruct { a: Some(5), b: Dummy(6) }");
}
