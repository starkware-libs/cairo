use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_utils::Upcast;
use salsa::{Database, Storage};
use test_log::test;

use crate::debug::DebugWithDb;

// Test database query group.
#[cairo_lang_proc_macros::query_group]
trait TestGroup: salsa::Database {}

// Structs.
#[salsa::interned]
#[derive(Debug)]
struct Dummy {
    id: usize,
}

// impl<'db> Fallback<Dummy<'db>, dyn TestGroup> for Dummy<'db> {}

#[salsa::db]
#[derive(Clone, Default)]
struct DummyDb {
    storage: Storage<Self>,
}

#[salsa::db]
impl Database for DummyDb {}

impl<'db, T: Database + ?Sized> DebugWithDb<'db, T> for Dummy<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db T) -> std::fmt::Result {
        write!(f, "Dummy({})", self.id(db))
    }
}

impl Upcast<dyn TestGroup> for DummyDb {
    fn upcast(&self) -> &dyn TestGroup {
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
