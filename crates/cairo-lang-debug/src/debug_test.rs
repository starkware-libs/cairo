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

impl<T: Database + ?Sized> DebugWithDb<T> for Dummy<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &T) -> std::fmt::Result {
        write!(f, "Dummy({})", self.id(db))
    }
}

impl Upcast<dyn TestGroup> for DummyDb {
    fn upcast(&self) -> &(dyn TestGroup + 'static) {
        self
    }
}

#[derive(DebugWithDb)]
#[debug_db(dyn TestGroup + 'static)]
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
    let a = ComplexStruct { a: Some(5), b: Dummy::new(&db, 6), c: 7, d: 8 };
    assert_eq!(format!("{:?}", a.debug(&db)), "ComplexStruct { a: Some(5), b: Dummy(6) }");
}
