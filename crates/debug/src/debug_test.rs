use std::fmt::Debug;

use db_utils::{define_short_id, Upcast};
use diagnostics_proc_macros::DebugWithDb;
use test_log::test;

use crate::debug;
use crate::debug::DebugWithDb;

// Test database query group.
#[salsa::query_group(TestDatabase)]
trait TestGroup {
    #[salsa::interned]
    fn intern_b(&self, crt: DummyLongId) -> DummyShortId;
}
// Database impl.
#[salsa::database(TestDatabase)]
#[derive(Default)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
impl salsa::Database for DatabaseForTesting {}

impl Upcast<dyn TestGroup> for DatabaseForTesting {
    fn upcast(&self) -> &(dyn TestGroup + 'static) {
        self
    }
}

// Structs.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct DummyLongId(usize);

define_short_id!(DummyShortId, DummyLongId, TestGroup, lookup_intern_b);

#[derive(DebugWithDb)]
#[debug_db(dyn TestGroup + 'static)]
struct ComplexStruct {
    a: Option<usize>,
    b: DummyShortId,
    #[hide_field_debug_with_db]
    c: usize,
    #[hide_field_debug_with_db]
    d: usize,
}

#[test]
fn test_debug() {
    let db = DatabaseForTesting::default();
    let a = ComplexStruct { a: Some(5), b: db.intern_b(DummyLongId(6)), c: 7, d: 8 };
    assert_eq!(format!("{:?}", a.debug(&db)), "ComplexStruct { a: Some(5), b: DummyLongId(6) }");
}
