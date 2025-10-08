#[salsa::db]
#[derive(Default, Clone)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
#[salsa::db]
impl salsa::Database for DatabaseForTesting {}
