use cairo_lang_utils::Upcast;

#[salsa::db]
#[derive(Default, Clone)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
#[salsa::db]
impl salsa::Database for DatabaseForTesting {}

impl<'a> Upcast<'a, dyn salsa::Database> for DatabaseForTesting {
    fn upcast(&'a self) -> &'a dyn salsa::Database {
        self
    }
}
