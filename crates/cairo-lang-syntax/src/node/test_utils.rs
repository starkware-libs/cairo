use cairo_lang_filesystem::db::{FilesDatabase, FilesGroup};
use cairo_lang_utils::Upcast;

use super::db::SyntaxDatabase;

#[salsa::database(SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
impl salsa::Database for DatabaseForTesting {}
impl Upcast<dyn FilesGroup> for DatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
