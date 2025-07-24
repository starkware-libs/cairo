use cairo_lang_filesystem::db::{ExternalFiles, FilesGroup};
use cairo_lang_utils::Upcast;

#[salsa::db]
#[derive(Default, Clone)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
#[salsa::db]
impl salsa::Database for DatabaseForTesting {}

impl ExternalFiles for DatabaseForTesting {}

impl<'a> Upcast<'a, dyn FilesGroup> for DatabaseForTesting {
    fn upcast(&'a self) -> &'a dyn FilesGroup {
        self
    }
}
