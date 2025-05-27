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

impl Upcast<dyn FilesGroup> for DatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup) {
        self
    }
}
