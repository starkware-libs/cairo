use cairo_lang_utils::Upcast;

use crate::db::{ExternalFiles, FilesDatabase, FilesGroup, init_files_group};

// Test salsa database.
#[salsa::database(FilesDatabase)]
pub struct FilesDatabaseForTesting {
    storage: salsa::Storage<FilesDatabaseForTesting>,
}
impl salsa::Database for FilesDatabaseForTesting {}
impl ExternalFiles for FilesDatabaseForTesting {}
impl Default for FilesDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}
impl Upcast<dyn FilesGroup> for FilesDatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
