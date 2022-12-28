use db_utils::Upcast;

use crate::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup};

// Test salsa database.
#[salsa::database(FilesDatabase)]
pub struct FilesDatabaseForTesting {
    storage: salsa::Storage<FilesDatabaseForTesting>,
}
impl salsa::Database for FilesDatabaseForTesting {}
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
impl AsFilesGroupMut for FilesDatabaseForTesting {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
