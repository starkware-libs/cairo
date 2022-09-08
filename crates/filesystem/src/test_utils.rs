use crate::db::{init_files_group, AsFilesGroup, FilesDatabase, FilesGroup};

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
impl AsFilesGroup for FilesDatabaseForTesting {
    fn as_files_group(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
