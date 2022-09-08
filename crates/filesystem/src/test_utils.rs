use crate::db::{init_files_group, FilesDatabase};

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
