use crate::db::FilesDatabase;

// Test salsa database.
#[salsa::database(FilesDatabase)]
#[derive(Default)]
pub struct FilesDatabaseForTesting {
    storage: salsa::Storage<FilesDatabaseForTesting>,
}
impl salsa::Database for FilesDatabaseForTesting {}
