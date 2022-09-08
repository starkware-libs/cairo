use crate::db::FilesDatabase;

// Test salsa database.
#[salsa::database(FilesDatabase)]
#[derive(Default)]
pub struct FilesDatabaseImpl {
    storage: salsa::Storage<FilesDatabaseImpl>,
}
impl salsa::Database for FilesDatabaseImpl {}
