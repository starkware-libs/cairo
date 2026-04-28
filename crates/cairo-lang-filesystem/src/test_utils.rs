use crate::db::{FileContentStorage, FileContentView, init_files_group, register_files_group_view};

// Test salsa database.
#[salsa::db]
#[derive(Clone)]
pub struct FilesDatabaseForTesting {
    storage: salsa::Storage<FilesDatabaseForTesting>,
    file_contents: FileContentStorage,
}

#[salsa::db]
impl salsa::Database for FilesDatabaseForTesting {}
impl FileContentView for FilesDatabaseForTesting {
    fn file_content_storage(&self) -> &FileContentStorage {
        &self.file_contents
    }
}

impl Default for FilesDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default(), file_contents: Default::default() };
        register_files_group_view(&res);
        init_files_group(&mut res);
        res
    }
}
