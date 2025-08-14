use crate::db::init_files_group;

// Test salsa database.
#[salsa::db]
#[derive(Clone)]
pub struct FilesDatabaseForTesting {
    storage: salsa::Storage<FilesDatabaseForTesting>,
}

#[salsa::db]
impl salsa::Database for FilesDatabaseForTesting {}

impl Default for FilesDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}
