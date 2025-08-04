use cairo_lang_utils::Upcast;

use crate::db::{ExternalFiles, FilesGroup, init_files_group};

// Test salsa database.
#[salsa::db]
#[derive(Clone)]
pub struct FilesDatabaseForTesting {
    storage: salsa::Storage<FilesDatabaseForTesting>,
}

#[salsa::db]
impl salsa::Database for FilesDatabaseForTesting {}

impl ExternalFiles for FilesDatabaseForTesting {}
impl Default for FilesDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}
impl<'db> Upcast<'db, dyn FilesGroup> for FilesDatabaseForTesting {
    fn upcast(&'db self) -> &'db dyn FilesGroup {
        self
    }
}
