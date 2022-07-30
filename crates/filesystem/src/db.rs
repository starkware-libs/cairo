#[cfg(test)]
#[path = "db_test.rs"]
mod db_test;

use std::collections::HashMap;
use std::fs;
use std::sync::Arc;

use crate::ids::{CrateId, CrateLongId, FileId, FileLongId};

// Salsa database interface.
#[salsa::query_group(FilesDatabase)]
pub trait FilesGroup {
    #[salsa::interned]
    fn intern_crate(&self, crt: CrateLongId) -> CrateId;
    #[salsa::interned]
    fn intern_file(&self, file: FileLongId) -> FileId;

    #[salsa::input]
    fn project_config(&self) -> ProjectConfig;
    fn crates(&self) -> Vec<CrateId>;
    fn crate_root_file(&self, crate_id: CrateId) -> Option<FileId>;
    fn file_content(&self, file_id: FileId) -> Option<Arc<String>>;
}

// Configuration of the project. This is the only input, and it defines everything else.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProjectConfig {
    pub crate_roots: HashMap<CrateId, FileId>,
}

fn crates(db: &dyn FilesGroup) -> Vec<CrateId> {
    db.project_config().crate_roots.keys().copied().collect()
}
fn crate_root_file(db: &dyn FilesGroup, crt: CrateId) -> Option<FileId> {
    db.project_config().crate_roots.get(&crt).copied()
}

fn file_content(db: &dyn FilesGroup, file: FileId) -> Option<Arc<String>> {
    match db.lookup_intern_file(file) {
        // TODO(spapini): If ide file watching is not enough, do it here.
        FileLongId::OnDisk(path) => match fs::read_to_string(path) {
            Ok(content) => Some(Arc::new(content)),
            Err(_) => None,
        },
        FileLongId::Virtual(virt) => Some(virt.content),
    }
}
