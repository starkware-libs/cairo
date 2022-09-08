#[cfg(test)]
#[path = "db_test.rs"]
mod test;

use std::collections::HashMap;
use std::fs;
use std::sync::Arc;

use crate::ids::{CrateId, CrateLongId, FileId, FileLongId};
use crate::span::{FileSummary, TextOffset};

// Salsa database interface.
#[salsa::query_group(FilesDatabase)]
pub trait FilesGroup {
    #[salsa::interned]
    fn intern_crate(&self, crt: CrateLongId) -> CrateId;
    #[salsa::interned]
    fn intern_file(&self, file: FileLongId) -> FileId;

    /// Main input of the project. Lists all the crates.
    #[salsa::input]
    fn project_config(&self) -> ProjectConfig;
    /// Overrides for file content. Mostly used by language server and tests.
    #[salsa::input]
    fn file_overrides(&self) -> Arc<HashMap<FileId, Arc<String>>>;

    /// List of crates in the project.
    fn crates(&self) -> Vec<CrateId>;
    /// Root file of the crate.
    fn crate_root_file(&self, crate_id: CrateId) -> Option<FileId>;
    /// Query for raw file contents. Private.
    fn priv_raw_file_content(&self, file_id: FileId) -> Option<Arc<String>>;
    /// Query for the file contents. This takes overrides into consideration.
    fn file_content(&self, file_id: FileId) -> Option<Arc<String>>;
    fn file_summary(&self, file_id: FileId) -> Option<Arc<FileSummary>>;
}

pub fn init_files_group(db: &mut dyn FilesGroup) {
    db.set_file_overrides(Arc::new(HashMap::new()));
}

pub trait AsFilesGroup {
    fn as_files_group(&self) -> &(dyn FilesGroup + 'static);
}

impl AsFilesGroup for dyn FilesGroup {
    fn as_files_group(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}

// Configuration of the project. This is the only input, and it defines everything else.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ProjectConfig {
    pub crate_roots: HashMap<CrateId, FileId>,
}
impl ProjectConfig {
    pub fn with_crate(mut self, crt: CrateId, root: FileId) -> Self {
        self.crate_roots.insert(crt, root);
        self
    }
}

fn crates(db: &dyn FilesGroup) -> Vec<CrateId> {
    db.project_config().crate_roots.keys().copied().collect()
}
fn crate_root_file(db: &dyn FilesGroup, crt: CrateId) -> Option<FileId> {
    db.project_config().crate_roots.get(&crt).copied()
}

fn priv_raw_file_content(db: &dyn FilesGroup, file: FileId) -> Option<Arc<String>> {
    match db.lookup_intern_file(file) {
        FileLongId::OnDisk(path) => match fs::read_to_string(path) {
            Ok(content) => Some(Arc::new(content)),
            Err(_) => None,
        },
        FileLongId::Virtual(virt) => Some(virt.content),
    }
}
fn file_content(db: &dyn FilesGroup, file: FileId) -> Option<Arc<String>> {
    let overrides = db.file_overrides();
    overrides.get(&file).cloned().or_else(|| db.priv_raw_file_content(file))
}
fn file_summary(db: &dyn FilesGroup, file: FileId) -> Option<Arc<FileSummary>> {
    let content = db.file_content(file)?;
    let mut line_offsets = vec![TextOffset(0)];
    let mut offset = 0;
    for ch in content.chars() {
        offset += 1;
        if ch == '\n' {
            line_offsets.push(TextOffset(offset));
        }
    }
    Some(Arc::new(FileSummary { line_offsets, total_length: offset }))
}
