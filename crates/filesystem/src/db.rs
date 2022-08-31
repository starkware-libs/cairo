#[cfg(test)]
#[path = "db_test.rs"]
mod test;

use std::collections::HashMap;
use std::fs;
use std::sync::Arc;

use crate::ids::{
    CrateId, CrateLongId, FileId, FileLongId, ModuleId, SubmoduleId, SubmoduleLongId,
};
use crate::span::{FileSummary, TextOffset};

// Salsa database interface.
#[salsa::query_group(FilesDatabase)]
pub trait FilesGroup {
    #[salsa::interned]
    fn intern_crate(&self, crt: CrateLongId) -> CrateId;
    #[salsa::interned]
    fn intern_file(&self, file: FileLongId) -> FileId;
    #[salsa::interned]
    fn intern_submodule(&self, id: SubmoduleLongId) -> SubmoduleId;

    #[salsa::input]
    fn project_config(&self) -> ProjectConfig;
    fn crates(&self) -> Vec<CrateId>;
    fn crate_root_file(&self, crate_id: CrateId) -> Option<FileId>;
    fn file_content(&self, file_id: FileId) -> Option<Arc<String>>;
    fn file_summary(&self, file_id: FileId) -> Option<Arc<FileSummary>>;
    fn module_file(&self, module_id: ModuleId) -> Option<FileId>;
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

// This query is treated as a pure computation. If the file content is changed, an explicit
// invalidation should be called on the DB by another entity.
// For example, in the language server, this is initiated by the language server itself.
fn file_content(db: &dyn FilesGroup, file: FileId) -> Option<Arc<String>> {
    match db.lookup_intern_file(file) {
        FileLongId::OnDisk(path) => match fs::read_to_string(path) {
            Ok(content) => Some(Arc::new(content)),
            Err(_) => None,
        },
        FileLongId::Virtual(virt) => Some(virt.content),
    }
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

fn module_file(db: &dyn FilesGroup, module_id: ModuleId) -> Option<FileId> {
    match module_id {
        ModuleId::CrateRoot(crate_id) => db.crate_root_file(crate_id),
        ModuleId::Submodule(submodule_id) => {
            let _submodule_long_id = db.lookup_intern_submodule(submodule_id);
            // TODO(yuval): support submodules.
            todo!()
        }
    }
}
