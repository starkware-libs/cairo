use std::path::PathBuf;
use std::sync::Arc;

use db_utils::define_short_id;
use smol_str::SmolStr;

use crate::db::FilesGroup;

// A crate is a standalone file tree representing a single compilation unit.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CrateLongId(pub SmolStr);
define_short_id!(CrateId);

// We use a higher level FileId struct, because not all files are on disk. Some might be online.
// Some might be virtual/computed on demand.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FileLongId {
    OnDisk(PathBuf),
    Virtual(VirtualFile),
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VirtualFile {
    pub parent: Option<FileId>,
    pub name: SmolStr,
    pub content: Arc<String>,
}
define_short_id!(FileId);
impl FileId {
    pub fn file_name(self, db: &dyn FilesGroup) -> String {
        match db.lookup_intern_file(self) {
            FileLongId::OnDisk(path) => {
                path.file_name().and_then(|x| x.to_str()).unwrap_or("<unknown>").to_string()
            }
            FileLongId::Virtual(vf) => vf.name.to_string(),
        }
    }
}
