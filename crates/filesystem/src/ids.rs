use std::path::PathBuf;
use std::sync::Arc;

use db_utils::define_short_id;
use smol_str::SmolStr;

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
