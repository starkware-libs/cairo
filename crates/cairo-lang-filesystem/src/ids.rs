use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_utils::define_short_id;
use path_clean::PathClean;
use smol_str::SmolStr;

use crate::db::FilesGroup;

pub const CAIRO_FILE_EXTENSION: &str = "cairo";

/// A crate is a standalone file tree representing a single compilation unit.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CrateLongId(pub SmolStr);
define_short_id!(CrateId, CrateLongId, FilesGroup, lookup_intern_crate);

/// A trait for getting the internal salsa::InternId of a short id object.
/// This id is unstable across runs and should not be used to anything that is externally visible.
/// This is currently used to pick representative for strongly connected components.
pub trait UnstableSalsaId {
    fn get_internal_id(&self) -> &salsa::InternId;
}
impl UnstableSalsaId for CrateId {
    fn get_internal_id(&self) -> &salsa::InternId {
        &self.0
    }
}

/// The long ID for a compilation flag.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FlagLongId(pub SmolStr);
define_short_id!(FlagId, FlagLongId, FilesGroup, lookup_intern_flag);
impl FlagId {
    pub fn new(db: &dyn FilesGroup, name: &str) -> Self {
        db.intern_flag(FlagLongId(name.into()))
    }
}

/// We use a higher level FileId struct, because not all files are on disk. Some might be online.
/// Some might be virtual/computed on demand.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FileLongId {
    OnDisk(PathBuf),
    Virtual(VirtualFile),
}
/// Whether the file holds syntax for a module or for an expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FileKind {
    Module,
    Expr,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VirtualFile {
    pub parent: Option<FileId>,
    pub name: SmolStr,
    pub content: Arc<String>,
    pub kind: FileKind,
}
define_short_id!(FileId, FileLongId, FilesGroup, lookup_intern_file);
impl FileId {
    pub fn new(db: &dyn FilesGroup, path: PathBuf) -> FileId {
        db.intern_file(FileLongId::OnDisk(path.clean()))
    }
    pub fn file_name(self, db: &dyn FilesGroup) -> String {
        match db.lookup_intern_file(self) {
            FileLongId::OnDisk(path) => {
                path.file_name().and_then(|x| x.to_str()).unwrap_or("<unknown>").to_string()
            }
            FileLongId::Virtual(vf) => vf.name.to_string(),
        }
    }
    pub fn kind(self, db: &dyn FilesGroup) -> FileKind {
        match db.lookup_intern_file(self) {
            FileLongId::OnDisk(_) => FileKind::Module,
            FileLongId::Virtual(vf) => vf.kind.clone(),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Directory(pub PathBuf);

impl Directory {
    /// Returns a file inside this directory. The file and directory don't necessarily exist on
    /// the file system. These are ids/paths to them.
    pub fn file(&self, db: &dyn FilesGroup, name: SmolStr) -> FileId {
        FileId::new(db, self.0.join(name.to_string()))
    }

    /// Returns a sub directory inside this directory. These directories don't necessarily exist on
    /// the file system. These are ids/paths to them.
    pub fn subdir(&self, name: SmolStr) -> Directory {
        Directory(self.0.join(name.to_string()))
    }
}
