use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_utils::define_short_id;
use path_clean::PathClean;
use smol_str::SmolStr;

use crate::db::{CrateConfiguration, FilesGroup};
use crate::span::{TextOffset, TextSpan};

pub const CAIRO_FILE_EXTENSION: &str = "cairo";

/// A crate is a standalone file tree representing a single compilation unit.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CrateLongId {
    /// A crate that appears in crate_roots(), and on the filesystem.
    Real(SmolStr),
    /// A virtual crate, not a part of the crate_roots(). Used mainly for tests.
    Virtual { name: SmolStr, config: CrateConfiguration },
}
impl CrateLongId {
    pub fn name(&self) -> SmolStr {
        match self {
            CrateLongId::Real(name) => name.clone(),
            CrateLongId::Virtual { name, .. } => name.clone(),
        }
    }
}
define_short_id!(CrateId, CrateLongId, FilesGroup, lookup_intern_crate);
impl CrateId {
    pub fn name(&self, db: &dyn FilesGroup) -> SmolStr {
        db.lookup_intern_crate(*self).name()
    }
}

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

/// A mapping for a code rewrite.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CodeMapping {
    pub span: TextSpan,
    pub origin: CodeOrigin,
}
impl CodeMapping {
    pub fn translate(&self, span: TextSpan) -> Option<TextSpan> {
        if self.span.contains(span) {
            Some(match self.origin {
                CodeOrigin::Start(origin_start) => {
                    let start = origin_start.add_width(span.start - self.span.start);
                    TextSpan { start, end: start.add_width(span.width()) }
                }
                CodeOrigin::Span(span) => span,
            })
        } else {
            None
        }
    }
}

/// The origin of a code mapping.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CodeOrigin {
    /// The origin is a copied node staring at the given offset.
    Start(TextOffset),
    /// The origin was generated from this span, but there's no direct mapping.
    Span(TextSpan),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VirtualFile {
    pub parent: Option<FileId>,
    pub name: SmolStr,
    pub content: Arc<String>,
    pub code_mappings: Arc<Vec<CodeMapping>>,
    pub kind: FileKind,
}
impl VirtualFile {
    fn full_path(&self, db: &dyn FilesGroup) -> String {
        if let Some(parent) = self.parent {
            // TODO(yuval): consider a different path format for virtual files.
            format!("{}[{}]", parent.full_path(db), self.name)
        } else {
            self.name.clone().into()
        }
    }
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
    pub fn full_path(self, db: &dyn FilesGroup) -> String {
        match db.lookup_intern_file(self) {
            FileLongId::OnDisk(path) => path.to_str().unwrap_or("<unknown>").to_string(),
            FileLongId::Virtual(vf) => vf.full_path(db),
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
pub enum Directory {
    /// A directory on the file system.
    Real(PathBuf),
    /// A virtual directory, not on the file system. Used mainly for virtual crates.
    Virtual { files: BTreeMap<SmolStr, FileId>, dirs: BTreeMap<SmolStr, Box<Directory>> },
}

impl Directory {
    /// Returns a file inside this directory. The file and directory don't necessarily exist on
    /// the file system. These are ids/paths to them.
    pub fn file(&self, db: &dyn FilesGroup, name: SmolStr) -> FileId {
        match self {
            Directory::Real(path) => FileId::new(db, path.join(name.to_string())),
            Directory::Virtual { files, dirs: _ } => files
                .get(&name)
                .copied()
                .unwrap_or_else(|| FileId::new(db, PathBuf::from(name.as_str()))),
        }
    }

    /// Returns a sub directory inside this directory. These directories don't necessarily exist on
    /// the file system. These are ids/paths to them.
    pub fn subdir(&self, name: SmolStr) -> Directory {
        match self {
            Directory::Real(path) => Directory::Real(path.join(name.to_string())),
            Directory::Virtual { files: _, dirs } => {
                if let Some(dir) = dirs.get(&name) {
                    dir.as_ref().clone()
                } else {
                    Directory::Virtual { files: BTreeMap::new(), dirs: BTreeMap::new() }
                }
            }
        }
    }
}
