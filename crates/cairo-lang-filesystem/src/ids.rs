use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_utils::{Intern, LookupIntern, define_short_id};
use path_clean::PathClean;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::db::{CORELIB_CRATE_NAME, FilesGroup};
use crate::span::{TextOffset, TextSpan};

pub const CAIRO_FILE_EXTENSION: &str = "cairo";

/// A crate is a standalone file tree representing a single compilation unit.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CrateLongId {
    /// A crate that appears in crate_roots(), and on the filesystem.
    Real { name: SmolStr, discriminator: Option<SmolStr> },
    /// A virtual crate, not a part of the crate_roots(). Used mainly for tests.
    Virtual { name: SmolStr, file_id: FileId, settings: String, cache_file: Option<BlobId> },
}
impl CrateLongId {
    pub fn name(&self) -> SmolStr {
        match self {
            CrateLongId::Real { name, .. } | CrateLongId::Virtual { name, .. } => name.clone(),
        }
    }
}
define_short_id!(CrateId, CrateLongId, FilesGroup, lookup_intern_crate, intern_crate);
impl CrateId {
    /// Gets the crate id for a real crate by name, without a discriminator.
    pub fn plain(
        db: &(impl cairo_lang_utils::Upcast<dyn FilesGroup> + ?Sized),
        name: &str,
    ) -> Self {
        CrateLongId::Real { name: name.into(), discriminator: None }.intern(db)
    }

    /// Gets the crate id for `core`.
    pub fn core(db: &(impl cairo_lang_utils::Upcast<dyn FilesGroup> + ?Sized)) -> Self {
        CrateLongId::Real { name: CORELIB_CRATE_NAME.into(), discriminator: None }.intern(db)
    }

    pub fn name(&self, db: &dyn FilesGroup) -> SmolStr {
        self.lookup_intern(db).name()
    }
}

/// A trait for getting the internal salsa::InternId of a short id object.
///
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
define_short_id!(FlagId, FlagLongId, FilesGroup, lookup_intern_flag, intern_flag);
impl FlagId {
    pub fn new(db: &dyn FilesGroup, name: &str) -> Self {
        FlagLongId(name.into()).intern(db)
    }
}

/// We use a higher level FileId struct, because not all files are on disk. Some might be online.
/// Some might be virtual/computed on demand.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FileLongId {
    OnDisk(PathBuf),
    Virtual(VirtualFile),
    External(salsa::InternId),
}
/// Whether the file holds syntax for a module or for an expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum FileKind {
    Module,
    Expr,
}

/// A mapping for a code rewrite.
#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
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
                CodeOrigin::CallSite(span) => span,
            })
        } else {
            None
        }
    }
}

/// The origin of a code mapping.
#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum CodeOrigin {
    /// The origin is a copied node starting at the given offset.
    Start(TextOffset),
    /// The origin was generated from this span, but there's no direct mapping.
    Span(TextSpan),
    /// The origin was generated because of this span, but no code has been copied.
    /// E.g. a macro defined attribute on a function.
    CallSite(TextSpan),
}

impl CodeOrigin {
    pub fn as_span(&self) -> Option<TextSpan> {
        match self {
            CodeOrigin::Start(_) => None,
            CodeOrigin::CallSite(_) => None,
            CodeOrigin::Span(span) => Some(*span),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VirtualFile {
    pub parent: Option<FileId>,
    pub name: SmolStr,
    pub content: Arc<str>,
    pub code_mappings: Arc<[CodeMapping]>,
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

define_short_id!(FileId, FileLongId, FilesGroup, lookup_intern_file, intern_file);
impl FileId {
    pub fn new(db: &dyn FilesGroup, path: PathBuf) -> FileId {
        FileLongId::OnDisk(path.clean()).intern(db)
    }
    pub fn file_name(self, db: &dyn FilesGroup) -> String {
        match self.lookup_intern(db) {
            FileLongId::OnDisk(path) => {
                path.file_name().and_then(|x| x.to_str()).unwrap_or("<unknown>").to_string()
            }
            FileLongId::Virtual(vf) => vf.name.to_string(),
            FileLongId::External(external_id) => db.ext_as_virtual(external_id).name.to_string(),
        }
    }
    pub fn full_path(self, db: &dyn FilesGroup) -> String {
        match self.lookup_intern(db) {
            FileLongId::OnDisk(path) => path.to_str().unwrap_or("<unknown>").to_string(),
            FileLongId::Virtual(vf) => vf.full_path(db),
            FileLongId::External(external_id) => db.ext_as_virtual(external_id).full_path(db),
        }
    }
    pub fn kind(self, db: &dyn FilesGroup) -> FileKind {
        match self.lookup_intern(db) {
            FileLongId::OnDisk(_) => FileKind::Module,
            FileLongId::Virtual(vf) => vf.kind.clone(),
            FileLongId::External(_) => FileKind::Module,
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
            Directory::Real(path) => FileId::new(db, path.join(&name)),
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
            Directory::Real(path) => Directory::Real(path.join(&name)),
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

/// A FileId for data that is not necessarily a valid UTF-8 string.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BlobLongId {
    OnDisk(PathBuf),
    Virtual(Arc<[u8]>),
}

define_short_id!(BlobId, BlobLongId, FilesGroup, lookup_intern_blob, intern_blob);

impl BlobId {
    pub fn new(db: &dyn FilesGroup, path: PathBuf) -> BlobId {
        BlobLongId::OnDisk(path.clean()).intern(db)
    }
}
