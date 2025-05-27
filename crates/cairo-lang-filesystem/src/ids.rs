use path_clean::PathClean;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;

use crate::db::{CORELIB_CRATE_NAME, FilesGroup};
use crate::span::{TextOffset, TextSpan};

pub const CAIRO_FILE_EXTENSION: &str = "cairo";

/// A crate is a standalone file tree representing a single compilation unit.
#[salsa::input]
pub struct CrateLongId {
    pub crate_info: CrateInfo,
}

/// The information about a crate.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CrateInfo {
    /// A crate that appears in crate_roots(), and on the filesystem.
    Real { name: SmolStr, discriminator: Option<SmolStr> },
    /// A virtual crate, not a part of the crate_roots(). Used mainly for tests.
    Virtual { name: SmolStr, file_id: FileLongId, settings: String, cache_file: Option<BlobLongId> },
}

impl CrateLongId {
    /// Gets the crate id for a real crate by name, without a discriminator.
    pub fn plain(
        db: &(impl cairo_lang_utils::Upcast<dyn FilesGroup> + ?Sized),
        name: &str,
    ) -> Self {
        CrateLongId::new(db.upcast(), CrateInfo::Real { name: name.into(), discriminator: None })
    }

    /// Gets the crate id for `core`.
    pub fn core(db: &(impl cairo_lang_utils::Upcast<dyn FilesGroup> + ?Sized)) -> Self {
        CrateLongId::new(
            db.upcast(),
            CrateInfo::Real { name: CORELIB_CRATE_NAME.into(), discriminator: None },
        )
    }

    pub fn name(&self, db: &dyn FilesGroup) -> SmolStr {
        match self.crate_info(db) {
            CrateInfo::Real { name, .. } => name.clone(),
            CrateInfo::Virtual { name, .. } => name.clone(),
        }
    }
}

/// A trait for getting the internal salsa::InternId of a short id object.
///
/// This id is unstable across runs and should not be used to anything that is externally visible.
/// This is currently used to pick representative for strongly connected components.
pub trait UnstableSalsaId {
    fn get_internal_id(&self) -> salsa::Id;
}
impl UnstableSalsaId for CrateLongId {
    fn get_internal_id(&self) -> salsa::Id {
        self.0
    }
}

/// The long ID for a compilation flag.
#[salsa::input]
pub struct FlagLongId {
    pub name: SmolStr,
}

/// We use a higher level FileId struct, because not all files are on disk. Some might be online.
/// Some might be virtual/computed on demand.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FileInfo {
    OnDisk(PathBuf),
    Virtual(VirtualFile),
    External(salsa::Id),
}

#[salsa::input]
#[derive(Debug)]
pub struct FileLongId {
    pub file_info: FileInfo,
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

    pub fn start(&self) -> TextOffset {
        match self {
            CodeOrigin::Start(start) => *start,
            CodeOrigin::CallSite(span) => span.start,
            CodeOrigin::Span(span) => span.start,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VirtualFile {
    pub parent: Option<FileLongId>,
    pub name: SmolStr,
    pub content: Arc<str>,
    pub code_mappings: Arc<[CodeMapping]>,
    pub kind: FileKind,
    /// Whether an original item was removed when this virtual file was created
    /// Relevant only for virtual files created during macros expansion.
    /// This field is used by `cairo-language-server` for optimization purposes.
    pub original_item_removed: bool,
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

impl FileLongId {
    pub fn new_on_disk(db: &dyn FilesGroup, path: PathBuf) -> FileLongId {
        FileLongId::new(db, FileInfo::OnDisk(path.clean()))
    }
    pub fn file_name(self, db: &dyn FilesGroup) -> String {
        match self.file_info(db) {
            FileInfo::OnDisk(path) => {
                path.file_name().and_then(|x| x.to_str()).unwrap_or("<unknown>").to_string()
            }
            FileInfo::Virtual(vf) => vf.name.to_string(),
            FileInfo::External(external_id) => db.ext_as_virtual(external_id).name.to_string(),
        }
    }
    pub fn full_path(self, db: &dyn FilesGroup) -> String {
        match self.file_info(db) {
            FileInfo::OnDisk(path) => path.to_str().unwrap_or("<unknown>").to_string(),
            FileInfo::Virtual(vf) => vf.full_path(db),
            FileInfo::External(external_id) => db.ext_as_virtual(external_id).full_path(db),
        }
    }
    pub fn kind(self, db: &dyn FilesGroup) -> FileKind {
        match self.file_info(db) {
            FileInfo::OnDisk(_) => FileKind::Module,
            FileInfo::Virtual(vf) => vf.kind,
            FileInfo::External(_) => FileKind::Module,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Directory {
    /// A directory on the file system.
    Real(PathBuf),
    /// A virtual directory, not on the file system. Used mainly for virtual crates.
    Virtual { files: BTreeMap<SmolStr, FileLongId>, dirs: BTreeMap<SmolStr, Box<Directory>> },
}

impl Directory {
    /// Returns a file inside this directory. The file and directory don't necessarily exist on
    /// the file system. These are ids/paths to them.
    pub fn file(&self, db: &dyn FilesGroup, name: SmolStr) -> FileLongId {
        match self {
            Directory::Real(path) => FileLongId::new_on_disk(db, path.join(name.as_str())),
            Directory::Virtual { files, dirs: _ } => files
                .get(&name)
                .copied()
                .unwrap_or_else(|| FileLongId::new_on_disk(db, PathBuf::from(name.as_str()))),
        }
    }

    /// Returns a sub directory inside this directory. These directories don't necessarily exist on
    /// the file system. These are ids/paths to them.
    pub fn subdir(&self, name: SmolStr) -> Directory {
        match self {
            Directory::Real(path) => Directory::Real(path.join(name.as_str())),
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

/// Interned [BlobInfo]. Necessary due to salsa limitations with enums.
#[salsa::input]
#[derive(Debug)]
pub struct BlobLongId {
    pub blob_info: BlobInfo,
}

/// A FileId for data that is not necessarily a valid UTF-8 string.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BlobInfo {
    OnDisk(PathBuf),
    Virtual(Arc<[u8]>),
}

impl BlobLongId {
    pub fn new_on_disk(db: &dyn FilesGroup, path: PathBuf) -> BlobLongId {
        BlobLongId::new(db, BlobInfo::OnDisk(path.clean()))
    }
}
