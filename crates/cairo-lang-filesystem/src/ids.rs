use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_utils::{Intern, define_short_id};
use path_clean::PathClean;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::db::{CORELIB_CRATE_NAME, FilesGroup};
use crate::span::{TextOffset, TextSpan};

pub const CAIRO_FILE_EXTENSION: &str = "cairo";

/// Same as `CrateLongId`, but without internal interning.
/// This is used as salsa database inputs.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CrateInput {
    Real {
        name: SmolStr,
        discriminator: Option<SmolStr>,
    },
    Virtual {
        name: SmolStr,
        file_long_id: FileInput,
        settings: String,
        cache_file: Option<BlobLongId>,
    },
}

impl CrateInput {
    pub fn into_crate_long_id(self, db: &dyn FilesGroup) -> CrateLongId<'_> {
        match self {
            CrateInput::Real { name, discriminator } => CrateLongId::Real { name, discriminator },
            CrateInput::Virtual { name, file_long_id, settings, cache_file } => {
                CrateLongId::Virtual {
                    name,
                    file_id: file_long_id.into_file_long_id(db).intern(db),
                    settings,
                    cache_file: cache_file.map(|blob_long_id| blob_long_id.intern(db)),
                }
            }
        }
    }

    pub fn into_crate_ids(
        db: &dyn FilesGroup,
        inputs: impl IntoIterator<Item = CrateInput>,
    ) -> Vec<CrateId<'_>> {
        inputs.into_iter().map(|input| input.into_crate_long_id(db).intern(db)).collect()
    }
}

/// A crate is a standalone file tree representing a single compilation unit.
#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub enum CrateLongId<'db> {
    /// A crate that appears in crate_roots(), and on the filesystem.
    Real { name: SmolStr, discriminator: Option<SmolStr> },
    /// A virtual crate, not a part of the crate_roots(). Used mainly for tests.
    Virtual {
        name: SmolStr,
        file_id: FileId<'db>,
        settings: String,
        cache_file: Option<BlobId<'db>>,
    },
}
impl<'db> CrateLongId<'db> {
    pub fn name(&self) -> SmolStr {
        match self {
            CrateLongId::Real { name, .. } | CrateLongId::Virtual { name, .. } => name.clone(),
        }
    }

    pub fn into_crate_input(self, db: &'db dyn FilesGroup) -> CrateInput {
        match self {
            CrateLongId::Real { name, discriminator } => CrateInput::Real { name, discriminator },
            CrateLongId::Virtual { name, file_id, settings, cache_file } => CrateInput::Virtual {
                name,
                file_long_id: file_id.long(db).clone().into_file_input(db),
                settings,
                cache_file: cache_file.map(|blob_id| blob_id.long(db).clone()),
            },
        }
    }

    pub fn core() -> Self {
        CrateLongId::Real { name: CORELIB_CRATE_NAME.into(), discriminator: None }
    }

    pub fn plain(name: &str) -> Self {
        CrateLongId::Real { name: name.into(), discriminator: None }
    }
}
define_short_id!(CrateId, CrateLongId<'db>, FilesGroup, lookup_intern_crate, intern_crate);
impl<'db> CrateId<'db> {
    /// Gets the crate id for a real crate by name, without a discriminator.
    pub fn plain(db: &'db dyn FilesGroup, name: &str) -> Self {
        CrateId::new(db, CrateLongId::plain(name))
    }

    /// Gets the crate id for `core`.
    pub fn core(db: &'db dyn FilesGroup) -> Self {
        CrateId::new(db, CrateLongId::core())
    }
}

/// A trait for getting the internal salsa::InternId of a short id object.
///
/// This id is unstable across runs and should not be used to anything that is externally visible.
/// This is currently used to pick representative for strongly connected components.
pub trait UnstableSalsaId {
    fn get_internal_id(&self) -> salsa::Id;
}
impl UnstableSalsaId for CrateId<'_> {
    fn get_internal_id(&self) -> salsa::Id {
        self.0
    }
}

/// The long ID for a compilation flag.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FlagLongId(pub SmolStr);
define_short_id!(FlagId, FlagLongId, FilesGroup, lookup_intern_flag, intern_flag);

/// Same as `FileLongId`, but without the interning inside virtual files.
/// This is used to avoid the need to intern the file id inside salsa database inputs.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FileInput {
    OnDisk(PathBuf),
    Virtual(VirtualFileInput),
    External(salsa::Id),
}

impl FileInput {
    pub fn into_file_long_id(self, db: &dyn FilesGroup) -> FileLongId<'_> {
        match self {
            FileInput::OnDisk(path) => FileLongId::OnDisk(path),
            FileInput::Virtual(vf) => FileLongId::Virtual(vf.into_virtual_file(db)),
            FileInput::External(id) => FileLongId::External(id),
        }
    }
}

/// We use a higher level FileId struct, because not all files are on disk. Some might be online.
/// Some might be virtual/computed on demand.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FileLongId<'db> {
    OnDisk(PathBuf),
    Virtual(VirtualFile<'db>),
    External(salsa::Id),
}
/// Whether the file holds syntax for a module or for an expression.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum FileKind {
    Module,
    Expr,
    StatementList,
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

/// Same as `VirtualFile`, but without the interning inside virtual files.
/// This is used to avoid the need to intern the file id inside salsa database inputs.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VirtualFileInput {
    pub parent: Option<Arc<FileInput>>,
    pub name: SmolStr,
    pub content: Arc<str>,
    pub code_mappings: Arc<[CodeMapping]>,
    pub kind: FileKind,
    pub original_item_removed: bool,
}

impl VirtualFileInput {
    fn into_virtual_file(self, db: &dyn FilesGroup) -> VirtualFile<'_> {
        VirtualFile {
            parent: self.parent.map(|id| id.as_ref().clone().into_file_long_id(db).intern(db)),
            name: self.name,
            content: self.content,
            code_mappings: self.code_mappings,
            kind: self.kind,
            original_item_removed: self.original_item_removed,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub struct VirtualFile<'db> {
    pub parent: Option<FileId<'db>>,
    // TODO(eytan-starkware): Use smol str id instead of SmolStr.
    pub name: SmolStr,
    // TODO(eytan-starkware): Use str id instead of Arc<str>.
    pub content: Arc<str>,
    pub code_mappings: Arc<[CodeMapping]>,
    pub kind: FileKind,
    /// Whether an original item was removed when this virtual file was created
    /// Relevant only for virtual files created during macros expansion.
    /// This field is used by `cairo-language-server` for optimization purposes.
    pub original_item_removed: bool,
}
impl<'db> VirtualFile<'db> {
    fn full_path(&self, db: &'db dyn FilesGroup) -> String {
        if let Some(parent) = self.parent {
            // TODO(yuval): consider a different path format for virtual files.
            format!("{}[{}]", parent.full_path(db), self.name)
        } else {
            self.name.clone().into()
        }
    }

    fn into_virtual_file_input(self, db: &dyn FilesGroup) -> VirtualFileInput {
        VirtualFileInput {
            parent: self.parent.map(|id| Arc::new(id.long(db).clone().into_file_input(db))),
            name: self.name,
            content: self.content,
            code_mappings: self.code_mappings,
            kind: self.kind,
            original_item_removed: self.original_item_removed,
        }
    }
}

impl<'db> FileLongId<'db> {
    pub fn file_name(&self, db: &'db dyn FilesGroup) -> String {
        match self {
            FileLongId::OnDisk(path) => {
                path.file_name().and_then(|x| x.to_str()).unwrap_or("<unknown>").to_string()
            }
            FileLongId::Virtual(vf) => vf.name.to_string(),
            FileLongId::External(external_id) => db.ext_as_virtual(*external_id).name.to_string(),
        }
    }
    pub fn full_path(&self, db: &'db dyn FilesGroup) -> String {
        match self {
            FileLongId::OnDisk(path) => path.to_str().unwrap_or("<unknown>").to_string(),
            FileLongId::Virtual(vf) => vf.full_path(db),
            FileLongId::External(external_id) => db.ext_as_virtual(*external_id).full_path(db),
        }
    }
    pub fn kind(&self) -> FileKind {
        match self {
            FileLongId::OnDisk(_) => FileKind::Module,
            FileLongId::Virtual(vf) => vf.kind,
            FileLongId::External(_) => FileKind::Module,
        }
    }

    pub fn into_file_input(&self, db: &dyn FilesGroup) -> FileInput {
        match self {
            FileLongId::OnDisk(path) => FileInput::OnDisk(path.clone()),
            FileLongId::Virtual(vf) => FileInput::Virtual(vf.clone().into_virtual_file_input(db)),
            FileLongId::External(id) => FileInput::External(*id),
        }
    }
}

define_short_id!(FileId, FileLongId<'db>, FilesGroup, lookup_intern_file, intern_file);
impl<'db> FileId<'db> {
    pub fn new_on_disk(db: &'db dyn FilesGroup, path: PathBuf) -> FileId<'db> {
        FileLongId::OnDisk(path.clean()).intern(db)
    }

    pub fn file_name(self, db: &dyn FilesGroup) -> String {
        self.long(db).file_name(db)
    }

    pub fn full_path(self, db: &dyn FilesGroup) -> String {
        self.long(db).full_path(db)
    }

    pub fn kind(self, db: &dyn FilesGroup) -> FileKind {
        self.long(db).kind()
    }
}

define_short_id!(StrId, Arc<str>, FilesGroup, lookup_intern_str, intern_str);

/// Same as `Directory`, but without the interning inside virtual directories.
/// This is used to avoid the need to intern the file id inside salsa database inputs.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum DirectoryInput {
    Real(PathBuf),
    Virtual { files: BTreeMap<SmolStr, FileInput>, dirs: BTreeMap<SmolStr, Box<DirectoryInput>> },
}

impl DirectoryInput {
    /// Converts the input into a [`Directory`].
    pub fn into_directory(self, db: &dyn FilesGroup) -> Directory<'_> {
        match self {
            DirectoryInput::Real(path) => Directory::Real(path),
            DirectoryInput::Virtual { files, dirs } => Directory::Virtual {
                files: files
                    .into_iter()
                    .map(|(name, file_input)| {
                        (name.intern(db), file_input.into_file_long_id(db).intern(db))
                    })
                    .collect(),
                dirs: dirs
                    .into_iter()
                    .map(|(name, dir_input)| {
                        (name.intern(db), Box::new(dir_input.into_directory(db)))
                    })
                    .collect(),
            },
        }
    }
}

define_short_id!(SmolStrId, SmolStr, FilesGroup, lookup_intern_smol_str, intern_smol_str);

impl<'db> SmolStrId<'db> {
    pub fn as_str(&self, db: &'db dyn FilesGroup) -> &str {
        self.long(db).as_str()
    }

    pub fn from_str(db: &'db dyn FilesGroup, s: impl Into<SmolStr>) -> Self {
        SmolStrId::new(db, s.into())
    }
}

impl<'db> Ord for SmolStrId<'db> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<'db> PartialOrd for SmolStrId<'db> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub enum Directory<'db> {
    /// A directory on the file system.
    Real(PathBuf),
    /// A virtual directory, not on the file system. Used mainly for virtual crates.
    Virtual {
        files: BTreeMap<SmolStrId<'db>, FileId<'db>>,
        dirs: BTreeMap<SmolStrId<'db>, Box<Directory<'db>>>,
    },
}

impl<'db> Directory<'db> {
    /// Returns a file inside this directory. The file and directory don't necessarily exist on
    /// the file system. These are ids/paths to them.
    pub fn file(&self, db: &'db dyn FilesGroup, name: SmolStrId<'db>) -> FileId<'db> {
        match self {
            Directory::Real(path) => FileId::new_on_disk(db, path.join(name.long(db).as_str())),
            Directory::Virtual { files, dirs: _ } => files
                .get(&name)
                .copied()
                .unwrap_or_else(|| FileId::new_on_disk(db, PathBuf::from(name.long(db).as_str()))),
        }
    }

    /// Returns a sub directory inside this directory. These directories don't necessarily exist on
    /// the file system. These are ids/paths to them.
    pub fn subdir(&self, db: &'db dyn FilesGroup, name: SmolStrId<'db>) -> Directory<'db> {
        match self {
            Directory::Real(path) => Directory::Real(path.join(name.long(db).as_str())),
            Directory::Virtual { files: _, dirs } => {
                if let Some(dir) = dirs.get(&name) {
                    dir.as_ref().clone()
                } else {
                    Directory::Virtual { files: BTreeMap::new(), dirs: BTreeMap::new() }
                }
            }
        }
    }

    /// Converts the directory into an [`DirectoryInput`].
    pub fn into_directory_input(self, db: &dyn FilesGroup) -> DirectoryInput {
        match self {
            Directory::Real(path) => DirectoryInput::Real(path),
            Directory::Virtual { files, dirs } => DirectoryInput::Virtual {
                files: files
                    .into_iter()
                    .map(|(name, file_id)| {
                        (name.long(db).clone(), file_id.long(db).clone().into_file_input(db))
                    })
                    .collect(),
                dirs: dirs
                    .into_iter()
                    .map(|(name, dir)| {
                        (name.long(db).clone(), Box::new(dir.into_directory_input(db)))
                    })
                    .collect(),
            },
        }
    }
}

/// A FileId for data that is not necessarily a valid UTF-8 string.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BlobLongId {
    OnDisk(PathBuf),
    Virtual(Vec<u8>),
}

impl BlobLongId {
    pub fn content(&self) -> Option<Vec<u8>> {
        match self {
            BlobLongId::OnDisk(path) => std::fs::read(path).ok(),
            BlobLongId::Virtual(content) => Some(content.clone()),
        }
    }
}

define_short_id!(BlobId, BlobLongId, FilesGroup, lookup_intern_blob, intern_blob);

impl<'db> BlobId<'db> {
    pub fn new_on_disk(db: &'db (dyn FilesGroup + 'db), path: PathBuf) -> Self {
        BlobId::new(db, BlobLongId::OnDisk(path.clean()))
    }
}
