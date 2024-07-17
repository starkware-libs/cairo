use std::hash::{BuildHasher, Hash, RandomState};
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::{fs, io};

use cairo_lang_project::PROJECT_FILE_NAME;
use cairo_lang_utils::{define_short_id, LookupIntern};

use crate::project::project_manifest_path::ProjectManifestPath;
use crate::toolchain::scarb::{SCARB_LOCK, SCARB_TOML};

/// An opaque wrapper over a [`Path`] that refers to a file that is relevant for project analysis.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Digestible(Arc<Path>);

impl Digestible {
    /// Creates a new digestible from given path.
    ///
    /// Returns `Some` if path points a file that is relevant for project analysis; otherwise,
    /// returns `None`.
    pub fn try_new(path: &Path) -> Option<Self> {
        [PROJECT_FILE_NAME, SCARB_TOML, SCARB_LOCK]
            .contains(&path.file_name()?.to_str()?)
            .then(|| Self(path.to_owned().into()))
    }
}

impl From<&'_ ProjectManifestPath> for Digestible {
    fn from(manifest_path: &'_ ProjectManifestPath) -> Self {
        Self::try_new(manifest_path.as_path()).unwrap()
    }
}

define_short_id!(DigestId, Digestible, LsDigestsGroup, lookup_intern_digest, intern_digest);

/// An opaque object carrying digests of files.
///
/// Digests carry internally one of 3 state kinds:
/// 1. File exists and has specific content hash.
/// 2. File does not exist.
/// 3. Some I/O error occurs when trying to read the file, each error occurrence yields new digest
///    instance.
///
/// By being computed and passed through Salsa queries, digests are used as a mean of cache-busting
/// when contents of relevant files change.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Digest(DigestKind);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum DigestKind {
    Ok(u64),
    FileNotFound,
    IoError(usize),
}

impl Digest {
    fn ok(hash: u64) -> Self {
        Self(DigestKind::Ok(hash))
    }

    fn file_not_found() -> Self {
        Self(DigestKind::FileNotFound)
    }

    fn io_error() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let count = COUNTER.fetch_add(1, Ordering::Relaxed);
        Self(DigestKind::IoError(count))
    }
}

/// A group of queries for tracking [`Digest`]s of files.
#[salsa::query_group(LsDigestsDatabase)]
pub trait LsDigestsGroup {
    #[salsa::interned]
    fn intern_digest(&self, path: Digestible) -> DigestId;

    /// Compute digest of a digestible file.
    fn digest(&self, digest: DigestId) -> Digest;
}

/// Invalidates the digest of the given digestible file, forcing the projects database to recompute
/// it on next query computation.
pub fn invalidate_digest(db: &mut dyn LsDigestsGroup, digest: DigestId) {
    DigestQuery.in_db_mut(db).invalidate(&digest);
}

fn digest(db: &dyn LsDigestsGroup, digest: DigestId) -> Digest {
    let Digestible(path) = digest.lookup_intern(db);
    match fs::read(&*path) {
        Ok(bytes) => {
            let hash = RandomState::new().hash_one(bytes);
            Digest::ok(hash)
        }
        Err(err) if err.kind() == io::ErrorKind::NotFound => Digest::file_not_found(),
        Err(_) => Digest::io_error(),
    }
}
