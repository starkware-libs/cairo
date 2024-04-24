use std::path::{Path, PathBuf};

use cairo_lang_filesystem::db::{init_dev_corelib, FilesGroup};
use tracing::warn;

use crate::config::Config;

/// A helper object for managing the unmanaged Cairo `core` crate.
///
/// An unmanaged `core` is one that is used in projects whose managers do not handle the `core`
/// crate themselves.
/// This is the case for `cairo_project.toml`-based projects and for detached files.
pub struct UnmanagedCoreCrate {
    fallback_path: Option<PathBuf>,
}

impl UnmanagedCoreCrate {
    /// Constructs a new [`UnmanagedCoreCrate`].
    pub fn new(config: &Config) -> Self {
        Self { fallback_path: config.unmanaged_core_fallback_path.clone() }
    }

    /// Reacts to changes in the [`Config`].
    pub fn on_config_changed(&mut self, config: &Config) {
        self.fallback_path.clone_from(&config.unmanaged_core_fallback_path);
    }

    /// Tries to find a Cairo `core` crate and initializes it in the provided database.
    pub fn apply_db_changes(&self, db: &mut (dyn FilesGroup + 'static)) {
        if let Some(path) = self.discover() {
            // FIXME(mkaput): This should actually read from cairo_project.toml/Scarb.toml.
            //   init_dev_corelib has to die.
            init_dev_corelib(db, path);
        } else {
            warn!("failed to find unmanaged core crate")
        }
    }

    /// Tries to find a Cairo `core` crate in various well-known places, for use in project backends
    /// that do not manage the `core` crate (i.e., anything non-Scarb).
    fn discover(&self) -> Option<PathBuf> {
        // TODO(mkaput): First, try to find Scarb-managed `core` package if we have Scarb toolchain.
        //   The most reliable way to do this is to create an empty Scarb package, and run
        //   `scarb metadata` on it. The `core` package will be a component of this empty package.
        //   For minimal packages, `scarb metadata` should be pretty fast.
        // TODO(mkaput): Shouldn't `fallback` actually be an `override`?
        cairo_lang_filesystem::detect::detect_corelib()
            .or_else(|| find_core_at_path(self.fallback_path.as_ref()?.as_path()))
    }
}

/// Attempts to find the `core` crate source root at a given path.
///
/// In the [starkware-libs/cairo] repository, the `core` crate sits in `./corelib/src`, this is the
/// first place looked for.
/// The `core` crate is a regular Scarb package, so it sounds obvious that the user may provide a
/// path to the directory containing the manifest file, hence next this function looks for `./src`.
/// Finally, the input path is considered as a candidate and is just checked for existence.
///
/// [starkware-libs/cairo]: https://github.com/starkware-libs/cairo
fn find_core_at_path(root_path: &Path) -> Option<PathBuf> {
    let mut path = root_path.to_owned();
    path.push("corelib");
    path.push("src");
    if path.exists() {
        return Some(path);
    }

    let mut path = root_path.to_owned();
    path.push("src");
    if path.exists() {
        return Some(path);
    }

    if root_path.exists() {
        return Some(root_path.to_owned());
    }

    None
}
