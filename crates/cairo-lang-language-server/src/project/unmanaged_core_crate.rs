use std::io;
use std::path::{Path, PathBuf};

use anyhow::Context;
use cairo_lang_filesystem::db::{init_dev_corelib, FilesGroup};
use tracing::{error, warn};

use crate::config::Config;

/// A helper object for managing the unmanaged Cairo `core` crate.
///
/// An unmanaged `core` is one that is used in projects whose managers do not handle the `core`
/// crate themselves.
/// This is the case for `cairo_project.toml`-based projects and for detached files.
pub struct UnmanagedCoreCrate {
    core_path: Option<PathBuf>,
}

impl UnmanagedCoreCrate {
    /// Constructs a new [`UnmanagedCoreCrate`].
    pub fn new(config: &Config) -> Self {
        Self { core_path: config.unmanaged_core_path.clone() }
    }

    /// Reacts to changes in the [`Config`].
    pub fn on_config_changed(&mut self, config: &Config) {
        self.core_path.clone_from(&config.unmanaged_core_path);
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
    ///
    /// The path is guaranteed to be absolute, so it can be safely used as a `FileId` in LS Salsa
    /// DB.
    fn discover(&self) -> Option<PathBuf> {
        // TODO(mkaput): First, try to find Scarb-managed `core` package if we have Scarb toolchain.
        //   The most reliable way to do this is to create an empty Scarb package, and run
        //   `scarb metadata` on it. The `core` package will be a component of this empty package.
        //   For minimal packages, `scarb metadata` should be pretty fast.
        find_core_at_config_path(&self.core_path)
            .or_else(cairo_lang_filesystem::detect::detect_corelib)
            .and_then(ensure_absolute)
    }
}

/// Attempts to find the `core` crate source root at the path provided in the configuration.
fn find_core_at_config_path(core_path: &Option<PathBuf>) -> Option<PathBuf> {
    find_core_at_path(core_path.as_ref()?.as_path())
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

/// Makes a path absolute, or logs an error.
fn ensure_absolute(path: PathBuf) -> Option<PathBuf> {
    path_absolute(&path)
        .with_context(|| {
            format!("failed to make `core` crate path absolute: {path}", path = path.display())
        })
        .inspect_err(|e| error!("{e:?}"))
        .ok()
}

// FIXME(mkaput): Replace this with `std::path::absolute` when Rust 1.79 is released.
/// Returns absolute form of a path, but does not resolve symlinks unlike [`std::fs::canonicalize`].
///
/// The function normalizes the path by resolving any `.` and `..` components which are present.
///
/// Copied from <https://internals.rust-lang.org/t/path-to-lexical-absolute/14940>.
fn path_absolute(p: &Path) -> io::Result<PathBuf> {
    use std::path::Component;

    let mut absolute = if p.is_absolute() { PathBuf::new() } else { std::env::current_dir()? };
    for component in p.components() {
        match component {
            Component::CurDir => {
                // Skip `.` components.
            }
            Component::ParentDir => {
                // Pop the last element that has been added for `..` components.
                absolute.pop();
            }
            component => {
                // Just push the component for any other component.
                absolute.push(component.as_os_str())
            }
        }
    }
    Ok(absolute)
}
