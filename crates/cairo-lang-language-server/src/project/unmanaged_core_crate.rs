use std::path::{Path, PathBuf};

use cairo_lang_filesystem::db::{init_dev_corelib, FilesGroup};
use tracing::warn;

use crate::config::Config;

/// Try to find a Cairo `core` crate (see [`find_unmanaged_core`]) and initialize it in the
/// provided database.
pub fn try_to_init_unmanaged_core(config: &Config, db: &mut (dyn FilesGroup + 'static)) {
    if let Some(path) = find_unmanaged_core(config) {
        init_dev_corelib(db, path);
    } else {
        warn!("failed to find unmanaged core crate")
    }
}

/// Try to find a Cairo `core` crate in various well-known places, for use in project backends that
/// do not manage the `core` crate (i.e., anything non-Scarb).
pub fn find_unmanaged_core(config: &Config) -> Option<PathBuf> {
    // TODO(mkaput): First, try to find Scarb-managed `core` package if we have Scarb toolchain.
    //   The most reliable way to do this is to create an empty Scarb package, and run
    //   `scarb metadata` on it. The `core` package will be a component of this empty package.
    //   For minimal packages, `scarb metadata` should be pretty fast.
    // TODO(mkaput): Shouldn't `fallback` actually be an `override`?
    cairo_lang_filesystem::detect::detect_corelib()
        .or_else(|| find_core_at_path(config.unmanaged_core_fallback_path.as_ref()?.as_path()))
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
