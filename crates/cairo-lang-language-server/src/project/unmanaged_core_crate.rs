use std::path;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::Context;
use cairo_lang_filesystem::db::CORELIB_CRATE_NAME;
use cairo_lang_project::ProjectConfig;
use salsa::Durability;
use smol_str::SmolStr;
use tracing::error;

use crate::config::Config;
use crate::project::Crate;

/// A group of queries responsible for dealing with the unmanaged Cairo `core` crate.
///
/// An unmanaged `core` is one that is used in projects whose managers do not handle the `core`
/// crate themselves.
/// This is the case for `cairo_project.toml`-based projects,
/// detached files and not-yet-fully loaded Scarb workspaces.
#[salsa::query_group(LsUnmanagedCoreDatabase)]
pub trait LsUnmanagedCoreGroup {
    /// Path to unmanaged `core` from LS configuration.
    #[salsa::input]
    fn config_unmanaged_core_path(&self) -> Option<PathBuf>;

    /// Tries to find a Cairo `core` crate in various well-known places, for use in project backends
    /// that do not manage the `core` crate (i.e., anything non-Scarb).
    fn unmanaged_core(&self) -> Option<Arc<Crate>>;
}

pub fn set_unmanaged_core_inputs_from_config(db: &mut dyn LsUnmanagedCoreGroup, config: &Config) {
    db.set_config_unmanaged_core_path_with_durability(
        config.unmanaged_core_path.clone(),
        Durability::HIGH,
    );
}

fn unmanaged_core(db: &dyn LsUnmanagedCoreGroup) -> Option<Arc<Crate>> {
    let crate_path = find_unmanaged_core(db)?;

    macro_rules! load_error {
        ($message:literal) => {
            error!(
                concat!($message, ", language server features may not work: {crate_path}"),
                crate_path = crate_path.display()
            );
        };
    }

    let name = SmolStr::new_static(CORELIB_CRATE_NAME);

    let Some(project_config) = try_load_core_project_config(&crate_path) else {
        load_error!("failed to load unmanaged `core` crate's cairo_project.toml");
        return None;
    };

    let Some(source_root) = project_config.content.crate_roots.get(&name) else {
        load_error!(
            "the found unmanaged `core` crate's cairo_project.toml does not actually define the \
             `core` crate"
        );
        return None;
    };

    let root = project_config.absolute_crate_root(source_root);

    let settings = project_config.content.crates_config.get(&name).clone();

    Some(Arc::new(Crate { name, root, custom_main_file_stem: None, settings }))
}

/// Tries to find a Cairo `core` crate in various well-known places.
fn find_unmanaged_core(db: &dyn LsUnmanagedCoreGroup) -> Option<PathBuf> {
    // TODO(mkaput): First, try to find Scarb-managed `core` package if we have Scarb toolchain.
    //   The most reliable way to do this is to create an empty Scarb package, and run
    //   `scarb metadata` on it. The `core` package will be a component of this empty package.
    //   For minimal packages, `scarb metadata` should be pretty fast.
    find_core_at_config_path(db)
        .or_else(cairo_lang_filesystem::detect::detect_corelib)
        .and_then(ensure_absolute)
}

/// Attempts to find the `core` crate source root at the path provided in the configuration.
fn find_core_at_config_path(db: &dyn LsUnmanagedCoreGroup) -> Option<PathBuf> {
    find_core_at_path(&db.config_unmanaged_core_path()?)
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
    path.push("../../../../../corelib");
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

/// Try to load `core`'s `cairo_project.toml` at the given path, or if this path points to `core`'s
/// `src` directory, looks in parent directory.
///
/// This fallback case is needed because:
/// 1. `detect_corelib` returns path to `src`, or
/// 2. the user who does not understand such details may provide such path in config.
fn try_load_core_project_config(crate_path: &Path) -> Option<ProjectConfig> {
    let attempt =
        |path| ProjectConfig::from_directory(path).inspect_err(|err| error!("{err:?}")).ok();

    attempt(crate_path).or_else(|| {
        if crate_path.file_name().and_then(|s| s.to_str()) == Some("src") {
            attempt(crate_path.parent().unwrap())
        } else {
            None
        }
    })
}

/// Makes a path absolute, or logs an error.
fn ensure_absolute(path: PathBuf) -> Option<PathBuf> {
    path::absolute(&path)
        .with_context(|| {
            format!("failed to make `core` crate path absolute: {path}", path = path.display())
        })
        .inspect_err(|e| error!("{e:?}"))
        .ok()
}
