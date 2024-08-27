use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::{fs, path};

use anyhow::Context;
use cairo_lang_filesystem::db::{init_dev_corelib, CORELIB_CRATE_NAME};
use indoc::indoc;
use tempfile::tempdir;
use tracing::{error, warn};

use crate::config::Config;
use crate::lang::db::AnalysisDatabase;
use crate::toolchain::scarb::{ScarbToolchain, SCARB_TOML};

/// Try to find a Cairo `core` crate (see [`find_unmanaged_core`]) and initialize it in the
/// provided database.
pub fn try_to_init_unmanaged_core(
    db: &mut AnalysisDatabase,
    config: &Config,
    scarb: &ScarbToolchain,
) {
    if let Some(path) = find_unmanaged_core(config, scarb) {
        init_dev_corelib(db, path);
    } else {
        warn!("failed to find unmanaged core crate")
    }
}

/// Try to find a Cairo `core` crate in various well-known places, for use in project backends that
/// do not manage the `core` crate (i.e., anything non-Scarb).
///
/// The path is guaranteed to be absolute, so it can be safely used as a `FileId` in LS Salsa DB.
pub fn find_unmanaged_core(config: &Config, scarb: &ScarbToolchain) -> Option<PathBuf> {
    find_core_at_config_path(config)
        .or_else(|| find_scarb_managed_core(scarb))
        .or_else(|| {
            if cfg!(feature = "testing") {
                cairo_lang_filesystem::detect::detect_corelib()
            } else {
                None
            }
        })
        .and_then(ensure_absolute)
}

/// Attempts to find the `core` crate source root at the path provided in the configuration.
fn find_core_at_config_path(config: &Config) -> Option<PathBuf> {
    find_core_at_path(config.unmanaged_core_path.as_ref()?.as_path())
}

/// Attempts to find the `core` crate source root at a given path.
///
/// In the [starkware-libs/cairo] repository, the `core` crate sits in `./corelib/src`.
/// This is the first place looked for.
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

// TODO(#6246): Add tests for this logic. Pay attention to usage of silent mode in ScarbToolchain.
/// Try to find a Scarb-managed `core` package if we have Scarb toolchain.
///
/// The easiest way to do this is to create an empty Scarb package and run `scarb metadata` on it.
/// The `core` package will be a component of this empty package.
/// For minimal packages, `scarb metadata` should be pretty fast.
///
/// Because CairoLS is tightly bound to Scarb (due to hard compiler version dependency),
/// we can safely make this lookup once and keep it cached for the entire LS lifetime.
#[tracing::instrument(level = "trace", skip_all)]
fn find_scarb_managed_core(scarb: &ScarbToolchain) -> Option<PathBuf> {
    let lookup = || {
        let workspace = tempdir()
            .context("failed to create temporary directory")
            .inspect_err(|e| warn!("{e:?}"))
            .ok()?;

        let scarb_toml = workspace.path().join(SCARB_TOML);
        fs::write(
            &scarb_toml,
            indoc! {r#"
                [package]
                name = "cairols_unmanaged_core_lookup"
                version = "1.0.0"
            "#},
        )
        .context("failed to write Scarb.toml")
        .inspect_err(|e| warn!("{e:?}"))
        .ok()?;

        let metadata = scarb.silent().metadata(&scarb_toml).inspect_err(|e| warn!("{e:?}")).ok()?;

        // Ensure the workspace directory is deleted after running Scarb.
        // We are ignoring the error, leaving doing proper cleanup to the OS.
        let _ = workspace
            .close()
            .context("failed to wipe temporary directory")
            .inspect_err(|e| warn!("{e:?}"));

        // Scarb is expected to generate only one compilation unit (for our stub package)
        // that will consist of this package and the `core` crate.
        // Therefore, we allow ourselves to liberally just look for any first usage of a package
        // named `core` in all compilation units components we got.
        metadata.compilation_units.into_iter().find_map(|compilation_unit| {
            compilation_unit
                .components
                .iter()
                .find(|component| component.name == CORELIB_CRATE_NAME)
                .map(|component| component.source_root().to_path_buf().into_std_path_buf())
        })
    };

    static CACHE: OnceLock<Option<PathBuf>> = OnceLock::new();
    CACHE.get_or_init(lookup).clone()
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
