use std::fs;
use std::path::Path;

use anyhow::{bail, ensure, Context, Result};
use cairo_lang_filesystem::db::{CrateSettings, Edition, ExperimentalFeaturesConfig};
use scarb_metadata::{Metadata, PackageMetadata};
use tracing::{debug, warn};

use crate::lang::db::AnalysisDatabase;
use crate::project::crate_data::Crate;

/// Updates crate roots in the database with the information from Scarb metadata.
///
/// This function attempts to be graceful. Any erroneous cases will be reported as warnings in logs,
/// and the database will be left intact for problematic crates.
///
/// In all real-world scenarios, this function should always initialize the `core` crate.
/// Technically, it is possible for `scarb metadata` to omit `core` if working on a `no-core`
/// package, but in reality enabling `no-core` makes sense only for the `core` package itself. To
/// leave a trace of unreal cases, this function will log a warning if `core` is missing.
// FIXME(mkaput): Currently this logic is feeding all compilation units of the single package at
//  once. Often packages declare several targets (lib, starknet-contract, test), which currently
//  causes overriding of the crate within single call of this function. This is an UX problem, for
//  which we do not know the solution yet.
pub fn update_crate_roots(metadata: &Metadata, db: &mut AnalysisDatabase) {
    let mut crates = Vec::<Crate>::new();
    for compilation_unit in &metadata.compilation_units {
        if compilation_unit.target.kind == "cairo-plugin" {
            debug!("skipping cairo plugin compilation unit: {}", compilation_unit.id);
            continue;
        }

        for component in &compilation_unit.components {
            let crate_name = component.name.as_str();

            let package = metadata.packages.iter().find(|package| package.id == component.package);
            if package.is_none() {
                warn!("package for component is missing in scarb metadata: {crate_name}");
            }

            let (root, file_stem) = match validate_and_chop_source_path(
                component.source_path.as_std_path(),
                crate_name,
            ) {
                Ok(t) => t,
                Err(e) => {
                    warn!("{e:?}");
                    continue;
                }
            };

            let settings = CrateSettings {
                edition: scarb_package_edition(&package, crate_name),
                cfg_set: scarb_cfg_set_to_cairo(
                    component.cfg.as_ref().unwrap_or(&compilation_unit.cfg),
                    crate_name,
                ),
                experimental_features: scarb_package_experimental_features(&package),
            };

            let custom_main_file_stem = (file_stem != "lib").then_some(file_stem.into());

            crates.push(Crate {
                name: crate_name.into(),
                root: root.into(),
                custom_main_file_stem,
                settings,
            });
        }
    }

    debug!("updating crate roots from scarb metadata: {crates:#?}");

    if !crates.iter().any(Crate::is_core) {
        warn!("core crate is missing in scarb metadata, did not initialize it");
    }

    for cr in crates {
        cr.apply(db);
    }
}

/// Perform sanity checks on crate _source path_, and chop it into directory path and file stem.
fn validate_and_chop_source_path<'a>(
    source_path: &'a Path,
    crate_name: &str,
) -> Result<(&'a Path, &'a str)> {
    let metadata = fs::metadata(source_path)
        .with_context(|| format!("io error when accessing source path of: {crate_name}"))?;

    ensure!(
        !metadata.is_dir(),
        "source path of component `{crate_name}` must not be a directory: {source_path}",
        source_path = source_path.display()
    );

    let Some(root) = source_path.parent() else {
        bail!(
            "unexpected fs root as a source path of component `{crate_name}`: {source_path}",
            source_path = source_path.display()
        );
    };

    ensure!(
        root.is_absolute(),
        "source path must be absolute: {source_path}",
        source_path = source_path.display()
    );

    let Some(file_stem) = source_path.file_stem() else {
        bail!(
            "failed to get file stem for component `{crate_name}`: {source_path}",
            source_path = source_path.display()
        );
    };

    let Some(file_stem) = file_stem.to_str() else {
        bail!("file stem is not utf-8: {source_path}", source_path = source_path.display());
    };

    Ok((root, file_stem))
}

/// Get the [`Edition`] from [`PackageMetadata`], or assume the default edition.
fn scarb_package_edition(package: &Option<&PackageMetadata>, crate_name: &str) -> Edition {
    package
        .and_then(|p| p.edition.clone())
        .and_then(|e| {
            serde_json::from_value(e.into())
                .with_context(|| format!("failed to parse edition of package: {crate_name}"))
                .inspect_err(|e| warn!("{e:?}"))
                .ok()
        })
        .unwrap_or_default()
}

/// Convert a slice of [`scarb_metadata::Cfg`]s to a [`cairo_lang_filesystem::cfg::CfgSet`].
///
/// The conversion is done the same way as in Scarb (except no panicking):
/// <https://github.com/software-mansion/scarb/blob/9fe97c8eb8620a1e2103e7f5251c5a9189e75716/scarb/src/ops/metadata.rs#L295-L302>
fn scarb_cfg_set_to_cairo(
    cfg_set: &[scarb_metadata::Cfg],
    crate_name: &str,
) -> Option<cairo_lang_filesystem::cfg::CfgSet> {
    serde_json::to_value(cfg_set)
        .and_then(serde_json::from_value)
        .with_context(|| {
            format!(
                "scarb metadata cfg did not convert identically to cairo one for crate: \
                 {crate_name}"
            )
        })
        .inspect_err(|e| warn!("{e:?}"))
        .ok()
}

/// Get [`ExperimentalFeaturesConfig`] from [`PackageMetadata`] fields.
fn scarb_package_experimental_features(
    package: &Option<&PackageMetadata>,
) -> ExperimentalFeaturesConfig {
    let contains = |feature: &str| -> bool {
        let Some(package) = package else { return false };
        package.experimental_features.iter().any(|f| f == feature)
    };

    ExperimentalFeaturesConfig {
        negative_impls: contains("negative_impls"),
        coupons: contains("coupons"),
    }
}
