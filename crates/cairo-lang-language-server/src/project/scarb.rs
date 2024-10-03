use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

use anyhow::{bail, ensure, Context, Result};
use cairo_lang_filesystem::db::{
    CORELIB_CRATE_NAME, CrateSettings, DependencySettings, Edition, ExperimentalFeaturesConfig,
};
use itertools::Itertools;
use scarb_metadata::{
    CompilationUnitComponentMetadata, CompilationUnitMetadata, Metadata, PackageMetadata,
};
use smol_str::{SmolStr, ToSmolStr};
use tracing::{debug, error, warn};

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
///
/// Package ID is used as a discriminator for all crates except `core`.
// FIXME(mkaput): Currently this logic is feeding all compilation units of the single package at
//  once. Often packages declare several targets (lib, starknet-contract, test), which currently
//  causes overriding of the crate within single call of this function. This is an UX problem, for
//  which we do not know the solution yet.
pub fn update_crate_roots(metadata: &Metadata, db: &mut AnalysisDatabase) {
    let mut crates = Vec::<Crate>::new();
    let mut crates_grouped_by_group_id = HashMap::new();

    for compilation_unit in &metadata.compilation_units {
        if compilation_unit.target.kind == "cairo-plugin" {
            debug!("skipping cairo plugin compilation unit: {}", compilation_unit.id);
            continue;
        }

        for component in &compilation_unit.components {
            let crate_name = component.name.as_str();

            let package = find_component_package(component, compilation_unit, metadata);
            if package.is_none() {
                error!("package for component is missing in scarb metadata: {crate_name}");
            }

            let edition = scarb_package_edition(&package, crate_name);
            let experimental_features = scarb_package_experimental_features(&package);
            let version = package.map(|p| p.version.clone());

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

            let cfg_set_from_scarb = scarb_cfg_set_to_cairo(
                component.cfg.as_ref().unwrap_or(&compilation_unit.cfg),
                crate_name,
            );

            // If `cfg_set` is not `None`, it overrides global cfg settings.
            // Therefore, we have to explicitly add `initial_cfg_set` to `cfg_set` of
            // workspace members to enable test code analysis.
            // For non-workspace members we only add `cfg(target: 'test')` to make sure
            // importing test items tagged with `cfg(test)`
            // from dependencies emits proper diagnostics.
            let cfg_set = if metadata.workspace.members.contains(&component.package) {
                cfg_set_from_scarb
                    .map(|cfg_set| cfg_set.union(&AnalysisDatabase::initial_cfg_set()))
            } else {
                cfg_set_from_scarb
                    .map(|cfg_set| cfg_set.union(&AnalysisDatabase::initial_cfg_set_for_deps()))
            };

            let dependencies = {
                let direct_dependencies = package
                    .map(|p| p.dependencies.iter().map(|dep| &*dep.name).collect::<HashSet<_>>())
                    .unwrap_or_default();

                compilation_unit
                    .components
                    .iter()
                    .filter(|component_as_dependency| {
                        direct_dependencies.contains(&*component_as_dependency.name) || {
                            // This is a hacky way of accommodating integration test components,
                            // which need to depend on the tested package.
                            if let Some(package) = find_component_package(
                                component_as_dependency,
                                compilation_unit,
                                metadata,
                            ) {
                                package.targets.iter().filter(|target| target.kind == "test").any(
                                    |target| {
                                        let group_name = target
                                            .params
                                            .get("group-id")
                                            .and_then(|g| g.as_str())
                                            .unwrap_or(&target.name);
                                        group_name == component.name
                                    },
                                )
                            } else {
                                false
                            }
                        }
                    })
                    .map(|component| {
                        let settings = DependencySettings {
                            discriminator: component_discriminator(component),
                        };
                        (component.name.clone(), settings)
                    })
                    .chain({
                        // Add the component itself to dependencies.
                        [(crate_name.into(), DependencySettings {
                            discriminator: component_discriminator(component),
                        })]
                    })
                    .collect()
            };

            let settings =
                CrateSettings { edition, version, dependencies, cfg_set, experimental_features };

            let custom_main_file_stems = (file_stem != "lib").then_some(vec![file_stem.into()]);

            let cr = Crate {
                name: crate_name.into(),
                discriminator: component_discriminator(component),
                root: root.into(),
                custom_main_file_stems,
                settings,
            };

            if compilation_unit.package == component.package {
                if let Some(group_id) = compilation_unit.target.params.get("group-id") {
                    if let Some(group_id) = group_id.as_str() {
                        if cr.custom_main_file_stems.is_none() {
                            error!(
                                "compilation unit component with name {} has `lib.cairo` root \
                                 file while being part of target grouped by group_id {group_id}",
                                crate_name
                            )
                        } else {
                            let crates = crates_grouped_by_group_id
                                .entry(group_id.to_string())
                                .or_insert(vec![]);
                            crates.push(cr);

                            continue;
                        }
                    } else {
                        error!(
                            "group-id for target {} was not a string",
                            compilation_unit.target.name
                        )
                    }
                }
            }

            crates.push(cr);
        }
    }

    // Merging crates grouped by group id into single crates.
    for (group_id, crs) in crates_grouped_by_group_id {
        if !crs.iter().map(|cr| (&cr.settings, &cr.root)).all_equal() {
            error!(
                "main crates of targets with group_id {group_id} had different settings and/or \
                 roots"
            )
        }
        let first_crate = &crs[0];

        // All crates within a group should have the same settings and root.
        let settings = first_crate.settings.clone();
        let root = first_crate.root.clone();
        // Name and discriminator don't really matter, so we take the first crate's ones.
        let name = first_crate.name.clone();
        let discriminator = first_crate.discriminator.clone();

        let custom_main_file_stems =
            crs.into_iter().flat_map(|cr| cr.custom_main_file_stems.unwrap()).collect();

        crates.push(Crate {
            name,
            discriminator,
            root,
            custom_main_file_stems: Some(custom_main_file_stems),
            settings,
        });
    }

    debug!("updating crate roots from scarb metadata: {crates:#?}");

    if !crates.iter().any(Crate::is_core) {
        warn!("core crate is missing in scarb metadata, did not initialize it");
    }

    for cr in crates {
        cr.apply(db);
    }
}

/// Find package metadata associated with a specific compilation unit component.
fn find_component_package<'a>(
    component: &'a CompilationUnitComponentMetadata,
    compilation_unit: &'a CompilationUnitMetadata,
    metadata: &'a Metadata,
) -> Option<&'a PackageMetadata> {
    let mut package = metadata.packages.iter().find(|package| package.id == component.package);

    // For some compilation units corresponding to integration tests, a main package of the
    // compilation unit may not be found in a list of packages
    // (metadata hack, intended by scarb).
    // We instead find a package that specifies the target of the compilation unit and later use it
    // to extract the edition and experimental features.
    if package.is_none() && compilation_unit.package == component.package {
        package = metadata
            .packages
            .iter()
            .find(|p| p.targets.iter().any(|t| t.name == compilation_unit.target.name));
    }

    package
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

/// Get crate discriminator from component metadata.
///
/// This function properly handles the fact that the `core` crate cannot have a discriminator.
fn component_discriminator(component: &CompilationUnitComponentMetadata) -> Option<SmolStr> {
    (component.name != CORELIB_CRATE_NAME).then(|| component.package.to_smolstr())
}
