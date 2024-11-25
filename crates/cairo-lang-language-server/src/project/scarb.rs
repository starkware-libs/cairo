use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail, ensure};
use cairo_lang_filesystem::cfg::CfgSet;
use cairo_lang_filesystem::db::{
    CrateSettings, DependencySettings, Edition, ExperimentalFeaturesConfig,
};
use itertools::Itertools;
use scarb_metadata::{
    CompilationUnitComponentDependencyMetadata, CompilationUnitComponentId, Metadata,
    PackageMetadata,
};
use smol_str::ToSmolStr;
use tracing::{debug, error, warn};

use crate::lang::db::AnalysisDatabase;
use crate::project::crate_data::Crate;

/// Get paths to manifests of the workspace members.
pub fn get_workspace_members_manifests(metadata: &Metadata) -> Vec<PathBuf> {
    metadata
        .workspace
        .members
        .iter()
        .map(|package_id| metadata[package_id].manifest_path.clone().into())
        .collect()
}

/// Extract information about crates that should be loaded to db from Scarb metadata.
///
/// This function attempts to be graceful. Any erroneous cases will be reported as warnings in logs.
///
/// In all real-world scenarios, this function should always extract info about the `core` crate.
/// Technically, it is possible for `scarb metadata` to omit `core` if working on a `no-core`
/// package, but in reality enabling `no-core` makes sense only for the `core` package itself. To
/// leave a trace of unreal cases, this function will log a warning if `core` is missing.
// FIXME(mkaput): Currently this logic is collecting all compilation units of the single package at
//  once. Often packages declare several targets (lib, starknet-contract, test), which currently
//  causes overriding of the crate within single call of this function. This is an UX problem, for
//  which we do not know the solution yet.
pub fn extract_crates(metadata: &Metadata) -> Vec<Crate> {
    // A crate can appear as a component in multiple compilation units.
    // We use a map here to make sure we include dependencies and cfg sets from all CUs.
    // We can keep components with assigned group id separately as they are not affected by this;
    // they are parts of integration tests crates which cannot appear in multiple compilation units.
    let mut crates_by_component_id: HashMap<CompilationUnitComponentId, Crate> = HashMap::new();
    let mut crates_grouped_by_group_id = HashMap::new();

    for compilation_unit in &metadata.compilation_units {
        if compilation_unit.target.kind == "cairo-plugin" {
            debug!("skipping cairo plugin compilation unit: {}", compilation_unit.id);
            continue;
        }

        for component in &compilation_unit.components {
            let crate_name = component.name.as_str();
            let Some(component_id) = component.id.clone() else {
                error!("id of component {crate_name} was None in metadata");
                continue;
            };

            let mut package =
                metadata.packages.iter().find(|package| package.id == component.package);

            // For some compilation units corresponding to integration tests,
            // a main package of the compilation unit may not be found in a list of packages
            // (metadata hack, intended by scarb).
            // We instead find a package that specifies the target of the compilation unit
            // and later use it to extract edition and experimental features.
            if package.is_none() && compilation_unit.package == component.package {
                package = metadata
                    .packages
                    .iter()
                    .find(|p| p.targets.iter().any(|t| t.name == compilation_unit.target.name));
            }

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
            }
            .map(|cfg_set| {
                let empty_cfg_set = CfgSet::new();
                let previous_cfg_set = crates_by_component_id
                    .get(&component_id)
                    .and_then(|cr| cr.settings.cfg_set.as_ref())
                    .unwrap_or(&empty_cfg_set);

                cfg_set.union(previous_cfg_set)
            });

            let dependencies = component
                .dependencies
                .as_deref()
                .unwrap_or_else(|| {
                    error!(
                        "dependencies of component {crate_name} with id {component_id:?} not \
                         found in metadata",
                    );
                    &[]
                })
                .iter()
                .filter_map(|CompilationUnitComponentDependencyMetadata { id, .. }| {
                    let dependency_component = compilation_unit
                        .components
                        .iter()
                        .find(|component| component.id.as_ref() == Some(id));

                    if let Some(dependency_component) = dependency_component {
                        Some((dependency_component.name.clone(), DependencySettings {
                            discriminator: dependency_component
                                .discriminator
                                .as_ref()
                                .map(ToSmolStr::to_smolstr),
                        }))
                    } else {
                        error!("component not found in metadata");
                        None
                    }
                })
                .chain(
                    crates_by_component_id
                        .get(&component_id)
                        .map(|cr| cr.settings.dependencies.clone())
                        .unwrap_or_default(),
                )
                .collect();

            let settings = CrateSettings {
                name: Some(crate_name.into()),
                edition,
                version,
                dependencies,
                cfg_set,
                experimental_features,
            };

            let custom_main_file_stems = (file_stem != "lib").then_some(vec![file_stem.into()]);

            let cr = Crate {
                name: crate_name.into(),
                discriminator: component.discriminator.as_ref().map(ToSmolStr::to_smolstr),
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

            crates_by_component_id.insert(component_id, cr);
        }
    }

    let mut crates: Vec<_> = crates_by_component_id.into_values().collect();

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

    if !crates.iter().any(Crate::is_core) {
        warn!("core crate is missing in scarb metadata, did not initialize it");
    }

    crates
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
