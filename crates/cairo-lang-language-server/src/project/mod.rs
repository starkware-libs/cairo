use std::collections::HashSet;
use std::path::{Path, PathBuf};

use anyhow::Context;
use cairo_lang_compiler::db::validate_corelib;
use cairo_lang_compiler::project::{setup_project, update_crate_roots_from_project_config};
use cairo_lang_project::ProjectConfig;
use tracing::{error, trace, warn};

pub use self::crate_data::Crate;
pub use self::project_manifest_path::*;
use crate::config::Config;
use crate::lang::db::AnalysisDatabase;
use crate::lsp::ext::{CorelibVersionMismatch, ScarbMetadataFailed};
use crate::project::scarb::{get_workspace_members_manifests, update_crate_roots};
use crate::project::unmanaged_core_crate::try_to_init_unmanaged_core;
use crate::server::client::Notifier;
use crate::state::Owned;
use crate::toolchain::scarb::ScarbToolchain;

mod crate_data;
mod project_manifest_path;
// TODO(mkaput): These two are `pub` temporarily.
pub(crate) mod scarb;
pub(crate) mod unmanaged_core_crate;

pub struct ProjectController {
    loaded_scarb_manifests: Owned<HashSet<PathBuf>>,
}

impl ProjectController {
    pub fn new() -> Self {
        ProjectController { loaded_scarb_manifests: Default::default() }
    }

    /// Tries to detect the crate root the config that contains a cairo file, and add it to the
    /// system.
    #[tracing::instrument(skip_all)]
    pub fn detect_crate_for(
        &mut self,
        db: &mut AnalysisDatabase,
        scarb_toolchain: &ScarbToolchain,
        config: &Config,
        file_path: &Path,
        notifier: &Notifier,
    ) {
        match ProjectManifestPath::discover(file_path) {
            Some(ProjectManifestPath::Scarb(manifest_path)) => {
                if self.loaded_scarb_manifests.contains(&manifest_path) {
                    trace!("scarb project is already loaded: {}", manifest_path.display());
                    return;
                }

                let metadata = scarb_toolchain
                    .metadata(&manifest_path)
                    .with_context(|| {
                        format!("failed to refresh scarb workspace: {}", manifest_path.display())
                    })
                    .inspect_err(|err| {
                        warn!("{err:?}");
                        notifier.notify::<ScarbMetadataFailed>(());
                    })
                    .ok();

                if let Some(metadata) = metadata {
                    self.loaded_scarb_manifests.extend(get_workspace_members_manifests(&metadata));
                    update_crate_roots(&metadata, db);
                } else {
                    // Try to set up a corelib at least.
                    try_to_init_unmanaged_core(db, config, scarb_toolchain);
                }

                if let Err(result) = validate_corelib(db) {
                    notifier.notify::<CorelibVersionMismatch>(result.to_string());
                }
            }

            Some(ProjectManifestPath::CairoProject(config_path)) => {
                // The base path of ProjectConfig must be absolute to ensure that all paths in Salsa
                // DB will also be absolute.
                assert!(config_path.is_absolute());

                try_to_init_unmanaged_core(db, config, scarb_toolchain);

                if let Ok(config) = ProjectConfig::from_file(&config_path) {
                    update_crate_roots_from_project_config(db, &config);
                };
            }

            None => {
                try_to_init_unmanaged_core(db, config, scarb_toolchain);

                if let Err(err) = setup_project(&mut *db, file_path) {
                    let file_path_s = file_path.to_string_lossy();
                    error!("error loading file {file_path_s} as a single crate: {err}");
                }
            }
        }
    }

    pub fn clear_loaded_workspaces(&mut self) {
        self.loaded_scarb_manifests.clear()
    }
}
