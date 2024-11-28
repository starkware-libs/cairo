use std::cell::OnceCell;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

use anyhow::Context;
use cairo_lang_compiler::db::validate_corelib;
use cairo_lang_compiler::project::{setup_project, update_crate_roots_from_project_config};
use cairo_lang_project::ProjectConfig;
use crossbeam::channel::{Receiver, Sender};
use tracing::{debug, error, trace, warn};

pub use self::crate_data::Crate;
pub use self::project_manifest_path::*;
use crate::lsp::ext::CorelibVersionMismatch;
use crate::project::scarb::{extract_crates, get_workspace_members_manifests};
use crate::project::unmanaged_core_crate::try_to_init_unmanaged_core;
use crate::server::client::Notifier;
use crate::state::{Owned, State};
use crate::toolchain::scarb::ScarbToolchain;

mod crate_data;
mod project_manifest_path;
mod scarb;
mod unmanaged_core_crate;

pub struct ProjectController {
    loaded_scarb_manifests: Owned<HashSet<PathBuf>>,
    sender: OnceCell<Sender<ProjectUpdate>>,
    scarb_toolchain: ScarbToolchain,
}

impl ProjectController {
    pub fn new(scarb_toolchain: ScarbToolchain) -> Self {
        ProjectController {
            loaded_scarb_manifests: Default::default(),
            sender: Default::default(),
            scarb_toolchain,
        }
    }

    pub fn init_channel(&mut self) -> Receiver<ProjectUpdate> {
        let (sender, receiver) = crossbeam::channel::unbounded();

        self.sender.set(sender).expect("failed to set sender once cell");
        receiver
    }

    /// Tries to fetch changes to the project model that are necessary for the file analysis.
    #[tracing::instrument(skip_all)]
    pub fn update_project_for_file(&mut self, file_path: &Path) {
        let project_update = match ProjectManifestPath::discover(file_path) {
            Some(ProjectManifestPath::Scarb(manifest_path)) => {
                if self.loaded_scarb_manifests.contains(&manifest_path) {
                    trace!("scarb project is already loaded: {}", manifest_path.display());
                    return;
                }

                let metadata = self
                    .scarb_toolchain
                    .metadata(&manifest_path)
                    .with_context(|| {
                        format!("failed to refresh scarb workspace: {}", manifest_path.display())
                    })
                    .inspect_err(|err| {
                        error!("{err:?}");
                    })
                    .ok();

                let maybe_crates = if let Some(metadata) = metadata {
                    self.loaded_scarb_manifests.extend(get_workspace_members_manifests(&metadata));
                    Some(extract_crates(&metadata))
                } else {
                    None
                };

                ProjectUpdate::Scarb(maybe_crates)
            }

            Some(ProjectManifestPath::CairoProject(config_path)) => {
                // The base path of ProjectConfig must be absolute to ensure that all paths in Salsa
                // DB will also be absolute.
                assert!(config_path.is_absolute());

                let maybe_project_config = ProjectConfig::from_file(&config_path)
                    // TODO: send failure notification
                    .inspect_err(|err| error!("{err:?}"))
                    .ok();
                ProjectUpdate::CairoProjectToml(maybe_project_config)
            }

            None => ProjectUpdate::NoConfig(file_path.to_path_buf()),
        };

        self.sender
            .get()
            .expect("failed to get sender once cell")
            .send(project_update)
            .expect("the receiver was expected to exist in the main event loop");
    }

    pub fn handle_update(state: &mut State, notifier: Notifier, project_update: ProjectUpdate) {
        let db = &mut state.db;
        match project_update {
            ProjectUpdate::Scarb(crates) => {
                if let Some(crates) = crates {
                    debug!("updating crate roots from scarb metadata: {crates:#?}");

                    for cr in crates {
                        cr.apply(db);
                    }
                } else {
                    // Try to set up a corelib at least.
                    try_to_init_unmanaged_core(db, &state.config, &state.scarb_toolchain);
                }
            }
            ProjectUpdate::CairoProjectToml(maybe_project_config) => {
                try_to_init_unmanaged_core(db, &state.config, &state.scarb_toolchain);

                if let Some(project_config) = maybe_project_config {
                    update_crate_roots_from_project_config(db, &project_config);
                }
            }
            ProjectUpdate::NoConfig(file_path) => {
                try_to_init_unmanaged_core(db, &state.config, &state.scarb_toolchain);

                if let Err(err) = setup_project(&mut *db, &file_path) {
                    let file_path_s = file_path.to_string_lossy();
                    error!("error loading file {file_path_s} as a single crate: {err}");
                }
            }
        }

        if let Err(result) = validate_corelib(db) {
            notifier.notify::<CorelibVersionMismatch>(result.to_string());
        }
    }

    pub fn clear_loaded_workspaces(&mut self) {
        self.loaded_scarb_manifests.clear()
    }
}

pub enum ProjectUpdate {
    Scarb(Option<Vec<Crate>>),
    CairoProjectToml(Option<ProjectConfig>),
    NoConfig(PathBuf),
}
