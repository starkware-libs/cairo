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
use crate::server::schedule::thread;
use crate::server::schedule::thread::{JoinHandle, ThreadPriority};
use crate::state::State;
use crate::toolchain::scarb::ScarbToolchain;

mod crate_data;
mod project_manifest_path;
mod scarb;
mod unmanaged_core_crate;

pub struct ProjectController {
    // NOTE: Member order matters here.
    //   The request sender MUST be dropped before controller's thread join handle.
    //   Otherwise, the controller thread will never stop, and the controller's
    //   JoinHandle drop will cause deadlock by waiting for the thread to join.
    requests_sender: Sender<ProjectControllerRequest>,
    response_receiver: Receiver<ProjectUpdate>,
    _thread: JoinHandle,
}

impl ProjectController {
    /// Initializes [`ProjectController`] by spawning a background thread to handle extracting
    /// project updates for files and initializing channels needed for communication with the
    /// thread.
    ///
    /// The background thread is responsible for fetching changes to the project model: check
    /// [`ProjectControllerThread::send_project_update_for_file`] for more information.
    pub fn initialize(scarb_toolchain: ScarbToolchain) -> Self {
        let (requests_sender, requests_receiver) = crossbeam::channel::unbounded();
        let (response_sender, response_receiver) = crossbeam::channel::unbounded();

        let thread =
            ProjectControllerThread::spawn(requests_receiver, response_sender, scarb_toolchain);

        ProjectController { requests_sender, response_receiver, _thread: thread }
    }

    pub fn response_receiver(&self) -> Receiver<ProjectUpdate> {
        self.response_receiver.clone()
    }

    pub fn request_updating_project_for_file(&self, file_path: PathBuf) {
        self.send_request(ProjectControllerRequest::SendProjectUpdate(file_path))
    }

    pub fn request_clearing_loaded_workspaces(&self) {
        self.send_request(ProjectControllerRequest::ClearLoadedWorkspaces)
    }

    /// Handles project update by applying necessary changes to the database.
    ///
    /// The project update is sent from [`ProjectControllerThread::send_project_update_for_file`]
    /// and received in the main [`event loop`](crate::Backend::event_loop).
    #[tracing::instrument(skip_all, fields(project_update))]
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

    /// Sends an action request to the background thread.
    fn send_request(&self, request: ProjectControllerRequest) {
        self.requests_sender
            .send(request)
            .inspect_err(|_err| error!("failed to send a request to the project controller thread"))
            .expect("project controller thread should not have panicked or dropped the receiver")
    }
}

/// Intermediate struct used to communicate what changes to the project model should be applied.
/// Associated with [`ProjectManifestPath`] (or its absence) that was detected for a given file.
pub enum ProjectUpdate {
    Scarb(Option<Vec<Crate>>),
    CairoProjectToml(Option<ProjectConfig>),
    NoConfig(PathBuf),
}

/// Stores entire state of project controller thread.
struct ProjectControllerThread {
    loaded_scarb_manifests: HashSet<PathBuf>,
    requests_receiver: Receiver<ProjectControllerRequest>,
    response_sender: Sender<ProjectUpdate>,
    scarb_toolchain: ScarbToolchain,
}

impl ProjectControllerThread {
    /// Spawns a new project controller thread and returns a handle to it.
    fn spawn(
        requests_receiver: Receiver<ProjectControllerRequest>,
        response_sender: Sender<ProjectUpdate>,
        scarb_toolchain: ScarbToolchain,
    ) -> JoinHandle {
        let this = Self {
            loaded_scarb_manifests: Default::default(),
            requests_receiver,
            response_sender,
            scarb_toolchain,
        };

        thread::Builder::new(ThreadPriority::Worker)
            .name("cairo-ls:project-controller".into())
            .spawn(move || this.event_loop())
            .expect("failed to spawn project controller thread")
    }

    /// Runs project controller's event loop.
    fn event_loop(mut self) {
        while let Ok(request) = self.requests_receiver.recv() {
            match request {
                ProjectControllerRequest::SendProjectUpdate(file_path) => {
                    let project_update = self.fetch_project_update_for_file(&file_path);
                    if let Some(project_update) = project_update {
                        self.send_project_update(project_update);
                    }
                }
                ProjectControllerRequest::ClearLoadedWorkspaces => {
                    self.loaded_scarb_manifests.clear()
                }
            }
        }
    }

    /// Tries to fetch changes to the project model that are necessary for the file analysis.
    ///
    /// NOTE: this function is potentially expensive as it may call `scarb metadata`.
    /// It is meant to be run only in the background thread.
    #[tracing::instrument(skip_all)]
    fn fetch_project_update_for_file(&mut self, file_path: &Path) -> Option<ProjectUpdate> {
        let project_update = match ProjectManifestPath::discover(file_path) {
            Some(ProjectManifestPath::Scarb(manifest_path)) => {
                if self.loaded_scarb_manifests.contains(&manifest_path) {
                    trace!("scarb project is already loaded: {}", manifest_path.display());
                    return None;
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

        Some(project_update)
    }

    ///  Sends [`ProjectUpdate`] to the main [`event loop`](crate::Backend::event_loop).
    fn send_project_update(&self, project_update: ProjectUpdate) {
        self.response_sender
            .send(project_update)
            .expect("the receiver was expected to exist in the main event loop");
    }
}

enum ProjectControllerRequest {
    SendProjectUpdate(PathBuf),
    ClearLoadedWorkspaces,
}
