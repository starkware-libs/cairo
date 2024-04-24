use std::path::Path;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_filesystem::ids::CAIRO_FILE_EXTENSION;
use tracing::{debug, error, trace};

use crate::config::Config;
use crate::project::cairo_project::CairoProject;
use crate::project::project_manifest_path::ProjectManifestPath;
use crate::project::scarb::ScarbWorkspace;
use crate::project::unmanaged_core_crate::UnmanagedCoreCrate;
use crate::project::Project;
use crate::toolchain::scarb::ScarbToolchain;

// TODO(mkaput): Test this extensively.
// FIXME(mkaput): Deal with situation when the project file is deleted/renamed.
// FIXME(mkaput): Deal with `scarb metadata` overriding `Scarb.lock` all the time.
// TODO(mkaput): Projects mutations should primarily be triggered by workspace-* requests.
// TODO(mkaput): Database should be only updated when some project's state actually changes.
//   To accomplish this, the best way would be for projects to compute a "digest" of their state
//   and compare it with the previous one to determine whether input changed and update is needed.
/// Manages all Cairo projects that are open in the server.
///
/// ## I/O
///
/// This object reacts to a bunch of events coming to the server:
/// - Filesystem operations:
///   - [`ProjectManager::on_file_opened`],
///   - [`ProjectManager::on_file_changed`],
/// - Configuration changes: [`ProjectManager::on_config_changed`],
/// - Manual reloads: [`ProjectManager::reload`].
///
/// As an output of processing these events, the manager updates the analysis database inputs with
/// the new state of projects, via the [`ProjectManager::apply_db_changes`] method.
pub struct ProjectManager {
    // FIXME(mkaput): Projects must be sorted from the most specific to the least specific,
    //   so that linear scanning for project related to a file would be correct.
    // FIXME(mkaput): Scarb workspace have an ability to self-modify their root path, and thus
    //   it is possible, that duplicate projects could happen during the lifetime of the server.
    //   These need to be deduplicated.
    /// List of loaded projects.
    projects: Vec<Box<dyn Project>>,

    /// The unmanaged `core` crate manager.
    unmanaged_core_crate: UnmanagedCoreCrate,

    /// The Scarb toolchain to use for all projects.
    scarb_toolchain: ScarbToolchain,
}

impl ProjectManager {
    /// Constructs a new [`ProjectManager`].
    pub fn new(config: &Config, scarb_toolchain: &ScarbToolchain) -> Self {
        Self {
            projects: Default::default(),
            unmanaged_core_crate: UnmanagedCoreCrate::new(config),
            scarb_toolchain: scarb_toolchain.clone(),
        }
    }

    /// Reacts to files being opened.
    ///
    /// This event allows the projects manager to learn about files outside editor workspaces.
    pub fn on_file_opened(&mut self, path: &Path) {
        match ProjectManifestPath::discover(path) {
            Some(manifest_path) if self.already_loaded(manifest_path.as_path()) => {
                trace!("project already loaded: {}", manifest_path.as_path().display());
            }
            Some(manifest_path) => {
                debug!("loading project: {}", manifest_path.as_path().display());
                let project = self.initialize_project(manifest_path);
                self.projects.push(project);
            }
            None if is_cairo_file(path) => {
                // TODO(mkaput): Implement detached files.
                error!("detached files are not implemented yet");
            }
            None => {
                trace!("ignoring file: {}", path.display());
            }
        }
    }

    /// Reacts to changes in the file system.
    pub fn on_file_changed(&mut self, path: &Path) {
        for project in &mut self.projects {
            if project.manifest_files().contains(&path) {
                debug!("reloading project, because file changed: {}", path.display());
                project.reload();
            }
        }
    }

    /// Reacts to changes in the [`Config`].
    pub fn on_config_changed(&mut self, config: &Config) {
        self.unmanaged_core_crate.on_config_changed(config);
    }

    /// Reload all projects.
    pub fn reload(&mut self) {
        for project in &mut self.projects {
            project.reload();
        }
    }

    /// Set up database input according to the current state of projects.
    pub fn apply_db_changes(&self, db: &mut RootDatabase) {
        if self.projects.iter().any(|p| p.requires_unmanaged_core()) {
            self.unmanaged_core_crate.apply_db_changes(db);
        }

        for project in &self.projects {
            project.apply_db_changes(db);
        }
    }

    /// Create a new [`Project`] for the given manifest path.
    fn initialize_project(&self, manifest: ProjectManifestPath) -> Box<dyn Project> {
        match manifest {
            ProjectManifestPath::CairoProject(manifest_path) => {
                Box::new(CairoProject::initialize(manifest_path))
            }
            ProjectManifestPath::Scarb(manifest_path) => {
                Box::new(ScarbWorkspace::initialize(manifest_path, &self.scarb_toolchain))
            }
        }
    }

    /// Checks if a project under given manifest path is already loaded.
    fn already_loaded(&self, manifest_path: &Path) -> bool {
        self.projects.iter().any(|project| project.manifest_files().contains(&manifest_path))
    }
}

fn is_cairo_file(path: &Path) -> bool {
    path.extension().map_or(false, |ext| ext == CAIRO_FILE_EXTENSION)
}
