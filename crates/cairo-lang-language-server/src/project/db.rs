use std::path::Path;
use std::sync::Arc;

use cairo_lang_filesystem::ids::CAIRO_FILE_EXTENSION;
use cairo_lang_utils::{Intern, LookupIntern, Upcast};
use salsa::Durability;
use tracing::{debug, error, trace};

use crate::config::Config;
use crate::lang::db::AnalysisDatabase;
use crate::project::digests::{invalidate_digest, Digestible, LsDigestsDatabase, LsDigestsGroup};
use crate::project::main::{LsProjectsDatabase, LsProjectsGroup};
use crate::project::project_manifest_path::ProjectManifestPath;
use crate::project::unmanaged_core_crate::{
    set_unmanaged_core_inputs_from_config, LsUnmanagedCoreDatabase, LsUnmanagedCoreGroup,
};
use crate::toolchain::scarb::ScarbToolchain;

// FIXME(mkaput): Deal with situation when the project file is deleted/renamed.
// FIXME(mkaput): Deal with `scarb metadata` overriding `Scarb.lock` all the time.
// TODO(mkaput): Test Scarb workspaces.
// TODO(mkaput): Projects mutations should primarily be triggered by workspace-* requests.
// TODO(mkaput): Project loading should happen asynchronously.
// TODO(mkaput): LS should gracefully handle cases when loading a project fails,
//   for example due to syntax error in the manifest file.
/// A Salsa database that stores information about projects.
///
/// ## I/O
///
/// This database reacts to a bunch of events coming from the server:
/// - Filesystem operations:
///   - [`ProjectsDatabase::on_file_opened`],
///   - [`ProjectsDatabase::on_file_changed`],
/// - Configuration changes: [`ProjectsDatabase::on_config_changed`],
/// - Manual reloads: [`ProjectsDatabase::reload`].
///
/// As an output of processing these events, the database can make updates to the
/// [`AnalysisDatabase`] inputs with the new state of projects, via the [`ProjectsDatabase::apply`]
/// method.
#[salsa::database(LsDigestsDatabase, LsUnmanagedCoreDatabase, LsProjectsDatabase)]
pub struct ProjectsDatabase {
    storage: salsa::Storage<Self>,
    scarb_toolchain: ScarbToolchain,
}

impl salsa::Database for ProjectsDatabase {}

impl ProjectsDatabase {
    /// Creates a new instance of the database.
    pub fn new(config: &Config, scarb_toolchain: &ScarbToolchain) -> Self {
        let mut db = Self { storage: Default::default(), scarb_toolchain: scarb_toolchain.clone() };
        db.set_open_projects(Default::default());
        db.on_config_changed(config);
        db
    }

    /// Reacts to files being opened.
    ///
    /// This event allows the projects manager to learn about files outside editor workspaces.
    pub fn on_file_opened(&mut self, path: &Path) {
        match ProjectManifestPath::discover(path) {
            Some(manifest_path) => {
                self.open_project(manifest_path);
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
        if let Some(digest) = Digestible::try_new(path) {
            let digest = digest.intern(self);

            // Invalidate the digest of the changed file, causing relevant projects to reload.
            invalidate_digest(self, digest);
        }
    }

    /// Reacts to changes in the [`Config`].
    pub fn on_config_changed(&mut self, config: &Config) {
        set_unmanaged_core_inputs_from_config(self, config);
    }

    /// Invalidate all queries preserving inputs.
    pub fn reload(&mut self) {
        // Preserve essential inputs before wiping the database.
        let open_projects =
            self.open_projects().iter().map(|id| id.lookup_intern(self)).collect::<Vec<_>>();
        let config_unmanaged_core_path = self.config_unmanaged_core_path();

        // Wipe the database.
        self.storage = Default::default();

        // Restore inputs.
        self.set_config_unmanaged_core_path_with_durability(
            config_unmanaged_core_path,
            Durability::HIGH,
        );
        self.set_open_projects(Arc::new(
            open_projects.into_iter().map(|project| project.intern(self)).collect(),
        ));
    }

    /// Set up database input according to the current state of projects.
    pub fn apply(&self, db: &mut AnalysisDatabase) {
        for cr in &*self.crates() {
            cr.apply(db);
        }
    }

    /// Add a [`ProjectManifestPath`] to the list of open projects.
    fn open_project(&mut self, manifest_path: ProjectManifestPath) {
        let id = manifest_path.clone().intern(self);

        let mut open_projects = self.open_projects();
        if open_projects.contains(&id) {
            trace!("project already opened: {manifest_path}");
        } else {
            debug!("opening project: {manifest_path}");
            Arc::make_mut(&mut open_projects).insert(id);
            self.set_open_projects(open_projects);
        }
    }
}

/// Accessors to the context fields of the projects' database.
pub trait ProjectsContext {
    /// Gets the Scarb toolchain to use for all projects.
    fn scarb_toolchain(&self) -> &ScarbToolchain;
}

impl ProjectsContext for ProjectsDatabase {
    fn scarb_toolchain(&self) -> &ScarbToolchain {
        &self.scarb_toolchain
    }
}

impl Upcast<dyn LsDigestsGroup> for ProjectsDatabase {
    fn upcast(&self) -> &(dyn LsDigestsGroup + 'static) {
        self
    }
}

impl Upcast<dyn LsProjectsGroup> for ProjectsDatabase {
    fn upcast(&self) -> &(dyn LsProjectsGroup + 'static) {
        self
    }
}

/// Checks if the file is a Cairo source file.
fn is_cairo_file(path: &Path) -> bool {
    path.extension().map_or(false, |ext| ext == CAIRO_FILE_EXTENSION)
}
