use std::path::Path;

use cairo_lang_compiler::db::RootDatabase;

pub use self::manager::ProjectManager;
#[cfg(doc)]
use self::unmanaged_core_crate::UnmanagedCoreCrate;

mod cairo_project;
mod manager;
mod project_manifest_path;
mod scarb;
mod unmanaged_core_crate;

// TODO(mkaput): Remove `Send` bound when migrating to new threading architecture.
/// A single Cairo project manager.
trait Project: Send {
    /// Gets a list of files that, when changed, should trigger a project reload.
    ///
    /// This list may also include lockfiles.
    fn manifest_files(&self) -> Vec<&Path>;

    /// Gets the main manifest file of this project.
    fn main_manifest_file(&self) -> &Path;

    /// Forces the project to reload its state.
    fn reload(&mut self);

    /// States if this project crates depend on the unmanaged `core` crate.
    ///
    /// If this is true for any loaded projects, [`UnmanagedCoreCrate::apply_db_changes`] will be
    /// called before applying changes of these projects.
    fn requires_unmanaged_core(&self) -> bool;

    /// Set up database input according to the current state of this project.
    fn apply_db_changes(&self, db: &mut RootDatabase);
}
