use std::ffi::OsStr;
use std::path::Path;
use std::sync::Arc;

use defs::db::DefsGroup;
use defs::ids::ModuleId;
use filesystem::db::{AsFilesGroupMut, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateId, CrateLongId, Directory};
pub use project::*;

use crate::db::RootDatabase;

#[derive(thiserror::Error, Debug)]
pub enum ProjectError {
    #[error("Only files with .cairo extension can be compiled.")]
    BadFileExtension,
    #[error("Couldn't read {path}: No such file.")]
    NoSuchFile { path: String },
    #[error("Couldn't handle {path}: Not a legal path.")]
    BadPath { path: String },
    #[error("Failed to load project config.")]
    LoadProjectError,
}

/// Setup to 'db' to compile the file at the given path.
/// Returns the id of the generated crate.
fn setup_single_file_project(db: &mut RootDatabase, path: &Path) -> Result<CrateId, ProjectError> {
    match path.extension().and_then(OsStr::to_str) {
        Some("cairo") => (),
        _ => {
            return Err(ProjectError::BadFileExtension);
        }
    }
    if !path.exists() {
        return Err(ProjectError::NoSuchFile { path: path.to_string_lossy().to_string() });
    }
    let bad_path_err = || ProjectError::BadPath { path: path.to_string_lossy().to_string() };
    let file_stemp = path.file_stem().and_then(OsStr::to_str).ok_or_else(bad_path_err)?;
    if file_stemp == "lib" {
        let canonical = path.canonicalize().map_err(|_| bad_path_err())?;
        let crate_name =
            canonical.parent().ok_or_else(bad_path_err)?.to_str().ok_or_else(bad_path_err)?;
        let crate_id = db.intern_crate(CrateLongId(crate_name.into()));
        db.set_crate_root(crate_id, Some(Directory(path.to_path_buf())));
        Ok(crate_id)
    } else {
        // If file_stemp is not lib, create a fake lib file.
        let crate_id = db.intern_crate(CrateLongId(file_stemp.into()));
        db.set_crate_root(crate_id, Some(Directory(path.parent().unwrap().to_path_buf())));

        let module_id = ModuleId::CrateRoot(crate_id);
        let file_id = db.module_main_file(module_id).unwrap();
        db.as_files_group_mut()
            .override_file_content(file_id, Some(Arc::new(format!("mod {};", file_stemp))));
        Ok(crate_id)
    }
}

/// Setup the 'db' to compile the project in the given path.
/// The path can be either a directory with cairo project file or a .cairo file.
/// Returns the ids of the project crates.
pub fn setup_project(db: &mut RootDatabase, path: &Path) -> Result<Vec<CrateId>, ProjectError> {
    if path.is_dir() {
        match ProjectConfig::from_directory(path) {
            Ok(config) => {
                let main_crate_ids = get_main_crate_ids_from_project(db, &config);
                db.with_project_config(config);
                Ok(main_crate_ids)
            }
            _ => Err(ProjectError::LoadProjectError),
        }
    } else {
        Ok(vec![setup_single_file_project(db, path)?])
    }
}

pub fn get_main_crate_ids_from_project(
    db: &mut RootDatabase,
    config: &ProjectConfig,
) -> Vec<CrateId> {
    config
        .content
        .crate_roots
        .keys()
        .map(|crate_id| db.intern_crate(CrateLongId(crate_id.clone())))
        .collect()
}
