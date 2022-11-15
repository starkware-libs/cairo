use std::ffi::OsStr;
use std::path::Path;
use std::sync::Arc;

use defs::db::DefsGroup;
use defs::ids::ModuleId;
use filesystem::db::{AsFilesGroupMut, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateLongId, Directory};
use project::ProjectConfig;

use crate::db::RootDatabase;

#[derive(thiserror::Error, Debug)]
pub enum ProjectError {
    #[error("Only files with .cairo extension can be compiled.")]
    BadFileExtension,
    #[error("Couldn't read {path}: No such file.")]
    NoSuchFile { path: String },
    #[error("Failed to load project config.")]
    LoadProjectError,
}

/// Setup to 'db' to compile the file at the given path.
fn setup_single_file_project(db: &mut RootDatabase, path: &Path) -> Result<(), ProjectError> {
    match path.extension().and_then(OsStr::to_str) {
        Some("cairo") => (),
        _ => {
            return Err(ProjectError::BadFileExtension);
        }
    }

    if let Some(file_stemp) = path.file_stem().and_then(OsStr::to_str) {
        if file_stemp != "lib" {
            // If file_stemp is not lib, create a fake lib file.

            if !path.exists() {
                return Err(ProjectError::NoSuchFile { path: path.to_string_lossy().to_string() });
            }

            let crate_id = db.intern_crate(CrateLongId(file_stemp.into()));
            db.set_crate_root(crate_id, Some(Directory(path.parent().unwrap().to_path_buf())));

            let module_id = ModuleId::CrateRoot(crate_id);
            let file_id = db.module_file(module_id).unwrap();
            db.as_files_group_mut()
                .override_file_content(file_id, Some(Arc::new(format!("mod {};", file_stemp))));
        }
    }
    Ok(())
}

// Setup the 'db' to compile the project in the given path.
// The path can be either a directory with cairo project file or a .cairo file.
pub fn setup_project(db: &mut RootDatabase, path: &Path) -> Result<(), ProjectError> {
    if path.is_dir() {
        match ProjectConfig::from_directory(path) {
            Ok(config) => {
                db.with_project_config(config);
                Ok(())
            }
            _ => Err(ProjectError::LoadProjectError),
        }
    } else {
        setup_single_file_project(db, path)
    }
}
