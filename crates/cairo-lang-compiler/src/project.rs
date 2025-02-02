use std::ffi::OsStr;
use std::path::Path;

use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::{
    CORELIB_CRATE_NAME, CrateConfiguration, CrateIdentifier, CrateSettings, FilesGroupEx,
};
use cairo_lang_filesystem::ids::{CrateId, CrateLongId, Directory};
pub use cairo_lang_project::*;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::Intern;

#[derive(thiserror::Error, Debug)]
pub enum ProjectError {
    #[error("Only files with .cairo extension can be compiled.")]
    BadFileExtension,
    #[error("Couldn't read {path}: No such file.")]
    NoSuchFile { path: String },
    #[error("Couldn't handle {path}: Not a legal path.")]
    BadPath { path: String },
    #[error("Failed to load project config: {0}")]
    LoadProjectError(DeserializationError),
}

/// Set up the 'db' to compile the file at the given path.
/// Returns the id of the generated crate.
pub fn setup_single_file_project(
    db: &mut dyn SemanticGroup,
    path: &Path,
) -> Result<CrateId, ProjectError> {
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
    let canonical = path.canonicalize().map_err(|_| bad_path_err())?;
    let file_dir = canonical.parent().ok_or_else(bad_path_err)?;
    let file_stem = path.file_stem().and_then(OsStr::to_str).ok_or_else(bad_path_err)?;
    if file_stem == "lib" {
        let crate_name = file_dir.to_str().ok_or_else(bad_path_err)?;
        let crate_id = CrateId::plain(db, crate_name);
        db.set_crate_config(
            crate_id,
            Some(CrateConfiguration::default_for_root(Directory::Real(file_dir.to_path_buf()))),
        );
        Ok(crate_id)
    } else {
        // If file_stem is not lib, create a fake lib file.
        let crate_id = CrateId::plain(db, file_stem);
        db.set_crate_config(
            crate_id,
            Some(CrateConfiguration::default_for_root(Directory::Real(file_dir.to_path_buf()))),
        );

        let module_id = ModuleId::CrateRoot(crate_id);
        let file_id = db.module_main_file(module_id).unwrap();
        db.as_files_group_mut()
            .override_file_content(file_id, Some(format!("mod {file_stem};").into()));
        Ok(crate_id)
    }
}

/// Updates the crate roots from a ProjectConfig object.
pub fn update_crate_roots_from_project_config(db: &mut dyn SemanticGroup, config: &ProjectConfig) {
    for (crate_identifier, directory_path) in config.content.crate_roots.iter() {
        let root = Directory::Real(config.absolute_crate_root(directory_path));
        update_crate_root(db, config, crate_identifier, root);
    }
}

/// Updates a single crate root from a ProjectConfig.
/// If the crate defines settings in the config, it will be used.
/// Crate is identified by name and the root directory.
pub fn update_crate_root(
    db: &mut dyn SemanticGroup,
    config: &ProjectConfig,
    crate_identifier: &CrateIdentifier,
    root: Directory,
) {
    let (crate_id, crate_settings) = get_crate_id_and_settings(db, crate_identifier, config);
    db.set_crate_config(
        crate_id,
        Some(CrateConfiguration { root, settings: crate_settings.clone(), cache_file: None }),
    );
}

/// Setup the 'db' to compile the project in the given path.
/// The path can be either a directory with cairo project file or a .cairo file.
/// Returns the ids of the project crates.
pub fn setup_project(
    db: &mut dyn SemanticGroup,
    path: &Path,
) -> Result<Vec<CrateId>, ProjectError> {
    if path.is_dir() {
        let config = ProjectConfig::from_directory(path).map_err(ProjectError::LoadProjectError)?;
        let main_crate_ids = get_main_crate_ids_from_project(db, &config);
        update_crate_roots_from_project_config(db, &config);
        Ok(main_crate_ids)
    } else {
        Ok(vec![setup_single_file_project(db, path)?])
    }
}

/// Checks that the given path is a valid compiler path.
pub fn check_compiler_path(single_file: bool, path: &Path) -> anyhow::Result<()> {
    if path.is_file() {
        if !single_file {
            anyhow::bail!("The given path is a file, but --single-file was not supplied.");
        }
    } else if path.is_dir() {
        if single_file {
            anyhow::bail!("The given path is a directory, but --single-file was supplied.");
        }
    } else {
        anyhow::bail!("The given path does not exist.");
    }
    Ok(())
}

pub fn get_main_crate_ids_from_project(
    db: &mut dyn SemanticGroup,
    config: &ProjectConfig,
) -> Vec<CrateId> {
    config
        .content
        .crate_roots
        .keys()
        .map(|crate_identifier| get_crate_id_and_settings(db, crate_identifier, config).0)
        .collect()
}

fn get_crate_id_and_settings<'a>(
    db: &mut dyn SemanticGroup,
    crate_identifier: &CrateIdentifier,
    config: &'a ProjectConfig,
) -> (CrateId, &'a CrateSettings) {
    let crate_settings = config.content.crates_config.get(crate_identifier);
    let name = crate_settings.name.clone().unwrap_or_else(|| crate_identifier.clone().into());
    // It has to be done due to how `CrateId::core` works.
    let discriminator =
        if name == CORELIB_CRATE_NAME { None } else { Some(crate_identifier.clone().into()) };

    let crate_id = CrateLongId::Real { name, discriminator }.intern(db);

    (crate_id, crate_settings)
}
