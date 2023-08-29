//! Cairo project specification. For example, crates and flags used for compilation.
#[cfg(test)]
mod test;

use std::path::{Path, PathBuf};

use cairo_lang_filesystem::ids::Directory;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

#[derive(thiserror::Error, Debug)]
pub enum DeserializationError {
    #[error(transparent)]
    TomlError(#[from] toml::de::Error),
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error("PathError")]
    PathError,
}

#[derive(thiserror::Error, Debug)]
pub enum CrateRootError {
    #[error("Couldn't handle {path}: Not a legal path.")]
    BadPath { path: String },
}

const PROJECT_FILE_NAME: &str = "cairo_project.toml";

/// Cairo project config, including its file content and metadata about the file.
/// This file is expected to be at a root of a crate and specify the crate name and location and
/// of its dependency crates.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProjectConfig {
    pub base_path: PathBuf,
    pub corelib: Option<Directory>,
    pub content: ProjectConfigContent,
}
/// Contents of a Cairo project config file.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProjectConfigContent {
    pub crate_roots: OrderedHashMap<SmolStr, CrateRootPath>,
}

/// A crate root specification.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CrateRootPath {
    /// Crate root specified as path, eg. `<crate_name> = "<crate_root>"`.
    Simple(PathBuf),
    /// Detailed crate root specification as a table, eg. `<crate_name> = { path = "<crate_path>"
    /// }`. Crate root must be a directory that contains a `lib.cairo` file.
    Detailed(DetailedCrateRootPath),
    /// Crate entrypoint specification as a table, eg. `<crate_name> = { entrypoint =
    /// "<entrypoint_path>" }`. Entrypoint must be a file.
    Entrypoint(DetailedEntrypointPath),
}

/// A detailed crate root specification.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DetailedCrateRootPath {
    /// Crate root path.
    pub path: PathBuf,
}

/// A detailed crate entrypoint specification.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DetailedEntrypointPath {
    /// Crate entrypoint file path.
    pub entrypoint: PathBuf,
}

impl CrateRootPath {
    pub fn inject_lib(&self) -> bool {
        matches!(self, CrateRootPath::Entrypoint(_))
    }

    pub fn file_stem(&self) -> Option<String> {
        match self {
            CrateRootPath::Entrypoint(detailed) => {
                detailed.entrypoint.file_stem().map(|s| s.to_string_lossy().to_string())
            }
            _ => None,
        }
    }

    pub fn root(&self) -> Result<PathBuf, CrateRootError> {
        match self {
            CrateRootPath::Simple(path) => Ok(path.clone()),
            CrateRootPath::Detailed(detailed) => Ok(detailed.path.clone()),
            CrateRootPath::Entrypoint(detailed) => detailed
                .entrypoint
                .clone()
                .parent()
                .ok_or_else(|| CrateRootError::BadPath {
                    path: detailed.entrypoint.to_string_lossy().to_string(),
                })
                .map(|p| p.to_path_buf()),
        }
    }
}

impl From<&str> for CrateRootPath {
    fn from(path: &str) -> Self {
        CrateRootPath::Simple(path.into())
    }
}

impl ProjectConfig {
    pub fn from_directory(directory: &Path) -> Result<Self, DeserializationError> {
        Self::from_file(&directory.join(PROJECT_FILE_NAME))
    }
    pub fn from_file(filename: &Path) -> Result<Self, DeserializationError> {
        let base_path = filename
            .parent()
            .and_then(|p| p.to_str())
            .ok_or(DeserializationError::PathError)?
            .into();
        let content = toml::from_str(&std::fs::read_to_string(filename)?)?;
        Ok(ProjectConfig { base_path, content, corelib: None })
    }
}
