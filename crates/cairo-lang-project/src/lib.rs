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
    pub crate_roots: OrderedHashMap<SmolStr, CrateSourcePath>,
}

/// A crate root specification.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CrateSourcePath {
    /// Crate root specified as path, eg. `<crate_name> = "<crate_root>"`.
    SimpleSourceRoot(PathBuf),
    /// Detailed crate root specification as a table, eg. `<crate_name> = { path = "<crate_path>"
    /// }`. Crate root must be a directory that contains a `lib.cairo` file.
    SourceRoot(DetailedSourceRoot),
    /// Crate entrypoint specification as a table, eg. `<crate_name> = { entrypoint =
    /// "<entrypoint_path>" }`. Entrypoint must be a file.
    SourcePath(DetailedSourcePath),
}

impl From<PathBuf> for CrateSourcePath {
    fn from(path: PathBuf) -> Self {
        CrateSourcePath::SimpleSourceRoot(path)
    }
}

/// A detailed crate root specification.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DetailedSourceRoot {
    /// Crate root path.
    pub source_root: PathBuf,
}

/// A detailed crate entrypoint specification.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DetailedSourcePath {
    /// Crate entrypoint file path.
    pub source_path: PathBuf,
}

impl CrateSourcePath {
    pub fn inject_lib(&self) -> bool {
        matches!(self, CrateSourcePath::SourcePath(_))
    }

    pub fn file_stem(&self) -> Option<String> {
        match self {
            CrateSourcePath::SourcePath(detailed) => {
                detailed.source_path.file_stem().map(|s| s.to_string_lossy().to_string())
            }
            _ => None,
        }
    }

    pub fn root(&self) -> Result<PathBuf, CrateRootError> {
        match self {
            CrateSourcePath::SimpleSourceRoot(path) => Ok(path.clone()),
            CrateSourcePath::SourceRoot(detailed) => Ok(detailed.source_root.clone()),
            CrateSourcePath::SourcePath(detailed) => detailed
                .source_path
                .clone()
                .parent()
                .ok_or_else(|| CrateRootError::BadPath {
                    path: detailed.source_path.to_string_lossy().to_string(),
                })
                .map(|p| p.to_path_buf()),
        }
    }
}

impl From<&str> for CrateSourcePath {
    fn from(path: &str) -> Self {
        CrateSourcePath::SimpleSourceRoot(path.into())
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
