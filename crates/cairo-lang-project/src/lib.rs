//! Cairo project specification. For example, crates and flags used for compilation.
#[cfg(test)]
mod test;

use std::path::{Path, PathBuf};

use cairo_lang_filesystem::db::Edition;
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
    pub crate_roots: OrderedHashMap<SmolStr, PathBuf>,
    #[serde(default)]
    #[serde(rename = "config")]
    pub crate_config: AllCratesConfig,
}

/// Additional configurations for all crates.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct AllCratesConfig {
    /// The configuration for non overriden crates.
    #[serde(default)]
    pub global: SingleCrateConfig,
    /// Configuration override per crate.
    #[serde(default)]
    #[serde(rename = "override")]
    pub crate_override: OrderedHashMap<SmolStr, SingleCrateConfig>,
}
impl AllCratesConfig {
    /// Returns the configuration for the given crate.
    pub fn get(&self, crate_name: &str) -> &SingleCrateConfig {
        self.crate_override.get(crate_name).unwrap_or(&self.global)
    }
}

/// Configuration per crate.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SingleCrateConfig {
    /// The crate's Cairo edition.
    pub edition: Edition,
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
