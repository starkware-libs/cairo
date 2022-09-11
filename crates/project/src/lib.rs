#[cfg(test)]
mod test;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

#[derive(thiserror::Error, Debug)]
pub enum DeserializationError {
    #[error(transparent)]
    TomlError(#[from] toml::de::Error),
    #[error(transparent)]
    IoError(#[from] std::io::Error),
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProjectConfig {
    pub crate_roots: HashMap<String, String>,
}
impl ProjectConfig {
    pub fn from_file(filename: &str) -> Result<Self, DeserializationError> {
        Ok(toml::from_str(&std::fs::read_to_string(filename)?)?)
    }
}
