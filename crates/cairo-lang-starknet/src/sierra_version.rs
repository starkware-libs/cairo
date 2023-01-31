use std::collections::HashSet;
use std::fs;

use cairo_lang_sierra::ids::GenericLibfuncId;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use thiserror::Error;

#[cfg(test)]
#[path = "sierra_version_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum SierraVersionError {
    #[error("Invalid Sierra program.")]
    SierraProgramError,
    #[error("Sierra version not found.")]
    SierraVersionNotFound,
    #[error("Libfunc {missing_libfunc} is not allowed in the sierra version of the contract.")]
    IncompatibleSierraVersion { missing_libfunc: String },
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SierraVersion {
    // TODO(Gil): consider using semantic versioning format.
    version_id: usize,
    allowed_libfuncs: HashSet<SmolStr>,
}

impl SierraVersion {
    pub fn get_allowed_libfuncs_ids(&self) -> HashSet<GenericLibfuncId> {
        HashSet::from_iter(
            self.allowed_libfuncs
                .iter()
                .map(|libfunc_name| GenericLibfuncId::from_string(libfunc_name.clone())),
        )
    }
}

/// The version id to use when compiling into ContractClass.
pub const CURRENT_VERSION_ID: usize = 1;

/// Search all files in the Sierra versions dir for a json file representing a SierraVersion with
/// the given version_id.
pub fn lookup_sierra_version(version_id: usize) -> Option<SierraVersion> {
    println!("{:?}", std::env::current_dir());
    // TODO(Gil): decide where to look for the sierra versions.
    for sierra_version_file in fs::read_dir("src/sierra_versions").unwrap() {
        let content = fs::read_to_string(sierra_version_file.unwrap().path()).unwrap();
        let sierra_version: Result<SierraVersion, serde_json::Error> =
            serde_json::from_str(&content);
        match sierra_version {
            Ok(sierra_version) => {
                if sierra_version.version_id == version_id {
                    return Some(sierra_version);
                }
            }
            Err(_) => {}
        }
    }
    None
}
