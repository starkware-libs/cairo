use std::collections::HashSet;

use cairo_lang_sierra::ids::GenericLibfuncId;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum SierraVersionError {
    #[error("Invalid Sierra program.")]
    SierraProgramError,
    #[error("Unexpected Sierra version.")]
    UnexpectedSierraVersion,
    #[error("Libfunc {invalid_libfunc} is not allowed in sierra version: {sierra_version_id}.")]
    UnsupportedLibfunc { invalid_libfunc: String, sierra_version_id: usize },
}

/// Represents the allowed sierra libfuncs in a given sierra version.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SierraVersion {
    // TODO(Gil): consider using semantic versioning format.
    version_id: usize,
    allowed_libfuncs: HashSet<SmolStr>,
}

impl SierraVersion {
    /// Maps the allwed libfuncs of the sierra version to their GenericLibfuncId.
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

/// Returns the sierra version corresponding to the given version id.
pub fn lookup_sierra_version(version_id: usize) -> Result<SierraVersion, SierraVersionError> {
    let sierra_version_str: &str = match version_id {
        1 => {
            include_str!("sierra_versions/sierra_version_1.json")
        }
        _ => {
            return Err(SierraVersionError::UnexpectedSierraVersion);
        }
    };
    let sierra_version: Result<SierraVersion, serde_json::Error> =
        serde_json::from_str(sierra_version_str);
    match sierra_version {
        Ok(sierra_version) => {
            assert!(sierra_version.version_id == version_id);
            Ok(sierra_version)
        }
        Err(_) => Err(SierraVersionError::UnexpectedSierraVersion),
    }
}
