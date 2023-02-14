use std::fmt::{Display, Formatter};

use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct VersionId {
    major: usize,
    minor: usize,
    patch: usize,
}

impl Display for VersionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// The version id to use when compiling into ContractClass.
pub const CURRENT_VERSION_ID: VersionId = VersionId { major: 0, minor: 1, patch: 0 };
