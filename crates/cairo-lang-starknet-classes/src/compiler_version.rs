use serde::{Deserialize, Serialize};

#[cfg(test)]
#[path = "compiler_version_test.rs"]
mod test;

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct VersionId {
    pub major: usize,
    pub minor: usize,
    pub patch: usize,
}
impl VersionId {
    /// Returns whether this version support the given version.
    pub fn supports(&self, other: VersionId) -> bool {
        self.major > other.major || (self.major == other.major && self.minor >= other.minor)
    }
}

impl std::fmt::Display for VersionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// The version of the high level compiler that compiled the contract. Should be the same as the
/// rust workspace version.
pub fn current_compiler_version_id() -> VersionId {
    VersionId {
        major: env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap(),
        minor: env!("CARGO_PKG_VERSION_MINOR").parse().unwrap(),
        patch: env!("CARGO_PKG_VERSION_PATCH").parse().unwrap(),
    }
}

/// The version of the Sierra compiler that compiled the contract.
///
/// Major version should be updated in any non-backwards compatible change of the Sierra compiler.
/// Minor version should be updated in any backwards compatible change of the Sierra compiler.
/// For more information see docs/CONTRIBUTING.md.
pub fn current_sierra_version_id() -> VersionId {
    VersionId { major: 1, minor: 7, patch: 0 }
}
