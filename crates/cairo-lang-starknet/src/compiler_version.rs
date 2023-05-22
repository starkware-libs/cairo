#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VersionId {
    pub major: usize,
    pub minor: usize,
    pub patch: usize,
}

impl std::fmt::Display for VersionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// The version of the high level compiler that compiled the contract. Should be the same as the
/// rust workspace version.
pub fn current_compiler_version_id() -> VersionId {
    VersionId { major: 1, minor: 1, patch: 0 }
}

/// The version of the Sierra compiler that compiled the contract.
///
/// When this value is changed, it should be updated to be the same as the current compiler version.
/// However, if the only changes in a version are in the high-level compiler, then the Sierra
/// version should not change.
pub fn current_sierra_version_id() -> VersionId {
    VersionId { major: 1, minor: 1, patch: 0 }
}
