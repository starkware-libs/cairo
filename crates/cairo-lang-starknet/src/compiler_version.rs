#[derive(Debug, PartialEq, Eq)]
pub struct VersionId {
    pub major: usize,
    pub minor: usize,
    pub patch: usize,
}

/// The version of the high level compiler that compiled the contract. Should be the same as the
/// rust workspace version.
pub fn current_compiler_version_id() -> VersionId {
    VersionId { major: 1, minor: 0, patch: 0 }
}

/// The version of the sierra compiler that compiled the contract. When changed should be the
/// updated to be the same as the current compiler version.
/// However, if the only changes in a version are in the high level compiler, then the sierra
/// version should lag behind and not be updated.
pub fn current_sierra_version_id() -> VersionId {
    VersionId { major: 1, minor: 0, patch: 0 }
}
