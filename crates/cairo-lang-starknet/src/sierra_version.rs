#[derive(Debug, PartialEq, Eq)]
pub struct VersionId {
    pub major: usize,
    pub minor: usize,
    pub patch: usize,
}

impl VersionId {
    pub fn current_version_id() -> Self {
        Self { major: 0, minor: 1, patch: 8 }
    }
}
