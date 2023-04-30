#[derive(Debug, PartialEq, Eq)]
pub struct VersionId {
    pub major: usize,
    pub minor: usize,
    pub patch: usize,
}

impl VersionId {
    pub fn current_version_id() -> Self {
        Self { major: 1, minor: 0, patch: 0 }
    }
}
