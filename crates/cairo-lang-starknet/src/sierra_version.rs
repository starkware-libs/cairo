#[derive(Debug, PartialEq, Eq)]
pub struct VersionId {
    pub version: String,
}

impl VersionId {
    pub fn current_version_id() -> Self {
        Self { version: "0.1.8".into() }
    }
}
