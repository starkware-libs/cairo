use lsp::notification::Notification;
use lsp::Url;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub enum UpdateVirtualFile {}

#[cfg(feature = "serde")]
impl Notification for UpdateVirtualFile {
    type Params = UpdateVirtualFileParams;
    const METHOD: &'static str = "vfs/update";
}

#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct UpdateVirtualFileParams {
    pub uri: Url,
}

#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ProvideVirtualFileRequest {
    pub uri: Url,
}

#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ProvideVirtualFileResponse {
    pub content: Option<String>,
}
