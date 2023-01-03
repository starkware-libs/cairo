use lsp::notification::Notification;
use lsp::Url;
use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub enum UpdateVirtualFile {}

impl Notification for UpdateVirtualFile {
    type Params = UpdateVirtualFileParams;
    const METHOD: &'static str = "vfs/update";
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct UpdateVirtualFileParams {
    pub uri: Url,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct ProvideVirtualFileRequest {
    pub uri: Url,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct ProvideVirtualFileResponse {
    pub content: Option<String>,
}
