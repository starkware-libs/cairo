use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::Url;

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct ProvideVirtualFileRequest {
    pub uri: Url,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct ProvideVirtualFileResponse {
    pub content: Option<String>,
}
