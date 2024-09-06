//! CairoLS extensions to the Language Server Protocol.

use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::request::Request;
use tower_lsp::lsp_types::{TextDocumentPositionParams, Url};

/// Provides content of virtual file from the database.
pub struct ProvideVirtualFile;

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct ProvideVirtualFileRequest {
    pub uri: Url,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct ProvideVirtualFileResponse {
    pub content: Option<String>,
}

impl Request for ProvideVirtualFile {
    type Params = ProvideVirtualFileRequest;
    type Result = ProvideVirtualFileResponse;
    const METHOD: &'static str = "vfs/provide";
}

/// Collects information about all Cairo crates that are currently being analyzed.
pub struct ViewAnalyzedCrates;

impl Request for ViewAnalyzedCrates {
    type Params = ();
    type Result = String;
    const METHOD: &'static str = "cairo/viewAnalyzedCrates";
}

/// Provides string with code after macros expansion.
pub struct ExpandMacro;

impl Request for ExpandMacro {
    type Params = TextDocumentPositionParams;
    type Result = Option<String>;
    const METHOD: &'static str = "cairo/expandMacro";
}

/// Notifies about corelib version mismatch.
#[derive(Debug)]
pub struct CorelibVersionMismatch;

impl Notification for CorelibVersionMismatch {
    type Params = String;
    const METHOD: &'static str = "cairo/corelib-version-mismatch";
}
