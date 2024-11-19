//! CairoLS extensions to the Language Server Protocol.

use lsp_types::notification::Notification;
use lsp_types::request::Request;
use lsp_types::{TextDocumentPositionParams, Url};
use serde::{Deserialize, Serialize};

/// Provides content of virtual file from the database.
pub(crate) struct ProvideVirtualFile;

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub(crate) struct ProvideVirtualFileRequest {
    pub uri: Url,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub(crate) struct ProvideVirtualFileResponse {
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

/// Notifies about scarb metadata failure.
#[derive(Debug)]
pub struct ScarbMetadataFailed;

impl Notification for ScarbMetadataFailed {
    type Params = ();
    const METHOD: &'static str = "cairo/scarb-metadata-failed";
}

/// Notifies about `proc-macro-server` fatal fail.
#[derive(Debug)]
pub struct ProcMacroServerInitializationFailed;

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", rename_all_fields = "camelCase", tag = "reason")]
pub enum ProcMacroServerInitializationFailedParams {
    NoMoreRetries { retries: u32, in_minutes: u64 },
    SpawnFail,
}

impl Notification for ProcMacroServerInitializationFailed {
    type Params = ProcMacroServerInitializationFailedParams;
    const METHOD: &'static str = "cairo/procMacroServerInitializationFailed";
}
