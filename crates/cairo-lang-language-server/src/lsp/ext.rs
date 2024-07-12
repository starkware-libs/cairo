//! CairoLS extensions to the Language Server Protocol.

use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::request::Request;
use tower_lsp::lsp_types::Url;

// TODO(mkaput): Provide this as a command in VSCode.
/// Collect information about all Cairo crates that are currently being analyzed.
pub struct ViewAnalyzedCrates;

impl Request for ViewAnalyzedCrates {
    type Params = ();
    type Result = String;
    const METHOD: &'static str = "cairo/viewAnalyzedCrates";
}

pub struct ExpandMacro;

#[derive(Serialize, Deserialize)]

pub struct TextDocument {
    pub uri: Url,
}

#[derive(Serialize, Deserialize)]

pub struct Position {
    pub line: usize,
    pub character: usize,
}

#[derive(Serialize, Deserialize)]
pub struct ExpandMacroParams {
    #[serde(rename = "textDocument")]
    pub text_document: TextDocument,
    pub position: Position,
}

impl Request for ExpandMacro {
    type Params = ExpandMacroParams;
    type Result = Option<String>;
    const METHOD: &'static str = "cairo/expandMacro";
}
