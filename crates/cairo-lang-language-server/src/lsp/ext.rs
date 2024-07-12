//! CairoLS extensions to the Language Server Protocol.

use tower_lsp::lsp_types::request::Request;
use tower_lsp::lsp_types::TextDocumentPositionParams;

// TODO(mkaput): Provide this as a command in VSCode.
/// Collect information about all Cairo crates that are currently being analyzed.
pub struct ViewAnalyzedCrates;

impl Request for ViewAnalyzedCrates {
    type Params = ();
    type Result = String;
    const METHOD: &'static str = "cairo/viewAnalyzedCrates";
}

/// Provides string with code after macros expansion
pub struct ExpandMacro;

impl Request for ExpandMacro {
    type Params = TextDocumentPositionParams;
    type Result = Option<String>;
    const METHOD: &'static str = "cairo/expandMacro";
}
