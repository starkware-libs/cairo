//! CairoLS extensions to the Language Server Protocol.

use tower_lsp::lsp_types::request::Request;

// TODO(mkaput): Provide this as a command in VSCode.
/// Collect information about all Cairo crates that are currently being analyzed.
pub struct ViewAnalyzedCrates;

impl Request for ViewAnalyzedCrates {
    type Params = ();
    type Result = String;
    const METHOD: &'static str = "cairo/viewAnalyzedCrates";
}
