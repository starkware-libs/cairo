use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use tower_lsp::lsp_types::{
    ClientCapabilities, Diagnostic, DiagnosticClientCapabilities,
    DiagnosticWorkspaceClientCapabilities, TextDocumentClientCapabilities,
    WorkspaceClientCapabilities,
};

use crate::support::sandbox;

cairo_lang_test_utils::test_file_test!(
    goto,
    "tests/test_data/diagnostics",
    {
        macro_diagnostics: "macro_diagnostics.txt",
    },
    test_diagnostics
);

/// Perform diagnostics test.
///
/// This function spawns a sandbox language server with the given code in the `src/lib.cairo` file.
/// The Cairo source code is expected to contain caret markers.
/// The function then requests goto definition information at each caret position and compares
/// the result with the expected hover information from the snapshot file.
fn test_diagnostics(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let mut ls = sandbox! {
        files {
            "cairo_project.toml" => inputs["cairo_project.toml"].clone(),
            "src/lib.cairo" => inputs["cairo_code"].clone(),
        }
    };
    let diagnostics = ls.open_and_wait_for_diagnostics("src/lib.cairo");
    let mut generated_diagnostics = OrderedHashMap::default();

    TestRunnerResult::success(generated_diagnostics)
}
