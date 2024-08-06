use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use tower_lsp::lsp_types::{
    lsp_request, ClientCapabilities, GotoCapability, GotoDefinitionParams, GotoDefinitionResponse,
    TextDocumentClientCapabilities, TextDocumentIdentifier, TextDocumentPositionParams,
};

use crate::support::cursor::{peek_caret, peek_selection};
use crate::support::{cursors, sandbox};

cairo_lang_test_utils::test_file_test!(
    goto,
    "tests/test_data/goto",
    {
        struct_members: "struct_members.txt",
    },
    test_goto_members
);

fn caps(base: ClientCapabilities) -> ClientCapabilities {
    ClientCapabilities {
        text_document: base.text_document.or_else(Default::default).map(|it| {
            TextDocumentClientCapabilities {
                definition: Some(GotoCapability {
                    dynamic_registration: Some(false),
                    link_support: None,
                }),
                ..it
            }
        }),
        ..base
    }
}

/// Perform hover test.
///
/// This function spawns a sandbox language server with the given code in the `src/lib.cairo` file.
/// The Cairo source code is expected to contain caret markers.
/// The function then requests goto definition information at each caret position and compares
/// the result with the expected hover information from the snapshot file.
fn test_goto_members(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let (cairo, cursors) = cursors(&inputs["cairo_code"]);

    let mut ls = sandbox! {
        files {
            "cairo_project.toml" => inputs["cairo_project.toml"].clone(),
            "src/lib.cairo" => cairo.clone(),
        }
        client_capabilities = caps;
    };
    ls.open_and_wait_for_diagnostics("src/lib.cairo");

    let mut goto_definitions = OrderedHashMap::default();

    for (n, position) in cursors.carets().into_iter().enumerate() {
        let mut report = String::new();

        report.push_str(&peek_caret(&cairo, position));
        let code_action_params = GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: ls.doc_id("src/lib.cairo").uri },
                position,
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        let goto_definition_response =
            ls.send_request::<lsp_request!("textDocument/definition")>(code_action_params);

        if let Some(goto_definition_response) = goto_definition_response {
            if let GotoDefinitionResponse::Scalar(location) = goto_definition_response {
                report.push_str(&peek_selection(&cairo, &location.range));
            } else {
                panic!("Unexpected GotoDefinitionResponse variant.")
            }
        } else {
            panic!("Goto definition request failed.");
        }
        goto_definitions.insert(format!("Goto definition #{}", n), report);
    }

    TestRunnerResult::success(goto_definitions)
}
