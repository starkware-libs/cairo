use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use lsp_types::{CompletionParams, TextDocumentPositionParams, lsp_request};

use crate::support::cursor::peek_caret;
use crate::support::{cursors, sandbox};

cairo_lang_test_utils::test_file_test!(
    completions,
    "tests/test_data/completions",
    {
        methods_text_edits: "methods_text_edits.txt",
    },
    test_completions_text_edits

);

/// Perform completions text edits test. Notice that the test shows many possible completions,
/// however in practice only those who have the same prefix as the existing code are shown.
///
/// This function spawns a sandbox language server with the given code in the `src/lib.cairo` file.
/// The Cairo source code is expected to contain caret markers.
/// The function then requests quick fixes at each caret position and compares the result with the
/// expected quick fixes from the snapshot file.
fn test_completions_text_edits(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let (cairo, cursors) = cursors(&inputs["cairo_code"]);

    let mut ls = sandbox! {
        files {
            "cairo_project.toml" => inputs["cairo_project.toml"].clone(),
            "src/lib.cairo" => cairo.clone(),
        }
    };

    ls.open("src/lib.cairo");

    let mut completions = OrderedHashMap::default();

    for (n, position) in cursors.carets().into_iter().enumerate() {
        let mut report = String::new();

        report.push_str(&peek_caret(&cairo, position));
        let completion_params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: ls.doc_id("src/lib.cairo"),
                position,
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: None,
        };
        let caret_completions =
            ls.send_request::<lsp_request!("textDocument/completion")>(completion_params);
        if let Some(completions) = caret_completions {
            let completion_items = match completions {
                lsp_types::CompletionResponse::Array(items) => items,
                lsp_types::CompletionResponse::List(list) => list.items,
            };
            for completion in completion_items {
                if let Some(text_edit) = completion.additional_text_edits {
                    report.push_str("--------------------------\n");
                    report.push_str(format!("Completion: {}\n", completion.label).as_str());
                    if let Some(text) = completion.insert_text {
                        report.push_str(format!("Insert text: {text}\n").as_str());
                    }
                    for edit in text_edit {
                        report.push_str(format!("Text edit: {}", edit.new_text).as_str());
                    }
                }
            }
        }
        completions.insert(format!("Completions #{}", n), report);
    }

    TestRunnerResult::success(completions)
}
