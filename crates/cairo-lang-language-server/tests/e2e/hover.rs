use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use tower_lsp::lsp_types::{
    lsp_request, ClientCapabilities, Hover, HoverClientCapabilities, HoverContents, HoverParams,
    MarkupContent, MarkupKind, TextDocumentClientCapabilities, TextDocumentPositionParams,
};

use crate::support::cursor::{peek_caret, peek_selection};
use crate::support::{cursors, sandbox};

cairo_lang_test_utils::test_file_test!(
    hover,
    "tests/test_data/hover",
    {
        basic: "basic.txt",
        partial: "partial.txt",
        starknet: "starknet.txt",
    },
    test_hover
);

fn caps(base: ClientCapabilities) -> ClientCapabilities {
    ClientCapabilities {
        text_document: base.text_document.or_else(Default::default).map(|it| {
            TextDocumentClientCapabilities {
                hover: Some(HoverClientCapabilities {
                    dynamic_registration: Some(false),
                    content_format: Some(vec![MarkupKind::Markdown, MarkupKind::PlainText]),
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
/// The function then requests hover information at each caret position and compares the result with
/// the expected hover information from the snapshot file.
fn test_hover(
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

    let mut hovers = OrderedHashMap::default();

    for (n, position) in cursors.carets().into_iter().enumerate() {
        let hover_name = format!("hover #{n}");

        let mut report = String::new();

        report.push_str("// = source context\n");
        report.push_str(&peek_caret(&cairo, position));

        let hover = ls.send_request::<lsp_request!("textDocument/hover")>(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: ls.doc_id("src/lib.cairo"),
                position,
            },
            work_done_progress_params: Default::default(),
        });

        report.push_str("// = highlight\n");
        if let Some(Hover { range: Some(range), .. }) = &hover {
            report.push_str(&peek_selection(&cairo, range));
        } else {
            report.push_str("No highlight information.\n");
        }

        report.push_str("// = popover\n");
        report.push_str(hover.as_ref().map(render).unwrap_or("No hover information.\n"));

        hovers.insert(hover_name, report);
    }

    TestRunnerResult::success(hovers)
}

/// Render a hover response to a Markdown string that resembles what would be shown in a hover popup
/// in the text editor.
fn render(h: &Hover) -> &str {
    match &h.contents {
        HoverContents::Markup(MarkupContent { value, .. }) => value,
        contents => {
            panic!("LS returned deprecated MarkedString-based hover contents: {:#?}", contents);
        }
    }
}
