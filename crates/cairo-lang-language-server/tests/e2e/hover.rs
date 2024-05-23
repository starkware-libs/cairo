use std::fmt::Write;

use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use tower_lsp::lsp_types;
use tower_lsp::lsp_types::lsp_request;

use crate::support::{cursors, sandbox};

cairo_lang_test_utils::test_file_test!(
    hover,
    "tests/test_data/hover",
    {
        basic: "basic.txt",
        starknet: "starknet.txt",
    },
    test_hover
);

fn caps(base: lsp_types::ClientCapabilities) -> lsp_types::ClientCapabilities {
    use lsp_types::*;
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
    let (cairo, cursors) = cursors(&inputs["src/lib.cairo"]);

    let mut ls = sandbox! {
        files {
            "cairo_project.toml" => inputs["cairo_project.toml"].clone(),
            "src/lib.cairo" => cairo.clone(),
        }
        client_capabilities = caps;
    };

    ls.open("src/lib.cairo");

    let mut hovers = OrderedHashMap::default();

    for position in cursors.carets() {
        let hover_name = format!("hover {}:{}", position.line, position.character);

        let mut report = String::new();

        let source_line = cairo.lines().nth(position.line as usize).unwrap();
        writeln!(&mut report, "{:-^1$}", " source context ", source_line.len()).unwrap();
        writeln!(&mut report, "{source_line}").unwrap();
        writeln!(
            &mut report,
            "{caret:>width$}",
            caret = "↑",
            width = position.character as usize + 1
        )
        .unwrap();

        let hover = ls.send_request::<lsp_request!("textDocument/hover")>(lsp_types::HoverParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: ls.doc_id("src/lib.cairo"),
                position,
            },
            work_done_progress_params: Default::default(),
        });

        let hover = match hover {
            Some(hover) => render(hover),
            None => "No hover information.\n".to_owned(),
        };
        writeln!(&mut report, "{:-^1$}", " popover ", source_line.len()).unwrap();
        write!(&mut report, "{hover}").unwrap();

        hovers.insert(hover_name, report);
    }

    TestRunnerResult::success(hovers)
}

/// Render a hover response to a Markdown string that resembles what would be shown in a hover popup
/// in the text editor.
///
/// Any additional hover metadata is rendered as HTML comments at the beginning of the output.
fn render(h: lsp_types::Hover) -> String {
    use lsp_types::*;
    let mut buf = String::new();

    if let Some(range) = h.range {
        writeln!(&mut buf, "<!-- range: {range:?} -->").unwrap();
    }

    let mut write_marked_string = |content| match content {
        MarkedString::String(contents) => buf.push_str(&contents),
        MarkedString::LanguageString(LanguageString { language, value }) => {
            write!(&mut buf, "```{language}\n{value}\n```").unwrap();
        }
    };

    match h.contents {
        HoverContents::Scalar(content) => write_marked_string(content),
        HoverContents::Markup(MarkupContent { value, .. }) => {
            buf.push_str(&value);
        }
        HoverContents::Array(contents) => {
            for content in contents {
                write_marked_string(content);
            }
        }
    }

    buf
}
