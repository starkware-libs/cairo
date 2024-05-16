use std::fmt::Write;
use std::path::Path;

use indoc::indoc;
use tower_lsp::lsp_types;
use tower_lsp::lsp_types::lsp_request;

use crate::support::{cursors, sandbox};

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

#[test]
fn basic() {
    check(
        "tests/test_data/hover/basic.txt",
        indoc! {r#"
            fn main() {
                f<caret>oo();
            }

            /// Foo documentation.
            fn f<caret>oo() {}
        "#},
    );
}

/// Perform hover test.
///
/// This function spawns a sandbox language server with the given code in the `src/lib.cairo` file.
/// The Cairo source code is expected to contain caret markers.
/// The function then requests hover information at each caret position and compares the result with
/// the expected hover information from the snapshot file.
fn check(test_data: &str, cairo: &str) {
    let (cairo, cursors) = cursors(cairo);

    let mut ls = sandbox! {
        files {
            "cairo_project.toml" => indoc! {r#"
                [crate_roots]
                hello = "src"

                [config.global]
                edition = "2023_11"
            "#},
            "src/lib.cairo" => cairo.clone(),
        }
        client_capabilities = caps;
    };

    ls.open("src/lib.cairo");

    let mut expected = String::new();

    for position in cursors.carets() {
        writeln!(&mut expected, "//! > hover at {position:?}").unwrap();

        writeln!(&mut expected).unwrap();
        writeln!(&mut expected, "//! > source context").unwrap();
        let source_line = cairo.lines().nth(position.line as usize).unwrap();
        writeln!(&mut expected, "{source_line}").unwrap();
        writeln!(
            &mut expected,
            "{caret:>width$}",
            caret = "↑",
            width = position.character as usize
        )
        .unwrap();

        let hover = ls.send_request::<lsp_request!("textDocument/hover")>(lsp_types::HoverParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: ls.doc_id("src/lib.cairo"),
                position,
            },
            work_done_progress_params: Default::default(),
        });

        writeln!(&mut expected).unwrap();
        writeln!(&mut expected, "//! > popover").unwrap();
        match hover {
            Some(hover) => expected.push_str(&render(hover)),
            None => expected.push_str("No hover information.\n"),
        }

        writeln!(&mut expected, "\n=========\n").unwrap();
    }

    cairo_lang_test_utils::compare_contents_or_fix_with_path(Path::new(test_data), expected);
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
