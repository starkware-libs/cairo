use std::fmt::Write;
use std::path::Path;

use tower_lsp::lsp_types;
use tower_lsp::lsp_types::lsp_request;

use crate::support::{cursors, sandbox, MockClient};

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
    let (cairo, cursors) = cursors(
        r#"
fn main() {
    f<caret=FooCall>oo();
}
            
/// Foo documentation.
fn f<caret=FooDefinition>oo() {}
"#,
    );

    let mut ls = sandbox! {
        files {
            "cairo_project.toml" => r#"
[crate_roots]
hello = "src"

[config.global]
edition = "2023_11"
"#,
            "src/lib.cairo" => cairo,
        }
        client_capabilities = caps;
    };

    ls.open("src/lib.cairo");

    check(&mut ls, "src/lib.cairo", cursors.named_carets(), "tests/test_data/hover/basic.txt");
}

fn check(
    ls: &mut MockClient,
    file: &str,
    positions: Vec<(String, lsp_types::Position)>,
    test_data: &str,
) {
    let mut expected = String::new();

    for (caret_name, position) in positions {
        writeln!(&mut expected, "//! > <caret={caret_name}> at {position:?}").unwrap();
        let hover = ls.send_request::<lsp_request!("textDocument/hover")>(lsp_types::HoverParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: ls.doc_id(file),
                position,
            },
            work_done_progress_params: Default::default(),
        });
        match hover {
            Some(hover) => expected.push_str(&render(hover)),
            None => expected.push_str("No hover information.\n"),
        }
        expected.push_str("=========\n");
    }

    cairo_lang_test_utils::compare_contents_or_fix_with_path(Path::new(test_data), expected);
}

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
