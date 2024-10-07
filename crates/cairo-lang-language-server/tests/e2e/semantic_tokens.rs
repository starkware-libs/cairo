use lsp_types::lsp_request;

use crate::support::sandbox;

fn caps(base: lsp_types::ClientCapabilities) -> lsp_types::ClientCapabilities {
    lsp_types::ClientCapabilities {
        text_document: base.text_document.or_else(Default::default).map(|it| {
            lsp_types::TextDocumentClientCapabilities {
                semantic_tokens: Some(lsp_types::SemanticTokensClientCapabilities {
                    dynamic_registration: Some(false),
                    requests: lsp_types::SemanticTokensClientCapabilitiesRequests {
                        full: Some(lsp_types::SemanticTokensFullOptions::Bool(true)),
                        ..Default::default()
                    },
                    ..Default::default()
                }),
                ..it
            }
        }),
        ..base
    }
}

#[test]
fn highlights_multiline_tokens() {
    let mut ls = sandbox! {
        files {
            "cairo_project.toml" => r#"
[crate_roots]
hello = "src"

[config.global]
edition = "2023_11"
"#,
            "src/lib.cairo" => r#"
fn main() {
    let _ = "
    ";
}
"#,
        }
        client_capabilities = caps;
    };

    ls.open("src/lib.cairo");

    let res = ls
        .send_request::<lsp_request!("textDocument/semanticTokens/full")>(
            lsp_types::SemanticTokensParams {
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                text_document: ls.doc_id("src/lib.cairo"),
            },
        )
        .unwrap();
    let lsp_types::SemanticTokensResult::Tokens(tokens) = res else {
        panic!("expected full tokens")
    };

    // There is a multiline (2) string, check if 2 consecutive tokens are of type string.
    assert!(tokens.data.windows(2).any(|tokens| {
        let string_type = 16; // SemanticTokenKind::String.as_u32()
        let first = tokens[0];
        let second = tokens[1];

        let are_on_consecutive_lines = first.delta_line + 1 == second.delta_line;
        let are_both_string =
            first.token_type == second.token_type && first.token_type == string_type;

        are_both_string && are_on_consecutive_lines
    }));
}
