use tower_lsp::lsp_types::{lsp_request, SemanticTokensParams, SemanticTokensResult};

use crate::support::sandbox;

#[test]
fn highlights_full_file() {
    let mut s = sandbox! {
        files {
            "cairo_project.toml" => r#"
[crate_roots]
hello = "src"

[config.global]
edition = "2023_11"
"#,
            "src/lib.cairo" => r#"fn main() {}"#,
        }
    };

    s.open("src/lib.cairo");

    let res = s
        .send_request::<lsp_request!("textDocument/semanticTokens/full")>(SemanticTokensParams {
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            text_document: s.doc_id("src/lib.cairo"),
        })
        .unwrap();
    let SemanticTokensResult::Tokens(tokens) = res else { panic!("expected full tokens") };
    assert!(tokens.data.len() > 1);
}
