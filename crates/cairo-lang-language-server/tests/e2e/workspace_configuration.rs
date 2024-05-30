use indoc::indoc;
use serde_json::json;
use tower_lsp::lsp_types::lsp_request;
use tower_lsp::lsp_types::request::Request as _;

use crate::support::jsonrpc::Message;
use crate::support::sandbox;

/// The LS used to panic when some files in Salsa database were interned with a relative path.
/// The panic happened while trying to create a `file://` URL to affected file.
/// The easiest way to reproduce this was to pass a relative path as `cairo1.corelibPath` in
/// workspace configuration.
///
/// This test checks that:
/// 1. The workspace configuration flow between language server and language client is working as
///    expected.
/// 2. The LS does not panic when it receives a relative path to the core crate.
#[test]
fn relative_path_to_core() {
    let core_path = {
        let detected = cairo_lang_filesystem::detect::detect_corelib().unwrap();
        let pwd = std::env::current_dir().unwrap();
        let path = pathdiff::diff_paths(detected, pwd).unwrap();
        assert!(path.is_relative());
        path.to_string_lossy().to_string()
    };

    let mut ls = sandbox! {
        files {
            "cairo_project.toml" => indoc! {r#"
                [crate_roots]
                hello = "src"
            "#},
            "src/lib.cairo" => r#"fn main() -> u8 { 42 }"#,
        }
        workspace_configuration = json!({
            "cairo1": {
                "corelibPath": core_path,
            }
        });
    };

    let diags = ls.open_and_wait_for_diagnostics("src/lib.cairo");
    assert!(diags.diagnostics.is_empty());

    assert_eq!(
        ls.trace()
            .iter()
            .filter(|msg| {
                let Message::Request(req) = msg else { return false };
                req.method() == <lsp_request!("workspace/configuration")>::METHOD
            })
            .count(),
        1
    );
}
