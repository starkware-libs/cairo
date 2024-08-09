use cairo_lang_language_server::lsp::ext::ExpandMacro;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use tower_lsp::lsp_types::{TextDocumentIdentifier, TextDocumentPositionParams};

use crate::support::cursor::peek_caret;
use crate::support::{cursors, sandbox};

cairo_lang_test_utils::test_file_test!(
    macro_expand,
    "tests/test_data/macro_expand",
    {
        simple_inline: "simple_inline.txt",
        attribute: "attribute.txt",
        empty: "empty.txt",
        derive: "derive.txt",
    },
    test_macro_expand
);

fn test_macro_expand(
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

    let mut results = OrderedHashMap::default();

    for (n, position) in cursors.carets().into_iter().enumerate() {
        let macro_expansion_name = format!("macro expand #{n}");

        let mut report = String::new();

        report.push_str("// = source context\n");
        report.push_str(&peek_caret(&cairo, position));

        let macro_expansion = ls.send_request::<ExpandMacro>(TextDocumentPositionParams {
            position,
            text_document: TextDocumentIdentifier { uri: ls.doc_id("src/lib.cairo").uri },
        });

        report.push_str("// = expansion\n");
        if let Some(expansion) = &macro_expansion {
            report.push_str(expansion);
        } else {
            report.push_str("No expansion information.\n");
        }

        results.insert(macro_expansion_name, report);
    }

    TestRunnerResult::success(results)
}
