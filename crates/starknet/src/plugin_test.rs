use defs::plugin::{MacroPlugin, PluginGeneratedFile, PluginResult};
use diagnostics::{format_diagnostics, DiagnosticLocation};
use parser::parser_test;
use parser::test_utils::create_virtual_file;
use parser::utils::{get_syntax_file_and_diagnostics, SimpleParserDatabase};
use syntax::node::TypedSyntaxNode;
use utils::ordered_hash_map::OrderedHashMap;

use crate::plugin::StarkNetPlugin;

pub fn test_expand_contract(
    db: &mut SimpleParserDatabase,
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    let cairo_code = &inputs["cairo_code"];
    let file_id = create_virtual_file(db, "dummy_file.cairo", cairo_code);

    let (syntax_file, diagnostics) = get_syntax_file_and_diagnostics(db, file_id, cairo_code);
    assert_eq!(diagnostics.format(db), "");
    let file_syntax_node = syntax_file.as_syntax_node();
    let plugin = StarkNetPlugin {};
    let mut generated_items: Vec<String> = Vec::new();
    let mut diagnostic_items: Vec<String> = Vec::new();
    for item in syntax_file.items(db).elements(db).into_iter() {
        let PluginResult { code, diagnostics } = plugin.generate_code(db, item);

        diagnostic_items.extend(diagnostics.iter().map(|diag| {
            let syntax_node = file_syntax_node.lookup_ptr(db, diag.stable_ptr);

            let location =
                DiagnosticLocation { file_id, span: syntax_node.span_without_trivia(db) };
            format_diagnostics(db, &diag.message, location)
        }));

        let content = match code {
            Some(PluginGeneratedFile { content, .. }) => content,
            None => continue,
        };
        generated_items.push(formatter::format_string(db, content));
    }

    OrderedHashMap::from([
        ("generated_cairo_code".into(), generated_items.join("\n")),
        ("expected_diagnostics".into(), diagnostic_items.join("\n")),
    ])
}

parser_test!(test_diagnostics, ["src/plugin_test_data/diagnostics",], test_expand_contract);
parser_test!(test_wrapper_expansion, ["src/plugin_test_data/contract",], test_expand_contract);
parser_test!(test_storage_expansion, ["src/plugin_test_data/storage",], test_expand_contract);
