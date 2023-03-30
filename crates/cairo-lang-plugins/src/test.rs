use cairo_lang_defs::plugin::{PluginGeneratedFile, PluginResult};
use cairo_lang_diagnostics::{format_diagnostics, DiagnosticLocation};
use cairo_lang_parser::test_utils::create_virtual_file;
use cairo_lang_parser::utils::{get_syntax_file_and_diagnostics, SimpleParserDatabase};
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::get_default_plugins;

cairo_lang_test_utils::test_file_test!(
    expand_plugin,
    "src/test_data",
    {
        config: "config",
        derive: "derive",
        panicable: "panicable",
    },
    test_expand_plugin
);

pub fn test_expand_plugin(
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    let db = &mut SimpleParserDatabase::default();
    let cairo_code = &inputs["cairo_code"];
    let file_id = create_virtual_file(db, "dummy_file.cairo", cairo_code);

    let (syntax_file, diagnostics) = get_syntax_file_and_diagnostics(db, file_id, cairo_code);
    assert!(diagnostics.is_empty(), "Unexpected diagnostics:\n{}", diagnostics.format(db));
    let file_syntax_node = syntax_file.as_syntax_node();
    let plugins = get_default_plugins();
    let mut generated_items: Vec<String> = Vec::new();
    let mut diagnostic_items: Vec<String> = Vec::new();
    for item in syntax_file.items(db).elements(db).into_iter() {
        for plugin in &plugins {
            let PluginResult { code, diagnostics, remove_original_item } =
                plugin.clone().as_dyn_macro_plugin().generate_code(db, item.clone());

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
            if !remove_original_item {
                generated_items.push(item.as_syntax_node().get_text(db));
            }
            generated_items.push(content);
        }
    }

    OrderedHashMap::from([
        ("generated_cairo_code".into(), generated_items.join("\n")),
        ("expected_diagnostics".into(), diagnostic_items.join("\n")),
    ])
}
