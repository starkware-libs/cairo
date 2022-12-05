use defs::db::{MacroPlugin, PluginResult};
use parser::parser_test;
use parser::test_utils::create_virtual_file;
use parser::utils::{get_syntax_file_and_diagnostics, SimpleParserDatabase};
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

    let plugin = StarkNetPlugin {};
    let mut generated_items: Vec<String> = Vec::new();
    for item in syntax_file.items(db).elements(db).into_iter() {
        let PluginResult { code, diagnostics } = plugin.generate_code(db, item);
        assert_eq!(diagnostics.len(), 0);
        let content = match code {
            Some((_path, content)) => content,
            None => continue,
        };
        generated_items.push(formatter::format_string(db, content));
    }

    OrderedHashMap::from([("generated_cairo_code".into(), generated_items.join("\n"))])
}

parser_test!(test1, ["src/plugin_test_data/starknet",], test_expand_contract);
