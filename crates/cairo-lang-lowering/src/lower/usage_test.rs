use std::fmt::Write;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::BlockUsages;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    inlining,
    "src/lower/test_data",
    {
        usage :"usage",
    },
    test_function_usage
);

fn test_function_usage(inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
    let db = &mut LoweringDatabaseForTesting::default();
    db.set_semantic_plugins(get_default_plugins());
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();

    let file_id = db.module_file(test_function.function_id.module_file_id(db)).unwrap();
    let file_sytnax = db.file_syntax(file_id).unwrap();

    let expr_formatter = ExprFormatter { db, function_id: test_function.function_id };
    let function_def =
        db.function_body(test_function.concrete_function_id.function_with_body_id(db)).unwrap();
    let usages = BlockUsages::from_function_body(&function_def);

    let mut usages_str = String::new();
    for (expr_id, usage) in usages.block_usages.iter() {
        let stable_ptr = function_def.exprs[*expr_id].stable_ptr();
        let node = file_sytnax.as_syntax_node().lookup_ptr(db, stable_ptr.untyped());
        let position = node.span_start_without_trivia(db).position_in_file(db, file_id).unwrap();

        writeln!(usages_str, "Block {}:{}:", position.line, position.col).unwrap();
        write!(usages_str, "  Usage: ").unwrap();
        for member_path in &usage.usage {
            write!(usages_str, "{:?}, ", member_path.debug(&expr_formatter)).unwrap();
        }
        writeln!(usages_str).unwrap();
        write!(usages_str, "  Changes: ").unwrap();
        for member_path in &usage.changes {
            write!(usages_str, "{:?}, ", member_path.debug(&expr_formatter)).unwrap();
        }
        writeln!(usages_str).unwrap();
        write!(usages_str, "  Introductions: ").unwrap();
        for var in &usage.introductions {
            write!(usages_str, "{:?}, ", var.debug(&expr_formatter)).unwrap();
        }
        writeln!(usages_str).unwrap();
    }

    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("usage".into(), usages_str),
    ])
}
