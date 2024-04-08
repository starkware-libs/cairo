use std::fmt::Write;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::BlockUsages;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    usage,
    "src/lower/test_data",
    {
        usage :"usage",
    },
    test_function_usage
);

fn test_function_usage(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();

    let file_id = db.module_file(test_function.function_id.module_file_id(db)).unwrap();

    let expr_formatter = ExprFormatter { db, function_id: test_function.function_id };
    let function_def =
        db.function_body(test_function.concrete_function_id.function_with_body_id(db)).unwrap();
    let usages = BlockUsages::from_function_body(&function_def);

    let mut usages_str = String::new();
    for (expr_id, usage) in usages.block_usages.iter() {
        let expr = &function_def.exprs[*expr_id];
        let stable_ptr = expr.stable_ptr();
        let node = stable_ptr.untyped().lookup(db);
        let position = node.span_start_without_trivia(db).position_in_file(db, file_id).unwrap();

        match expr {
            semantic::Expr::Block(_) => write!(usages_str, "Block").unwrap(),
            semantic::Expr::Loop(_) => write!(usages_str, "Loop").unwrap(),
            semantic::Expr::While(_) => write!(usages_str, "While").unwrap(),
            _ => unreachable!(),
        }
        writeln!(usages_str, " {}:{}:", position.line, position.col).unwrap();
        write!(usages_str, "  Usage: ").unwrap();
        for (_, expr) in usage.usage.iter() {
            write!(usages_str, "{:?}, ", expr.debug(&expr_formatter)).unwrap();
        }
        writeln!(usages_str).unwrap();
        write!(usages_str, "  Changes: ").unwrap();
        for (_, expr) in usage.changes.iter() {
            write!(usages_str, "{:?}, ", expr.debug(&expr_formatter)).unwrap();
        }
        writeln!(usages_str).unwrap();
        write!(usages_str, "  Introductions: ").unwrap();
        for var in &usage.introductions {
            write!(usages_str, "{:?}, ", var.debug(&expr_formatter)).unwrap();
        }
        writeln!(usages_str).unwrap();
    }

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("usage".into(), usages_str),
    ]))
}
