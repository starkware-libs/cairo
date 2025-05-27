use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::create_graph::create_graph_expr_if;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    create_graph,
    "src/lower/flow_control/test_data",
    {
        if_: "if",
    },
    test_create_graph
);

fn test_create_graph(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function_code"].as_str(),
        "foo",
        inputs.get("module_code").unwrap_or(&"".into()),
    )
    .split();
    let semantic_db: &dyn SemanticGroup = db;

    // Extract the expression from the function's body.
    let semantic::Expr::Block(semantic::ExprBlock { tail: Some(expr_id), .. }) =
        semantic_db.expr_semantic(test_function.function_id, test_function.body)
    else {
        panic!("Expected a block expression.");
    };

    let expr = semantic_db.expr_semantic(test_function.function_id, expr_id);
    let expr_formatter = ExprFormatter { db, function_id: test_function.function_id };

    let graph = match &expr {
        semantic::Expr::If(expr) => create_graph_expr_if(expr),
        _ => {
            panic!("Unsupported expression: {:?}", expr.debug(&expr_formatter));
        }
    };

    let error = verify_diagnostics_expectation(args, &semantic_diagnostics);

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("graph".into(), format!("{graph:?}")),
            ("semantic_diagnostics".into(), semantic_diagnostics),
        ]),
        error,
    }
}
