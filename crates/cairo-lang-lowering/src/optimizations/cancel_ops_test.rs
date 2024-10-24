use std::ops::Deref;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::cancel_ops;
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::inline::apply_inlining;
use crate::optimizations::remappings::optimize_remappings;
use crate::optimizations::reorder_statements::reorder_statements;
use crate::reorganize_blocks::reorganize_blocks;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    test_cancel_ops,
    "src/optimizations/test_data",
    {
        cancel_ops: "cancel_ops",
    },
    test_cancel_ops
);

fn test_cancel_ops(
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
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    if !semantic_diagnostics.is_empty() {
        return TestRunnerResult::success(OrderedHashMap::from([(
            "semantic_diagnostics".into(),
            semantic_diagnostics,
        )]));
    }

    let mut before =
        db.concrete_function_with_body_postpanic_lowered(function_id).unwrap().deref().clone();
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

    apply_inlining(db, function_id, &mut before).unwrap();
    optimize_remappings(&mut before);
    reorganize_blocks(&mut before);
    reorder_statements(db, &mut before);

    let mut after = before.clone();
    cancel_ops(&mut after);

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        (
            "before".into(),
            format!("{:?}", before.debug(&LoweredFormatter::new(db, &before.variables))),
        ),
        (
            "after".into(),
            format!("{:?}", after.debug(&LoweredFormatter::new(db, &after.variables))),
        ),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ]))
}
