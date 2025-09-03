use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::cse;
use crate::LoweringStage;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};

cairo_lang_test_utils::test_file_test!(
    cse,
    "src/optimizations/test_data",
    {
        cse: "cse",
    },
    test_cse
);

fn test_cse(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs.get("module_code").map(|s| s.as_str()).unwrap_or(""),
    )
    .split();

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let lowered = db.lowered_body(function_id, LoweringStage::Monomorphized);

    if let Ok(lowered) = lowered {
        let before_str = formatted_lowered(db, Some(lowered));

        let mut lowered_clone = (*lowered).clone();
        cse(&mut lowered_clone);
        let after_str = formatted_lowered(db, Some(&lowered_clone));

        TestRunnerResult::success(OrderedHashMap::from([
            ("before".into(), before_str),
            ("after".into(), after_str),
        ]))
    } else {
        TestRunnerResult::success(OrderedHashMap::from([(
            "semantic_diagnostics".into(),
            semantic_diagnostics,
        )]))
    }
}
