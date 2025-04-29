use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use num_bigint::BigInt;
use num_traits::One;

use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::{ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, SpecializedFunction};
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    specialized,
    "src/lower/test_data",
    {
        specialized :"specialized",
    },
    test_specialized_function
);

fn test_specialized_function(
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

    let core = db.core_info();

    let specialized_func = SpecializedFunction {
        base: function_id,
        args: Arc::new([None, Some(ConstValue::Int(BigInt::one(), core.felt252))]),
    };

    let specialized_func = ConcreteFunctionWithBodyLongId::Specialized(specialized_func).intern(db);
    let lowered = db.final_concrete_function_with_body_lowered(specialized_func).unwrap();
    let lowered_formatter = LoweredFormatter::new(db, &lowered.variables);
    let lowered = format!("{:?}", lowered.debug(&lowered_formatter));

    let lowering_diagnostics =
        db.module_lowering_diagnostics(test_function.module_id).unwrap_or_default();

    TestRunnerResult::success(OrderedHashMap::from([
        ("full_path".into(), specialized_func.full_path(db)),
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering".into(), lowered),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ]))
}
