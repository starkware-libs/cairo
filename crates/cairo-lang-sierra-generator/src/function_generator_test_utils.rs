use std::sync::Arc;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagLongId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::SierraGenGroup;
use crate::replace_ids::replace_sierra_ids;
use crate::test_utils::SierraGenDatabaseForTesting;

/// Compiles a single function to Sierra and checks the generated code.
pub fn test_function_generator(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    // Tests have recursions for revoking AP. Automatic addition of 'withdraw_gas` calls would add
    // unnecessary complication to them.

    let db = if let Some(v) = args.get("future_sierra").map(|s| s.to_lowercase())
        && v == "true"
    {
        // When turning on future_sierra, we might affect other tests using the same db, so an empty
        // db is needed.
        let mut db = SierraGenDatabaseForTesting::new_empty();
        db.set_flag(
            FlagLongId("add_withdraw_gas".into()),
            Some(Arc::new(Flag::AddWithdrawGas(false))),
        );
        db.set_flag(FlagLongId("future_sierra".into()), Some(Arc::new(Flag::FutureSierra(true))));
        db
    } else {
        SierraGenDatabaseForTesting::without_add_withdraw_gas()
    };
    let db = &db;
    // Parse code and create semantic model.
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();

    // Verify that there are no diagnostics.
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id);

    // Compile the function.
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let function = db.function_with_body_sierra(function_id);
    let sierra_code: String = function.map_or("None".into(), |func| {
        func.body
            .iter()
            .map(|x| replace_sierra_ids(db, x).statement.to_string(db))
            .collect::<Vec<String>>()
            .join("\n")
    });

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        (
            "lowering_diagnostics".into(),
            lowering_diagnostics.map_or("".into(), |diagnostics| diagnostics.format(db)),
        ),
        ("sierra_code".into(), sierra_code),
    ]))
}
