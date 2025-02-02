use cairo_lang_debug::DebugWithDb;
use cairo_lang_filesystem::ids::BlobLongId;
use cairo_lang_semantic::test_utils::{setup_test_function, setup_test_function_ex};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::CacheSavingContext;
use crate::FlatLowered;
use crate::cached::{DefsFunctionWithBodyIdCached, MultiLoweringCached};
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    precompute,
    "src/precompute/test_data",
    {
        precompute :"precompute",
    },
    test_precompute_check
);

fn formatted_lowered(db: &dyn LoweringGroup, lowered: &FlatLowered) -> String {
    let lowered_formatter = LoweredFormatter::new(db, &lowered.variables);
    format!("{:?}", lowered.debug(&lowered_formatter))
}

fn test_precompute_check(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();

    let (test_function, _semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let mut ctx = CacheSavingContext::new(db, test_function.module_id.owning_crate(db));

    let semantic_id = function_id.function_with_body_id(db).base_semantic_function(db);
    let multi = db.priv_function_with_body_multi_lowering(semantic_id).unwrap();
    let precomputed = vec![(
        DefsFunctionWithBodyIdCached::new(semantic_id, &mut ctx.semantic_ctx),
        MultiLoweringCached::new((*multi).clone(), &mut ctx),
    )];
    let _lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();
    let artifact = if let Ok(lowered) =
        bincode::serialize(&(&ctx.lookups, &ctx.semantic_ctx.lookups, precomputed))
    {
        lowered
    } else {
        "".into()
    };
    let new_db = LoweringDatabaseForTesting::new();
    let precompute_file = BlobLongId::Virtual(artifact.into()).intern(&new_db);
    let (test_function, semantic_diagnostics) = setup_test_function_ex(
        &new_db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
        None,
        Some(precompute_file),
    )
    .split();

    let function_id: ConcreteFunctionWithBodyId =
        ConcreteFunctionWithBodyId::from_semantic(&new_db, test_function.concrete_function_id);

    let lowered = new_db.final_concrete_function_with_body_lowered(function_id);
    if let Ok(lowered) = &lowered {
        assert!(
            lowered.blocks.iter().all(|(_, b)| b.is_set()),
            "There should not be any unset flat blocks"
        );
    }
    let diagnostics =
        new_db.module_lowering_diagnostics(test_function.module_id).unwrap_or_default();
    let formatted_lowering_diagnostics = diagnostics.format(&new_db);
    let combined_diagnostics =
        format!("{}\n{}", semantic_diagnostics, formatted_lowering_diagnostics);
    let error = verify_diagnostics_expectation(args, &combined_diagnostics);
    let lowering_format = lowered.map(|lowered| formatted_lowered(&new_db, &lowered)).unwrap_or(
        "<Failed lowering function - run with RUST_LOG=warn (or less) to see diagnostics>"
            .to_string(),
    );
    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowering_diagnostics".into(), formatted_lowering_diagnostics),
            ("lowering_flat".into(), lowering_format),
        ]),
        error,
    }
}
