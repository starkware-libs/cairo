//! File-based tests for the equality analysis.

use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::equality_analysis::EqualityAnalysis;
use crate::LoweringStage;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};

cairo_lang_test_utils::test_file_test!(
    equality_analysis,
    "src/analysis/test_data",
    {
        equality: "equality",
    },
    test_equality_analysis
);

fn test_equality_analysis(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    // Use an earlier stage to see the snapshot/box operations before they're optimized away.
    let lowered = db.lowered_body(function_id, LoweringStage::PostBaseline);

    let (lowering_str, analysis_state_str) = if let Ok(lowered) = lowered {
        let lowering_str = formatted_lowered(db, Some(lowered));
        let block_states = EqualityAnalysis::analyze(lowered);

        // Format each block's state
        let analysis_state_str = block_states
            .iter()
            .enumerate()
            .filter_map(|(i, s)| s.as_ref().map(|state| (i, state)))
            .map(|(block_idx, state)| format!("Block {block_idx}:\n{:?}", state.debug(db)))
            .collect::<Vec<_>>()
            .join("\n\n");

        (lowering_str, analysis_state_str)
    } else {
        ("Lowering failed.".to_string(), "".to_string())
    };

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering".into(), lowering_str),
        ("analysis_state".into(), analysis_state_str),
    ]))
}
