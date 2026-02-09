//! File-based tests for the def-site analysis.

use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::def_site::DefSiteAnalysis;
use crate::LoweringStage;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};

cairo_lang_test_utils::test_file_test!(
    def_site,
    "src/analysis/test_data",
    {
        def_site: "def_site",
    },
    test_def_site_analysis
);

fn test_def_site_analysis(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let lowered = db.lowered_body(function_id, LoweringStage::PostBaseline);

    let (lowering_str, result_str) = if let Ok(lowered) = lowered.cloned() {
        let lowering_str = formatted_lowered(db, Some(&lowered));
        let result_str = DefSiteAnalysis::analyze(&lowered).to_string();
        (lowering_str, result_str)
    } else {
        ("Lowering failed.".to_string(), "".to_string())
    };

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering".into(), lowering_str),
        ("result".into(), result_str),
    ]))
}
