//! File-based tests for the dominator analysis.

use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::dom::Dominators;
use crate::LoweringStage;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};

cairo_lang_test_utils::test_file_test!(
    dom,
    "src/analysis/test_data",
    {
        dom: "dom",
    },
    test_dom_analysis
);

fn test_dom_analysis(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let lowered = db.lowered_body(function_id, LoweringStage::PostBaseline);

    let (lowering_str, dominators_str) = if let Ok(lowered) = lowered.cloned() {
        let lowering_str = formatted_lowered(db, Some(&lowered));
        let dominators = Dominators::analyze(&lowered);

        let dominators_str = lowered
            .blocks
            .iter()
            .map(|(block_id, _)| {
                if let Some(doms) = dominators.dominators_of(block_id) {
                    let mut dom_list: Vec<usize> = doms.iter().map(|d| d.0).collect();
                    dom_list.sort();
                    let dom_strs: Vec<String> =
                        dom_list.iter().map(|d| format!("blk{d}")).collect();
                    format!("blk{}: {{{}}}", block_id.0, dom_strs.join(", "))
                } else {
                    format!("blk{}: unreachable", block_id.0)
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        (lowering_str, dominators_str)
    } else {
        ("Lowering failed.".to_string(), "".to_string())
    };

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering".into(), lowering_str),
        ("dominators".into(), dominators_str),
    ]))
}
