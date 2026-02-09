//! File-based tests for the dominator analysis.

use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::Itertools;

use super::Dominators;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};
use crate::{BlockId, LoweringStage};

cairo_lang_test_utils::test_file_test!(
    dominator,
    "src/analysis/test_data",
    {
        dominator: "dominator",
    },
    test_dominator_analysis
);

fn test_dominator_analysis(
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
                let doms = dominators.dominators_of(block_id);
                let mut dom_strs = doms.iter().map(|d| d.0).sorted().map(|d| format!("blk{d}"));
                format!("blk{}: {{{}}}", block_id.0, dom_strs.join(", "))
            })
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

#[test]
fn test_dominates() {
    // Double diamond CFG dominator sets:
    //   blk0:        {blk0}
    //   blk1, blk2:  {blk0, self}     (arms)
    //   blk3:        {blk0, blk3}     (merge)
    //   blk4, blk5:  {blk3, self, blk0}     (arms)
    //   blk6:        {blk3, blk0, blk6}          (merge)
    let dominators = Dominators {
        per_block: vec![
            OrderedHashSet::from_iter([BlockId(0)]),
            OrderedHashSet::from_iter([BlockId(0), BlockId(1)]),
            OrderedHashSet::from_iter([BlockId(0), BlockId(2)]),
            OrderedHashSet::from_iter([BlockId(0), BlockId(3)]),
            OrderedHashSet::from_iter([BlockId(3), BlockId(4), BlockId(0)]),
            OrderedHashSet::from_iter([BlockId(3), BlockId(5), BlockId(0)]),
            OrderedHashSet::from_iter([BlockId(3), BlockId(0), BlockId(6)]),
        ],
    };

    // Reflexivity: every block dominates itself.
    for i in 0..dominators.per_block.len() {
        assert!(dominators.dominates(BlockId(i), BlockId(i)));
    }

    // Root dominates every block.
    for i in 0..dominators.per_block.len() {
        assert!(dominators.dominates(BlockId(0), BlockId(i)));
    }

    // Merge block dominates its arms and final merge block.
    assert!(dominators.dominates(BlockId(3), BlockId(4)));
    assert!(dominators.dominates(BlockId(3), BlockId(5)));
    assert!(dominators.dominates(BlockId(3), BlockId(6)));

    // Diamond arms do not dominate each other.
    assert!(!dominators.dominates(BlockId(1), BlockId(2)));
    assert!(!dominators.dominates(BlockId(2), BlockId(1)));

    // Arms do not dominate the merge block.
    assert!(!dominators.dominates(BlockId(1), BlockId(3)));
    assert!(!dominators.dominates(BlockId(2), BlockId(3)));

    // Merge block does not dominate the arms.
    assert!(!dominators.dominates(BlockId(3), BlockId(1)));
    assert!(!dominators.dominates(BlockId(3), BlockId(2)));
}
