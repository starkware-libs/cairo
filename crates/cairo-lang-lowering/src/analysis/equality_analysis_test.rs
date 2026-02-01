//! File-based tests for the equality analysis.

use std::collections::BTreeMap;

use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::equality_analysis::{EqualityAnalysis, EqualityState};
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};
use crate::{Lowered, LoweringStage, VariableId};

cairo_lang_test_utils::test_file_test!(
    equality_analysis,
    "src/analysis/test_data",
    {
        equality: "equality",
    },
    test_equality_analysis
);

/// Formats the equality state for display in tests.
///
/// Shows equivalence classes and relationships in a human-readable format.
fn format_equality_state(state: &EqualityState, lowered: &Lowered<'_>) -> String {
    let mut lines = Vec::new();

    // Collect equivalence classes: map representative index -> member indices
    let mut class_members: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
    // Also keep track of var_ids for later use
    let mut idx_to_var: BTreeMap<usize, VariableId> = BTreeMap::new();

    for (var_id, _) in lowered.variables.iter() {
        let rep = state.get_representative(var_id);
        class_members.entry(rep.index()).or_default().push(var_id.index());
        idx_to_var.insert(var_id.index(), var_id);
    }

    // Format equivalence classes (only those with more than one member)
    let mut equiv_classes: Vec<String> = Vec::new();
    for members in class_members.values() {
        if members.len() > 1 {
            let vars: Vec<String> = members.iter().map(|i| format!("v{}", i)).collect();
            equiv_classes.push(format!("{{{}}}", vars.join(", ")));
        }
    }
    if !equiv_classes.is_empty() {
        equiv_classes.sort();
        lines.push(format!("Equivalences: {}", equiv_classes.join(", ")));
    }

    // Format snapshot relationships
    let mut snapshot_rels: Vec<String> = Vec::new();
    for &rep_idx in class_members.keys() {
        if let Some(&rep_var) = idx_to_var.get(&rep_idx)
            && let Some(snapshot_rep) = state.get_snapshot_var(rep_var)
        {
            snapshot_rels.push(format!("v{} -> @v{}", rep_idx, snapshot_rep.index()));
        }
    }
    if !snapshot_rels.is_empty() {
        snapshot_rels.sort();
        lines.push(format!("Snapshot: {}", snapshot_rels.join(", ")));
    }

    // Format box relationships
    let mut box_rels: Vec<String> = Vec::new();
    for &rep_idx in class_members.keys() {
        if let Some(&rep_var) = idx_to_var.get(&rep_idx)
            && let Some(boxed_rep) = state.get_boxed_var(rep_var)
        {
            box_rels.push(format!("v{} -> Box(v{})", rep_idx, boxed_rep.index()));
        }
    }
    if !box_rels.is_empty() {
        box_rels.sort();
        lines.push(format!("Box: {}", box_rels.join(", ")));
    }

    if lines.is_empty() { "No relationships.".to_string() } else { lines.join("\n") }
}

fn test_equality_analysis(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    // Use an earlier stage to see the snapshot/box operations before they're optimized away
    let lowered = db.lowered_body(function_id, LoweringStage::PostBaseline);

    let (lowering_str, analysis_state_str) = if let Ok(lowered) = lowered {
        let lowering_str = formatted_lowered(db, Some(lowered));
        let block_states = EqualityAnalysis::analyze(lowered);

        // Format each block's state
        let analysis_state_str = block_states
            .iter()
            .enumerate()
            .filter_map(|(i, s)| s.as_ref().map(|state| (i, state)))
            .map(|(block_idx, state)| {
                format!("Block {}:\n{}", block_idx, format_equality_state(state, lowered))
            })
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
