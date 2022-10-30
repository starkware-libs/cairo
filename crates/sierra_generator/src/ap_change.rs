#[cfg(test)]
#[path = "ap_change_test.rs"]
mod test;

use defs::ids::FreeFunctionId;
use sierra::program::{GenBranchTarget, GenStatement};
use utils::ordered_hash_map::OrderedHashMap;

use crate::db::SierraGenGroup;
use crate::pre_sierra;

/// Query implementation of [SierraGenGroup::contains_cycle].
pub fn contains_cycle(db: &dyn SierraGenGroup, function_id: FreeFunctionId) -> Option<bool> {
    let lowered_function = &*db.free_function_lowered(function_id)?;
    for (_, block) in &lowered_function.blocks {
        for statement in &block.statements {
            if let lowering::Statement::Call(statement_call) = statement {
                match db.lookup_intern_function(statement_call.function) {
                    semantic::FunctionLongId::Concrete(concrete) => {
                        match concrete.generic_function {
                            defs::ids::GenericFunctionId::Free(free_function_id) => {
                                if db.contains_cycle(free_function_id)? {
                                    return Some(true);
                                }
                            }
                            defs::ids::GenericFunctionId::Extern(_) => {}
                        }
                    }
                    semantic::FunctionLongId::Missing => {
                        return None;
                    }
                }
            }
        }
    }

    Some(false)
}

/// Cycle handling for [SierraGenGroup::contains_cycle].
pub fn contains_cycle_handle_cycle(
    _db: &dyn SierraGenGroup,
    _cycle: &[String],
    _function_id: &FreeFunctionId,
) -> Option<bool> {
    Some(true)
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum ApChange {
    Known(usize),
    Unknown,
}

/// Query implementation of [SierraGenGroup::get_ap_change].
pub fn get_ap_change(db: &dyn SierraGenGroup, function_id: FreeFunctionId) -> Option<ApChange> {
    let function = &*db.free_function_sierra(function_id)?;

    // The ap change from the beginning of the function to the current instruction.
    // None means the current instruction is not reachable.
    let mut current_ap_change_opt: Option<usize> = Some(0);

    // The ap change of the function.
    // None means it's not known yet.
    let mut function_ap_change_opt: Option<usize> = None;

    let mut ap_change_at_label = OrderedHashMap::<pre_sierra::LabelId, usize>::default();

    for statement in &function.body {
        match statement {
            pre_sierra::Statement::Sierra(GenStatement::Invocation(invocation)) => {
                let current_ap_change = current_ap_change_opt
                    .expect("Internal compiler error: found unreachable statement.");
                // Mark the next instruction as unreachable. This will be overridden if we have a
                // Fallthrough branch.
                current_ap_change_opt = None;
                // Go over the branches.
                for branch in &invocation.branches {
                    // TODO(lior): Replace ap_change with the actual ap-change of the branch.
                    let branch_ap_change = 0;
                    let new_ap_change = current_ap_change + branch_ap_change;
                    match branch.target {
                        GenBranchTarget::Fallthrough => {
                            if !merge_ap_change(&mut current_ap_change_opt, new_ap_change) {
                                return Some(ApChange::Unknown);
                            }
                        }
                        GenBranchTarget::Statement(label_id) => {
                            let mut at_label = ap_change_at_label.get(&label_id).cloned();
                            // Merge with the next jump.
                            if !merge_ap_change(&mut at_label, new_ap_change) {
                                return Some(ApChange::Unknown);
                            }
                            ap_change_at_label.insert(label_id, at_label.unwrap());
                        }
                    }
                }
            }
            pre_sierra::Statement::Sierra(GenStatement::Return(_return_statement)) => {
                let current_ap_change = current_ap_change_opt
                    .expect("Internal compiler error: found unreachable statement.");
                // Mark the next instruction as unreachable.
                current_ap_change_opt = None;
                if !merge_ap_change(&mut function_ap_change_opt, current_ap_change) {
                    return Some(ApChange::Unknown);
                }
            }
            pre_sierra::Statement::Label(pre_sierra::Label { id: label_id }) => {
                if let Some(at_current_label) = ap_change_at_label.swap_remove(label_id) {
                    if !merge_ap_change(&mut current_ap_change_opt, at_current_label) {
                        return Some(ApChange::Unknown);
                    }
                }
            }
            pre_sierra::Statement::PushValues(_) => panic!(
                "Unexpected pre_sierra::Statement::PushValues in \
                 calculate_statement_dups_and_drops()."
            ),
        }
    }

    // Make sure all labels were visited.
    assert!(
        ap_change_at_label.is_empty(),
        "Internal compiler error: Unhandled label in 'store_variables'."
    );
    assert!(
        current_ap_change_opt.is_none(),
        "Internal compiler error: Found a reachable statement at the end of the function."
    );
    Some(ApChange::Known(
        function_ap_change_opt.expect("Internal compiler error: Function does not return."),
    ))
}

/// Merges two ap-changes.
/// current_ap_change_opt=None means there is not existing ap-change.
/// In this case it will be replaced by new_ap_change is
/// returned).
/// Returns true if the result is a known ap change and false otherwise.
fn merge_ap_change(current_ap_change_opt: &mut Option<usize>, new_ap_change: usize) -> bool {
    if let Some(_current_ap_change) = *current_ap_change_opt {
        // TODO(lior): Use the if below, once ap values are correct.
        // if new_ap_change != current_ap_change {
        return false;
    } else {
        *current_ap_change_opt = Some(new_ap_change);
    }
    true
}
