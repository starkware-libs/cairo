#[cfg(test)]
#[path = "ap_change_test.rs"]
mod test;

use defs::ids::FreeFunctionId;
use itertools::zip_eq;
use sierra::program::{GenBranchTarget, GenStatement};
use utils::ordered_hash_map::OrderedHashMap;

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::utils::get_libfunc_signature;

/// Query implementation of [SierraGenGroup::contains_cycle].
pub fn contains_cycle(db: &dyn SierraGenGroup, function_id: FreeFunctionId) -> Option<bool> {
    let lowered_function = &*db.free_function_lowered(function_id)?;
    for (_, block) in &lowered_function.blocks {
        for statement in &block.statements {
            if let lowering::Statement::Call(statement_call) = statement {
                let concrete = db.lookup_intern_function(statement_call.function).function;
                match concrete.generic_function {
                    defs::ids::GenericFunctionId::Free(free_function_id) => {
                        if db.contains_cycle(free_function_id)? {
                            return Some(true);
                        }
                    }
                    defs::ids::GenericFunctionId::Extern(_) => {}
                    defs::ids::GenericFunctionId::TraitFunction(_) => {
                        panic!("Trait function should be replaced with concrete functions.")
                    }
                    defs::ids::GenericFunctionId::ImplFunction(_) => todo!(),
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
    // The implementation of get_ap_change() may call this function recursively. To guarantee no
    // salsa query cycles are created, we first verify that there are no cycles.
    if db.contains_cycle(function_id)? {
        return Some(ApChange::Unknown);
    }

    let function = &*db.free_function_sierra(function_id)?;

    // The ap change from the beginning of the function to the current instruction.
    // None means the current instruction is not reachable.
    let mut current_ap_change_opt = CurrentKnownApChange(Some(0));

    // The ap change of the function.
    // None means it's not known yet.
    let mut function_ap_change_opt = CurrentKnownApChange(None);

    let mut ap_change_at_label = OrderedHashMap::<pre_sierra::LabelId, usize>::default();

    for statement in &function.body {
        match statement {
            pre_sierra::Statement::Sierra(GenStatement::Invocation(invocation)) => {
                // Get the ap-change value, and mark the next instruction as unreachable.
                // This will be overridden if we have a Fallthrough branch.
                let current_ap_change = current_ap_change_opt
                    .pop("Internal compiler error: found an unreachable statement.");

                let signature = get_libfunc_signature(db, invocation.libfunc_id.clone());

                // Go over the branches.
                for (branch, branch_signature) in
                    zip_eq(&invocation.branches, signature.branch_signatures)
                {
                    let branch_ap_change = match branch_signature.ap_change {
                        sierra::extensions::lib_func::SierraApChange::Known(value) => value,
                        sierra::extensions::lib_func::SierraApChange::Unknown
                        | sierra::extensions::lib_func::SierraApChange::NotImplemented
                        | sierra::extensions::lib_func::SierraApChange::FinalizeLocals => {
                            return Some(ApChange::Unknown);
                        }
                    };
                    let new_ap_change = current_ap_change + branch_ap_change;
                    match branch.target {
                        GenBranchTarget::Fallthrough => {
                            if !current_ap_change_opt.merge_ap_change(new_ap_change) {
                                return Some(ApChange::Unknown);
                            }
                        }
                        GenBranchTarget::Statement(label_id) => {
                            let mut at_label =
                                CurrentKnownApChange(ap_change_at_label.get(&label_id).cloned());
                            // Merge with the next jump.
                            if !at_label.merge_ap_change(new_ap_change) {
                                return Some(ApChange::Unknown);
                            }
                            ap_change_at_label.insert(label_id, at_label.0.unwrap());
                        }
                    }
                }
            }
            pre_sierra::Statement::Sierra(GenStatement::Return(_return_statement)) => {
                let current_ap_change = current_ap_change_opt
                    .pop("Internal compiler error: found an unreachable statement.");
                if !function_ap_change_opt.merge_ap_change(current_ap_change) {
                    return Some(ApChange::Unknown);
                }
            }
            pre_sierra::Statement::Label(pre_sierra::Label { id: label_id }) => {
                if let Some(at_current_label) = ap_change_at_label.swap_remove(label_id) {
                    if !current_ap_change_opt.merge_ap_change(at_current_label) {
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
        function_ap_change_opt.pop("Internal compiler error: Function does not return."),
    ))
}

/// Helper struct for [get_ap_change].
struct CurrentKnownApChange(Option<usize>);

impl CurrentKnownApChange {
    /// Merges two ap-changes.
    /// current_ap_change_opt=None means there is not existing ap-change.
    /// In this case it will be replaced by new_ap_change is
    /// returned).
    /// Returns true if the result is a known ap change and false otherwise.
    fn merge_ap_change(&mut self, new_ap_change: usize) -> bool {
        if let Some(_current_ap_change) = self.0 {
            // TODO(lior): Use the if below, once ap values are correct.
            // if new_ap_change != current_ap_change {
            return false;
        } else {
            self.0 = Some(new_ap_change);
        }
        true
    }

    fn pop(&mut self, error_message: &str) -> usize {
        let res = self.0.expect(error_message);
        self.0 = None;
        res
    }

    fn is_none(&self) -> bool {
        self.0.is_none()
    }
}
