use sierra::extensions::lib_func::{BranchSignature, OutputVarInfo};
use sierra::extensions::OutputVarReferenceInfo;
use utils::ordered_hash_map::OrderedHashMap;

use super::known_stack::KnownStack;

/// Represents information known about the state of the variables.
/// For example, which variable contains a deferred value and which variable is on the stack.
#[derive(Clone, Debug, Default)]
pub struct State {
    /// A map from [sierra::ids::VarId] of a deferred reference
    /// (for example, `[ap - 1] + [ap - 2]`) to its type.
    pub deferred_variables: OrderedHashMap<sierra::ids::VarId, sierra::ids::ConcreteTypeId>,
    /// The information known about the top of the stack.
    pub known_stack: KnownStack,
}
impl State {
    /// Registers output variables of a libfunc. See `add_output`.
    /// Clears the stack if needed.
    pub fn register_outputs(
        &mut self,
        results: &[sierra::ids::VarId],
        branch_signature: &BranchSignature,
    ) {
        // Clear the stack if needed.
        match branch_signature.ap_change {
            sierra::extensions::lib_func::SierraApChange::NotImplemented
            | sierra::extensions::lib_func::SierraApChange::Unknown => {
                self.clear_known_stack();
            }
            sierra::extensions::lib_func::SierraApChange::Known => {}
        }

        for (var, var_info) in itertools::zip_eq(results, &branch_signature.vars) {
            self.register_output(var.clone(), var_info);
        }

        // Update `known_stack_size`. It is one more than the maximum of the indices in
        // `variables_on_stack` (or 0 if empty).
        self.known_stack.update_offset_by_max();
    }

    /// Register an output variable of a libfunc.
    ///
    /// If the variable is marked as Deferred output by the libfunc, it is added to
    /// `self.deferred_variables`.
    fn register_output(&mut self, res: sierra::ids::VarId, output_info: &OutputVarInfo) {
        self.deferred_variables.swap_remove(&res);
        self.known_stack.remove_variable(&res);
        match output_info.ref_info {
            OutputVarReferenceInfo::Deferred(_) => {
                // TODO(lior): Mark it as AddConst according to kind, instead of a regular Deferred.
                self.deferred_variables.insert(res, output_info.ty.clone());
            }
            OutputVarReferenceInfo::NewTempVar { idx } => {
                self.known_stack.insert(res, idx);
            }
            OutputVarReferenceInfo::SameAsParam { .. }
            | OutputVarReferenceInfo::NewLocalVar
            | OutputVarReferenceInfo::Const => {}
        }
    }

    /// Clears the known information about the stack.
    ///
    /// This is called where the change in the value of `ap` is not known at compile time.
    fn clear_known_stack(&mut self) {
        self.known_stack.clear();
    }
}

/// Merges the information from two [State]s.
/// Used to determine the state at the merge of two code branches.
///
/// If one of the given states is None, the second is returned.
pub fn merge_optional_states(a_opt: Option<State>, b_opt: Option<State>) -> Option<State> {
    match (a_opt, b_opt) {
        (None, None) => None,
        (None, Some(b)) => Some(b),
        (Some(a), None) => Some(a),
        (Some(a), Some(b)) => {
            assert_eq!(
                a.deferred_variables, b.deferred_variables,
                "Internal compiler error: Branch merges with different list of deferred variables \
                 is not supported yet."
            );
            Some(State {
                deferred_variables: a.deferred_variables,
                known_stack: a.known_stack.merge_with(&b.known_stack),
            })
        }
    }
}
