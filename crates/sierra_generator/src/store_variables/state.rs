use sierra::extensions::lib_func::{BranchSignature, DeferredOutputKind, OutputVarInfo};
use sierra::extensions::OutputVarReferenceInfo;
use utils::ordered_hash_map::OrderedHashMap;

use super::known_stack::KnownStack;

/// Represents the known information about a Sierra variable which contains a deferred value.
/// For example, `[ap - 1] + [ap - 2]`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DeferredVariableInfo {
    /// The type of the variable.
    pub ty: sierra::ids::ConcreteTypeId,
    /// The deferred type.
    pub kind: DeferredVariableKind,
}

/// The type of a deferred variable.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DeferredVariableKind {
    /// See [DeferredOutputKind::AddConst].
    AddConst,
    /// See [DeferredOutputKind::Generic].
    Generic,
}

/// Represents information known about the state of the variables.
/// For example, which variable contains a deferred value and which variable is on the stack.
#[derive(Clone, Debug, Default)]
pub struct State {
    /// A map from [sierra::ids::VarId] of a deferred reference
    /// (for example, `[ap - 1] + [ap - 2]`) to its type.
    pub deferred_variables: OrderedHashMap<sierra::ids::VarId, DeferredVariableInfo>,
    /// The information known about the top of the stack.
    pub known_stack: KnownStack,
}
impl State {
    /// Registers output variables of a libfunc. See [Self::register_outputs].
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
            sierra::extensions::lib_func::SierraApChange::Known(_) => {}
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
        match &output_info.ref_info {
            OutputVarReferenceInfo::Deferred(kind) => {
                let deferred_variable_info_kind = match kind {
                    DeferredOutputKind::AddConst { .. } => DeferredVariableKind::AddConst,
                    DeferredOutputKind::Generic => DeferredVariableKind::Generic,
                };
                self.deferred_variables.insert(
                    res,
                    DeferredVariableInfo {
                        ty: output_info.ty.clone(),
                        kind: deferred_variable_info_kind,
                    },
                );
            }
            OutputVarReferenceInfo::NewTempVar { idx } => {
                self.known_stack.insert(res, *idx);
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
