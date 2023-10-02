use cairo_lang_sierra as sierra;
use cairo_lang_sierra::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, OutputVarInfo, SierraApChange,
};
use cairo_lang_sierra::extensions::OutputVarReferenceInfo;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

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
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DeferredVariableKind {
    /// See [DeferredOutputKind::Const].
    Const,
    /// See [DeferredOutputKind::AddConst].
    AddConst,
    /// See [DeferredOutputKind::Generic].
    Generic,
}

/// Represents the state of Sierra variable.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VarState {
    /// The variable is a temporary variable with the given type.
    TempVar {
        ty: sierra::ids::ConcreteTypeId,
    },
    /// The variable is deferred with the given DeferredVariableInfo.
    Deferred {
        info: DeferredVariableInfo,
    },
    LocalVar,
}

/// Represents information known about the state of the variables.
/// For example, which variable contains a deferred value and which variable is on the stack.
#[derive(Clone, Debug, Default)]
pub struct State {
    /// A map from [sierra::ids::VarId] of to its state.
    pub variables: OrderedHashMap<sierra::ids::VarId, VarState>,
    /// The information known about the top of the stack.
    pub known_stack: KnownStack,
}
impl State {
    /// Registers output variables of a libfunc. See [Self::register_output].
    /// Clears the stack if needed.
    pub fn register_outputs(
        &mut self,
        results: &[sierra::ids::VarId],
        branch_signature: &BranchSignature,
        args: &[sierra::ids::VarId],
        deferred_args: &OrderedHashMap<sierra::ids::VarId, DeferredVariableInfo>,
    ) {
        // Clear the stack if needed.
        match branch_signature.ap_change {
            SierraApChange::BranchAlign
            | SierraApChange::Unknown
            | SierraApChange::Known { new_vars_only: false } => {
                // Clear the stack in this case since it's possible that undeclared (not part of the
                // output) temporary variables are created by the libfunc.
                self.clear_known_stack();
            }
            SierraApChange::Known { new_vars_only: true } => {}
        }

        for (var, var_info) in itertools::zip_eq(results, &branch_signature.vars) {
            self.register_output(var.clone(), var_info, args, deferred_args);
        }

        // Update `known_stack_size`. It is one more than the maximum of the indices in
        // `variables_on_stack` (or 0 if empty).
        self.known_stack.update_offset_by_max();
    }

    /// Register an output variable of a libfunc in [Self::variables].
    fn register_output(
        &mut self,
        res: sierra::ids::VarId,
        output_info: &OutputVarInfo,
        args: &[sierra::ids::VarId],
        deferred_args: &OrderedHashMap<sierra::ids::VarId, DeferredVariableInfo>,
    ) {
        let mut add_to_known_stack: Option<isize> = None;

        let var_state = match &output_info.ref_info {
            OutputVarReferenceInfo::Deferred(kind) => VarState::Deferred {
                info: DeferredVariableInfo {
                    ty: output_info.ty.clone(),
                    kind: match kind {
                        DeferredOutputKind::Const => DeferredVariableKind::Const,
                        DeferredOutputKind::AddConst { .. } => DeferredVariableKind::AddConst,
                        DeferredOutputKind::Generic => DeferredVariableKind::Generic,
                    },
                },
            },
            OutputVarReferenceInfo::NewTempVar { idx } => {
                add_to_known_stack = Some(idx.into_or_panic::<isize>());
                VarState::TempVar { ty: output_info.ty.clone() }
            }
            OutputVarReferenceInfo::SimpleDerefs => {
                VarState::TempVar { ty: output_info.ty.clone() }
            }
            OutputVarReferenceInfo::SameAsParam { param_idx }
            | OutputVarReferenceInfo::PartialParam { param_idx } => {
                let arg = &args[*param_idx];
                if let Some(deferred_info) = deferred_args.get(arg) {
                    VarState::Deferred {
                        info: DeferredVariableInfo {
                            // Note that the output type may differ from the param type.
                            ty: output_info.ty.clone(),
                            kind: deferred_info.kind,
                        },
                    }
                } else {
                    match self.variables.get(arg) {
                        Some(VarState::TempVar { .. }) => {
                            if matches!(
                                output_info.ref_info,
                                OutputVarReferenceInfo::SameAsParam { .. }
                            ) {
                                add_to_known_stack = self.known_stack.get(arg);
                            }
                            VarState::TempVar { ty: output_info.ty.clone() }
                        }
                        Some(VarState::LocalVar) => VarState::LocalVar,
                        _ => {
                            panic!("Unknown state for {}", arg);
                        }
                    }
                }
            }
            OutputVarReferenceInfo::NewLocalVar => VarState::LocalVar,
        };
        self.variables.insert(res.clone(), var_state);

        self.known_stack.remove_variable(&res);
        if let Some(idx) = add_to_known_stack {
            self.known_stack.insert_signed(res, idx);
        }
    }

    /// Clears the known information about the stack.
    ///
    /// This is called where the change in the value of `ap` is not known at compile time.
    fn clear_known_stack(&mut self) {
        self.known_stack.clear();
    }

    /// Marks `dst` as a rename of `src`.
    ///
    /// Updates [Self::known_stack] and [Self::variables] if necessary.
    pub fn rename_var(&mut self, src: &sierra::ids::VarId, dst: &sierra::ids::VarId) {
        self.known_stack.clone_if_on_stack(src, dst);
        if let Some(var_state) = self.variables.get(src) {
            self.variables.insert(dst.clone(), var_state.clone());
        }
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
            // Merge the lists of deferred variables.
            let mut variables = OrderedHashMap::default();
            for (var, var_state_a) in a.variables {
                if let Some(var_state_b) = b.variables.get(&var) {
                    assert_eq!(
                        var_state_a, *var_state_b,
                        "Internal compiler error: Found different deferred variables."
                    );
                    variables.insert(var, var_state_a);
                }
            }

            Some(State { variables, known_stack: a.known_stack.merge_with(&b.known_stack) })
        }
    }
}
