use cairo_lang_sierra as sierra;
use cairo_lang_sierra::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, OutputVarInfo, SierraApChange,
};
use cairo_lang_sierra::extensions::OutputVarReferenceInfo;
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

/// Represents information known about the state of the variables.
/// For example, which variable contains a deferred value and which variable is on the stack.
#[derive(Clone, Debug, Default)]
pub struct State {
    /// A map from [sierra::ids::VarId] of a deferred reference
    /// (for example, `[ap - 1] + [ap - 2]`) to [DeferredVariableInfo].
    pub deferred_variables: OrderedHashMap<sierra::ids::VarId, DeferredVariableInfo>,
    /// A map from [sierra::ids::VarId] of temporary variables to their type.
    pub temporary_variables: OrderedHashMap<sierra::ids::VarId, sierra::ids::ConcreteTypeId>,
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
        args: &[sierra::ids::VarId],
        deferred_args: &OrderedHashMap<sierra::ids::VarId, DeferredVariableInfo>,
    ) {
        // Clear the stack if needed.
        match branch_signature.ap_change {
            SierraApChange::NotImplemented
            | SierraApChange::BranchAlign
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

    /// Register an output variable of a libfunc.
    ///
    /// If the variable is marked as Deferred output by the libfunc, it is added to
    /// [Self::deferred_variables]. Similarly for [Self::temporary_variables].
    fn register_output(
        &mut self,
        res: sierra::ids::VarId,
        output_info: &OutputVarInfo,
        args: &[sierra::ids::VarId],
        deferred_args: &OrderedHashMap<sierra::ids::VarId, DeferredVariableInfo>,
    ) {
        let mut is_deferred: Option<DeferredVariableKind> = None;
        let mut is_temp_var: bool = false;
        let mut add_to_known_stack: Option<isize> = None;

        match &output_info.ref_info {
            OutputVarReferenceInfo::Deferred(kind) => {
                is_deferred = Some(match kind {
                    DeferredOutputKind::Const => DeferredVariableKind::Const,
                    DeferredOutputKind::AddConst { .. } => DeferredVariableKind::AddConst,
                    DeferredOutputKind::Generic => DeferredVariableKind::Generic,
                });
            }
            OutputVarReferenceInfo::NewTempVar { idx } => {
                add_to_known_stack = idx.map(|idx| idx.try_into().unwrap());
                is_temp_var = true;
            }
            OutputVarReferenceInfo::SameAsParam { param_idx }
            | OutputVarReferenceInfo::PartialParam { param_idx } => {
                let arg = &args[*param_idx];
                if let Some(deferred_info) = deferred_args.get(arg) {
                    is_deferred = Some(deferred_info.kind);
                }
                is_temp_var = self.temporary_variables.get(arg).is_some();
                if matches!(output_info.ref_info, OutputVarReferenceInfo::SameAsParam { .. }) {
                    add_to_known_stack = self.known_stack.get(arg);
                }
            }
            OutputVarReferenceInfo::NewLocalVar => {}
        }

        self.deferred_variables.swap_remove(&res);
        self.temporary_variables.swap_remove(&res);
        self.known_stack.remove_variable(&res);

        if let Some(deferred_variable_info_kind) = is_deferred {
            self.deferred_variables.insert(
                res.clone(),
                DeferredVariableInfo {
                    ty: output_info.ty.clone(),
                    kind: deferred_variable_info_kind,
                },
            );
        }

        if is_temp_var {
            self.temporary_variables.insert(res.clone(), output_info.ty.clone());
        }

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
    /// Updates [Self::known_stack] and [Self::temporary_variables] if necessary.
    pub fn rename_var(&mut self, src: &sierra::ids::VarId, dst: &sierra::ids::VarId) {
        self.known_stack.clone_if_on_stack(src, dst);
        if let Some(uninitialized_local_var_id) = self.temporary_variables.get(src) {
            self.temporary_variables.insert(dst.clone(), uninitialized_local_var_id.clone());
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
            let mut deferred_variables = OrderedHashMap::default();
            for (var, info_a) in a.deferred_variables {
                if let Some(info_b) = b.deferred_variables.get(&var) {
                    assert_eq!(
                        info_a, *info_b,
                        "Internal compiler error: Found different deferred variables."
                    );
                    deferred_variables.insert(var, info_a);
                }
            }

            // Merge the lists of temporary variables.
            let mut temporary_variables = OrderedHashMap::default();
            for (var, ty_a) in a.temporary_variables {
                if let Some(ty_b) = b.temporary_variables.get(&var) {
                    assert_eq!(
                        ty_a, *ty_b,
                        "Internal compiler error: Found different types for the same variable: \
                         {var}."
                    );
                    temporary_variables.insert(var, ty_a);
                }
            }

            Some(State {
                deferred_variables,
                temporary_variables,
                known_stack: a.known_stack.merge_with(&b.known_stack),
            })
        }
    }
}
