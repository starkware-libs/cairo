//! Handles the automatic addition of store_temp() and store_local() statements.

mod known_stack;
mod state;

#[cfg(test)]
mod test;

use cairo_lang_sierra as sierra;
use cairo_lang_sierra::extensions::lib_func::{LibfuncSignature, ParamSignature, SierraApChange};
use cairo_lang_sierra::ids::ConcreteLibfuncId;
use cairo_lang_sierra::program::{GenBranchInfo, GenBranchTarget, GenStatement};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{LookupIntern, extract_matches};
use itertools::zip_eq;
use sierra::extensions::NamedLibfunc;
use sierra::extensions::function_call::{CouponCallLibfunc, FunctionCallLibfunc};
use state::{
    DeferredVariableInfo, DeferredVariableKind, VarState, VariablesState, merge_optional_states,
};

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::store_variables::known_stack::KnownStack;
use crate::utils::{
    dup_libfunc_id, rename_libfunc_id, simple_statement, store_local_libfunc_id,
    store_temp_libfunc_id,
};

/// A map from variables that should be stored as local to their allocated
/// space.
pub type LocalVariables = OrderedHashMap<sierra::ids::VarId, sierra::ids::VarId>;

/// Information about a libfunc, required by the `store_variables` module.
pub struct LibfuncInfo {
    pub signature: LibfuncSignature,
}

/// Automatically adds store_temp() statements to the given list of [pre_sierra::Statement].
/// For example, a deferred reference (e.g., `[ap] + [fp - 3]`) needs to be stored as a temporary
/// or local variable before being included in additional computation.
/// The function will add the necessary `store_temp()` instruction before the first use of the
/// deferred reference.
///
/// `local_variables` is a map from variables that should be stored as local to their allocated
/// space.
pub fn add_store_statements<GetLibfuncSignature>(
    db: &dyn SierraGenGroup,
    statements: Vec<pre_sierra::StatementWithLocation>,
    get_lib_func_signature: &GetLibfuncSignature,
    local_variables: LocalVariables,
    params: &[sierra::program::Param],
) -> Vec<pre_sierra::StatementWithLocation>
where
    GetLibfuncSignature: Fn(ConcreteLibfuncId) -> LibfuncInfo,
{
    let mut handler = AddStoreVariableStatements::new(db, local_variables);
    let mut state_opt = Some(VariablesState {
        variables: OrderedHashMap::from_iter(params.iter().map(|param| {
            (
                param.id.clone(),
                if db.get_type_info(param.ty.clone()).unwrap().zero_sized {
                    VarState::ZeroSizedVar
                } else {
                    VarState::LocalVar
                },
            )
        })),
        known_stack: Default::default(),
    });
    // Go over the statements, restarting whenever we see a branch or a label.
    for statement in statements.into_iter() {
        let prev_len = handler.result.len();
        let location = statement.location.clone();
        state_opt = handler.handle_statement(state_opt, statement, get_lib_func_signature);
        for statement in &mut handler.result[prev_len..] {
            statement.set_location(location.clone())
        }
    }
    handler.finalize(state_opt)
}

struct AddStoreVariableStatements<'a> {
    db: &'a dyn SierraGenGroup,
    local_variables: LocalVariables,
    /// A list of output statements (the original statement, together with the added statements,
    /// such as "store_temp").
    result: Vec<pre_sierra::StatementWithLocation>,
    /// A map from [LabelId](pre_sierra::LabelId) to the known state (so far).
    ///
    /// For every branch that does not continue to the next statement, the current known variables
    /// state is added to the map. When the label is visited, it is merged with the known variables
    /// state, and removed from the map.
    future_states: OrderedHashMap<pre_sierra::LabelId, VariablesState>,
}
impl<'a> AddStoreVariableStatements<'a> {
    /// Constructs a new [AddStoreVariableStatements] object.
    fn new(db: &'a dyn SierraGenGroup, local_variables: LocalVariables) -> Self {
        AddStoreVariableStatements {
            db,
            local_variables,
            result: Vec::new(),
            future_states: OrderedHashMap::default(),
        }
    }

    /// Handles a single statement, including adding required store statements and the statement
    /// itself.
    fn handle_statement<GetLibfuncInfo>(
        &mut self,
        state_opt: Option<VariablesState>,
        statement: pre_sierra::StatementWithLocation,
        get_lib_func_signature: &GetLibfuncInfo,
    ) -> Option<VariablesState>
    where
        GetLibfuncInfo: Fn(ConcreteLibfuncId) -> LibfuncInfo,
    {
        let mut state_opt = state_opt;
        match &statement.statement {
            pre_sierra::Statement::Sierra(GenStatement::Invocation(invocation)) => {
                let libfunc_id = invocation.libfunc_id.clone();
                let libfunc_info = get_lib_func_signature(libfunc_id.clone());
                let signature = libfunc_info.signature;
                let state = &mut state_opt.unwrap_or_default();

                let libfunc_long_id = libfunc_id.lookup_intern(self.db);
                let arg_states = match libfunc_long_id.generic_id.0.as_str() {
                    FunctionCallLibfunc::STR_ID | CouponCallLibfunc::STR_ID => {
                        // The arguments were already stored using `push_values`.
                        // Avoid calling `prepare_libfunc_arguments` as it might copy the
                        // arguments to a local variables.
                        invocation.args.iter().map(|var| state.pop_var_state(var)).collect()
                    }
                    _ => self.prepare_libfunc_arguments(
                        state,
                        &invocation.args,
                        &signature.param_signatures,
                    ),
                };
                match &invocation.branches[..] {
                    [GenBranchInfo { target: GenBranchTarget::Fallthrough, results }] => {
                        // A simple invocation.
                        let branch_signature = &signature.branch_signatures[0];
                        match branch_signature.ap_change {
                            SierraApChange::Unknown => {
                                // If the ap-change is unknown, variables that will be revoked
                                // otherwise should be stored as locals.
                                self.store_variables_as_locals(state);
                            }
                            SierraApChange::BranchAlign | SierraApChange::Known { .. } => {}
                        }

                        state.register_outputs(
                            results,
                            branch_signature,
                            &invocation.args,
                            &arg_states,
                        );
                        state_opt = Some(std::mem::take(state));
                    }
                    _ => {
                        // This starts a branch. Store all deferred variables.
                        if invocation.branches.len() > 1 {
                            self.store_all_possibly_lost_variables(state);
                        }

                        // Go over the branches. The state of a branch that points to `Fallthrough`
                        // is merged into `fallthrough_state`.
                        let mut fallthrough_state: Option<VariablesState> = None;
                        for (branch, branch_signature) in
                            zip_eq(&invocation.branches, signature.branch_signatures)
                        {
                            let mut state_at_branch = state.clone();
                            state_at_branch.register_outputs(
                                &branch.results,
                                &branch_signature,
                                &invocation.args,
                                &arg_states,
                            );

                            self.add_future_state(
                                &branch.target,
                                state_at_branch,
                                &mut fallthrough_state,
                            );
                        }
                        state_opt = fallthrough_state;
                    }
                }
                self.result.push(statement);
                state_opt
            }
            pre_sierra::Statement::Sierra(GenStatement::Return(_return_statement)) => {
                self.result.push(statement);
                // `return` statements are preceded by `PushValues` which takes care of pushing
                // the return values onto the stack. The rest of the variables are not
                // needed.

                // The next statement is not reachable from this one. Set `state` to `None`.
                None
            }
            pre_sierra::Statement::Label(pre_sierra::Label { id: label_id }) => {
                // Merge self.known_stack with the future_stack that corresponds to the label, if
                // any.
                state_opt = merge_optional_states(
                    std::mem::take(&mut state_opt),
                    self.future_states.swap_remove(label_id),
                );

                self.result.push(statement);
                state_opt
            }
            pre_sierra::Statement::PushValues(push_values) => {
                let state = &mut state_opt.unwrap_or_default();
                self.push_values(state, push_values);
                Some(std::mem::take(state))
            }
        }
    }

    /// Prepares the given `args` to be used as arguments for a libfunc.
    ///
    /// Returns a map from arguments' [sierra::ids::VarId] to [DeferredVariableInfo] for arguments
    /// that have a deferred value after the function (that is, they were not stored as
    /// temp/local by the function).
    fn prepare_libfunc_arguments(
        &mut self,
        state: &mut VariablesState,
        args: &[sierra::ids::VarId],
        param_signatures: &[ParamSignature],
    ) -> Vec<VarState> {
        zip_eq(args, param_signatures)
            .map(|(arg, param_signature)| {
                self.prepare_libfunc_argument(
                    state,
                    arg,
                    param_signature.allow_deferred,
                    param_signature.allow_add_const,
                    param_signature.allow_const,
                )
            })
            .collect()
    }

    /// Prepares the given `arg` to be used as an argument for a libfunc.
    ///
    /// Returns the VarState of the argument.
    fn prepare_libfunc_argument(
        &mut self,
        state: &mut VariablesState,
        arg: &sierra::ids::VarId,
        allow_deferred: bool,
        allow_add_const: bool,
        allow_const: bool,
    ) -> VarState {
        match state.pop_var_state(arg) {
            VarState::Removed => {
                unreachable!("`{arg}` was previously moved.");
            }
            VarState::Deferred { info: deferred_info } => {
                // If a deferred argument was marked as a local variable, then store
                // it. This is important in case an alias of the variable is used later
                // (for example, due to `SameAsParam` output).
                if self.store_var_as_local(arg, &deferred_info.ty) {
                    VarState::LocalVar
                } else {
                    match deferred_info.kind {
                        state::DeferredVariableKind::Const => {
                            if !allow_const {
                                return self.store_deferred(
                                    &mut state.known_stack,
                                    arg,
                                    &deferred_info.ty,
                                );
                            }
                        }
                        state::DeferredVariableKind::AddConst => {
                            if !allow_add_const {
                                return self.store_deferred(
                                    &mut state.known_stack,
                                    arg,
                                    &deferred_info.ty,
                                );
                            }
                        }
                        state::DeferredVariableKind::Generic => {
                            if !allow_deferred {
                                return self.store_deferred(
                                    &mut state.known_stack,
                                    arg,
                                    &deferred_info.ty,
                                );
                            }
                        }
                    };
                    VarState::Deferred { info: deferred_info }
                }
            }
            VarState::TempVar { ty } => {
                if self.store_var_as_local(arg, &ty) {
                    return VarState::LocalVar;
                }
                VarState::TempVar { ty }
            }
            VarState::LocalVar => VarState::LocalVar,
            VarState::ZeroSizedVar => VarState::ZeroSizedVar,
        }
    }

    /// Adds a store_temp() or store_local() instruction for the given deferred variable.
    /// The variable should be removed from the `deferred_variables` map prior to this call.
    ///
    /// Returns the variable state after the store.
    fn store_deferred(
        &mut self,
        known_stack: &mut KnownStack,
        var: &sierra::ids::VarId,
        ty: &sierra::ids::ConcreteTypeId,
    ) -> VarState {
        self.store_deferred_ex(known_stack, var, var, ty)
    }

    /// Same as `store_deferred` only allows the `store_temp` case to use a different variable.
    fn store_deferred_ex(
        &mut self,
        known_stack: &mut KnownStack,
        var: &sierra::ids::VarId,
        var_on_stack: &sierra::ids::VarId,
        ty: &sierra::ids::ConcreteTypeId,
    ) -> VarState {
        // Check if this variable should be a local variable.
        if let Some(uninitialized_local_var_id) = self.local_variables.get(var) {
            self.store_local(var, &uninitialized_local_var_id.clone(), ty);
            VarState::LocalVar
        } else {
            self.store_temp(known_stack, var, var_on_stack, ty);
            VarState::TempVar { ty: ty.clone() }
        }
    }

    fn push_values(&mut self, state: &mut VariablesState, push_values: &[pre_sierra::PushValue]) {
        if push_values.is_empty() {
            return;
        }

        // Optimization: check if there is a prefix of `push_values` that is already on the stack.
        let prefix_size = state.known_stack.compute_on_stack_prefix_size(push_values);
        for (i, pre_sierra::PushValue { var, var_on_stack, ty, dup }) in
            push_values.iter().enumerate()
        {
            let (is_on_stack, var_state) = match state.pop_var_state(var) {
                VarState::Deferred { info: deferred_info } => {
                    if let DeferredVariableKind::Const = deferred_info.kind {
                        // TODO(orizi): This is an ugly fix for case of literals. Fix properly.
                        if *dup {
                            self.dup(var, var_on_stack, ty);
                            state.variables.insert(
                                var.clone(),
                                VarState::Deferred { info: deferred_info.clone() },
                            );
                            self.store_temp(&mut state.known_stack, var_on_stack, var_on_stack, ty);
                        } else {
                            self.store_temp(&mut state.known_stack, var, var_on_stack, ty);
                        }
                        state
                            .variables
                            .insert(var_on_stack.clone(), VarState::TempVar { ty: ty.clone() });
                        continue;
                    } else {
                        let var_state = self.store_deferred_ex(
                            &mut state.known_stack,
                            var,
                            var_on_stack,
                            &deferred_info.ty,
                        );
                        if matches!(var_state, VarState::TempVar { .. }) {
                            state.variables.insert(var_on_stack.clone(), var_state.clone());
                            if *dup {
                                // In the dup case we dup `var_on_stack` that is ready for push into
                                // `var` that should still be available as a temporary var.
                                state.variables.insert(var.clone(), var_state);
                                self.dup(var_on_stack, var, ty);
                            }
                            continue;
                        } else {
                            state.variables.insert(var.clone(), var_state.clone());
                            (false, var_state)
                        }
                    }
                }
                VarState::ZeroSizedVar => (true, VarState::ZeroSizedVar),
                var_state => {
                    // Check if this is part of the prefix. If it is, rename instead of adding
                    // `store_temp`.
                    (i < prefix_size, var_state)
                }
            };

            if is_on_stack {
                if *dup {
                    state.variables.insert(var.clone(), var_state.clone());
                    state.variables.insert(var_on_stack.clone(), var_state.clone());
                    self.dup(var, var_on_stack, ty);
                } else {
                    self.rename_var(var, var_on_stack, ty);
                    state.known_stack.clone_if_on_stack(var, var_on_stack);
                    state.variables.insert(var_on_stack.clone(), var_state);
                }
            } else {
                let src = if *dup {
                    state.variables.insert(var.clone(), var_state.clone());
                    self.dup(var, var_on_stack, ty);
                    var_on_stack
                } else {
                    var
                };
                self.store_temp(&mut state.known_stack, src, var_on_stack, ty);
                state.variables.insert(var_on_stack.clone(), VarState::TempVar { ty: ty.clone() });
            }
        }
    }

    /// Stores all the variables that may possibly get misaligned due to branches and removes them
    /// The variables will be added according to the order of creation.
    fn store_all_possibly_lost_variables(&mut self, state: &mut VariablesState) {
        for (var, var_state) in state.variables.iter_mut() {
            match var_state {
                VarState::TempVar { ty } => {
                    if self.store_var_as_local(var, ty) {
                        *var_state = VarState::LocalVar;
                    }
                }
                VarState::Deferred { info } => {
                    if info.kind != DeferredVariableKind::Const {
                        *var_state = self.store_deferred(&mut state.known_stack, var, &info.ty);
                    }
                }
                VarState::ZeroSizedVar | VarState::LocalVar | VarState::Removed => {}
            }
        }
    }

    /// Copies the given variable into a local variable if it is marked as local.
    /// Returns true if the variable was marked as local.
    fn store_var_as_local(
        &mut self,
        var: &sierra::ids::VarId,
        ty: &sierra::ids::ConcreteTypeId,
    ) -> bool {
        if let Some(uninitialized_local_var_id) = self.local_variables.get(var) {
            self.store_local(var, &uninitialized_local_var_id.clone(), ty);
            return true;
        }
        false
    }

    /// Stores all the deferred and temporary variables as local variables.
    fn store_variables_as_locals(&mut self, state: &mut VariablesState) {
        for (var, var_state) in state.variables.iter_mut() {
            if let Some(uninitialized_local_var_id) = self.local_variables.get(var) {
                match var_state {
                    VarState::Deferred { info: DeferredVariableInfo { ty, .. } }
                    | VarState::TempVar { ty } => {
                        self.store_local(var, &uninitialized_local_var_id.clone(), ty);
                        *var_state = VarState::LocalVar;
                    }
                    VarState::ZeroSizedVar | VarState::LocalVar | VarState::Removed => {}
                };
            }
        }
    }

    fn finalize(self, state_opt: Option<VariablesState>) -> Vec<pre_sierra::StatementWithLocation> {
        assert!(
            state_opt.is_none(),
            "Internal compiler error: Found a reachable statement at the end of the function."
        );
        assert!(
            self.future_states.is_empty(),
            "Internal compiler error: Unhandled label in 'store_variables'."
        );
        self.result
    }

    /// Adds a `store_temp` command storing `var` into `var_on_stack`.
    fn store_temp(
        &mut self,
        known_stack: &mut KnownStack,
        var: &sierra::ids::VarId,
        var_on_stack: &sierra::ids::VarId,
        ty: &sierra::ids::ConcreteTypeId,
    ) {
        self.result.push(simple_statement(
            store_temp_libfunc_id(self.db, ty.clone()),
            &[var.clone()],
            &[var_on_stack.clone()],
        ));
        known_stack.push(var_on_stack);
    }

    /// Adds a `store_local` command storing `var` into itself using the preallocated
    /// `uninitialized_local_var_id`.
    fn store_local(
        &mut self,
        var: &sierra::ids::VarId,
        uninitialized_local_var_id: &sierra::ids::VarId,
        ty: &sierra::ids::ConcreteTypeId,
    ) {
        self.result.push(simple_statement(
            store_local_libfunc_id(self.db, ty.clone()),
            &[uninitialized_local_var_id.clone(), var.clone()],
            &[var.clone()],
        ));
    }

    /// Adds a call to the dup() libfunc, duplicating `var` into `dup_var`.
    fn dup(
        &mut self,
        var: &sierra::ids::VarId,
        dup_var: &sierra::ids::VarId,
        ty: &sierra::ids::ConcreteTypeId,
    ) {
        self.result.push(simple_statement(
            dup_libfunc_id(self.db, ty.clone()),
            &[var.clone()],
            &[var.clone(), dup_var.clone()],
        ));
    }

    /// Adds a call to the rename() libfunc, renaming `src` to `dst`.
    /// if `src` == `dst`, nothing is added.
    fn rename_var(
        &mut self,
        src: &sierra::ids::VarId,
        dst: &sierra::ids::VarId,
        ty: &sierra::ids::ConcreteTypeId,
    ) {
        if src != dst {
            self.result.push(simple_statement(
                rename_libfunc_id(self.db, ty.clone()),
                &[src.clone()],
                &[dst.clone()],
            ));
        }
    }

    /// Merges the given `state` into the future state that corresponds to `target`.
    /// If `target` refers to `Fallthrough`, `state` is merged into the input-output argument
    /// `fallthrough_state`.
    /// If it refers to a label, `state` is merged into `future_states`.
    fn add_future_state(
        &mut self,
        target: &GenBranchTarget<pre_sierra::LabelId>,
        state: VariablesState,
        fallthrough_state: &mut Option<VariablesState>,
    ) {
        match target {
            GenBranchTarget::Fallthrough => {
                let new_state =
                    merge_optional_states(std::mem::take(fallthrough_state), Some(state));
                *fallthrough_state = new_state;
            }
            GenBranchTarget::Statement(label_id) => {
                let new_state =
                    merge_optional_states(self.future_states.swap_remove(label_id), Some(state));
                self.future_states.insert(*label_id, extract_matches!(new_state, Some));
            }
        }
    }
}
