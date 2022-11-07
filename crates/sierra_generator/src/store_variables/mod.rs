//! Handles the automatic addition of store_temp() and store_local() statements.

mod known_stack;
mod state;

#[cfg(test)]
mod test;

use itertools::zip_eq;
use sierra::extensions::lib_func::{LibFuncSignature, ParamSignature};
use sierra::ids::ConcreteLibFuncId;
use sierra::program::{GenBranchInfo, GenBranchTarget, GenStatement};
use state::{merge_optional_states, State};
use utils::extract_matches;
use utils::ordered_hash_map::OrderedHashMap;
use utils::ordered_hash_set::OrderedHashSet;

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::store_variables::known_stack::KnownStack;
use crate::utils::{rename_libfunc_id, simple_statement, store_temp_libfunc_id, store_local_libfunc_id};

/// Automatically adds store_temp() statements to the given list of [pre_sierra::Statement].
/// For example, a deferred reference (e.g., `[ap] + [fp - 3]`) needs to be stored as a temporary
/// or local variable before being included in additional computation.
/// The function will add the necessary `store_temp()` instruction before the first use of the
/// deferred reference.
pub fn add_store_statements<GetLibFuncSignature>(
    db: &dyn SierraGenGroup,
    statements: Vec<pre_sierra::Statement>,
    get_lib_func_signature: &GetLibFuncSignature,
    local_variables: OrderedHashSet<sierra::ids::VarId>,
) -> Vec<pre_sierra::Statement>
where
    GetLibFuncSignature: Fn(ConcreteLibFuncId) -> LibFuncSignature,
{
    let mut handler = AddStoreVariableStatements::new(db, local_variables);
    // Go over the statements, restarting whenever we see a branch or a label.
    for statement in statements.into_iter() {
        handler.handle_statement(statement, get_lib_func_signature);
    }
    handler.finalize()
}

struct AddStoreVariableStatements<'a> {
    db: &'a dyn SierraGenGroup,
    /// The set of variables that should be stored as local variables since they will otherwise
    /// be revoked.
    local_variables: OrderedHashSet<sierra::ids::VarId>,
    /// A list of output statements (the original statement, together with the added statements,
    /// such as "store_temp").
    result: Vec<pre_sierra::Statement>,
    /// The current information known about the state of the variables. None means the statement is
    /// not reachable from the previous statement.
    state_opt: Option<State>,
    /// A map from [LabelId](pre_sierra::LabelId) to the known state (so far).
    ///
    /// For every branch that does not continue to the next statement, the current known state is
    /// added to the map. When the label is visited, it is merged with the known state, and removed
    /// from the map.
    future_states: OrderedHashMap<pre_sierra::LabelId, State>,
}
impl<'a> AddStoreVariableStatements<'a> {
    /// Constructs a new [AddStoreVariableStatements] object.
    fn new(
        db: &'a dyn SierraGenGroup,
        local_variables: OrderedHashSet<sierra::ids::VarId>,
    ) -> Self {
        AddStoreVariableStatements {
            db,
            local_variables,
            result: Vec::new(),
            state_opt: Some(State::default()),
            future_states: OrderedHashMap::default(),
        }
    }

    /// Handles a single statement, including adding required store statements and the statement
    /// itself.
    fn handle_statement<GetLibFuncSignature>(
        &mut self,
        statement: pre_sierra::Statement,
        get_lib_func_signature: &GetLibFuncSignature,
    ) where
        GetLibFuncSignature: Fn(ConcreteLibFuncId) -> LibFuncSignature,
    {
        match &statement {
            pre_sierra::Statement::Sierra(GenStatement::Invocation(invocation)) => {
                let signature = get_lib_func_signature(invocation.libfunc_id.clone());
                match &invocation.branches[..] {
                    [GenBranchInfo { target: GenBranchTarget::Fallthrough, results }] => {
                        // A simple invocation.
                        self.prepare_libfunc_arguments(
                            &invocation.args,
                            &signature.param_signatures,
                        );
                        self.state().register_outputs(results, &signature.branch_signatures[0]);
                    }
                    _ => {
                        // This starts a branch. Store all deferred variables.
                        self.store_all_deffered_variables();

                        // Go over the branches. The state of a branch that points to `Fallthrough`
                        // is merged into `fallthrough_state`.
                        let mut fallthrough_state: Option<State> = None;
                        for (branch, branch_signature) in
                            zip_eq(&invocation.branches, signature.branch_signatures)
                        {
                            let mut state_at_branch = self.state().clone();
                            state_at_branch.register_outputs(&branch.results, &branch_signature);

                            self.add_future_state(
                                &branch.target,
                                state_at_branch,
                                &mut fallthrough_state,
                            );
                        }
                        self.state_opt = fallthrough_state;
                    }
                }
                self.result.push(statement);
            }
            pre_sierra::Statement::Sierra(GenStatement::Return(_return_statement)) => {
                self.result.push(statement);
                // `return` statements are preceded by `PushValues` which takes care of pushing
                // the return values onto the stack. The rest of the deferred variables are not
                // needed.
                self.clear_deffered_variables();
                // The next statement is not reachable from this one. Set `state` to `None`.
                self.state_opt = None;
            }
            pre_sierra::Statement::Label(pre_sierra::Label { id: label_id }) => {
                if self.state_opt.is_some() {
                    self.store_all_deffered_variables();
                }

                // Merge self.known_stack with the future_stack that corresponds to the label, if
                // any.
                self.state_opt = merge_optional_states(
                    std::mem::take(&mut self.state_opt),
                    self.future_states.swap_remove(label_id),
                );

                self.result.push(statement);
            }
            pre_sierra::Statement::PushValues(push_values) => {
                self.push_values(push_values);
            }
        }
    }

    /// Prepares the given `args` to be used as arguments for a libfunc.
    fn prepare_libfunc_arguments(
        &mut self,
        args: &[sierra::ids::VarId],
        param_signatures: &[ParamSignature],
    ) {
        for (arg, param_signature) in zip_eq(args, param_signatures) {
            self.prepare_libfunc_argument(
                arg,
                param_signature.allow_deferred,
                param_signature.allow_add_const,
            );
        }
    }

    /// Prepares the given `arg` to be used as an argument for a libfunc.
    fn prepare_libfunc_argument(
        &mut self,
        arg: &sierra::ids::VarId,
        allow_deferred: bool,
        allow_add_const: bool,
    ) {
        if let Some(deferred_info) = self.state().deferred_variables.get(arg) {
            match deferred_info.kind {
                state::DeferredVariableKind::Generic => {
                    if !allow_deferred {
                        self.store_deferred(arg)
                    }
                }
                state::DeferredVariableKind::AddConst => {
                    if !allow_add_const {
                        self.store_deferred(arg)
                    }
                }
            };
        }
    }

    /// Adds a store_temp() or store_local() instruction for the given deferred variable and removes
    /// it from the `deferred_variables` map.
    fn store_deferred(&mut self, var: &sierra::ids::VarId) {
        let deferred_info = self.state().deferred_variables[var.clone()].clone();
        if self.local_variables.contains(var) {
            self.store_local(var, var, &deferred_info.ty);
        } else {
            self.store_temp(var, var, &deferred_info.ty);
        }
        self.state().deferred_variables.swap_remove(var);
    }

    fn push_values(&mut self, push_values: &Vec<pre_sierra::PushValue>) {
        if push_values.is_empty() {
            return;
        }

        // Optimization: check if there is a prefix of `push_values` that is already on the stack.
        let prefix_size = self.known_stack().compute_on_stack_prefix_size(push_values);

        for (i, pre_sierra::PushValue { var, var_on_stack, ty }) in push_values.iter().enumerate() {
            if self.state().deferred_variables.contains_key(var) {
                // Convert the deferred variable into a temporary variable, by calling
                // `prepare_libfunc_argument`.
                self.prepare_libfunc_argument(var, false, false);
                // Note: the original variable may still be used after the following `rename`
                // statement. In such a case, it will be dupped before the `rename`
                // by `add_dups_and_drops()`.
                self.rename_var(var, var_on_stack, ty);
            } else {
                // Check if this is part of the prefix. If it is, rename instead of adding
                // `store_temp`.
                if i < prefix_size {
                    self.rename_var(var, var_on_stack, ty);
                } else {
                    self.store_temp(var, var_on_stack, ty);
                }
            }
        }
    }

    /// Stores all the deffered variables and clears `deferred_variables`.
    /// The variables will be added according to the order of creation.
    fn store_all_deffered_variables(&mut self) {
        for (var, deferred_info) in self.state().deferred_variables.clone() {
            self.store_temp(&var, &var, &deferred_info.ty);
        }
        self.clear_deffered_variables();
    }

    /// Clears all the deferred variables.
    fn clear_deffered_variables(&mut self) {
        self.state().deferred_variables.clear();
    }

    fn finalize(self) -> Vec<pre_sierra::Statement> {
        assert!(
            self.state_opt.is_none(),
            "Internal compiler error: Found a reachable statement at the end of the function."
        );
        assert!(
            self.future_states.is_empty(),
            "Internal compiler error: Unhandled label in 'store_variables'."
        );
        self.result
    }

    fn store_temp(
        &mut self,
        var: &sierra::ids::VarId,
        var_on_stack: &sierra::ids::VarId,
        ty: &sierra::ids::ConcreteTypeId,
    ) {
        self.result.push(simple_statement(
            store_temp_libfunc_id(self.db, ty.clone()),
            &[var.clone()],
            &[var_on_stack.clone()],
        ));

        self.known_stack().push(var_on_stack);
    }

    fn store_local(
        &mut self,
        var: &sierra::ids::VarId,
        var_on_stack: &sierra::ids::VarId,
        ty: &sierra::ids::ConcreteTypeId,
    ) {
        self.result.push(simple_statement(
            store_local_libfunc_id(self.db, ty.clone()),
            &[var.clone()],
            &[var_on_stack.clone()],
        ));
    }

    fn rename_var(
        &mut self,
        src: &sierra::ids::VarId,
        dst: &sierra::ids::VarId,
        ty: &sierra::ids::ConcreteTypeId,
    ) {
        self.result.push(simple_statement(
            rename_libfunc_id(self.db, ty.clone()),
            &[src.clone()],
            &[dst.clone()],
        ));

        self.known_stack().clone_if_on_stack(src, dst);
    }

    /// Returns the current state, assuming the current statement is reachable.
    /// Fails otherwise.
    fn state(&mut self) -> &mut State {
        if matches!(self.state_opt.as_mut(), None) {
            return self.state_opt.as_mut().unwrap();
        }
        self.state_opt.as_mut().unwrap()
    }

    /// Returns the current known stack, assuming the current statement is reachable.
    /// Fails otherwise.
    fn known_stack(&mut self) -> &mut KnownStack {
        &mut self.state().known_stack
    }

    /// Merges the given `state` into the future state that corresponds to `target`.
    /// If `target` refers to `Fallthrough`, `state` is merged into the input-output argument
    /// `fallthrough_state`.
    /// If it refers to a label, `state` is merged into `future_states`.
    fn add_future_state(
        &mut self,
        target: &GenBranchTarget<pre_sierra::LabelId>,
        state: State,
        fallthrough_state: &mut Option<State>,
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
