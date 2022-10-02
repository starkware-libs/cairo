//! Handles the automatic addition of store_temp() and store_local() statements.

mod known_stack;
mod state;

#[cfg(test)]
mod test;

use sierra::extensions::lib_func::OutputBranchInfo;
use sierra::ids::ConcreteLibFuncId;
use sierra::program::{GenBranchInfo, GenBranchTarget, GenStatement};
use state::State;

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::store_variables::known_stack::KnownStack;
use crate::utils::{rename_libfunc_id, simple_statement, store_temp_libfunc_id};

/// Automatically adds store_temp() statements to the given list of [pre_sierra::Statement].
/// For example, a deferred reference (e.g., `[ap] + [fp - 3]`) needs to be stored as a temporary
/// or local variable before being included in additional computation.
/// The function will add the necessary `store_temp()` instruction before the first use of the
/// deferred reference.
pub fn add_store_statements<GetOutputInfo>(
    db: &dyn SierraGenGroup,
    statements: Vec<pre_sierra::Statement>,
    get_output_info: &GetOutputInfo,
) -> Vec<pre_sierra::Statement>
where
    GetOutputInfo: Fn(ConcreteLibFuncId) -> Vec<OutputBranchInfo>,
{
    let mut handler = AddStoreVariableStatements::new(db);
    // Go over the statements, restarting whenever we see a branch or a label.
    for statement in statements.into_iter() {
        handler.handle_statement(statement, get_output_info);
    }
    handler.finalize()
}

struct AddStoreVariableStatements<'a> {
    db: &'a dyn SierraGenGroup,
    /// A list of output statements (the original statement, together with the added statements,
    /// such as "store_temp").
    result: Vec<pre_sierra::Statement>,
    /// The current information known about the state of the variables. None means the statement is
    /// not reachable from the previous statement.
    state_opt: Option<State>,
}
impl<'a> AddStoreVariableStatements<'a> {
    /// Constructs a new [AddStoreVariableStatements] object.
    fn new(db: &'a dyn SierraGenGroup) -> Self {
        AddStoreVariableStatements { db, result: Vec::new(), state_opt: Some(State::default()) }
    }

    /// Handles a single statement, including adding required store statements and the statement
    /// itself.
    fn handle_statement<GetOutputInfo>(
        &mut self,
        statement: pre_sierra::Statement,
        get_output_info: &GetOutputInfo,
    ) where
        GetOutputInfo: Fn(ConcreteLibFuncId) -> Vec<OutputBranchInfo>,
    {
        match &statement {
            pre_sierra::Statement::Sierra(GenStatement::Invocation(invocation)) => {
                let output_infos = get_output_info(invocation.libfunc_id.clone());
                // Currently, treat every statement as if it has unknown `ap` change.
                // TODO(lior): Call `clear_known_stack` only if the libfunc revokes `ap`.
                self.state().clear_known_stack();
                match &invocation.branches[..] {
                    [GenBranchInfo { target: GenBranchTarget::Fallthrough, results }] => {
                        // A simple invocation.
                        // TODO(lior): consider not calling prepare_libfunc_arguments for functions
                        //   that accept deferred, such as store_temp().
                        self.prepare_libfunc_arguments(&invocation.args);
                        self.state().register_outputs(results, &output_infos[0].vars);
                    }
                    _ => {
                        // This starts a branch. Store all deferred variables.
                        self.store_all_deffered_variables();
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
                self.state().clear_known_stack();
            }
            pre_sierra::Statement::Label(_) => {
                self.store_all_deffered_variables();
                self.result.push(statement);
                self.state().clear_known_stack();
            }
            pre_sierra::Statement::PushValues(push_values) => {
                self.push_values(push_values);
            }
        }
    }

    /// Prepares the given `args` to be used as arguments for a libfunc.
    fn prepare_libfunc_arguments(&mut self, args: &[sierra::ids::VarId]) {
        for arg in args {
            self.prepare_libfunc_argument(arg);
        }
    }

    /// Prepares the given `arg` to be used as an argument for a libfunc.
    fn prepare_libfunc_argument(&mut self, arg: &sierra::ids::VarId) {
        if !self.state().deferred_variables.contains_key(arg) {
            return;
        }

        self.store_temp_deferred(arg);
    }

    /// Adds a store_temp() instruction for the given deferred variable and removes it from the
    /// `deferred_variables` map.
    fn store_temp_deferred(&mut self, var: &sierra::ids::VarId) {
        let ty = self.state().deferred_variables[var.clone()].clone();
        self.store_temp(var, var, &ty);
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
                self.prepare_libfunc_argument(var);
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
        for (var, ty) in self.state().deferred_variables.clone() {
            self.store_temp(&var, &var, &ty);
        }
        self.clear_deffered_variables();
    }

    /// Clears all the deferred variables.
    fn clear_deffered_variables(&mut self) {
        self.state().deferred_variables.clear();
    }

    // TODO(lior): Remove "mut" on self once deferred_variables is no longer checked by this
    //   function.
    fn finalize(mut self) -> Vec<pre_sierra::Statement> {
        assert!(
            self.state().deferred_variables.is_empty(),
            "Internal compiler error: Remaining unhandled deferred variables."
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
        self.state_opt.as_mut().unwrap()
    }

    /// Returns the current known stack, assuming the current statement is reachable.
    /// Fails otherwise.
    fn known_stack(&mut self) -> &mut KnownStack {
        &mut self.state().known_stack
    }
}
