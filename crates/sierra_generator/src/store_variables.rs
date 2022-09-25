//! Handles the automatic addition of store_temp() and store_local() statements.

#[cfg(test)]
#[path = "store_variables_test.rs"]
mod test;

use sierra::extensions::lib_func::{OutputBranchInfo, OutputVarInfo};
use sierra::extensions::OutputVarReferenceInfo;
use sierra::ids::ConcreteLibFuncId;
use sierra::program::{GenBranchInfo, GenBranchTarget, GenStatement};
use utils::ordered_hash_map::OrderedHashMap;

use crate::db::SierraGenGroup;
use crate::pre_sierra;
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
    result: Vec<pre_sierra::Statement>,
    /// A map from [sierra::ids::VarId] of a deferred reference
    /// (for example, `[ap - 1] + [ap - 2]`) to its type.
    deferred_variables: OrderedHashMap<sierra::ids::VarId, sierra::ids::ConcreteTypeId>,
    /// A map from [sierra::ids::VarId] of variables that are located on the stack (`[ap - 2]`)
    /// to their index on the stack, relative to `known_stack_size`. A variable with index `i`
    /// is at `[ap - known_stack_size + i]`.
    // TODO(lior): Consider unifying this with `deferred_variables`.
    variables_on_stack: OrderedHashMap<sierra::ids::VarId, usize>,
    known_stack_size: usize,
}
impl<'a> AddStoreVariableStatements<'a> {
    /// Constructs a new [AddStoreVariableStatements] object.
    fn new(db: &'a dyn SierraGenGroup) -> Self {
        AddStoreVariableStatements {
            db,
            result: Vec::new(),
            deferred_variables: OrderedHashMap::default(),
            variables_on_stack: OrderedHashMap::default(),
            known_stack_size: 0,
        }
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
                self.clear_known_stack();
                match &invocation.branches[..] {
                    [GenBranchInfo { target: GenBranchTarget::Fallthrough, results }] => {
                        // A simple invocation.
                        // TODO(lior): consider not calling prepare_libfunc_arguments for functions
                        //   that accept deferred, such as store_temp().
                        self.prepare_libfunc_arguments(&invocation.args);
                        self.register_outputs(results, &output_infos[0].vars);
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
                self.clear_known_stack();
            }
            pre_sierra::Statement::Label(_) => {
                self.store_all_deffered_variables();
                self.result.push(statement);
                self.clear_known_stack();
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
        if !self.deferred_variables.contains_key(arg) {
            return;
        }

        self.store_temp_deferred(arg);
    }

    /// Adds a store_temp() instruction for the given deferred variable and removes it from the
    /// `deferred_variables` map.
    fn store_temp_deferred(&mut self, var: &sierra::ids::VarId) {
        let ty = self.deferred_variables[var.clone()].clone();
        self.store_temp(var, var, &ty);
        self.deferred_variables.swap_remove(var);
    }

    /// Registers output variables of a libfunc. See `add_output`.
    fn register_outputs(&mut self, results: &[sierra::ids::VarId], output_infos: &[OutputVarInfo]) {
        for (var, output_info) in itertools::zip_eq(results, output_infos) {
            self.register_output(var.clone(), output_info);
        }
        // Update `known_stack_size`. It is one more than the maximum of the indices in
        // `variables_on_stack` (or 0 if empty).
        self.known_stack_size =
            self.variables_on_stack.values().max().map(|idx| idx + 1).unwrap_or(0);
    }

    /// Register an output variable of a libfunc.
    ///
    /// If the variable is marked as Deferred output by the libfunc, it is added to
    /// `self.deferred_variables`.
    fn register_output(&mut self, res: sierra::ids::VarId, output_info: &OutputVarInfo) {
        self.deferred_variables.swap_remove(&res);
        self.variables_on_stack.swap_remove(&res);
        match output_info.ref_info {
            OutputVarReferenceInfo::Deferred => {
                self.deferred_variables.insert(res, output_info.ty.clone());
            }
            OutputVarReferenceInfo::NewTempVar { idx } => {
                self.variables_on_stack.insert(res, self.known_stack_size + idx);
            }
            OutputVarReferenceInfo::SameAsParam { .. }
            | OutputVarReferenceInfo::NewLocalVar
            | OutputVarReferenceInfo::Const => {}
        }
    }

    fn push_values(&mut self, push_values: &Vec<pre_sierra::PushValue>) {
        for pre_sierra::PushValue { var, var_on_stack, ty } in push_values {
            if self.deferred_variables.contains_key(var) {
                // Convert the deferred variable into a temporary variable, by calling
                // `prepare_libfunc_argument`.
                self.prepare_libfunc_argument(var);
                // Note: the original variable may still be used after the following `rename`
                // statement. In such a case, it will be dupped before the `rename`
                // by `add_dups_and_drops()`.
                self.rename_var(var, var_on_stack, ty);
            } else {
                // TODO(lior): If the variable is already in the correct place, only rename
                //   it, instead of adding a `store_temp()` statement.
                self.store_temp(var, var_on_stack, ty);
            }
        }
    }

    /// Stores all the deffered variables and clears `deferred_variables`.
    /// The variables will be added according to the order of creation.
    fn store_all_deffered_variables(&mut self) {
        for (var, ty) in self.deferred_variables.clone() {
            self.store_temp(&var, &var, &ty);
        }
        self.clear_deffered_variables();
    }

    /// Clears all the deferred variables.
    fn clear_deffered_variables(&mut self) {
        self.deferred_variables.clear();
    }

    /// Clears the known information about the stack.
    ///
    /// This is called where the change in the value of `ap` is not known at compile time.
    fn clear_known_stack(&mut self) {
        self.known_stack_size = 0;
        self.variables_on_stack.clear();
    }

    fn finalize(self) -> Vec<pre_sierra::Statement> {
        assert!(
            self.deferred_variables.is_empty(),
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

        self.variables_on_stack.insert(var_on_stack.clone(), self.known_stack_size);
        self.known_stack_size += 1;
    }

    fn rename_var(
        &mut self,
        var: &sierra::ids::VarId,
        var_on_stack: &sierra::ids::VarId,
        ty: &sierra::ids::ConcreteTypeId,
    ) {
        self.result.push(simple_statement(
            rename_libfunc_id(self.db, ty.clone()),
            &[var.clone()],
            &[var_on_stack.clone()],
        ));

        if let Some(index_on_stack) = self.variables_on_stack.get(var).cloned() {
            self.variables_on_stack.insert(var_on_stack.clone(), index_on_stack);
        }
    }
}
