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
use crate::utils::{simple_statement, store_temp_libfunc_id};

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
}
impl<'a> AddStoreVariableStatements<'a> {
    /// Constructs a new [AddStoreVariableStatements] object.
    fn new(db: &'a dyn SierraGenGroup) -> Self {
        AddStoreVariableStatements {
            db,
            result: Vec::new(),
            deferred_variables: OrderedHashMap::default(),
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
                match &invocation.branches[..] {
                    [GenBranchInfo { target: GenBranchTarget::Fallthrough, results }] => {
                        // A simple invocation.
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
            }
            pre_sierra::Statement::Label(_) => {
                self.store_all_deffered_variables();
                self.result.push(statement);
            }
            pre_sierra::Statement::PushValues(push_values) => {
                for pre_sierra::PushValue { var, var_on_stack, ty } in push_values {
                    // TODO(lior): If the variable is already in the correct place, only rename
                    //   it, instead of adding a `store_temp()` statement.
                    self.store_temp(var, var_on_stack, ty);
                }
            }
        }
    }

    /// Registers output variables of a libfunc. See `add_output`.
    fn register_outputs(&mut self, results: &[sierra::ids::VarId], output_infos: &[OutputVarInfo]) {
        for (var, output_info) in itertools::zip_eq(results, output_infos) {
            self.register_output(var.clone(), output_info);
        }
    }

    /// Register an output variable of a libfunc.
    ///
    /// If the variable is marked as Deferred output by the libfunc, it is added to
    /// `self.deferred_variables`.
    fn register_output(&mut self, res: sierra::ids::VarId, output_info: &OutputVarInfo) {
        match output_info.ref_info {
            OutputVarReferenceInfo::Deferred => {
                self.deferred_variables.insert(res, output_info.ty.clone());
            }
            OutputVarReferenceInfo::SameAsParam { .. }
            | OutputVarReferenceInfo::NewTempVar { .. }
            | OutputVarReferenceInfo::NewLocalVar
            | OutputVarReferenceInfo::Const => {}
        }
    }

    /// Stores all the deffered variables and clears `deferred_variables`.
    /// The variables will be added according to the order of creation.
    fn store_all_deffered_variables(&mut self) {
        // TODO(lior): Store all deferred variables.
    }

    /// Clears all the deferred variables.
    fn clear_deffered_variables(&mut self) {
        self.deferred_variables.clear();
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
    }
}
