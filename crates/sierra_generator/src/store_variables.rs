//! Handles the automatic addition of store_temp() and store_local() statements.

#[cfg(test)]
#[path = "store_variables_test.rs"]
mod test;

use sierra::extensions::OutputVarReferenceInfo;
use sierra::ids::ConcreteLibFuncId;
use sierra::program::{GenBranchInfo, GenBranchTarget, GenStatement};
use utils::ordered_hash_set::OrderedHashSet;

use crate::pre_sierra;

/// Automatically adds store_temp() statements to the given list of [pre_sierra::Statement].
/// For example, a deferred reference (e.g., `[ap] + [fp - 3]`) needs to be stored as a temporary
/// or local variable before being included in additional computation.
/// The function will add the necessary `store_temp()` instruction before the first use of the
/// deferred reference.
// TODO(lior): Remove #[allow(dead_code)] once this function is used.
#[allow(dead_code)]
fn add_store_statements<GetOutputInfo>(
    statements: Vec<pre_sierra::Statement>,
    get_output_info: &GetOutputInfo,
) -> Vec<pre_sierra::Statement>
where
    GetOutputInfo: Fn(ConcreteLibFuncId) -> Vec<OutputVarReferenceInfo>,
{
    let mut handler = AddStoreVariableStatements::new();
    // Go over the statements, restarting whenever we see a branch or a label.
    for statement in statements.into_iter() {
        handler.handle_statement(statement, get_output_info);
    }
    handler.finalize()
}

struct AddStoreVariableStatements {
    result: Vec<pre_sierra::Statement>,
    deferred_variables: OrderedHashSet<sierra::ids::VarId>,
}
impl AddStoreVariableStatements {
    /// Constructs a new [AddStoreVariableStatements] object.
    fn new() -> Self {
        AddStoreVariableStatements {
            result: Vec::new(),
            deferred_variables: OrderedHashSet::default(),
        }
    }

    /// Handles a single statement, including adding required store statements and the statement
    /// itself.
    fn handle_statement<GetOutputInfo>(
        &mut self,
        statement: pre_sierra::Statement,
        get_output_info: &GetOutputInfo,
    ) where
        GetOutputInfo: Fn(ConcreteLibFuncId) -> Vec<OutputVarReferenceInfo>,
    {
        match &statement {
            pre_sierra::Statement::Sierra(GenStatement::Invocation(invocation)) => {
                let _output_infos = get_output_info(invocation.libfunc_id.clone());
                match &invocation.branches[..] {
                    [GenBranchInfo { target: GenBranchTarget::Fallthrough, results: _ }] => {
                        // A simple invocation.
                    }
                    _ => {
                        // This starts a branch. Store all deferred variables.
                        self.store_all_deffered_variables();
                    }
                }
            }
            pre_sierra::Statement::Sierra(GenStatement::Return(_return_statement)) => {
                // TODO(lior): Add a store_temp() statement for each returned value.
                self.store_all_deffered_variables();
            }
            pre_sierra::Statement::Label(_) => {
                self.store_all_deffered_variables();
            }
        }
        self.result.push(statement);
    }

    /// Stores all the deffered variables and clears `deferred_variables`.
    /// The variables will be added according to the order of creation.
    fn store_all_deffered_variables(&mut self) {
        // TODO(lior): Store all deferred variables.
        self.deferred_variables.clear();
    }

    fn finalize(self) -> Vec<pre_sierra::Statement> {
        assert!(
            self.deferred_variables.is_empty(),
            "Internal compiler error: Remaining unhandled deferred variables."
        );
        self.result
    }
}
