#[cfg(test)]
#[path = "dup_and_drop_test.rs"]
mod test;

use std::collections::HashSet;

use itertools::{chain, Itertools};
use sierra::ids::VarId;
use sierra::program::Param;
use utils::ordered_hash_set::{self, OrderedHashSet};
use utils::unordered_hash_set::UnorderedHashSet;

use crate::next_statement_index_fetch::NextStatementIndexFetch;
use crate::pre_sierra::Statement;

/// Variables that needs to be dupped and dropped in a statement.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct VarsDupsAndDrops {
    pub dups: OrderedHashSet<VarId>,
    pub drops: OrderedHashSet<VarId>,
}

/// Calculates the set of variables requiring duplication and variables that should be dropped
/// for each statement.
/// Variables that should have been dropped earlier are not considered.
pub fn calculate_statement_dups_and_drops(
    params: &[Param],
    statements: &[Statement],
) -> Vec<VarsDupsAndDrops> {
    let next_statement_index_fetch = &NextStatementIndexFetch::new(statements, true);
    let mut statement_required_vars = vec![None; statements.len()];
    let statement_existing_vars = get_existing_vars_per_statement(
        statements,
        params,
        next_statement_index_fetch,
        &mut statement_required_vars,
    );
    statements
        .iter()
        .enumerate()
        .map(|(i, statement)| {
            let RequiredVarsSets { curr_vars, future_vars } = get_required_vars(
                next_statement_index_fetch,
                &mut statement_required_vars,
                i,
                statements,
            );
            match statement {
                Statement::Sierra(sierra::program::GenStatement::Invocation(_))
                | Statement::Sierra(sierra::program::GenStatement::Return(_)) => {
                    // Variable scopes:
                    // Current | Future | Decision
                    // ----------------------------
                    // In      | In     | Duplicate
                    // In      | Out    | Default handle (just let it be used in the current scope)
                    // Out     | In     | Default handle (just let it pass to next scope)
                    // Out     | Out    | Ignore
                    VarsDupsAndDrops {
                        // Double usages (which would require duplicates as well) are not handled
                        // here: For return statements - this can't happen -
                        // as all returned variables are stored - so would have different ids.
                        // For simple libfuncs invocations - this is handled externally, as it is
                        // easily detectable - and would be a more complex state to return. (see the
                        // implementation in `function_generator.rs` for extra information)
                        dups: curr_vars
                            .iter()
                            .filter(|var| future_vars.contains(*var))
                            .cloned()
                            .collect(),
                        drops: statement_existing_vars[i]
                            .as_ref()
                            .unwrap()
                            .iter()
                            .filter(|var| !curr_vars.contains(*var) && !future_vars.contains(*var))
                            .cloned()
                            .collect(),
                    }
                }
                Statement::Label(_) => {
                    // Label is a no-op - so we do no changes to it.
                    VarsDupsAndDrops {
                        dups: OrderedHashSet::<VarId>::default(),
                        drops: OrderedHashSet::<VarId>::default(),
                    }
                }
                Statement::PushValues(_) => panic!(
                    "Unexpected pre_sierra::Statement::PushValues in \
                     calculate_statement_dups_and_drops()."
                ),
            }
        })
        .collect()
}

/// Calculates and returns the existing variables for each statement, by propagating all the
/// unignorable (used by ANY following branch) existing variables to the following statements.
fn get_existing_vars_per_statement(
    statements: &[Statement],
    params: &[Param],
    next_statement_index_fetch: &NextStatementIndexFetch,
    statement_required_vars: &mut [Option<RequiredVarsSets>],
) -> Vec<Option<OrderedHashSet<VarId>>> {
    // The calculated value for each statement.
    let mut statement_existing_vars: Vec<Option<OrderedHashSet<_>>> = vec![None; statements.len()];
    statement_existing_vars[0] = Some(params.iter().map(|p| p.id.clone()).collect());
    for (i, statement) in statements.iter().enumerate() {
        match statement {
            Statement::Sierra(sierra::program::GenStatement::Invocation(invocation)) => {
                let future_vars = &get_required_vars(
                    next_statement_index_fetch,
                    statement_required_vars,
                    i,
                    statements,
                )
                .future_vars;
                // Updating `statement_existing_vars` for the branches - which are all the
                // `next_required_vars` with the branches inserted results.
                for branch in &invocation.branches {
                    let next_index = next_statement_index_fetch.get(i, &branch.target);
                    let next_existing_vars: OrderedHashSet<VarId> =
                        chain!(future_vars.iter(), branch.results.iter()).cloned().collect();
                    let entry = &mut statement_existing_vars[next_index];
                    if let Some(previous_value) = entry {
                        assert_eq!(
                            previous_value.iter().collect::<HashSet<_>>(),
                            next_existing_vars.iter().collect::<HashSet<_>>(),
                            "Got a different set of variables to the same entry point."
                        );
                    }
                    *entry = Some(next_existing_vars)
                }
            }
            Statement::Sierra(sierra::program::GenStatement::Return(_)) => {}
            Statement::Label(_) => {
                // Label is a no-op - so we do no changes to it.
                statement_existing_vars[i + 1] = statement_existing_vars[i].clone();
            }
            Statement::PushValues(_) => {
                panic!(
                    "Unexpected pre_sierra::Statement::PushValues in \
                     get_existing_vars_per_statement()."
                )
            }
        }
    }
    statement_existing_vars
}

/// The required vars for a statement.
#[derive(Clone, Default)]
struct RequiredVarsSets {
    /// Variables required by the current command.
    pub curr_vars: OrderedHashSet<VarId>,
    /// Variables required by following commands.
    /// Note that variables may appear in both sets.
    pub future_vars: OrderedHashSet<VarId>,
}
impl RequiredVarsSets {
    fn all_vars(
        &self,
    ) -> std::iter::Chain<ordered_hash_set::Iter<'_, VarId>, ordered_hash_set::Iter<'_, VarId>>
    {
        chain!(self.curr_vars.iter(), self.future_vars.iter())
    }
}

/// Returns the set of required variables for a pre-Sierra statement.
/// For more info read `calculate_required_vars_for_statement` documentation.
fn get_required_vars<'a>(
    next_statement_index_fetch: &NextStatementIndexFetch,
    required_vars: &'a mut [Option<RequiredVarsSets>],
    index: usize,
    statements: &[Statement],
) -> &'a RequiredVarsSets {
    if required_vars[index].is_none() {
        calculate_required_vars_for_statement(
            next_statement_index_fetch,
            required_vars,
            index,
            statements,
        );
    }
    required_vars[index].as_ref().unwrap()
}

/// Calculates the set of required variables for a pre-Sierra statement and all of its dependencies.
///
/// For example, a return statement requires the returned variable.
/// A libfunc invocation requires the libfunc arguments and the variables required
/// by the next statement (excluding the ones created by the libfunc).
///
/// The function fills the given vector, starting from the given index and including
/// everything it depends on (using DFS).
fn calculate_required_vars_for_statement(
    next_statement_index_fetch: &NextStatementIndexFetch,
    required_vars: &mut [Option<RequiredVarsSets>],
    index: usize,
    statements: &[Statement],
) {
    // Finding the missing dependencies using DFS.
    let mut missing_deps = vec![];
    let mut visited = UnorderedHashSet::<usize>::default();
    let mut handle_stack = vec![];
    let push_if_relevant =
        |handle_stack: &mut Vec<usize>, visited: &UnorderedHashSet<usize>, index: usize| {
            if required_vars[index].is_none() && !visited.contains(&index) {
                handle_stack.push(index);
            }
        };
    push_if_relevant(&mut handle_stack, &visited, index);
    while let Some(index) = handle_stack.last().copied() {
        if !visited.insert(index) {
            handle_stack.pop();
            missing_deps.push(index);
            continue;
        }
        match &statements[index] {
            Statement::Sierra(sierra::program::GenStatement::Invocation(invocation)) => {
                invocation.branches.iter().for_each(|branch| {
                    push_if_relevant(
                        &mut handle_stack,
                        &visited,
                        next_statement_index_fetch.get(index, &branch.target),
                    );
                });
            }
            Statement::Sierra(sierra::program::GenStatement::Return(_)) => {}
            Statement::Label(_) => {
                push_if_relevant(&mut handle_stack, &visited, index + 1);
            }
            Statement::PushValues(_) => {
                panic!(
                    "Unexpected pre_sierra::Statement::PushValues in \
                     calculate_required_vars_for_statement()."
                )
            }
        }
    }
    // Iterating over the dependencies in the dfs order, so the required elements would be prepared.
    for index in missing_deps {
        match &statements[index] {
            // The required variables for an invocation, is all of its branches required vars,
            // without vars generated by the call, and its arguments.
            Statement::Sierra(sierra::program::GenStatement::Invocation(invocation)) => {
                let future_vars = invocation
                    .branches
                    .iter()
                    .flat_map(|branch| {
                        let next_index = next_statement_index_fetch.get(index, &branch.target);
                        let results: UnorderedHashSet<VarId> =
                            branch.results.iter().cloned().collect();
                        required_vars[next_index]
                            .as_ref()
                            .unwrap()
                            .all_vars()
                            .filter(move |v| !results.contains(v))
                            .cloned()
                            .collect_vec()
                    })
                    .collect();
                required_vars[index] = Some(RequiredVarsSets {
                    curr_vars: invocation.args.iter().cloned().collect(),
                    future_vars,
                });
            }
            Statement::Sierra(sierra::program::GenStatement::Return(ret_vars)) => {
                // At return - the only required variables are the returned variables.
                required_vars[index] = Some(RequiredVarsSets {
                    curr_vars: ret_vars.iter().cloned().collect(),
                    future_vars: OrderedHashSet::default(),
                })
            }
            Statement::Label(_) => {
                // Labels are no-ops - so we just use the same as next line.
                required_vars[index] = Some(RequiredVarsSets {
                    curr_vars: OrderedHashSet::default(),
                    future_vars: required_vars[index + 1]
                        .as_ref()
                        .unwrap()
                        .all_vars()
                        .cloned()
                        .collect(),
                })
            }
            Statement::PushValues(_) => {}
        }
    }
}
