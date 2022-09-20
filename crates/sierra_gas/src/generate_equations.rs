use itertools::zip_eq;
use sierra::ids::ConcreteLibFuncId;
use sierra::program::{Program, StatementIdx};

use super::CostError;
use crate::cost_expr::{CostExpr, Var};

#[cfg(test)]
#[path = "generate_equations_test.rs"]
mod test;

/// Generates a set of equations from a program, and a function to extract cost expressions from a
/// library function id.
pub fn generate_equations<GetCost: Fn(&ConcreteLibFuncId) -> Vec<CostExpr>>(
    program: &Program,
    get_cost: GetCost,
) -> Result<Vec<(CostExpr, CostExpr)>, CostError> {
    let mut future_costs = vec![None; program.statements.len()];
    // Adding a variale for every function entry point.
    for func in &program.funcs {
        future_costs[func.entry.0] = Some(CostExpr::from_var(Var::Statement(func.entry)));
    }
    let mut equations: Vec<(CostExpr, CostExpr)> = vec![];
    // Using reverse topological order to go over the program statement so that we'd use less
    // variables (since we create variables in any case where we don't already have a cost
    // expression).
    for idx in get_reverse_topological_ordering(program) {
        match &program.get_statement(&idx).ok_or(CostError::StatementOutOfBounds(idx))? {
            sierra::program::Statement::Return(_) => {
                let existing_cost = &mut future_costs[idx.0];
                if let Some(other) = existing_cost {
                    equations.push((other.clone(), CostExpr::from_const(0)));
                } else {
                    *existing_cost = Some(CostExpr::from_const(0));
                }
            }
            sierra::program::Statement::Invocation(invocation) => {
                let libfunc_cost = get_cost(&invocation.libfunc_id);
                // Reading an existing future cost in case a variable was injected to the current
                // index in the case of a cycle or function.
                let mut some_future = future_costs[idx.0].clone();
                for (branch, branch_cost) in zip_eq(&invocation.branches, libfunc_cost) {
                    let next = idx.next(&branch.target);
                    let next_cost = &mut future_costs[next.0];
                    if next_cost.is_none() {
                        // If next cost is not known - we've reached a cycle, and we add a variable
                        // to solve.
                        *next_cost = Some(CostExpr::from_var(Var::Statement(next)));
                    }
                    let future_through_branch: CostExpr = next_cost.clone().unwrap() + branch_cost;
                    if let Some(other) = &some_future {
                        equations.push((other.clone(), future_through_branch));
                    } else {
                        some_future = Some(future_through_branch);
                    }
                }
                future_costs[idx.0] = some_future;
            }
        }
    }
    Ok(equations)
}

/// Returns the reverse topological ordering of the program statements.
fn get_reverse_topological_ordering(program: &Program) -> Vec<StatementIdx> {
    let mut ordering = vec![];
    let mut visited = vec![false; program.statements.len()];
    for f in &program.funcs {
        calculate_reverse_topological_ordering(program, &mut ordering, &mut visited, &f.entry);
    }
    ordering
}

/// Recursively calculates the topological ordering of the program.
fn calculate_reverse_topological_ordering(
    program: &Program,
    ordering: &mut Vec<StatementIdx>,
    visited: &mut Vec<bool>,
    idx: &StatementIdx,
) {
    if visited[idx.0] {
        return;
    }
    visited[idx.0] = true;
    match program.get_statement(idx).unwrap() {
        sierra::program::Statement::Invocation(invocation) => {
            for branch in &invocation.branches {
                calculate_reverse_topological_ordering(
                    program,
                    ordering,
                    visited,
                    &idx.next(&branch.target),
                )
            }
        }
        sierra::program::Statement::Return(_) => {}
    }
    // Adding element to ordering after visiting all children - therefore we have reverse
    // topological ordering.
    ordering.push(*idx);
}
