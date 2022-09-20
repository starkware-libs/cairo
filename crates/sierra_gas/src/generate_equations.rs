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
pub fn generate_equations<GetCost: Fn(&StatementIdx, &ConcreteLibFuncId) -> Vec<CostExpr>>(
    program: &Program,
    get_cost: GetCost,
) -> Result<Vec<(CostExpr, CostExpr)>, CostError> {
    // Calculating first to fail early.
    let statement_topological_ordering = get_reverse_topological_ordering(program)?;
    // Vector containing the cost from a statement until the end of the function in some path (may
    // be a variable).
    let mut future_costs = vec![None; program.statements.len()];
    // Adding a variable for every function entry point.
    for func in &program.funcs {
        future_costs[func.entry.0] = Some(CostExpr::from_var(Var::StatementFuture(func.entry)));
    }
    let mut equations: Vec<(CostExpr, CostExpr)> = vec![];
    // Using reverse topological order to go over the program statement so that we'd use less
    // variables (since we create variables in any case where we don't already have a cost
    // expression).
    for idx in statement_topological_ordering {
        match &program.get_statement(&idx).unwrap() {
            sierra::program::Statement::Return(_) => {
                set_or_add_constraint(
                    &mut future_costs[idx.0],
                    CostExpr::from_const(0),
                    &mut equations,
                );
            }
            sierra::program::Statement::Invocation(invocation) => {
                let libfunc_cost = get_cost(&idx, &invocation.libfunc_id);
                // Reading an existing future cost in case a variable was injected to the current
                // index in the case of a cycle or function.
                let mut some_future = future_costs[idx.0].clone();
                for (branch, branch_cost) in zip_eq(&invocation.branches, libfunc_cost) {
                    let next = idx.next(&branch.target);
                    let next_cost = &mut future_costs[next.0];
                    if next_cost.is_none() {
                        // If next cost is not known - we've reached a cycle, and we add a variable
                        // to solve.
                        *next_cost = Some(CostExpr::from_var(Var::StatementFuture(next)));
                    }
                    set_or_add_constraint(
                        &mut some_future,
                        next_cost.clone().unwrap() + branch_cost,
                        &mut equations,
                    );
                }
                future_costs[idx.0] = some_future;
            }
        }
    }
    Ok(equations)
}

/// Sets some future, or adds a matching equation if already set.
fn set_or_add_constraint(
    some_future: &mut Option<CostExpr>,
    known_future: CostExpr,
    equations: &mut Vec<(CostExpr, CostExpr)>,
) {
    if let Some(other) = &*some_future {
        equations.push((other.clone(), known_future));
    } else {
        *some_future = Some(known_future);
    }
}

/// Returns the reverse topological ordering of the program statements.
fn get_reverse_topological_ordering(program: &Program) -> Result<Vec<StatementIdx>, CostError> {
    let mut ordering = vec![];
    let mut visited = vec![false; program.statements.len()];
    for f in &program.funcs {
        calculate_reverse_topological_ordering(program, &mut ordering, &mut visited, &f.entry)?;
    }
    Ok(ordering)
}

/// Recursively calculates the topological ordering of the program.
fn calculate_reverse_topological_ordering(
    program: &Program,
    ordering: &mut Vec<StatementIdx>,
    visited: &mut Vec<bool>,
    idx: &StatementIdx,
) -> Result<(), CostError> {
    match visited.get(idx.0) {
        Some(true) => {
            return Ok(());
        }
        Some(false) => {}
        None => {
            return Err(CostError::StatementOutOfBounds(*idx));
        }
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
                )?;
            }
        }
        sierra::program::Statement::Return(_) => {}
    }
    // Adding element to ordering after visiting all children - therefore we have reverse
    // topological ordering.
    ordering.push(*idx);
    Ok(())
}
