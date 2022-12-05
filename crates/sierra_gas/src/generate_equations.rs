use itertools::zip_eq;
use sierra::extensions::builtin_cost::CostTokenType;
use sierra::ids::ConcreteLibFuncId;
use sierra::program::{Program, StatementIdx};
use utils::collection_arithmetics::{add_maps, sub_maps};
use utils::ordered_hash_map::OrderedHashMap;

use super::CostError;
use crate::core_libfunc_cost_expr::CostExprMap;
use crate::cost_expr::{CostExpr, Var};

#[cfg(test)]
#[path = "generate_equations_test.rs"]
mod test;

/// Trait for getting the future cost expressions of statements.
pub trait StatementFutureCost {
    /// Returns the future cost starting from a statement.
    fn get_future_cost(&mut self, idx: &StatementIdx) -> &CostExprMap;
}

/// Generates a set of equations from a program, and a function to extract cost expressions from a
/// library function id.
pub fn generate_equations<
    GetCost: Fn(&mut dyn StatementFutureCost, &StatementIdx, &ConcreteLibFuncId) -> Vec<CostExprMap>,
>(
    program: &Program,
    get_cost: GetCost,
) -> Result<OrderedHashMap<CostTokenType, Vec<CostExpr>>, CostError> {
    // Calculating first to fail early.
    let statement_topological_ordering = get_reverse_topological_ordering(program)?;
    // Vector containing the cost from a statement until the end of the function in some path (may
    // be a variable).
    let mut generator = EquationGenerator::new(vec![None; program.statements.len()]);
    // Adding a variable for every function entry point.
    for func in &program.funcs {
        generator.get_future_cost(&func.entry_point);
    }
    // Using reverse topological order to go over the program statement so that we'd use less
    // variables (since we create variables in any case where we don't already have a cost
    // expression).
    for idx in statement_topological_ordering {
        match &program.get_statement(&idx).unwrap() {
            sierra::program::Statement::Return(_) => {
                generator.set_or_add_constraint(&idx, CostExprMap::default());
            }
            sierra::program::Statement::Invocation(invocation) => {
                let libfunc_cost = get_cost(&mut generator, &idx, &invocation.libfunc_id);
                for (branch, branch_cost) in zip_eq(&invocation.branches, libfunc_cost) {
                    let next_future_cost =
                        generator.get_future_cost(&idx.next(&branch.target)).clone();
                    generator.set_or_add_constraint(&idx, add_maps(branch_cost, next_future_cost));
                }
            }
        }
    }
    Ok(generator.equations)
}

/// Helper to generate the equations for calculating gas variables.
struct EquationGenerator {
    pub future_costs: Vec<Option<CostExprMap>>,
    pub equations: OrderedHashMap<CostTokenType, Vec<CostExpr>>,
}
impl EquationGenerator {
    fn new(future_costs: Vec<Option<CostExprMap>>) -> Self {
        Self {
            future_costs,
            equations: OrderedHashMap::from_iter(
                CostTokenType::iter().map(|token_type| (*token_type, vec![])),
            ),
        }
    }

    /// Sets some future or adds a matching equation if already set.
    fn set_or_add_constraint(&mut self, idx: &StatementIdx, cost: CostExprMap) {
        let entry = &mut self.future_costs[idx.0];
        if let Some(other) = entry {
            for (token_type, val) in sub_maps(other.clone(), cost) {
                self.equations[token_type].push(val);
            }
        } else {
            *entry = Some(cost);
        }
    }
}
impl StatementFutureCost for EquationGenerator {
    /// Returns the future cost starting from a statement, will additionally make sure this
    /// statement actually exists.
    fn get_future_cost(&mut self, idx: &StatementIdx) -> &CostExprMap {
        let entry = &mut self.future_costs[idx.0];
        if let Some(other) = entry {
            other
        } else {
            entry.insert(CostExprMap::from_iter(CostTokenType::iter().map(|token_type| {
                (*token_type, CostExpr::from_var(Var::StatementFuture(*idx, *token_type)))
            })))
        }
    }
}

/// Returns the reverse topological ordering of the program statements.
fn get_reverse_topological_ordering(program: &Program) -> Result<Vec<StatementIdx>, CostError> {
    let mut ordering = vec![];
    let mut visited = vec![false; program.statements.len()];
    for f in &program.funcs {
        calculate_reverse_topological_ordering(
            program,
            &mut ordering,
            &mut visited,
            &f.entry_point,
        )?;
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
