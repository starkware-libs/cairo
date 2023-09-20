use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::ConcreteLibfuncId;
use cairo_lang_sierra::program::{Program, StatementIdx};
use cairo_lang_utils::collection_arithmetics::{add_maps, sub_maps};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::zip_eq;

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
    GetCost: Fn(&mut dyn StatementFutureCost, &StatementIdx, &ConcreteLibfuncId) -> Vec<CostExprMap>,
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
            cairo_lang_sierra::program::Statement::Return(_) => {
                generator.set_or_add_constraint(&idx, CostExprMap::default());
            }
            cairo_lang_sierra::program::Statement::Invocation(invocation) => {
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

#[derive(Clone, Debug)]
enum TopologicalOrderStatus {
    /// The computation for that statement did not start.
    NotStarted,
    /// The computation is in progress.
    InProgress,
    /// The computation was completed, and all the children were visited.
    Done,
}

/// Returns the reverse topological ordering of the program statements.
fn get_reverse_topological_ordering(program: &Program) -> Result<Vec<StatementIdx>, CostError> {
    let mut ordering = vec![];
    let mut status = vec![TopologicalOrderStatus::NotStarted; program.statements.len()];
    for f in &program.funcs {
        calculate_reverse_topological_ordering(
            &mut ordering,
            &mut status,
            &f.entry_point,
            |idx| match program.get_statement(&idx).unwrap() {
                cairo_lang_sierra::program::Statement::Invocation(invocation) => invocation
                    .branches
                    .iter()
                    .rev()
                    .map(|branch| idx.next(&branch.target))
                    .collect(),
                cairo_lang_sierra::program::Statement::Return(_) => {
                    vec![]
                }
            },
        )?;
    }
    Ok(ordering)
}

/// Recursively calculates the topological ordering of the program.
fn calculate_reverse_topological_ordering(
    ordering: &mut Vec<StatementIdx>,
    status: &mut [TopologicalOrderStatus],
    idx0: &StatementIdx,
    children_callback: impl Fn(&StatementIdx) -> Vec<StatementIdx>,
) -> Result<(), CostError> {
    // A stack of statements to visit.
    // When the pair is popped out of the stack, `is_done=true` means that we've already visited
    // all of its children, and we just need to add it to the ordering.
    let mut stack = vec![*idx0];

    while let Some(idx) = stack.pop() {
        match status.get(idx.0) {
            Some(TopologicalOrderStatus::NotStarted) => {
                // Mark the statement as `InProgress`.
                status[idx.0] = TopologicalOrderStatus::InProgress;

                // Push the statement back to the stack, so that after visiting all
                // of its children, we would add it to the ordering.
                // Add the missing children on top of it.
                stack.push(idx);
                for child in children_callback(&idx) {
                    match status.get(child.0) {
                        Some(TopologicalOrderStatus::InProgress)
                        | Some(TopologicalOrderStatus::Done) => {
                            continue;
                        }
                        Some(TopologicalOrderStatus::NotStarted) => {
                            stack.push(child);
                        }
                        None => {
                            return Err(CostError::StatementOutOfBounds(idx));
                        }
                    }
                }
            }
            Some(TopologicalOrderStatus::InProgress) => {
                // Mark the statement as `Done`.
                status[idx.0] = TopologicalOrderStatus::Done;

                // Add the element to the ordering after visiting all its children.
                // This gives us reverse topological ordering.
                ordering.push(idx);
            }
            Some(TopologicalOrderStatus::Done) => {
                // Do nothing.
            }
            None => {
                return Err(CostError::StatementOutOfBounds(idx));
            }
        }
    }

    Ok(())
}
