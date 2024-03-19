use cairo_lang_sierra::algorithm::topological_order::get_topological_ordering;
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
                CostTokenType::iter_casm_tokens().map(|token_type| (*token_type, vec![])),
            ),
        }
    }

    /// Sets some future or adds a matching equation if already set.
    fn set_or_add_constraint(&mut self, idx: &StatementIdx, cost: CostExprMap) {
        let entry = &mut self.future_costs[idx.0];
        if let Some(other) = entry {
            for (token_type, val) in sub_maps(other.clone(), cost) {
                self.equations[&token_type].push(val);
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
            entry.insert(CostExprMap::from_iter(CostTokenType::iter_casm_tokens().map(
                |token_type| {
                    (*token_type, CostExpr::from_var(Var::StatementFuture(*idx, *token_type)))
                },
            )))
        }
    }
}

/// Returns the reverse topological ordering of the program statements.
fn get_reverse_topological_ordering(program: &Program) -> Result<Vec<StatementIdx>, CostError> {
    get_topological_ordering(
        false,
        program.funcs.iter().map(|f| f.entry_point),
        program.statements.len(),
        |idx| {
            Ok(match program.get_statement(&idx).unwrap() {
                cairo_lang_sierra::program::Statement::Invocation(invocation) => invocation
                    .branches
                    .iter()
                    .rev()
                    .map(|branch| idx.next(&branch.target))
                    .collect(),
                cairo_lang_sierra::program::Statement::Return(_) => {
                    vec![]
                }
            })
        },
        CostError::StatementOutOfBounds,
        |_| unreachable!("Cycles are not detected."),
    )
}
