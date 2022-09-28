use std::collections::hash_map::Entry;
use std::collections::HashMap;

use good_lp::{default_solver, variable, variables, Expression, Solution, SolverModel};
use itertools::chain;
use sierra::program::StatementIdx;

use super::CostError;
use crate::cost_expr::{CostExpr, Var};

/// Solving a set of equations and returning the values of the symbols contained in them.
pub fn solve_equations(
    equations: Vec<(CostExpr, CostExpr)>,
) -> Result<HashMap<StatementIdx, i64>, CostError> {
    let mut vars = variables!();
    let mut orig_to_solver_var = HashMap::new();
    // Add all variables to structure and map.
    for (lhs, rhs) in &equations {
        for var in chain!(lhs.var_to_coef.keys(), rhs.var_to_coef.keys()) {
            match orig_to_solver_var.entry(var.clone()) {
                Entry::Occupied(_) => {}
                Entry::Vacant(e) => {
                    e.insert(vars.add(variable().min(0).name(format!("{}", var))));
                }
            }
        }
    }
    // Minimizing the value of all variables would make us take the least amount of gas.
    let target: Expression = vars.iter_variables_with_def().map(|(v, _)| v).sum();
    let mut problem = vars.minimise(target).using(default_solver);
    // Adding constraints for all equations.
    for (lhs, rhs) in equations {
        let as_solver_expr = |cost: CostExpr| {
            Expression::from_other_affine(cost.const_term)
                + cost
                    .var_to_coef
                    .into_iter()
                    .map(|(var, coef)| (coef as i32) * *orig_to_solver_var.get(&var).unwrap())
                    .sum::<Expression>()
        };
        problem = problem.with(as_solver_expr(lhs).eq(as_solver_expr(rhs)));
    }
    let solution = problem.solve().map_err(|_| CostError::SolvingGasEquationFailed)?;
    Ok(orig_to_solver_var
        .into_iter()
        .filter_map(|(orig, solver)| match orig {
            Var::LibFuncImplicitGasVariable(symbol) => {
                Some((symbol, solution.value(solver) as i64))
            }
            Var::StatementFuture(_) => None,
        })
        .collect())
}
