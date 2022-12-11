pub mod expr;

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

pub use expr::Expr;
use good_lp::{default_solver, variable, variables, Expression, Solution, SolverModel};

/// Solving a set of equations and returning the values of the symbols contained in them.
pub fn try_solve_equations<Var: Clone + Debug + PartialEq + Eq + Hash>(
    equations: Vec<Expr<Var>>,
) -> Option<HashMap<Var, i64>> {
    let mut vars = variables!();
    let mut orig_to_solver_var = HashMap::new();
    // Add all variables to structure and map.
    for eq in &equations {
        for var in eq.var_to_coef.keys() {
            match orig_to_solver_var.entry(var.clone()) {
                Entry::Occupied(_) => {}
                Entry::Vacant(e) => {
                    e.insert(vars.add(variable().min(0).name(format!("{:?}", var))));
                }
            }
        }
    }
    // Minimizing the value of all variables would make us take the least amount of gas.
    let target: Expression = vars.iter_variables_with_def().map(|(v, _)| v).sum();
    let mut problem = vars.minimise(target).using(default_solver);
    // Adding constraints for all equations.
    for eq in equations {
        let as_solver_expr = |expr: Expr<Var>| {
            Expression::from_other_affine(expr.const_term)
                + expr
                    .var_to_coef
                    .into_iter()
                    .map(|(var, coef)| (coef as i32) * *orig_to_solver_var.get(&var).unwrap())
                    .sum::<Expression>()
        };
        problem = problem.with(as_solver_expr(eq).eq(Expression::from_other_affine(0)));
    }
    let solution = problem.solve().ok()?;
    Some(
        orig_to_solver_var
            .into_iter()
            .map(|(orig, solver)| (orig, solution.value(solver) as i64))
            .collect(),
    )
}
