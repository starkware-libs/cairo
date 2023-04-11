//! Sierra gas computation.
//!
//! This crate provides the gas computation for the Cairo programs.

use cairo_lang_eq_solver::Expr;
use cairo_lang_sierra::extensions::core::{CoreConcreteLibfunc, CoreLibfunc, CoreType};
use cairo_lang_sierra::extensions::gas::{CostTokenType, GasConcreteLibfunc};
use cairo_lang_sierra::extensions::ConcreteType;
use cairo_lang_sierra::ids::{ConcreteLibfuncId, ConcreteTypeId, FunctionId};
use cairo_lang_sierra::program::{Program, Statement, StatementIdx};
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use core_libfunc_cost_base::InvocationCostInfoProvider;
use core_libfunc_cost_expr::CostExprMap;
use cost_expr::Var;
use gas_info::GasInfo;
use generate_equations::StatementFutureCost;
use itertools::Itertools;
use thiserror::Error;

pub mod core_libfunc_cost;
mod core_libfunc_cost_base;
mod core_libfunc_cost_expr;
mod cost_expr;
pub mod gas_info;
mod generate_equations;
pub mod objects;
mod starknet_libfunc_cost_base;

#[cfg(test)]
mod test;

/// Error occurring while calculating the costing of a program's variables.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum CostError {
    #[error("error from the program registry")]
    ProgramRegistryError(#[from] Box<ProgramRegistryError>),
    #[error("found an illegal statement index during cost calculations")]
    StatementOutOfBounds(StatementIdx),
    #[error("failed solving the symbol tables")]
    SolvingGasEquationFailed,
}

/// Helper to implement the `InvocationCostInfoProvider` for the equation generation.
struct InvocationCostInfoProviderForEqGen<
    'a,
    TokenUsages: Fn(CostTokenType) -> usize,
    ApChangeVarValue: Fn() -> usize,
> {
    /// Registry for providing the sizes of the types.
    registry: &'a ProgramRegistry<CoreType, CoreLibfunc>,
    /// Closure providing the token usages for the invocation.
    token_usages: TokenUsages,
    /// Closure providing the ap changes for the invocation.
    ap_change_var_value: ApChangeVarValue,
}

impl<'a, TokenUsages: Fn(CostTokenType) -> usize, ApChangeVarValue: Fn() -> usize>
    InvocationCostInfoProvider
    for InvocationCostInfoProviderForEqGen<'a, TokenUsages, ApChangeVarValue>
{
    fn type_size(&self, ty: &ConcreteTypeId) -> usize {
        self.registry.get_type(ty).unwrap().info().size as usize
    }

    fn token_usages(&self, token_type: CostTokenType) -> usize {
        (self.token_usages)(token_type)
    }

    fn ap_change_var_value(&self) -> usize {
        (self.ap_change_var_value)()
    }
}

/// Calculates gas precost information for a given program - the gas costs of non-step tokens.
pub fn calc_gas_precost_info(
    program: &Program,
    function_set_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i32>>,
) -> Result<GasInfo, CostError> {
    let registry = ProgramRegistry::<CoreType, CoreLibfunc>::new(program)?;
    calc_gas_info_inner(
        program,
        |statement_future_cost, idx, libfunc_id| -> Vec<OrderedHashMap<CostTokenType, Expr<Var>>> {
            let libfunc = registry
                .get_libfunc(libfunc_id)
                .expect("Program registry creation would have already failed.");
            core_libfunc_cost_expr::core_libfunc_precost_expr(statement_future_cost, idx, libfunc)
        },
        function_set_costs,
        &registry,
    )
}

/// Calculates gas postcost information for a given program - the gas costs of step token.
pub fn calc_gas_postcost_info<ApChangeVarValue: Fn(StatementIdx) -> usize>(
    program: &Program,
    function_set_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i32>>,
    precost_gas_info: &GasInfo,
    ap_change_var_value: ApChangeVarValue,
) -> Result<GasInfo, CostError> {
    let registry = ProgramRegistry::<CoreType, CoreLibfunc>::new(program)?;
    calc_gas_info_inner(
        program,
        |statement_future_cost, idx, libfunc_id| {
            let libfunc = registry
                .get_libfunc(libfunc_id)
                .expect("Program registry creation would have already failed.");
            core_libfunc_cost_expr::core_libfunc_postcost_expr(
                statement_future_cost,
                idx,
                libfunc,
                &InvocationCostInfoProviderForEqGen {
                    registry: &registry,
                    token_usages: |token_type| {
                        precost_gas_info.variable_values[(*idx, token_type)] as usize
                    },
                    ap_change_var_value: || ap_change_var_value(*idx),
                },
            )
        },
        function_set_costs,
        &registry,
    )
}

/// Calculates gas information. Used for both precost and postcost.
fn calc_gas_info_inner<
    GetCost: Fn(&mut dyn StatementFutureCost, &StatementIdx, &ConcreteLibfuncId) -> Vec<CostExprMap>,
>(
    program: &Program,
    get_cost: GetCost,
    function_set_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i32>>,
    registry: &ProgramRegistry<CoreType, CoreLibfunc>,
) -> Result<GasInfo, CostError> {
    let mut equations = generate_equations::generate_equations(program, get_cost)?;
    let non_set_cost_func_entry_points: UnorderedHashSet<_> = program
        .funcs
        .iter()
        .filter(|f| !function_set_costs.contains_key(&f.id))
        .map(|f| f.entry_point)
        .collect();
    for (func_id, cost_terms) in function_set_costs {
        for token_type in CostTokenType::iter() {
            equations[*token_type].push(
                Expr::from_var(Var::StatementFuture(
                    registry.get_function(&func_id)?.entry_point,
                    *token_type,
                )) - Expr::from_const(cost_terms.get(token_type).copied().unwrap_or_default()),
            );
        }
    }

    let mut variable_values = OrderedHashMap::default();
    let mut function_costs = OrderedHashMap::default();
    for (token_type, token_equations) in equations {
        // Setting up minimization vars with three ranks:
        // 1. Minimizing function costs variables.
        // 2. Minimizing gas withdraw variables.
        // 3. Minimizing branch align (burn gas) variables.
        // We use this ordering to solve several issues:
        // * In cases where we have a function with a set cost, that calls another function, and
        //   then several calls to branch align, the inner function's price may be increased in
        //   order to reduce the value of the burn gas variables, although we would prefer that the
        //   function's value would be reduced (since it may be called from another point as well).
        //   Therefore we should optimize over function costs before optimizing over branch aligns.
        // * In cases where we have a function with an unset cost - that call `withdraw_gas` we can
        //   decide to make the function pricier to reduce the amount of withdrawn gas. Therefore we
        //   should optimize over function costs before optimizing over withdraw variables.
        // * Generally we would of course prefer optimizing over withdraw variables before branch
        //   align variables, as they cost gas to the user.
        let mut minimization_vars = vec![vec![], vec![], vec![]];
        for v in token_equations.iter().flat_map(|eq| eq.var_to_coef.keys()).unique() {
            minimization_vars[match v {
                Var::LibfuncImplicitGasVariable(idx, _) => {
                    match program.get_statement(idx).unwrap() {
                        Statement::Invocation(invocation) => {
                            match registry.get_libfunc(&invocation.libfunc_id).unwrap() {
                                CoreConcreteLibfunc::BranchAlign(_) => 2,
                                CoreConcreteLibfunc::Gas(GasConcreteLibfunc::WithdrawGas(_)) => 1,
                                CoreConcreteLibfunc::Gas(
                                    GasConcreteLibfunc::BuiltinWithdrawGas(_),
                                ) => 0,
                                // TODO(orizi): Make this actually maximized.
                                CoreConcreteLibfunc::Gas(GasConcreteLibfunc::RedepositGas(_)) => {
                                    continue;
                                }
                                _ => unreachable!(
                                    "Gas variables variables cannot originate from {}.",
                                    invocation.libfunc_id
                                ),
                            }
                        }
                        Statement::Return(_) => continue,
                    }
                }
                Var::StatementFuture(idx, _) if non_set_cost_func_entry_points.contains(idx) => 0,
                Var::StatementFuture(_, _) => {
                    continue;
                }
            }]
            .push(v.clone())
        }
        let solution =
            cairo_lang_eq_solver::try_solve_equations(token_equations, minimization_vars)
                .ok_or(CostError::SolvingGasEquationFailed)?;
        for func in &program.funcs {
            let id = &func.id;
            if !function_costs.contains_key(id) {
                function_costs.insert(id.clone(), OrderedHashMap::default());
            }
            let value = solution[Var::StatementFuture(func.entry_point, token_type)];
            if value != 0 {
                function_costs.get_mut(id).unwrap().insert(token_type, value);
            }
        }
        for (var, value) in solution {
            if let Var::LibfuncImplicitGasVariable(idx, var_token_type) = var {
                assert_eq!(
                    token_type, var_token_type,
                    "Unexpected variable of type {var_token_type:?} while handling {token_type:?}."
                );
                variable_values.insert((idx, var_token_type), value);
            }
        }
    }
    Ok(GasInfo { variable_values, function_costs })
}
