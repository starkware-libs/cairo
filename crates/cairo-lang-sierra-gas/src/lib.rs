//! Sierra gas computation.
//!
//! This crate provides the gas computation for the Cairo programs.

use cairo_lang_eq_solver::Expr;
use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType};
use cairo_lang_sierra::extensions::ConcreteType;
use cairo_lang_sierra::ids::{ConcreteLibfuncId, ConcreteTypeId, FunctionId};
use cairo_lang_sierra::program::{Program, StatementIdx};
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
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
                .expect("Program registery creation would have already failed.");
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
                .expect("Program registery creation would have already failed.");
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
        let all_vars = token_equations.iter().flat_map(|eq| eq.var_to_coef.keys());
        let function_vars = all_vars
            .clone()
            .filter(|v| matches!(v, Var::StatementFuture(_, _)))
            .unique()
            .cloned()
            .collect();
        let gas_vars = all_vars
            .filter(|v| matches!(v, Var::LibfuncImplicitGasVariable(_, _)))
            .unique()
            .cloned()
            .collect();
        let solution = cairo_lang_eq_solver::try_solve_equations(
            token_equations,
            vec![function_vars, gas_vars],
        )
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
