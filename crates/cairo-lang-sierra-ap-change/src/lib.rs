//! Sierra AP change model.
use ap_change_info::ApChangeInfo;
use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType};
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::{ConcreteTypeId, FunctionId};
use cairo_lang_sierra::program::{Program, StatementIdx};
use cairo_lang_sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use cairo_lang_sierra_type_size::{get_type_size_map, TypeSizeMap};
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use core_libfunc_ap_change::InvocationApChangeInfoProvider;
use generate_equations::{Effects, Var};
use itertools::Itertools;
use thiserror::Error;

pub mod ap_change_info;
/// Direct linear computation of AP-Changes instead of equation solver.
pub mod compute;
pub mod core_libfunc_ap_change;
mod generate_equations;

/// Describes the effect on the `ap` register in a given libfunc branch.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ApChange {
    /// The libfunc changes `ap` in an unknown way.
    Unknown,
    /// The libfunc changes `ap` by a known size.
    Known(usize),
    /// The libfunc changes `ap` by a known size, provided in the metadata. Currently this only
    /// includes `branch_align` libfunc.
    FromMetadata,
    /// The libfunc changes `ap` by a known size at locals finalization stage.
    AtLocalsFinalization(usize),
    /// The libfunc is a function call - it changes according to the given function and call cost.
    FunctionCall(FunctionId),
    /// The libfunc allocates locals, the `ap` change depends on the environment.
    FinalizeLocals,
    /// The libfunc is the ap tracking enabler.
    EnableApTracking,
    /// The libfunc is the ap tracking disabler.
    DisableApTracking,
}

/// Error occurring while calculating the costing of a program's variables.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ApChangeError {
    #[error("error from the program registry")]
    ProgramRegistryError(#[from] Box<ProgramRegistryError>),
    #[error("found an illegal statement index during ap change calculations")]
    StatementOutOfBounds(StatementIdx),
    #[error("got a statement out of order during ap change calculations")]
    StatementOutOfOrder(StatementIdx),
    #[error("Wrong number of libfunc branches in ap-change information")]
    WrongNumApChangeBranches(StatementIdx),
    #[error("Attempted to merge branches with different number of allocated locals")]
    BadMergeAllocatedLocalsMismatch(StatementIdx),
    #[error("Attempted to merge branches with different bases to align")]
    BadMergeBaseMismatch(StatementIdx),
    #[error("failed solving the ap changes")]
    SolvingApChangeEquationFailed,
}

/// Helper to implement the `InvocationApChangeInfoProvider` for the equation generation.
struct InvocationApChangeInfoProviderForEqGen<'a, TokenUsages: Fn(CostTokenType) -> usize> {
    /// Registry for providing the sizes of the types.
    type_sizes: &'a TypeSizeMap,
    /// Closure providing the token usages for the invocation.
    token_usages: TokenUsages,
}

impl<'a, TokenUsages: Fn(CostTokenType) -> usize> InvocationApChangeInfoProvider
    for InvocationApChangeInfoProviderForEqGen<'a, TokenUsages>
{
    fn type_size(&self, ty: &ConcreteTypeId) -> usize {
        self.type_sizes[ty].into_or_panic()
    }

    fn token_usages(&self, token_type: CostTokenType) -> usize {
        (self.token_usages)(token_type)
    }
}

/// Calculates gas information for a given program.
pub fn calc_ap_changes<TokenUsages: Fn(StatementIdx, CostTokenType) -> usize>(
    program: &Program,
    token_usages: TokenUsages,
) -> Result<ApChangeInfo, ApChangeError> {
    let registry = ProgramRegistry::<CoreType, CoreLibfunc>::new(program)?;
    let type_sizes = get_type_size_map(program, &registry).unwrap();
    let equations = generate_equations::generate_equations(program, |idx, libfunc_id| {
        let libfunc = registry.get_libfunc(libfunc_id)?;
        core_libfunc_ap_change::core_libfunc_ap_change(
            libfunc,
            &InvocationApChangeInfoProviderForEqGen {
                type_sizes: &type_sizes,
                token_usages: |token_type| token_usages(idx, token_type),
            },
        )
        .into_iter()
        .map(|ap_change| {
            Ok(match ap_change {
                ApChange::AtLocalsFinalization(known) => {
                    Effects { ap_change: ApChange::Known(0), locals: known }
                }
                _ => Effects { ap_change, locals: 0 },
            })
        })
        .collect::<Result<Vec<_>, _>>()
    })?;
    let minimization_vars =
        equations.iter().flat_map(|eq| eq.var_to_coef.keys()).unique().cloned().collect();
    let solution = cairo_lang_eq_solver::try_solve_equations(equations, vec![minimization_vars])
        .ok_or(ApChangeError::SolvingApChangeEquationFailed)?;

    let mut variable_values = OrderedHashMap::default();
    let mut function_ap_change = OrderedHashMap::default();
    for (var, value) in solution {
        match var {
            Var::LibfuncImplicitApChangeVariable(idx) => {
                variable_values.insert(idx, value as usize)
            }
            Var::FunctionApChange(func_id) => function_ap_change.insert(func_id, value as usize),
        };
    }
    Ok(ApChangeInfo { variable_values, function_ap_change })
}
