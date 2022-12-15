use std::collections::HashMap;

use ap_change_info::ApChangeInfo;
use generate_equations::{Effects, Var};
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::extensions::ConcreteType;
use sierra::ids::{ConcreteTypeId, FunctionId};
use sierra::program::{Program, StatementIdx};
use sierra::program_registry::{ProgramRegistry, ProgramRegistryError};
use thiserror::Error;

pub mod ap_change_info;
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
    /// The libfunc changes `ap` by a known size, which is the size of the given type at locals
    /// finalization stage.
    AtLocalsFinalizationByTypeSize(ConcreteTypeId),
    /// The libfunc changes `ap` by a known size, which is the size of the given type.
    KnownByTypeSize(ConcreteTypeId),
    /// The libfunc is a function call - it changes according to the given function and call cost.
    FunctionCall(FunctionId),
    // The libfunc allocates locals, the `ap` change depends on the environment.
    FinalizeLocals,
}

/// Error occurring while calculating the costing of a program's variables.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ApChangeError {
    #[error("error from the program registry")]
    ProgramRegistryError(#[from] Box<ProgramRegistryError>),
    #[error("found an illegal statement index during ap change calculations")]
    StatementOutOfBounds(StatementIdx),
    #[error("found an illegal statement index during ap change calculations")]
    StatementOutOfOrder(StatementIdx),
    #[error("found an illegal invocation during cost calculations")]
    IllegalInvocation(StatementIdx),
    #[error("failed solving the ap changes")]
    SolvingApChangeEquationFailed,
}

/// Calculates gas information for a given program.
pub fn calc_ap_changes(program: &Program) -> Result<ApChangeInfo, ApChangeError> {
    let registry = ProgramRegistry::<CoreType, CoreLibFunc>::new(program)?;
    let equations = generate_equations::generate_equations(program, |libfunc_id| {
        let libfunc = registry.get_libfunc(libfunc_id)?;
        core_libfunc_ap_change::core_libfunc_ap_change(libfunc)
            .into_iter()
            .map(|ap_change| {
                Ok(match ap_change {
                    ApChange::KnownByTypeSize(ty) => Effects {
                        ap_change: ApChange::Known(registry.get_type(&ty)?.info().size as usize),
                        locals: 0,
                    },
                    ApChange::AtLocalsFinalizationByTypeSize(ty) => Effects {
                        ap_change: ApChange::Known(0),
                        locals: registry.get_type(&ty)?.info().size as usize,
                    },
                    _ => Effects { ap_change, locals: 0 },
                })
            })
            .collect::<Result<Vec<_>, _>>()
    })?;
    let solution = solver::try_solve_equations(equations)
        .ok_or(ApChangeError::SolvingApChangeEquationFailed)?;

    let mut variable_values = HashMap::<StatementIdx, usize>::default();
    let mut function_ap_change = HashMap::<sierra::ids::FunctionId, usize>::default();
    for (var, value) in solution {
        match var {
            Var::LibFuncImplicitApChangeVariable(idx) => {
                variable_values.insert(idx, value as usize)
            }
            Var::FunctionApChange(func_id) => function_ap_change.insert(func_id, value as usize),
        };
    }
    Ok(ApChangeInfo { variable_values, function_ap_change })
}
