//! Sierra AP change model.
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_sierra::program_registry::ProgramRegistryError;
use thiserror::Error;

pub mod ap_change_info;
mod compute;
pub mod core_libfunc_ap_change;

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

pub use compute::calc_ap_changes;
