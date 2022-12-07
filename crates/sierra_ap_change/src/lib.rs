use sierra::ids::{ConcreteTypeId, FunctionId};

pub mod core_libfunc_ap_change;

/// Describes the effect on the `ap` register in a given libfunc branch.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ApChange {
    /// The libfunc changes `ap` in an unknown way.
    Unknown,
    /// The libfunc changes `ap` by a known size.
    Known(usize),
    /// The libfunc changes `ap` by a known size.
    KnownByTypeSize(ConcreteTypeId),
    /// The libfunc changes `ap` by a known size.
    FunctionCall(FunctionId),
    // The libfunc allocates locals, the `ap` change depends on the environment.
    FinalizeLocals,
}
