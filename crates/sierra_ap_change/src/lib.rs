use sierra::ids::{ConcreteTypeId, FunctionId};

pub mod core_libfunc_ap_change;

/// Describes the effect on the `ap` register in a given libfunc branch.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ApChange {
    /// The libfunc changes `ap` in an unknown way.
    Unknown,
    /// The libfunc changes `ap` by a known size.
    Known(usize),
    /// The libfunc changes `ap` by a known size, which is the size of the given type.
    KnownByTypeSize(ConcreteTypeId),
    /// The libfunc is a function call - it changes according to the given function and call cost.
    FunctionCall(FunctionId),
    // The libfunc allocates locals, the `ap` change depends on the environment.
    FinalizeLocals,
}
