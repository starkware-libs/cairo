pub extern fn revoke_ap_tracking() implicits() nopanic;

/// Function to enforce that `Implicit` is used by a function calling it.
/// Note: This extern function is not mapped to a Sierra function, and all usages of it are removed
/// during compilation.
pub extern fn require_implicit<Implicit>() implicits(Implicit) nopanic;

extern type index_enum_type<const NUM_VARIANTS: felt252>;

/// Function for marking the current state of execution.
/// Useful for debugging and panic tracking.
extern fn trace<const FLAG: felt252>() nopanic;

pub(crate) mod bounded_int;

/// Same as `Option`, except that the order of the variants is reversed.
/// This is used as the return type of some libfuncs for efficiency reasons.
#[must_use]
#[derive(Copy, Drop, Debug, PartialEq)]
pub enum OptionRev<T> {
    None,
    Some: T,
}

/// Wrapper type to ensure that a type `T` is dropped using a specific `Drop` impl.
pub struct DropWith<T, impl DropT: Drop<T>> {
    pub value: T,
}
impl DropWithDrop<T, impl DropT: Drop<T>> of Drop<DropWith<T, DropT>>;

/// Helper to have the same interface as `DropWith` while inferring the `Drop` implementation.
#[derive(Drop)]
pub struct InferDrop<T> {
    pub value: T,
}

/// Wrapper type to ensure that a type `T` is destructed using a specific `Destruct` impl.
pub struct DestructWith<T, impl DestructT: Destruct<T>> {
    pub value: T,
}
impl DestructWithDestruct<T, impl DestructT: Destruct<T>> of Destruct<DestructWith<T, DestructT>> {
    fn destruct(self: DestructWith<T, DestructT>) nopanic {
        DestructT::destruct(self.value)
    }
}

/// Helper to have the same interface as `DestructWith` while inferring the `Destruct`
/// implementation.
#[derive(Destruct)]
pub struct InferDestruct<T> {
    pub value: T,
}

/// The return type for loops with an early return.
pub enum LoopResult<N, E> {
    Normal: N,
    EarlyReturn: E,
}
