pub extern fn revoke_ap_tracking() implicits() nopanic;

/// Function to enforce that `Implicit` is used by a function calling it.
/// Note: This extern function is not mapped to a Sierra function, and all usages of it are removed
/// during compilation.
pub extern fn require_implicit<Implicit>() implicits(Implicit) nopanic;

extern type index_enum_type<const NUM_VARIANTS: felt252>;

extern type BoundedInt<const min: felt252, const max: felt252>;

extern fn constrain_range<T, S>(value: T) -> Option<S> implicits(RangeCheck) nopanic;
