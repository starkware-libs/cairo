pub extern fn revoke_ap_tracking() implicits() nopanic;

/// Function to enforce that `Implicit` is used by a function calling it.
/// Note: This extern function is not mapped to a Sierra function, and all usages of it are removed
/// during compilation.
pub extern fn require_implicit<Implicit>() implicits(Implicit) nopanic;

extern type index_enum_type<const NUM_VARIANTS: felt252>;

#[derive(Copy, Drop)]
pub(crate) extern type BoundedInt<const MIN: felt252, const MAX: felt252>;
impl NumericLiteralBoundedInt<
    const MIN: felt252, const MAX: felt252
> of core::integer::NumericLiteral<BoundedInt<MIN, MAX>>;

