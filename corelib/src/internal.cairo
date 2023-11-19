extern fn revoke_ap_tracking() implicits() nopanic;

/// Function to enforce that `Implicit` is used by a function calling it.
/// Note: This extern function is not mapped to a Sierra function, and all usages of it are removed
/// during compilation.
extern fn require_implicit<Implicit>() implicits(Implicit) nopanic;

extern type index_enum_type<const NUM_VARIANTS: felt252>;

// extern fn index_enum_type_from_value<T, const NUM_VARIANTS: felt252>(
//     value: T
// ) -> Option<index_enum_type<-1>> implicits(RangeCheck) nopanic;

extern fn felt252_bounded_from_felt252(value: felt252) -> index_enum_type<-1> nopanic;

extern fn felt252_bounded_constrain_range<
    const LOWER_IN: felt252,
    const UPPER_IN: felt252,
    const LOWER_OUT: felt252,
    const UPPER_OUT: felt252,
>(
    value: felt252
) -> Option<index_enum_type<-1>> implicits(RangeCheck) nopanic;
