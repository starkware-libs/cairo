use core::integer::upcast;
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

impl BoundedIntIntoFelt252<
    const MIN: felt252, const MAX: felt252
> of Into<BoundedInt<MIN, MAX>, felt252> {
    fn into(self: BoundedInt<MIN, MAX>) -> felt252 {
        upcast(self)
    }
}

impl BoundedIntPartialEq<
    const MIN: felt252, const MAX: felt252
> of PartialEq<BoundedInt<MIN, MAX>> {
    #[inline(always)]
    fn eq(lhs: @BoundedInt<MIN, MAX>, rhs: @BoundedInt<MIN, MAX>) -> bool {
        Into::<_, felt252>::into(*lhs) == (*rhs).into()
    }
}

impl BoundedIntDebug<const MIN: felt252, const MAX: felt252> =
    core::fmt::into_felt252_based::DebugImpl<BoundedInt<MIN, MAX>>;
