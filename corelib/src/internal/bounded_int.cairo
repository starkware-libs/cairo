use core::integer::{downcast, upcast};
use core::RangeCheck;

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

impl Felt252TryIntoBoundedInt<
    const MIN: felt252, const MAX: felt252
> of TryInto<felt252, BoundedInt<MIN, MAX>> {
    fn try_into(self: felt252) -> Option<BoundedInt<MIN, MAX>> {
        // Using `downcast` is allowed, since `BoundedInt` itself is not `pub`, and only has few
        // specific `pub` instances, such as `u96`, `ConstZero` and `ConstOne`.
        downcast(self)
    }
}

impl BoundedIntSerde<const MIN: felt252, const MAX: felt252> =
    core::serde::into_felt252_based::SerdeImpl<BoundedInt<MIN, MAX>>;

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

/// A helper trait for adding two `BoundedInt` instances.
pub trait AddHelper<Lhs, Rhs> {
    type Result;
}
impl AddBI01BI01Helper of AddHelper<BoundedInt<0, 1>, BoundedInt<0, 1>> {
    type Result = BoundedInt<0, 2>;
}
impl AddBI02BI01Helper of AddHelper<BoundedInt<0, 2>, BoundedInt<0, 1>> {
    type Result = BoundedInt<0, 3>;
}
extern fn bounded_int_add<Lhs, Rhs, impl H: AddHelper<Lhs, Rhs>>(
    lhs: Lhs, rhs: Rhs
) -> H::Result nopanic;

/// A helper trait for subtracting two `BoundedInt` instances.
pub trait SubHelper<Lhs, Rhs> {
    type Result;
}
extern fn bounded_int_sub<Lhs, Rhs, impl H: SubHelper<Lhs, Rhs>>(
    lhs: Lhs, rhs: Rhs
) -> H::Result nopanic;

/// A helper trait for multiplying two `BoundedInt` instances.
pub trait MulHelper<Lhs, Rhs> {
    type Result;
}
impl NonZeroMulHelper<
    Lhs, Rhs, impl H: MulHelper<Lhs, Rhs>
> of MulHelper<NonZero<Lhs>, NonZero<Rhs>> {
    type Result = NonZero<H::Result>;
}
extern fn bounded_int_mul<Lhs, Rhs, impl H: MulHelper<Lhs, Rhs>>(
    lhs: Lhs, rhs: Rhs
) -> H::Result nopanic;

/// A helper trait for dividing two `BoundedInt` instances.
pub trait DivRemHelper<Lhs, Rhs> {
    type DivT;
    type RemT;
}
extern fn bounded_int_div_rem<Lhs, Rhs, impl H: DivRemHelper<Lhs, Rhs>>(
    lhs: Lhs, rhs: NonZero<Rhs>,
) -> (H::DivT, H::RemT) implicits(RangeCheck) nopanic;

/// A helper trait for constraining a `BoundedInt` instance.
pub trait ConstrainHelper<T, const BOUNDARY: felt252> {
    type LowT;
    type HighT;
}
impl NonZeroConstrainHelper<
    T, const BOUNDARY: felt252, impl H: ConstrainHelper<T, BOUNDARY>
> of ConstrainHelper<NonZero<T>, BOUNDARY> {
    type LowT = NonZero<H::LowT>;
    type HighT = NonZero<H::HighT>;
}

extern fn bounded_int_constrain<T, const BOUNDARY: felt252, impl H: ConstrainHelper<T, BOUNDARY>>(
    value: T
) -> Result<H::LowT, H::HighT> implicits(RangeCheck) nopanic;

pub use {
    bounded_int_add as add, bounded_int_sub as sub, bounded_int_mul as mul,
    bounded_int_div_rem as div_rem, bounded_int_constrain as constrain
};
