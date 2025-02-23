use crate::integer::{downcast, upcast};
use crate::RangeCheck;

pub(crate) extern type BoundedInt<const MIN: felt252, const MAX: felt252>;

impl BoundedIntCopy<const MIN: felt252, const MAX: felt252> of Copy<BoundedInt<MIN, MAX>>;
impl BoundedIntDrop<const MIN: felt252, const MAX: felt252> of Drop<BoundedInt<MIN, MAX>>;

impl NumericLiteralBoundedInt<
    const MIN: felt252, const MAX: felt252,
> of crate::integer::NumericLiteral<BoundedInt<MIN, MAX>>;

impl BoundedIntIntoFelt252<
    const MIN: felt252, const MAX: felt252,
> of Into<BoundedInt<MIN, MAX>, felt252> {
    const fn into(self: BoundedInt<MIN, MAX>) -> felt252 {
        upcast(self)
    }
}

impl Felt252TryIntoBoundedInt<
    const MIN: felt252, const MAX: felt252,
> of TryInto<felt252, BoundedInt<MIN, MAX>> {
    const fn try_into(self: felt252) -> Option<BoundedInt<MIN, MAX>> {
        // Using `downcast` is allowed, since `BoundedInt` itself is not `pub`, and only has a few
        // specific `pub` instances, such as `u96`, `ConstZero` and `ConstOne`.
        downcast(self)
    }
}

impl BoundedIntSerde<const MIN: felt252, const MAX: felt252> =
    crate::serde::into_felt252_based::SerdeImpl<BoundedInt<MIN, MAX>>;

impl BoundedIntPartialEq<
    const MIN: felt252, const MAX: felt252,
> of PartialEq<BoundedInt<MIN, MAX>> {
    #[inline(always)]
    fn eq(lhs: @BoundedInt<MIN, MAX>, rhs: @BoundedInt<MIN, MAX>) -> bool {
        Into::<_, felt252>::into(*lhs) == (*rhs).into()
    }
}

impl BoundedIntDebug<const MIN: felt252, const MAX: felt252> =
    crate::fmt::into_felt252_based::DebugImpl<BoundedInt<MIN, MAX>>;

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
    lhs: Lhs, rhs: Rhs,
) -> H::Result nopanic;

/// A helper trait for subtracting two `BoundedInt` instances.
pub trait SubHelper<Lhs, Rhs> {
    type Result;
}
extern fn bounded_int_sub<Lhs, Rhs, impl H: SubHelper<Lhs, Rhs>>(
    lhs: Lhs, rhs: Rhs,
) -> H::Result nopanic;

/// A helper trait for multiplying two `BoundedInt` instances.
pub trait MulHelper<Lhs, Rhs> {
    type Result;
}
impl NonZeroMulHelper<
    Lhs, Rhs, impl H: MulHelper<Lhs, Rhs>,
> of MulHelper<NonZero<Lhs>, NonZero<Rhs>> {
    type Result = NonZero<H::Result>;
}
extern fn bounded_int_mul<Lhs, Rhs, impl H: MulHelper<Lhs, Rhs>>(
    lhs: Lhs, rhs: Rhs,
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
    T, const BOUNDARY: felt252, impl H: ConstrainHelper<T, BOUNDARY>,
> of ConstrainHelper<NonZero<T>, BOUNDARY> {
    type LowT = NonZero<H::LowT>;
    type HighT = NonZero<H::HighT>;
}
mod constrain0 {
    pub impl Impl<T, const MIN: felt252, const MAX: felt252> of super::ConstrainHelper<T, 0> {
        type LowT = super::BoundedInt<MIN, -1>;
        type HighT = super::BoundedInt<0, MAX>;
    }
}
impl I8Constrain0 = constrain0::Impl<i8, -0x80, 0x7f>;
impl I16Constrain0 = constrain0::Impl<i16, -0x8000, 0x7fff>;
impl I32Constrain0 = constrain0::Impl<i32, -0x80000000, 0x7fffffff>;
impl I64Constrain0 = constrain0::Impl<i64, -0x8000000000000000, 0x7fffffffffffffff>;
impl I128Constrain0 =
    constrain0::Impl<i128, -0x80000000000000000000000000000000, 0x7fffffffffffffffffffffffffffffff>;

extern fn bounded_int_constrain<T, const BOUNDARY: felt252, impl H: ConstrainHelper<T, BOUNDARY>>(
    value: T,
) -> Result<H::LowT, H::HighT> implicits(RangeCheck) nopanic;

/// A helper trait for trimming a `BoundedInt` instance min value.
pub trait TrimMinHelper<T> {
    type Target;
}
/// A helper trait for trimming a `BoundedInt` instance max value.
pub trait TrimMaxHelper<T> {
    type Target;
}
mod trim_impl {
    pub impl Min<T, const MIN: felt252, const MAX: felt252> of super::TrimMinHelper<T> {
        type Target = super::BoundedInt<MIN, MAX>;
    }
    pub impl Max<T, const MIN: felt252, const MAX: felt252> of super::TrimMaxHelper<T> {
        type Target = super::BoundedInt<MIN, MAX>;
    }
}
impl U8TrimBelow = trim_impl::Min<u8, 1, 0xff>;
impl U8TrimAbove = trim_impl::Max<u8, 0, 0xfe>;
impl I8TrimBelow = trim_impl::Min<i8, -0x7f, 0x7f>;
impl I8TrimAbove = trim_impl::Max<i8, -0x80, 0x7e>;
impl U16TrimBelow = trim_impl::Min<u16, 1, 0xffff>;
impl U16TrimAbove = trim_impl::Max<u16, 0, 0xfffe>;
impl I16TrimBelow = trim_impl::Min<i16, -0x7fff, 0x7fff>;
impl I16TrimAbove = trim_impl::Max<i16, -0x8000, 0x7ffe>;
impl U32TrimBelow = trim_impl::Min<u32, 1, 0xffffffff>;
impl U32TrimAbove = trim_impl::Max<u32, 0, 0xfffffffe>;
impl I32TrimBelow = trim_impl::Min<i32, -0x7fffffff, 0x7fffffff>;
impl I32TrimAbove = trim_impl::Max<i32, -0x80000000, 0x7ffffffe>;
impl U64TrimBelow = trim_impl::Min<u64, 1, 0xffffffffffffffff>;
impl U64TrimAbove = trim_impl::Max<u64, 0, 0xfffffffffffffffe>;
impl I64TrimBelow = trim_impl::Min<i64, -0x7fffffffffffffff, 0x7fffffffffffffff>;
impl I64TrimAbove = trim_impl::Max<i64, -0x8000000000000000, 0x7ffffffffffffffe>;
impl U128TrimBelow = trim_impl::Min<u128, 1, 0xffffffffffffffffffffffffffffffff>;
impl U128TrimAbove = trim_impl::Max<u128, 0, 0xfffffffffffffffffffffffffffffffe>;
impl I128TrimBelow =
    trim_impl::Min<i128, -0x7fffffffffffffffffffffffffffffff, 0x7fffffffffffffffffffffffffffffff>;
impl I128TrimAbove =
    trim_impl::Max<i128, -0x80000000000000000000000000000000, 0x7ffffffffffffffffffffffffffffffe>;

extern fn bounded_int_trim_min<T, impl H: TrimMinHelper<T>>(
    value: T,
) -> core::internal::OptionRev<H::Target> nopanic;
extern fn bounded_int_trim_max<T, impl H: TrimMaxHelper<T>>(
    value: T,
) -> core::internal::OptionRev<H::Target> nopanic;

extern fn bounded_int_is_zero<T>(value: T) -> crate::zeroable::IsZeroResult<T> implicits() nopanic;

/// Returns the negation of the given `felt252` value.
trait NegFelt252<const NUM: felt252> {
    /// The negation of the given `felt252` value.
    const VALUE: felt252;
}

/// Helper implementation for `NegFelt252`.
mod neg_felt252 {
    pub impl Impl<const INPUT: felt252, const OUTPUT: felt252> of super::NegFelt252<INPUT> {
        const VALUE: felt252 = OUTPUT;
    }
}
impl NegFelt2520 = neg_felt252::Impl<0, 0>;
impl NegFelt2521 = neg_felt252::Impl<1, -1>;
impl NegFelt252Minus1 = neg_felt252::Impl<-1, 1>;
impl NegFelt2520x7e = neg_felt252::Impl<0x7e, -0x7e>;
impl NegFelt252Minus0x7e = neg_felt252::Impl<-0x7e, 0x7e>;
impl NegFelt2520x7f = neg_felt252::Impl<0x7f, -0x7f>;
impl NegFelt252Minus0x7f = neg_felt252::Impl<-0x7f, 0x7f>;
impl NegFelt2520x80 = neg_felt252::Impl<0x80, -0x80>;
impl NegFelt252Minus0x80 = neg_felt252::Impl<-0x80, 0x80>;
impl NegFelt2520x7ffe = neg_felt252::Impl<0x7ffe, -0x7ffe>;
impl NegFelt252Minus0x7ffe = neg_felt252::Impl<-0x7ffe, 0x7ffe>;
impl NegFelt2520x7fff = neg_felt252::Impl<0x7fff, -0x7fff>;
impl NegFelt252Minus0x7fff = neg_felt252::Impl<-0x7fff, 0x7fff>;
impl NegFelt2520x8000 = neg_felt252::Impl<0x8000, -0x8000>;
impl NegFelt252Minus0x8000 = neg_felt252::Impl<-0x8000, 0x8000>;
impl NegFelt2520x7ffffffe = neg_felt252::Impl<0x7ffffffe, -0x7ffffffe>;
impl NegFelt252Minus0x7ffffffe = neg_felt252::Impl<-0x7ffffffe, 0x7ffffffe>;
impl NegFelt2520x7fffffff = neg_felt252::Impl<0x7fffffff, -0x7fffffff>;
impl NegFelt252Minus0x7fffffff = neg_felt252::Impl<-0x7fffffff, 0x7fffffff>;
impl NegFelt2520x80000000 = neg_felt252::Impl<0x80000000, -0x80000000>;
impl NegFelt252Minus0x80000000 = neg_felt252::Impl<-0x80000000, 0x80000000>;
impl NegFelt2520x7ffffffffffffffe = neg_felt252::Impl<0x7ffffffffffffffe, -0x7ffffffffffffffe>;
impl NegFelt252Minus0x7ffffffffffffffe = neg_felt252::Impl<-0x7ffffffffffffffe, 0x7ffffffffffffffe>;
impl NegFelt2520x7fffffffffffffff = neg_felt252::Impl<0x7fffffffffffffff, -0x7fffffffffffffff>;
impl NegFelt252Minus0x7fffffffffffffff = neg_felt252::Impl<-0x7fffffffffffffff, 0x7fffffffffffffff>;
impl NegFelt2520x8000000000000000 = neg_felt252::Impl<0x8000000000000000, -0x8000000000000000>;
impl NegFelt252Minus0x8000000000000000 = neg_felt252::Impl<-0x8000000000000000, 0x8000000000000000>;
impl NegFelt2520x7ffffffffffffffffffffffffffffffe =
    neg_felt252::Impl<0x7ffffffffffffffffffffffffffffffe, -0x7ffffffffffffffffffffffffffffffe>;
impl NegFelt252Minus0x7ffffffffffffffffffffffffffffffe =
    neg_felt252::Impl<-0x7ffffffffffffffffffffffffffffffe, 0x7ffffffffffffffffffffffffffffffe>;
impl NegFelt2520x7fffffffffffffffffffffffffffffff =
    neg_felt252::Impl<0x7fffffffffffffffffffffffffffffff, -0x7fffffffffffffffffffffffffffffff>;
impl NegFelt252Minus0x7fffffffffffffffffffffffffffffff =
    neg_felt252::Impl<-0x7fffffffffffffffffffffffffffffff, 0x7fffffffffffffffffffffffffffffff>;
impl NegFelt2520x80000000000000000000000000000000 =
    neg_felt252::Impl<0x80000000000000000000000000000000, -0x80000000000000000000000000000000>;
impl NegFelt252Minus0x80000000000000000000000000000000 =
    neg_felt252::Impl<-0x80000000000000000000000000000000, 0x80000000000000000000000000000000>;

pub type UnitInt<const VALUE: felt252> = BoundedInt<VALUE, VALUE>;

impl MulMinus1<
    const MIN: felt252,
    const MAX: felt252,
    impl NegMin: NegFelt252<MIN>,
    impl NegMax: NegFelt252<MAX>,
> of MulHelper<BoundedInt<MIN, MAX>, UnitInt<-1>> {
    type Result = BoundedInt<NegMax::VALUE, NegMin::VALUE>;
}

/// A helper trait for negating a `BoundedInt` instance.
pub trait NegateHelper<T> {
    /// The result of negating the given value.
    type Result;

    /// Negates the given value.
    fn negate(self: T) -> Self::Result;
}

impl MulMinusOneNegateHelper<T, impl H: MulHelper<T, UnitInt<-1>>> of NegateHelper<T> {
    type Result = H::Result;

    fn negate(self: T) -> H::Result {
        bounded_int_mul::<_, UnitInt<-1>>(self, -1)
    }
}

impl NonZeroMulMinusOneNegateHelper<
    T, impl H: MulHelper<T, UnitInt<-1>>,
> of NegateHelper<NonZero<T>> {
    type Result = NonZero<H::Result>;

    fn negate(self: NonZero<T>) -> NonZero<H::Result> {
        bounded_int_mul::<_, NonZero<UnitInt<-1>>>(self, -1)
    }
}

pub use {
    bounded_int_add as add, bounded_int_constrain as constrain, bounded_int_div_rem as div_rem,
    bounded_int_is_zero as is_zero, bounded_int_mul as mul, bounded_int_sub as sub,
    bounded_int_trim_max as trim_max, bounded_int_trim_min as trim_min,
};
