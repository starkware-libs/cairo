use crate::integer::IsZeroResult;
#[feature("bounded-int-utils")]
use crate::internal::bounded_int::{
    BoundedInt, ConstrainHelper, DivRemHelper, MulHelper, NegateHelper, UnitInt, constrain, div_rem,
    downcast, is_zero, upcast,
};

impl DivRemImpl<
    T,
    impl CH: ConstrainHelper<T, 0>,
    impl NH: MulHelper<CH::LowT, UnitInt<-1>>,
    // Positive by Positive Div Rem (PPDR) Helper.
    impl PPDR: DivRemHelper<CH::HighT, CH::HighT>,
    // Negative by Positive Div Rem (NPDR) Helper.
    impl NPDR: DivRemHelper<NH::Result, CH::HighT>,
    // Positive by Negative Div Rem (PNDR) Helper.
    impl PNDR: DivRemHelper<CH::HighT, NH::Result>,
    // Negative by Negative Div Rem (NNDR) Helper.
    impl NNDR: DivRemHelper<NH::Result, NH::Result>,
    +MulHelper<NNDR::RemT, UnitInt<-1>>,
    +MulHelper<NPDR::DivT, UnitInt<-1>>,
    +MulHelper<NPDR::RemT, UnitInt<-1>>,
    +MulHelper<PNDR::DivT, UnitInt<-1>>,
    +Drop<T>,
    +Drop<NH::Result>,
    +Drop<CH::LowT>,
    +Drop<CH::HighT>,
    +Drop<PNDR::RemT>,
    +Drop<NPDR::RemT>,
    +Drop<NNDR::RemT>,
> of DivRem<T> {
    fn div_rem(lhs: T, rhs: NonZero<T>) -> (T, T) {
        match constrain::<T, 0>(lhs) {
            Ok(lhs_lt0) => {
                match constrain::<NonZero<T>, 0>(rhs) {
                    Ok(rhs_lt0) => {
                        let (q, r) = div_rem(lhs_lt0.negate(), rhs_lt0.negate());
                        ( // Catching the case for division of `i{8,16,32,64,128}::MIN` by
                            // `-1`, which overflows.
                            downcast(q).expect('attempt to divide with overflow'),
                            upcast(r.negate()),
                        )
                    },
                    Err(rhs_ge0) => {
                        let (q, r) = div_rem(lhs_lt0.negate(), rhs_ge0);
                        (upcast(q.negate()), upcast(r.negate()))
                    },
                }
            },
            Err(lhs_ge0) => {
                match constrain::<NonZero<T>, 0>(rhs) {
                    Ok(rhs_lt0) => {
                        let (q, r) = div_rem(lhs_ge0, rhs_lt0.negate());
                        (upcast(q.negate()), upcast(r))
                    },
                    Err(rhs_ge0) => {
                        let (q, r) = div_rem(lhs_ge0, rhs_ge0);
                        (upcast(q), upcast(r))
                    },
                }
            },
        }
    }
}

mod impls {
    pub impl DivRem<Lhs, Rhs, DivT, RemT> of super::DivRemHelper<Lhs, Rhs> {
        type DivT = DivT;
        type RemT = RemT;
    }
}

type i8_neg = ConstrainHelper::<i8>::LowT;
type i8_pos = ConstrainHelper::<i8>::HighT;
type minus_i8_neg = NegateHelper::<i8_neg>::Result;

impl I8PPDR = impls::DivRem<i8_pos, i8_pos, i8_pos, BoundedInt<0, 0x7e>>;
impl I8NPDR = impls::DivRem<minus_i8_neg, i8_pos, BoundedInt<0, 0x80>, BoundedInt<0, 0x7e>>;
impl I8PNDR = impls::DivRem<i8_pos, minus_i8_neg, i8_pos, i8_pos>;
impl I8NNDR = impls::DivRem<minus_i8_neg, minus_i8_neg, BoundedInt<0, 0x80>, i8_pos>;
pub impl I8DivRem = DivRemImpl<i8>;

type i16_neg = ConstrainHelper::<i16>::LowT;
type i16_pos = ConstrainHelper::<i16>::HighT;
type minus_i16_neg = NegateHelper::<i16_neg>::Result;

impl I16PPDR = impls::DivRem<i16_pos, i16_pos, i16_pos, BoundedInt<0, 0x7ffe>>;
impl I16NPDR = impls::DivRem<minus_i16_neg, i16_pos, BoundedInt<0, 0x8000>, BoundedInt<0, 0x7ffe>>;
impl I16PNDR = impls::DivRem<i16_pos, minus_i16_neg, i16_pos, i16_pos>;
impl I16NNDR = impls::DivRem<minus_i16_neg, minus_i16_neg, BoundedInt<0, 0x8000>, i16_pos>;
pub impl I16DivRem = DivRemImpl<i16>;

type i32_neg = ConstrainHelper::<i32>::LowT;
type i32_pos = ConstrainHelper::<i32>::HighT;
type minus_i32_neg = NegateHelper::<i32_neg>::Result;

impl I32PPDR = impls::DivRem<i32_pos, i32_pos, i32_pos, BoundedInt<0, 0x7ffffffe>>;
impl I32NPDR =
    impls::DivRem<minus_i32_neg, i32_pos, BoundedInt<0, 0x80000000>, BoundedInt<0, 0x7ffffffe>>;
impl I32PNDR = impls::DivRem<i32_pos, minus_i32_neg, i32_pos, i32_pos>;
impl I32NNDR = impls::DivRem<minus_i32_neg, minus_i32_neg, BoundedInt<0, 0x80000000>, i32_pos>;
pub impl I32DivRem = DivRemImpl<i32>;

type i64_neg = ConstrainHelper::<i64>::LowT;
type i64_pos = ConstrainHelper::<i64>::HighT;
type minus_i64_neg = NegateHelper::<i64_neg>::Result;

impl I64PPDR = impls::DivRem<i64_pos, i64_pos, i64_pos, BoundedInt<0, 0x7ffffffffffffffe>>;
impl I64NPDR =
    impls::DivRem<
        minus_i64_neg,
        i64_pos,
        BoundedInt<0, 0x8000000000000000>,
        BoundedInt<0, 0x7ffffffffffffffe>,
    >;
impl I64PNDR = impls::DivRem<i64_pos, minus_i64_neg, i64_pos, i64_pos>;
impl I64NNDR =
    impls::DivRem<minus_i64_neg, minus_i64_neg, BoundedInt<0, 0x8000000000000000>, i64_pos>;
pub impl I64DivRem = DivRemImpl<i64>;

type i128_neg = ConstrainHelper::<i128>::LowT;
type i128_pos = ConstrainHelper::<i128>::HighT;
type minus_i128_neg = NegateHelper::<i128_neg>::Result;

impl I128PPDR =
    impls::DivRem<i128_pos, i128_pos, i128_pos, BoundedInt<0, 0x7ffffffffffffffffffffffffffffffe>>;
impl I128NPDR =
    impls::DivRem<
        minus_i128_neg,
        i128_pos,
        BoundedInt<0, 0x80000000000000000000000000000000>,
        BoundedInt<0, 0x7ffffffffffffffffffffffffffffffe>,
    >;
impl I128PNDR = impls::DivRem<i128_pos, minus_i128_neg, i128_pos, i128_pos>;
impl I128NNDR =
    impls::DivRem<
        minus_i128_neg, minus_i128_neg, BoundedInt<0, 0x80000000000000000000000000000000>, i128_pos,
    >;
pub impl I128DivRem = DivRemImpl<i128>;

pub impl TryIntoNonZero<T> of TryInto<T, NonZero<T>> {
    fn try_into(self: T) -> Option<NonZero<T>> {
        match is_zero(self) {
            IsZeroResult::Zero => None,
            IsZeroResult::NonZero(x) => Some(x),
        }
    }
}
