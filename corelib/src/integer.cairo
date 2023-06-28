use option::OptionTrait;
use result::ResultTrait;
use traits::{Into, TryInto, Default, Felt252DictValue};
use zeroable::{IsZeroResult, NonZeroIntoImpl, Zeroable};
use serde::Serde;
use array::ArrayTrait;
use array::SpanTrait;

// TODO(spapini): Add method for const creation from Integer.
trait NumericLiteral<T>;
impl NumericLiteralfelt252 of NumericLiteral<felt252>;

#[derive(Copy, Drop)]
extern type u128;
impl NumericLiteralu128 of NumericLiteral<u128>;
extern fn u128_const<value>() -> u128 nopanic;

impl U128Serde of Serde<u128> {
    fn serialize(self: @u128, ref output: Array<felt252>) {
        Into::<u128, felt252>::into(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u128> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

enum U128sFromFelt252Result {
    Narrow: u128,
    Wide: (u128, u128),
}
extern fn u128s_from_felt252(a: felt252) -> U128sFromFelt252Result implicits(RangeCheck) nopanic;

#[panic_with('u128_from Overflow', u128_from_felt252)]
fn u128_try_from_felt252(a: felt252) -> Option<u128> implicits(RangeCheck) nopanic {
    match u128s_from_felt252(a) {
        U128sFromFelt252Result::Narrow(x) => Option::Some(x),
        U128sFromFelt252Result::Wide(x) => Option::None,
    }
}

extern fn u128_to_felt252(a: u128) -> felt252 nopanic;

extern fn u128_overflowing_add(
    lhs: u128, rhs: u128
) -> Result<u128, u128> implicits(RangeCheck) nopanic;
extern fn u128_overflowing_sub(
    lhs: u128, rhs: u128
) -> Result<u128, u128> implicits(RangeCheck) nopanic;

fn u128_wrapping_add(lhs: u128, rhs: u128) -> u128 implicits(RangeCheck) nopanic {
    match u128_overflowing_add(lhs, rhs) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u128_wrapping_sub(a: u128, b: u128) -> u128 implicits(RangeCheck) nopanic {
    match u128_overflowing_sub(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

/// A type that contains 4 u128s (a, b, c, d) and guarantees that `a * b = 2**128 * c + d`.
///
/// The guarantee is verified by `u128_mul_guarantee_verify`, which is the only way to destruct this
/// type. This way, one can trust that the guarantee holds although it has not yet been verified.
extern type U128MulGuarantee;

/// Multiplies two u128s and returns a `U128MulGuarantee` for the result of `a * b`.
extern fn u128_guarantee_mul(a: u128, b: u128) -> (u128, u128, U128MulGuarantee) nopanic;

/// Verifies the guarantee and returns the result of `a * b`.
extern fn u128_mul_guarantee_verify(guarantee: U128MulGuarantee) implicits(RangeCheck) nopanic;

/// Multiplies two u128s and returns `(high, low)` - the 128-bit parts of the result.
#[inline(always)]
fn u128_wide_mul(a: u128, b: u128) -> (u128, u128) nopanic {
    let (high, low, _) = u128_guarantee_mul(a, b);
    (high, low)
}

impl U128MulGuaranteeDestruct of Destruct<U128MulGuarantee> {
    fn destruct(self: U128MulGuarantee) nopanic {
        u128_mul_guarantee_verify(self);
    }
}

extern fn u128_sqrt(value: u128) -> u64 implicits(RangeCheck) nopanic;

fn u128_overflowing_mul(lhs: u128, rhs: u128) -> (u128, bool) implicits(RangeCheck) nopanic {
    let (top_word, bottom_word) = u128_wide_mul(lhs, rhs);
    match u128_to_felt252(top_word) {
        0 => (bottom_word, false),
        _ => (bottom_word, true),
    }
}


fn u128_checked_add(lhs: u128, rhs: u128) -> Option<u128> implicits(RangeCheck) nopanic {
    match u128_overflowing_add(lhs, rhs) {
        Result::Ok(r) => Option::Some(r),
        Result::Err(r) => Option::None,
    }
}

impl U128Add of Add<u128> {
    fn add(lhs: u128, rhs: u128) -> u128 {
        u128_overflowing_add(lhs, rhs).expect('u128_add Overflow')
    }
}
impl U128AddEq of AddEq<u128> {
    #[inline(always)]
    fn add_eq(ref self: u128, other: u128) {
        self = Add::add(self, other);
    }
}

#[panic_with('u128_sub Overflow', u128_sub)]
fn u128_checked_sub(lhs: u128, rhs: u128) -> Option<u128> implicits(RangeCheck) nopanic {
    match u128_overflowing_sub(lhs, rhs) {
        Result::Ok(r) => Option::Some(r),
        Result::Err(r) => Option::None,
    }
}

impl U128Sub of Sub<u128> {
    fn sub(lhs: u128, rhs: u128) -> u128 {
        u128_overflowing_sub(lhs, rhs).expect('u128_sub Overflow')
    }
}
impl U128SubEq of SubEq<u128> {
    #[inline(always)]
    fn sub_eq(ref self: u128, other: u128) {
        self = Sub::sub(self, other);
    }
}

fn u128_checked_mul(lhs: u128, rhs: u128) -> Option<u128> implicits(RangeCheck) nopanic {
    let (top_word, bottom_word) = u128_wide_mul(lhs, rhs);
    match u128_to_felt252(top_word) {
        0 => Option::Some(bottom_word),
        _ => Option::None,
    }
}

impl U128Mul of Mul<u128> {
    fn mul(lhs: u128, rhs: u128) -> u128 {
        u128_checked_mul(lhs, rhs).expect('u128_mul Overflow')
    }
}
impl U128MulEq of MulEq<u128> {
    #[inline(always)]
    fn mul_eq(ref self: u128, other: u128) {
        self = Mul::mul(self, other);
    }
}

#[panic_with('u128 is 0', u128_as_non_zero)]
fn u128_try_as_non_zero(a: u128) -> Option<NonZero<u128>> nopanic {
    match u128_is_zero(a) {
        IsZeroResult::Zero => Option::None,
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U128TryIntoNonZero of TryInto<u128, NonZero<u128>> {
    fn try_into(self: u128) -> Option<NonZero<u128>> {
        Option::Some(u128_as_non_zero(self))
    }
}

impl U128Div of Div<u128> {
    fn div(lhs: u128, rhs: u128) -> u128 {
        let (q, r) = u128_safe_divmod(lhs, rhs.try_into().expect('Division by 0'));
        q
    }
}
impl U128DivEq of DivEq<u128> {
    #[inline(always)]
    fn div_eq(ref self: u128, other: u128) {
        self = Div::div(self, other);
    }
}

impl U128Rem of Rem<u128> {
    fn rem(lhs: u128, rhs: u128) -> u128 {
        let (q, r) = u128_safe_divmod(lhs, rhs.try_into().expect('Division by 0'));
        r
    }
}
impl U128RemEq of RemEq<u128> {
    #[inline(always)]
    fn rem_eq(ref self: u128, other: u128) {
        self = Rem::rem(self, other);
    }
}

impl U128DivRem of DivRem<u128> {
    fn div_rem(lhs: u128, rhs: NonZero<u128>) -> (u128, u128) {
        u128_safe_divmod(lhs, rhs)
    }
}

extern fn u128_safe_divmod(
    lhs: u128, rhs: NonZero<u128>
) -> (u128, u128) implicits(RangeCheck) nopanic;

extern fn u128_eq(lhs: u128, rhs: u128) -> bool implicits() nopanic;

impl U128PartialEq of PartialEq<u128> {
    #[inline(always)]
    fn eq(lhs: @u128, rhs: @u128) -> bool {
        u128_eq(*lhs, *rhs)
    }
    #[inline(always)]
    fn ne(lhs: @u128, rhs: @u128) -> bool {
        !(*lhs == *rhs)
    }
}

impl U128PartialOrd of PartialOrd<u128> {
    #[inline(always)]
    fn le(lhs: u128, rhs: u128) -> bool {
        u128_overflowing_sub(rhs, lhs).into_is_ok()
    }
    #[inline(always)]
    fn ge(lhs: u128, rhs: u128) -> bool {
        u128_overflowing_sub(lhs, rhs).into_is_ok()
    }
    #[inline(always)]
    fn lt(lhs: u128, rhs: u128) -> bool {
        u128_overflowing_sub(lhs, rhs).into_is_err()
    }
    #[inline(always)]
    fn gt(lhs: u128, rhs: u128) -> bool {
        u128_overflowing_sub(rhs, lhs).into_is_err()
    }
}

extern type Bitwise;
extern fn bitwise(lhs: u128, rhs: u128) -> (u128, u128, u128) implicits(Bitwise) nopanic;
impl U128BitAnd of BitAnd<u128> {
    #[inline(always)]
    fn bitand(lhs: u128, rhs: u128) -> u128 {
        let (v, _, _) = bitwise(lhs, rhs);
        v
    }
}
impl U128BitXor of BitXor<u128> {
    #[inline(always)]
    fn bitxor(lhs: u128, rhs: u128) -> u128 {
        let (_, v, _) = bitwise(lhs, rhs);
        v
    }
}
impl U128BitOr of BitOr<u128> {
    #[inline(always)]
    fn bitor(lhs: u128, rhs: u128) -> u128 {
        let (_, _, v) = bitwise(lhs, rhs);
        v
    }
}
impl U128BitNot of BitNot<u128> {
    fn bitnot(a: u128) -> u128 {
        BoundedInt::max() - a
    }
}

extern fn u128_is_zero(a: u128) -> IsZeroResult<u128> implicits() nopanic;

extern fn u128_byte_reverse(input: u128) -> u128 implicits(Bitwise) nopanic;

#[derive(Copy, Drop)]
extern type u8;
impl NumericLiteralu8 of NumericLiteral<u8>;
extern fn u8_const<value>() -> u8 nopanic;
extern fn u8_to_felt252(a: u8) -> felt252 nopanic;

#[panic_with('u8_from Overflow', u8_from_felt252)]
extern fn u8_try_from_felt252(a: felt252) -> Option<u8> implicits(RangeCheck) nopanic;

extern fn u8_eq(lhs: u8, rhs: u8) -> bool implicits() nopanic;

impl U8Serde of Serde<u8> {
    fn serialize(self: @u8, ref output: Array<felt252>) {
        Into::<u8, felt252>::into(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u8> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U8PartialEq of PartialEq<u8> {
    #[inline(always)]
    fn eq(lhs: @u8, rhs: @u8) -> bool {
        u8_eq(*lhs, *rhs)
    }
    #[inline(always)]
    fn ne(lhs: @u8, rhs: @u8) -> bool {
        !(*lhs == *rhs)
    }
}

impl U8PartialOrd of PartialOrd<u8> {
    #[inline(always)]
    fn le(lhs: u8, rhs: u8) -> bool {
        u8_overflowing_sub(rhs, lhs).into_is_ok()
    }
    #[inline(always)]
    fn ge(lhs: u8, rhs: u8) -> bool {
        u8_overflowing_sub(lhs, rhs).into_is_ok()
    }
    #[inline(always)]
    fn lt(lhs: u8, rhs: u8) -> bool {
        u8_overflowing_sub(lhs, rhs).into_is_err()
    }
    #[inline(always)]
    fn gt(lhs: u8, rhs: u8) -> bool {
        u8_overflowing_sub(rhs, lhs).into_is_err()
    }
}

extern fn u8_overflowing_add(lhs: u8, rhs: u8) -> Result<u8, u8> implicits(RangeCheck) nopanic;
extern fn u8_overflowing_sub(lhs: u8, rhs: u8) -> Result<u8, u8> implicits(RangeCheck) nopanic;

fn u8_wrapping_add(lhs: u8, rhs: u8) -> u8 implicits(RangeCheck) nopanic {
    match u8_overflowing_add(lhs, rhs) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u8_wrapping_sub(lhs: u8, rhs: u8) -> u8 implicits(RangeCheck) nopanic {
    match u8_overflowing_sub(lhs, rhs) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u8_checked_add(lhs: u8, rhs: u8) -> Option<u8> implicits(RangeCheck) nopanic {
    match u8_overflowing_add(lhs, rhs) {
        Result::Ok(r) => Option::Some(r),
        Result::Err(r) => Option::None,
    }
}

impl U8Add of Add<u8> {
    fn add(lhs: u8, rhs: u8) -> u8 {
        u8_overflowing_add(lhs, rhs).expect('u8_add Overflow')
    }
}
impl U8AddEq of AddEq<u8> {
    #[inline(always)]
    fn add_eq(ref self: u8, other: u8) {
        self = Add::add(self, other);
    }
}

fn u8_checked_sub(lhs: u8, rhs: u8) -> Option<u8> implicits(RangeCheck) nopanic {
    match u8_overflowing_sub(lhs, rhs) {
        Result::Ok(r) => Option::Some(r),
        Result::Err(r) => Option::None,
    }
}

impl U8Sub of Sub<u8> {
    fn sub(lhs: u8, rhs: u8) -> u8 {
        u8_overflowing_sub(lhs, rhs).expect('u8_sub Overflow')
    }
}
impl U8SubEq of SubEq<u8> {
    #[inline(always)]
    fn sub_eq(ref self: u8, other: u8) {
        self = Sub::sub(self, other);
    }
}

extern fn u8_wide_mul(lhs: u8, rhs: u8) -> u16 implicits() nopanic;
extern fn u8_sqrt(value: u8) -> u8 implicits(RangeCheck) nopanic;

impl U8Mul of Mul<u8> {
    fn mul(lhs: u8, rhs: u8) -> u8 {
        u8_wide_mul(lhs, rhs).try_into().expect('u8_mul Overflow')
    }
}
impl U8MulEq of MulEq<u8> {
    #[inline(always)]
    fn mul_eq(ref self: u8, other: u8) {
        self = Mul::mul(self, other);
    }
}

extern fn u8_is_zero(a: u8) -> IsZeroResult<u8> implicits() nopanic;
extern fn u8_safe_divmod(lhs: u8, rhs: NonZero<u8>) -> (u8, u8) implicits(RangeCheck) nopanic;

#[panic_with('u8 is 0', u8_as_non_zero)]
fn u8_try_as_non_zero(a: u8) -> Option<NonZero<u8>> nopanic {
    match u8_is_zero(a) {
        IsZeroResult::Zero => Option::None,
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U8TryIntoNonZero of TryInto<u8, NonZero<u8>> {
    fn try_into(self: u8) -> Option<NonZero<u8>> {
        Option::Some(u8_as_non_zero(self))
    }
}

impl U8Div of Div<u8> {
    fn div(lhs: u8, rhs: u8) -> u8 {
        let (q, r) = u8_safe_divmod(lhs, rhs.try_into().expect('Division by 0'));
        q
    }
}
impl U8DivEq of DivEq<u8> {
    #[inline(always)]
    fn div_eq(ref self: u8, other: u8) {
        self = Div::div(self, other);
    }
}

impl U8Rem of Rem<u8> {
    fn rem(lhs: u8, rhs: u8) -> u8 {
        let (q, r) = u8_safe_divmod(lhs, rhs.try_into().expect('Division by 0'));
        r
    }
}
impl U8RemEq of RemEq<u8> {
    #[inline(always)]
    fn rem_eq(ref self: u8, other: u8) {
        self = Rem::rem(self, other);
    }
}

impl U8DivRem of DivRem<u8> {
    fn div_rem(lhs: u8, rhs: NonZero<u8>) -> (u8, u8) {
        u8_safe_divmod(lhs, rhs)
    }
}

impl U8BitNot of BitNot<u8> {
    fn bitnot(a: u8) -> u8 {
        BoundedInt::max() - a
    }
}
extern fn u8_bitwise(lhs: u8, rhs: u8) -> (u8, u8, u8) implicits(Bitwise) nopanic;
impl U8BitAnd of BitAnd<u8> {
    #[inline(always)]
    fn bitand(lhs: u8, rhs: u8) -> u8 {
        let (v, _, _) = u8_bitwise(lhs, rhs);
        v
    }
}
impl U8BitXor of BitXor<u8> {
    #[inline(always)]
    fn bitxor(lhs: u8, rhs: u8) -> u8 {
        let (_, v, _) = u8_bitwise(lhs, rhs);
        v
    }
}
impl U8BitOr of BitOr<u8> {
    #[inline(always)]
    fn bitor(lhs: u8, rhs: u8) -> u8 {
        let (_, _, v) = u8_bitwise(lhs, rhs);
        v
    }
}

#[derive(Copy, Drop)]
extern type u16;
impl NumericLiteralu16 of NumericLiteral<u16>;
extern fn u16_const<value>() -> u16 nopanic;
extern fn u16_to_felt252(a: u16) -> felt252 nopanic;

#[panic_with('u16_from Overflow', u16_from_felt252)]
extern fn u16_try_from_felt252(a: felt252) -> Option<u16> implicits(RangeCheck) nopanic;

extern fn u16_eq(lhs: u16, rhs: u16) -> bool implicits() nopanic;

impl U16Serde of Serde<u16> {
    fn serialize(self: @u16, ref output: Array<felt252>) {
        Into::<u16, felt252>::into(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u16> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U16PartialEq of PartialEq<u16> {
    #[inline(always)]
    fn eq(lhs: @u16, rhs: @u16) -> bool {
        u16_eq(*lhs, *rhs)
    }
    #[inline(always)]
    fn ne(lhs: @u16, rhs: @u16) -> bool {
        !(*lhs == *rhs)
    }
}

impl U16PartialOrd of PartialOrd<u16> {
    #[inline(always)]
    fn le(lhs: u16, rhs: u16) -> bool {
        u16_overflowing_sub(rhs, lhs).into_is_ok()
    }
    #[inline(always)]
    fn ge(lhs: u16, rhs: u16) -> bool {
        u16_overflowing_sub(lhs, rhs).into_is_ok()
    }
    #[inline(always)]
    fn lt(lhs: u16, rhs: u16) -> bool {
        u16_overflowing_sub(lhs, rhs).into_is_err()
    }
    #[inline(always)]
    fn gt(lhs: u16, rhs: u16) -> bool {
        u16_overflowing_sub(rhs, lhs).into_is_err()
    }
}

extern fn u16_overflowing_add(lhs: u16, rhs: u16) -> Result<u16, u16> implicits(RangeCheck) nopanic;
extern fn u16_overflowing_sub(lhs: u16, rhs: u16) -> Result<u16, u16> implicits(RangeCheck) nopanic;

fn u16_wrapping_add(lhs: u16, rhs: u16) -> u16 implicits(RangeCheck) nopanic {
    match u16_overflowing_add(lhs, rhs) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u16_wrapping_sub(lhs: u16, rhs: u16) -> u16 implicits(RangeCheck) nopanic {
    match u16_overflowing_sub(lhs, rhs) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u16_checked_add(lhs: u16, rhs: u16) -> Option<u16> implicits(RangeCheck) nopanic {
    match u16_overflowing_add(lhs, rhs) {
        Result::Ok(r) => Option::Some(r),
        Result::Err(r) => Option::None,
    }
}

impl U16Add of Add<u16> {
    fn add(lhs: u16, rhs: u16) -> u16 {
        u16_overflowing_add(lhs, rhs).expect('u16_add Overflow')
    }
}
impl U16AddEq of AddEq<u16> {
    #[inline(always)]
    fn add_eq(ref self: u16, other: u16) {
        self = Add::add(self, other);
    }
}

fn u16_checked_sub(lhs: u16, rhs: u16) -> Option<u16> implicits(RangeCheck) nopanic {
    match u16_overflowing_sub(lhs, rhs) {
        Result::Ok(r) => Option::Some(r),
        Result::Err(r) => Option::None,
    }
}

impl U16Sub of Sub<u16> {
    fn sub(lhs: u16, rhs: u16) -> u16 {
        u16_overflowing_sub(lhs, rhs).expect('u16_sub Overflow')
    }
}
impl U16SubEq of SubEq<u16> {
    #[inline(always)]
    fn sub_eq(ref self: u16, other: u16) {
        self = Sub::sub(self, other);
    }
}

extern fn u16_wide_mul(lhs: u16, rhs: u16) -> u32 implicits() nopanic;
extern fn u16_sqrt(value: u16) -> u8 implicits(RangeCheck) nopanic;

impl U16Mul of Mul<u16> {
    fn mul(lhs: u16, rhs: u16) -> u16 {
        u16_wide_mul(lhs, rhs).try_into().expect('u16_mul Overflow')
    }
}
impl U16MulEq of MulEq<u16> {
    #[inline(always)]
    fn mul_eq(ref self: u16, other: u16) {
        self = Mul::mul(self, other);
    }
}

extern fn u16_is_zero(a: u16) -> IsZeroResult<u16> implicits() nopanic;
extern fn u16_safe_divmod(lhs: u16, rhs: NonZero<u16>) -> (u16, u16) implicits(RangeCheck) nopanic;

#[panic_with('u16 is 0', u16_as_non_zero)]
fn u16_try_as_non_zero(a: u16) -> Option<NonZero<u16>> nopanic {
    match u16_is_zero(a) {
        IsZeroResult::Zero => Option::None,
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U16TryIntoNonZero of TryInto<u16, NonZero<u16>> {
    fn try_into(self: u16) -> Option<NonZero<u16>> {
        Option::Some(u16_as_non_zero(self))
    }
}

impl U16Div of Div<u16> {
    fn div(lhs: u16, rhs: u16) -> u16 {
        let (q, r) = u16_safe_divmod(lhs, rhs.try_into().expect('Division by 0'));
        q
    }
}
impl U16DivEq of DivEq<u16> {
    #[inline(always)]
    fn div_eq(ref self: u16, other: u16) {
        self = Div::div(self, other);
    }
}

impl U16Rem of Rem<u16> {
    fn rem(lhs: u16, rhs: u16) -> u16 {
        let (q, r) = u16_safe_divmod(lhs, rhs.try_into().expect('Division by 0'));
        r
    }
}
impl U16RemEq of RemEq<u16> {
    #[inline(always)]
    fn rem_eq(ref self: u16, other: u16) {
        self = Rem::rem(self, other);
    }
}

impl U16DivRem of DivRem<u16> {
    fn div_rem(lhs: u16, rhs: NonZero<u16>) -> (u16, u16) {
        u16_safe_divmod(lhs, rhs)
    }
}

impl U16BitNot of BitNot<u16> {
    fn bitnot(a: u16) -> u16 {
        BoundedInt::max() - a
    }
}
extern fn u16_bitwise(lhs: u16, rhs: u16) -> (u16, u16, u16) implicits(Bitwise) nopanic;
impl U16BitAnd of BitAnd<u16> {
    #[inline(always)]
    fn bitand(lhs: u16, rhs: u16) -> u16 {
        let (v, _, _) = u16_bitwise(lhs, rhs);
        v
    }
}
impl U16BitXor of BitXor<u16> {
    #[inline(always)]
    fn bitxor(lhs: u16, rhs: u16) -> u16 {
        let (_, v, _) = u16_bitwise(lhs, rhs);
        v
    }
}
impl U16BitOr of BitOr<u16> {
    #[inline(always)]
    fn bitor(lhs: u16, rhs: u16) -> u16 {
        let (_, _, v) = u16_bitwise(lhs, rhs);
        v
    }
}

#[derive(Copy, Drop)]
extern type u32;
impl NumericLiteralu32 of NumericLiteral<u32>;
extern fn u32_const<value>() -> u32 nopanic;
extern fn u32_to_felt252(a: u32) -> felt252 nopanic;

#[panic_with('u32_from Overflow', u32_from_felt252)]
extern fn u32_try_from_felt252(a: felt252) -> Option<u32> implicits(RangeCheck) nopanic;

extern fn u32_eq(lhs: u32, rhs: u32) -> bool implicits() nopanic;

impl U32Serde of Serde<u32> {
    fn serialize(self: @u32, ref output: Array<felt252>) {
        Into::<u32, felt252>::into(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u32> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U32PartialEq of PartialEq<u32> {
    #[inline(always)]
    fn eq(lhs: @u32, rhs: @u32) -> bool {
        u32_eq(*lhs, *rhs)
    }
    #[inline(always)]
    fn ne(lhs: @u32, rhs: @u32) -> bool {
        !(*lhs == *rhs)
    }
}

impl U32PartialOrd of PartialOrd<u32> {
    #[inline(always)]
    fn le(lhs: u32, rhs: u32) -> bool {
        u32_overflowing_sub(rhs, lhs).into_is_ok()
    }
    #[inline(always)]
    fn ge(lhs: u32, rhs: u32) -> bool {
        u32_overflowing_sub(lhs, rhs).into_is_ok()
    }
    #[inline(always)]
    fn lt(lhs: u32, rhs: u32) -> bool {
        u32_overflowing_sub(lhs, rhs).into_is_err()
    }
    #[inline(always)]
    fn gt(lhs: u32, rhs: u32) -> bool {
        u32_overflowing_sub(rhs, lhs).into_is_err()
    }
}

extern fn u32_overflowing_add(lhs: u32, rhs: u32) -> Result<u32, u32> implicits(RangeCheck) nopanic;
extern fn u32_overflowing_sub(lhs: u32, rhs: u32) -> Result<u32, u32> implicits(RangeCheck) nopanic;

fn u32_wrapping_add(lhs: u32, rhs: u32) -> u32 implicits(RangeCheck) nopanic {
    match u32_overflowing_add(lhs, rhs) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u32_wrapping_sub(lhs: u32, rhs: u32) -> u32 implicits(RangeCheck) nopanic {
    match u32_overflowing_sub(lhs, rhs) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u32_checked_add(lhs: u32, rhs: u32) -> Option<u32> implicits(RangeCheck) nopanic {
    match u32_overflowing_add(lhs, rhs) {
        Result::Ok(r) => Option::Some(r),
        Result::Err(r) => Option::None,
    }
}

impl U32Add of Add<u32> {
    fn add(lhs: u32, rhs: u32) -> u32 {
        u32_overflowing_add(lhs, rhs).expect('u32_add Overflow')
    }
}
impl U32AddEq of AddEq<u32> {
    #[inline(always)]
    fn add_eq(ref self: u32, other: u32) {
        self = Add::add(self, other);
    }
}

fn u32_checked_sub(lhs: u32, rhs: u32) -> Option<u32> implicits(RangeCheck) nopanic {
    match u32_overflowing_sub(lhs, rhs) {
        Result::Ok(r) => Option::Some(r),
        Result::Err(r) => Option::None,
    }
}

impl U32Sub of Sub<u32> {
    fn sub(lhs: u32, rhs: u32) -> u32 {
        u32_overflowing_sub(lhs, rhs).expect('u32_sub Overflow')
    }
}
impl U32SubEq of SubEq<u32> {
    #[inline(always)]
    fn sub_eq(ref self: u32, other: u32) {
        self = Sub::sub(self, other);
    }
}

extern fn u32_wide_mul(lhs: u32, rhs: u32) -> u64 implicits() nopanic;
extern fn u32_sqrt(value: u32) -> u16 implicits(RangeCheck) nopanic;

impl U32Mul of Mul<u32> {
    fn mul(lhs: u32, rhs: u32) -> u32 {
        u32_wide_mul(lhs, rhs).try_into().expect('u32_mul Overflow')
    }
}
impl U32MulEq of MulEq<u32> {
    #[inline(always)]
    fn mul_eq(ref self: u32, other: u32) {
        self = Mul::mul(self, other);
    }
}

extern fn u32_is_zero(a: u32) -> IsZeroResult<u32> implicits() nopanic;
extern fn u32_safe_divmod(lhs: u32, rhs: NonZero<u32>) -> (u32, u32) implicits(RangeCheck) nopanic;

#[panic_with('u32 is 0', u32_as_non_zero)]
fn u32_try_as_non_zero(a: u32) -> Option<NonZero<u32>> nopanic {
    match u32_is_zero(a) {
        IsZeroResult::Zero => Option::None,
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U32TryIntoNonZero of TryInto<u32, NonZero<u32>> {
    fn try_into(self: u32) -> Option<NonZero<u32>> {
        Option::Some(u32_as_non_zero(self))
    }
}

impl U32Div of Div<u32> {
    fn div(lhs: u32, rhs: u32) -> u32 {
        let (q, r) = u32_safe_divmod(lhs, rhs.try_into().expect('Division by 0'));
        q
    }
}
impl U32DivEq of DivEq<u32> {
    #[inline(always)]
    fn div_eq(ref self: u32, other: u32) {
        self = Div::div(self, other);
    }
}

impl U32Rem of Rem<u32> {
    fn rem(lhs: u32, rhs: u32) -> u32 {
        let (q, r) = u32_safe_divmod(lhs, rhs.try_into().expect('Division by 0'));
        r
    }
}
impl U32RemEq of RemEq<u32> {
    #[inline(always)]
    fn rem_eq(ref self: u32, other: u32) {
        self = Rem::rem(self, other);
    }
}

impl U32DivRem of DivRem<u32> {
    fn div_rem(lhs: u32, rhs: NonZero<u32>) -> (u32, u32) {
        u32_safe_divmod(lhs, rhs)
    }
}

impl U32BitNot of BitNot<u32> {
    fn bitnot(a: u32) -> u32 {
        BoundedInt::max() - a
    }
}
extern fn u32_bitwise(lhs: u32, rhs: u32) -> (u32, u32, u32) implicits(Bitwise) nopanic;
impl U32BitAnd of BitAnd<u32> {
    #[inline(always)]
    fn bitand(lhs: u32, rhs: u32) -> u32 {
        let (v, _, _) = u32_bitwise(lhs, rhs);
        v
    }
}
impl U32BitXor of BitXor<u32> {
    #[inline(always)]
    fn bitxor(lhs: u32, rhs: u32) -> u32 {
        let (_, v, _) = u32_bitwise(lhs, rhs);
        v
    }
}
impl U32BitOr of BitOr<u32> {
    #[inline(always)]
    fn bitor(lhs: u32, rhs: u32) -> u32 {
        let (_, _, v) = u32_bitwise(lhs, rhs);
        v
    }
}

#[derive(Copy, Drop)]
extern type u64;
impl NumericLiteralu64 of NumericLiteral<u64>;
extern fn u64_const<value>() -> u64 nopanic;
extern fn u64_to_felt252(a: u64) -> felt252 nopanic;

#[panic_with('u64_from Overflow', u64_from_felt252)]
extern fn u64_try_from_felt252(a: felt252) -> Option<u64> implicits(RangeCheck) nopanic;

extern fn u64_eq(lhs: u64, rhs: u64) -> bool implicits() nopanic;

impl U64Serde of Serde<u64> {
    fn serialize(self: @u64, ref output: Array<felt252>) {
        Into::<u64, felt252>::into(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u64> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U64PartialEq of PartialEq<u64> {
    #[inline(always)]
    fn eq(lhs: @u64, rhs: @u64) -> bool {
        u64_eq(*lhs, *rhs)
    }
    #[inline(always)]
    fn ne(lhs: @u64, rhs: @u64) -> bool {
        !(*lhs == *rhs)
    }
}

impl U64PartialOrd of PartialOrd<u64> {
    #[inline(always)]
    fn le(lhs: u64, rhs: u64) -> bool {
        u64_overflowing_sub(rhs, lhs).into_is_ok()
    }
    #[inline(always)]
    fn ge(lhs: u64, rhs: u64) -> bool {
        u64_overflowing_sub(lhs, rhs).into_is_ok()
    }
    #[inline(always)]
    fn lt(lhs: u64, rhs: u64) -> bool {
        u64_overflowing_sub(lhs, rhs).into_is_err()
    }
    #[inline(always)]
    fn gt(lhs: u64, rhs: u64) -> bool {
        u64_overflowing_sub(rhs, lhs).into_is_err()
    }
}

extern fn u64_overflowing_add(lhs: u64, rhs: u64) -> Result<u64, u64> implicits(RangeCheck) nopanic;
extern fn u64_overflowing_sub(lhs: u64, rhs: u64) -> Result<u64, u64> implicits(RangeCheck) nopanic;

fn u64_wrapping_add(lhs: u64, rhs: u64) -> u64 implicits(RangeCheck) nopanic {
    match u64_overflowing_add(lhs, rhs) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u64_wrapping_sub(lhs: u64, rhs: u64) -> u64 implicits(RangeCheck) nopanic {
    match u64_overflowing_sub(lhs, rhs) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u64_checked_add(lhs: u64, rhs: u64) -> Option<u64> implicits(RangeCheck) nopanic {
    match u64_overflowing_add(lhs, rhs) {
        Result::Ok(r) => Option::Some(r),
        Result::Err(r) => Option::None,
    }
}

impl U64Add of Add<u64> {
    fn add(lhs: u64, rhs: u64) -> u64 {
        u64_overflowing_add(lhs, rhs).expect('u64_add Overflow')
    }
}
impl U64AddEq of AddEq<u64> {
    #[inline(always)]
    fn add_eq(ref self: u64, other: u64) {
        self = Add::add(self, other);
    }
}

fn u64_checked_sub(lhs: u64, rhs: u64) -> Option<u64> implicits(RangeCheck) nopanic {
    match u64_overflowing_sub(lhs, rhs) {
        Result::Ok(r) => Option::Some(r),
        Result::Err(r) => Option::None,
    }
}

impl U64Sub of Sub<u64> {
    fn sub(lhs: u64, rhs: u64) -> u64 {
        u64_overflowing_sub(lhs, rhs).expect('u64_sub Overflow')
    }
}
impl U64SubEq of SubEq<u64> {
    #[inline(always)]
    fn sub_eq(ref self: u64, other: u64) {
        self = Sub::sub(self, other);
    }
}

extern fn u64_wide_mul(lhs: u64, rhs: u64) -> u128 implicits() nopanic;
extern fn u64_sqrt(value: u64) -> u32 implicits(RangeCheck) nopanic;

impl U64Mul of Mul<u64> {
    fn mul(lhs: u64, rhs: u64) -> u64 {
        u64_wide_mul(lhs, rhs).try_into().expect('u64_mul Overflow')
    }
}
impl U64MulEq of MulEq<u64> {
    #[inline(always)]
    fn mul_eq(ref self: u64, other: u64) {
        self = Mul::mul(self, other);
    }
}

extern fn u64_is_zero(a: u64) -> IsZeroResult<u64> implicits() nopanic;
extern fn u64_safe_divmod(lhs: u64, rhs: NonZero<u64>) -> (u64, u64) implicits(RangeCheck) nopanic;

#[panic_with('u64 is 0', u64_as_non_zero)]
fn u64_try_as_non_zero(a: u64) -> Option<NonZero<u64>> nopanic {
    match u64_is_zero(a) {
        IsZeroResult::Zero => Option::None,
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U64TryIntoNonZero of TryInto<u64, NonZero<u64>> {
    fn try_into(self: u64) -> Option<NonZero<u64>> {
        Option::Some(u64_as_non_zero(self))
    }
}

impl U64Div of Div<u64> {
    fn div(lhs: u64, rhs: u64) -> u64 {
        let (q, r) = u64_safe_divmod(lhs, rhs.try_into().expect('Division by 0'));
        q
    }
}
impl U64DivEq of DivEq<u64> {
    #[inline(always)]
    fn div_eq(ref self: u64, other: u64) {
        self = Div::div(self, other);
    }
}

impl U64Rem of Rem<u64> {
    fn rem(lhs: u64, rhs: u64) -> u64 {
        let (q, r) = u64_safe_divmod(lhs, rhs.try_into().expect('Division by 0'));
        r
    }
}
impl U64RemEq of RemEq<u64> {
    #[inline(always)]
    fn rem_eq(ref self: u64, other: u64) {
        self = Rem::rem(self, other);
    }
}

impl U64DivRem of DivRem<u64> {
    fn div_rem(lhs: u64, rhs: NonZero<u64>) -> (u64, u64) {
        u64_safe_divmod(lhs, rhs)
    }
}

impl U64BitNot of BitNot<u64> {
    fn bitnot(a: u64) -> u64 {
        BoundedInt::max() - a
    }
}
extern fn u64_bitwise(lhs: u64, rhs: u64) -> (u64, u64, u64) implicits(Bitwise) nopanic;
impl U64BitAnd of BitAnd<u64> {
    #[inline(always)]
    fn bitand(lhs: u64, rhs: u64) -> u64 {
        let (v, _, _) = u64_bitwise(lhs, rhs);
        v
    }
}
impl U64BitXor of BitXor<u64> {
    #[inline(always)]
    fn bitxor(lhs: u64, rhs: u64) -> u64 {
        let (_, v, _) = u64_bitwise(lhs, rhs);
        v
    }
}
impl U64BitOr of BitOr<u64> {
    #[inline(always)]
    fn bitor(lhs: u64, rhs: u64) -> u64 {
        let (_, _, v) = u64_bitwise(lhs, rhs);
        v
    }
}

#[derive(Copy, Drop, PartialEq, Serde, starknet::Store)]
struct u256 {
    low: u128,
    high: u128,
}
impl NumericLiteralU256 of NumericLiteral<u256>;

fn u256_overflowing_add(lhs: u256, rhs: u256) -> (u256, bool) implicits(RangeCheck) nopanic {
    let (high, overflow) = match u128_overflowing_add(lhs.high, rhs.high) {
        Result::Ok(high) => (high, false),
        Result::Err(high) => (high, true),
    };
    match u128_overflowing_add(lhs.low, rhs.low) {
        Result::Ok(low) => (u256 { low, high }, overflow),
        Result::Err(low) => {
            match u128_overflowing_add(high, 1_u128) {
                Result::Ok(high) => (u256 { low, high }, overflow),
                Result::Err(high) => (u256 { low, high }, true),
            }
        },
    }
}

fn u256_overflow_sub(lhs: u256, rhs: u256) -> (u256, bool) implicits(RangeCheck) nopanic {
    let (high, overflow) = match u128_overflowing_sub(lhs.high, rhs.high) {
        Result::Ok(high) => (high, false),
        Result::Err(high) => (high, true),
    };
    match u128_overflowing_sub(lhs.low, rhs.low) {
        Result::Ok(low) => (u256 { low, high }, overflow),
        Result::Err(low) => {
            match u128_overflowing_sub(high, 1_u128) {
                Result::Ok(high) => (u256 { low, high }, overflow),
                Result::Err(high) => (u256 { low, high }, true),
            }
        },
    }
}

fn u256_overflow_mul(lhs: u256, rhs: u256) -> (u256, bool) {
    let (high1, low) = u128_wide_mul(lhs.low, rhs.low);
    let (overflow_value1, high2) = u128_wide_mul(lhs.low, rhs.high);
    let (overflow_value2, high3) = u128_wide_mul(lhs.high, rhs.low);
    let (high, overflow) = match u128_overflowing_add(high1, high2) {
        Result::Ok(high) => (
            high,
            overflow_value1 != 0_u128
                || overflow_value2 != 0_u128
                || (lhs.high > 0_u128 && rhs.high > 0_u128)
        ),
        Result::Err(high) => (high, true),
    };
    let (high, overflow) = match u128_overflowing_add(high, high3) {
        Result::Ok(high) => (high, overflow),
        Result::Err(high) => (high, true),
    };
    (u256 { low, high }, overflow)
}

fn u256_checked_add(lhs: u256, rhs: u256) -> Option<u256> implicits(RangeCheck) nopanic {
    let (r, overflow) = u256_overflowing_add(lhs, rhs);
    if overflow {
        Option::None
    } else {
        Option::Some(r)
    }
}

impl U256Add of Add<u256> {
    fn add(lhs: u256, rhs: u256) -> u256 {
        u256_checked_add(lhs, rhs).expect('u256_add Overflow')
    }
}
impl U256AddEq of AddEq<u256> {
    #[inline(always)]
    fn add_eq(ref self: u256, other: u256) {
        self = Add::add(self, other);
    }
}

#[panic_with('u256_sub Overflow', u256_sub)]
fn u256_checked_sub(lhs: u256, rhs: u256) -> Option<u256> implicits(RangeCheck) nopanic {
    let (r, overflow) = u256_overflow_sub(lhs, rhs);
    if overflow {
        Option::None
    } else {
        Option::Some(r)
    }
}

impl U256Sub of Sub<u256> {
    fn sub(lhs: u256, rhs: u256) -> u256 {
        u256_checked_sub(lhs, rhs).expect('u256_sub Overflow')
    }
}
impl U256SubEq of SubEq<u256> {
    #[inline(always)]
    fn sub_eq(ref self: u256, other: u256) {
        self = Sub::sub(self, other);
    }
}

fn u256_checked_mul(lhs: u256, rhs: u256) -> Option<u256> implicits(RangeCheck) {
    let (r, overflow) = u256_overflow_mul(lhs, rhs);
    if overflow {
        Option::None
    } else {
        Option::Some(r)
    }
}

impl U256Mul of Mul<u256> {
    fn mul(lhs: u256, rhs: u256) -> u256 {
        u256_checked_mul(lhs, rhs).expect('u256_mul Overflow')
    }
}
impl U256MulEq of MulEq<u256> {
    #[inline(always)]
    fn mul_eq(ref self: u256, other: u256) {
        self = Mul::mul(self, other);
    }
}

impl U256PartialOrd of PartialOrd<u256> {
    #[inline(always)]
    fn le(lhs: u256, rhs: u256) -> bool {
        !(rhs < lhs)
    }
    #[inline(always)]
    fn ge(lhs: u256, rhs: u256) -> bool {
        !(lhs < rhs)
    }
    fn lt(lhs: u256, rhs: u256) -> bool {
        if lhs.high < rhs.high {
            true
        } else if lhs.high == rhs.high {
            lhs.low < rhs.low
        } else {
            false
        }
    }
    #[inline(always)]
    fn gt(lhs: u256, rhs: u256) -> bool {
        rhs < lhs
    }
}

impl U256BitAnd of BitAnd<u256> {
    #[inline(always)]
    fn bitand(lhs: u256, rhs: u256) -> u256 {
        u256 { low: lhs.low & rhs.low, high: lhs.high & rhs.high }
    }
}
impl U256BitXor of BitXor<u256> {
    #[inline(always)]
    fn bitxor(lhs: u256, rhs: u256) -> u256 {
        u256 { low: lhs.low ^ rhs.low, high: lhs.high ^ rhs.high }
    }
}
impl U256BitOr of BitOr<u256> {
    #[inline(always)]
    fn bitor(lhs: u256, rhs: u256) -> u256 {
        u256 { low: lhs.low | rhs.low, high: lhs.high | rhs.high }
    }
}

fn u256_from_felt252(lhs: felt252) -> u256 implicits(RangeCheck) nopanic {
    match u128s_from_felt252(lhs) {
        U128sFromFelt252Result::Narrow(low) => u256 { low, high: 0_u128 },
        U128sFromFelt252Result::Wide((high, low)) => u256 { low, high },
    }
}

extern fn u256_is_zero(a: u256) -> IsZeroResult<u256> implicits() nopanic;

/// Calculates division with remainder of a u256 by a non-zero u256.
/// Additionally returns a `U128MulGuarantee` that is required for validating the calculation.
extern fn u256_safe_divmod(
    lhs: u256, rhs: NonZero<u256>
) -> (u256, u256, U128MulGuarantee) implicits(RangeCheck) nopanic;

/// Calculates division with remainder of a u256 by a non-zero u256.
#[inline(always)]
fn u256_safe_div_rem(lhs: u256, rhs: NonZero<u256>) -> (u256, u256) implicits(RangeCheck) nopanic {
    let (q, r, _) = u256_safe_divmod(lhs, rhs);
    (q, r)
}
extern fn u256_sqrt(a: u256) -> u128 implicits(RangeCheck) nopanic;

#[panic_with('u256 is 0', u256_as_non_zero)]
fn u256_try_as_non_zero(a: u256) -> Option<NonZero<u256>> nopanic {
    match u256_is_zero(a) {
        IsZeroResult::Zero => Option::None,
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U256TryIntoNonZero of TryInto<u256, NonZero<u256>> {
    fn try_into(self: u256) -> Option<NonZero<u256>> {
        Option::Some(u256_as_non_zero(self))
    }
}

impl U256Div of Div<u256> {
    fn div(lhs: u256, rhs: u256) -> u256 {
        let (q, r) = u256_safe_div_rem(lhs, rhs.try_into().expect('Division by 0'));
        q
    }
}
impl U256DivEq of DivEq<u256> {
    #[inline(always)]
    fn div_eq(ref self: u256, other: u256) {
        self = Div::div(self, other);
    }
}

impl U256Rem of Rem<u256> {
    fn rem(lhs: u256, rhs: u256) -> u256 {
        let (q, r) = u256_safe_div_rem(lhs, rhs.try_into().expect('Division by 0'));
        r
    }
}
impl U256RemEq of RemEq<u256> {
    #[inline(always)]
    fn rem_eq(ref self: u256, other: u256) {
        self = Rem::rem(self, other);
    }
}

impl U256DivRem of DivRem<u256> {
    fn div_rem(lhs: u256, rhs: NonZero<u256>) -> (u256, u256) {
        u256_safe_div_rem(lhs, rhs)
    }
}

impl U256BitNot of BitNot<u256> {
    fn bitnot(a: u256) -> u256 {
        u256 { low: ~a.low, high: ~a.high }
    }
}

#[derive(Copy, Drop, PartialEq, Serde)]
struct u512 {
    limb0: u128,
    limb1: u128,
    limb2: u128,
    limb3: u128,
}

// Returns the result of u128 addition, including an overflow word.
fn u128_add_with_carry(a: u128, b: u128) -> (u128, u128) nopanic {
    match u128_overflowing_add(a, b) {
        Result::Ok(v) => (v, 0),
        Result::Err(v) => (v, 1),
    }
}

fn u256_wide_mul(a: u256, b: u256) -> u512 nopanic {
    let (limb1, limb0) = u128_wide_mul(a.low, b.low);
    let (limb2, limb1_part) = u128_wide_mul(a.low, b.high);
    let (limb1, limb1_overflow0) = u128_add_with_carry(limb1, limb1_part);
    let (limb2_part, limb1_part) = u128_wide_mul(a.high, b.low);
    let (limb1, limb1_overflow1) = u128_add_with_carry(limb1, limb1_part);
    let (limb2, limb2_overflow) = u128_add_with_carry(limb2, limb2_part);
    let (limb3, limb2_part) = u128_wide_mul(a.high, b.high);
    // No overflow since no limb4.
    let limb3 = u128_wrapping_add(limb3, limb2_overflow);
    let (limb2, limb2_overflow) = u128_add_with_carry(limb2, limb2_part);
    // No overflow since no limb4.
    let limb3 = u128_wrapping_add(limb3, limb2_overflow);
    // No overflow possible in this addition since both operands are 0/1.
    let limb1_overflow = u128_wrapping_add(limb1_overflow0, limb1_overflow1);
    let (limb2, limb2_overflow) = u128_add_with_carry(limb2, limb1_overflow);
    // No overflow since no limb4.
    let limb3 = u128_wrapping_add(limb3, limb2_overflow);
    u512 { limb0, limb1, limb2, limb3 }
}

/// Calculates division with remainder of a u512 by a non-zero u256.
#[inline(always)]
fn u512_safe_div_rem_by_u256(
    lhs: u512, rhs: NonZero<u256>
) -> (u512, u256) implicits(RangeCheck) nopanic {
    let (q, r, _, _, _, _, _) = u512_safe_divmod_by_u256(lhs, rhs);
    (q, r)
}

/// Calculates division with remainder of a u512 by a non-zero u256.
/// Additionally returns several `U128MulGuarantee`s that are required for validating the calculation.
extern fn u512_safe_divmod_by_u256(
    lhs: u512, rhs: NonZero<u256>
) -> (
    u512,
    u256,
    U128MulGuarantee,
    U128MulGuarantee,
    U128MulGuarantee,
    U128MulGuarantee,
    U128MulGuarantee
) implicits(RangeCheck) nopanic;

/// Bounded
trait BoundedInt<T> {
    fn min() -> T nopanic;
    fn max() -> T nopanic;
}

impl BoundedU8 of BoundedInt<u8> {
    #[inline(always)]
    fn min() -> u8 nopanic {
        0_u8
    }
    #[inline(always)]
    fn max() -> u8 nopanic {
        0xff_u8
    }
}

impl BoundedU16 of BoundedInt<u16> {
    #[inline(always)]
    fn min() -> u16 nopanic {
        0_u16
    }
    #[inline(always)]
    fn max() -> u16 nopanic {
        0xffff_u16
    }
}

impl BoundedU32 of BoundedInt<u32> {
    #[inline(always)]
    fn min() -> u32 nopanic {
        0_u32
    }
    #[inline(always)]
    fn max() -> u32 nopanic {
        0xffffffff_u32
    }
}

impl BoundedU64 of BoundedInt<u64> {
    #[inline(always)]
    fn min() -> u64 nopanic {
        0_u64
    }
    #[inline(always)]
    fn max() -> u64 nopanic {
        0xffffffffffffffff_u64
    }
}

impl BoundedU128 of BoundedInt<u128> {
    #[inline(always)]
    fn min() -> u128 nopanic {
        0_u128
    }
    #[inline(always)]
    fn max() -> u128 nopanic {
        0xffffffffffffffffffffffffffffffff_u128
    }
}

impl BoundedU256 of BoundedInt<u256> {
    #[inline(always)]
    fn min() -> u256 nopanic {
        0_u256
    }
    #[inline(always)]
    fn max() -> u256 nopanic {
        u256 { low: BoundedInt::max(), high: BoundedInt::max() }
    }
}

impl BoundedI8 of BoundedInt<i8> {
    #[inline(always)]
    fn min() -> i8 nopanic {
        -0x80
    }
    #[inline(always)]
    fn max() -> i8 nopanic {
        0x7f
    }
}

impl BoundedI16 of BoundedInt<i16> {
    #[inline(always)]
    fn min() -> i16 nopanic {
        -0x8000
    }
    #[inline(always)]
    fn max() -> i16 nopanic {
        0x7fff
    }
}

impl BoundedI32 of BoundedInt<i32> {
    #[inline(always)]
    fn min() -> i32 nopanic {
        -0x80000000
    }
    #[inline(always)]
    fn max() -> i32 nopanic {
        0x7fffffff
    }
}

impl BoundedI64 of BoundedInt<i64> {
    #[inline(always)]
    fn min() -> i64 nopanic {
        -0x8000000000000000
    }
    #[inline(always)]
    fn max() -> i64 nopanic {
        0x7fffffffffffffff
    }
}

impl BoundedI128 of BoundedInt<i128> {
    #[inline(always)]
    fn min() -> i128 nopanic {
        -0x80000000000000000000000000000000
    }
    #[inline(always)]
    fn max() -> i128 nopanic {
        0x7fffffffffffffffffffffffffffffff
    }
}

/// Conversions.
impl Felt252TryIntoU8 of TryInto<felt252, u8> {
    fn try_into(self: felt252) -> Option<u8> {
        u8_try_from_felt252(self)
    }
}
impl U8IntoFelt252 of Into<u8, felt252> {
    fn into(self: u8) -> felt252 {
        u8_to_felt252(self)
    }
}
impl Felt252TryIntoU16 of TryInto<felt252, u16> {
    fn try_into(self: felt252) -> Option<u16> {
        u16_try_from_felt252(self)
    }
}
impl U16IntoFelt252 of Into<u16, felt252> {
    fn into(self: u16) -> felt252 {
        u16_to_felt252(self)
    }
}
impl Felt252TryIntoU32 of TryInto<felt252, u32> {
    fn try_into(self: felt252) -> Option<u32> {
        u32_try_from_felt252(self)
    }
}
impl U32IntoFelt252 of Into<u32, felt252> {
    fn into(self: u32) -> felt252 {
        u32_to_felt252(self)
    }
}
impl Felt252TryIntoU64 of TryInto<felt252, u64> {
    fn try_into(self: felt252) -> Option<u64> {
        u64_try_from_felt252(self)
    }
}
impl U64IntoFelt252 of Into<u64, felt252> {
    fn into(self: u64) -> felt252 {
        u64_to_felt252(self)
    }
}
impl Felt252TryIntoU128 of TryInto<felt252, u128> {
    fn try_into(self: felt252) -> Option<u128> {
        u128_try_from_felt252(self)
    }
}
impl U128IntoFelt252 of Into<u128, felt252> {
    fn into(self: u128) -> felt252 {
        u128_to_felt252(self)
    }
}
impl Felt252IntoU256 of Into<felt252, u256> {
    fn into(self: felt252) -> u256 {
        u256_from_felt252(self)
    }
}
impl U256TryIntoFelt252 of TryInto<u256, felt252> {
    fn try_into(self: u256) -> Option<felt252> {
        let FELT252_PRIME_HIGH = 0x8000000000000110000000000000000_u128;
        if self.high > FELT252_PRIME_HIGH {
            return Option::None;
        }
        if self.high == FELT252_PRIME_HIGH {
            // since FELT252_PRIME_LOW is 1.
            if self.low != 0 {
                return Option::None;
            }
        }
        Option::Some(
            self.high.into() * 0x100000000000000000000000000000000_felt252 + self.low.into()
        )
    }
}
impl Felt252TryIntoI8 of TryInto<felt252, i8> {
    fn try_into(self: felt252) -> Option<i8> {
        i8_try_from_felt252(self)
    }
}
impl I8IntoFelt252 of Into<i8, felt252> {
    fn into(self: i8) -> felt252 {
        i8_to_felt252(self)
    }
}
impl Felt252TryIntoI16 of TryInto<felt252, i16> {
    fn try_into(self: felt252) -> Option<i16> {
        i16_try_from_felt252(self)
    }
}
impl I16IntoFelt252 of Into<i16, felt252> {
    fn into(self: i16) -> felt252 {
        i16_to_felt252(self)
    }
}
impl Felt252TryIntoI32 of TryInto<felt252, i32> {
    fn try_into(self: felt252) -> Option<i32> {
        i32_try_from_felt252(self)
    }
}
impl I32IntoFelt252 of Into<i32, felt252> {
    fn into(self: i32) -> felt252 {
        i32_to_felt252(self)
    }
}
impl Felt252TryIntoI64 of TryInto<felt252, i64> {
    fn try_into(self: felt252) -> Option<i64> {
        i64_try_from_felt252(self)
    }
}
impl I64IntoFelt252 of Into<i64, felt252> {
    fn into(self: i64) -> felt252 {
        i64_to_felt252(self)
    }
}
impl Felt252TryIntoI128 of TryInto<felt252, i128> {
    fn try_into(self: felt252) -> Option<i128> {
        i128_try_from_felt252(self)
    }
}
impl I128IntoFelt252 of Into<i128, felt252> {
    fn into(self: i128) -> felt252 {
        i128_to_felt252(self)
    }
}

// TODO(lior): Restrict the function (using traits) in the high-level compiler so that wrong types
//   will not lead to Sierra errors.
extern fn upcast<FromType, ToType>(x: FromType) -> ToType nopanic;

// TODO(lior): Restrict the function (using traits) in the high-level compiler so that wrong types
//   will not lead to Sierra errors.
extern fn downcast<FromType, ToType>(x: FromType) -> Option<ToType> implicits(RangeCheck) nopanic;

// Marks `FromType` as upcastable to `ToType`.
// Do not add user code implementing this trait.
trait Upcastable<FromType, ToType>;
impl UpcastableU8U16 of Upcastable<u8, u16> {}
impl UpcastableU8I16 of Upcastable<u8, i16> {}
impl UpcastableU8U32 of Upcastable<u8, u32> {}
impl UpcastableU8I32 of Upcastable<u8, i32> {}
impl UpcastableU8U64 of Upcastable<u8, u64> {}
impl UpcastableU8I64 of Upcastable<u8, i64> {}
impl UpcastableU8U128 of Upcastable<u8, u128> {}
impl UpcastableU8I128 of Upcastable<u8, i128> {}
impl UpcastableI8I16 of Upcastable<i8, i16> {}
impl UpcastableI8I32 of Upcastable<i8, i32> {}
impl UpcastableI8I64 of Upcastable<i8, i64> {}
impl UpcastableI8I128 of Upcastable<i8, i128> {}
impl UpcastableU16U32 of Upcastable<u16, u32> {}
impl UpcastableU16I32 of Upcastable<u16, i32> {}
impl UpcastableU16U64 of Upcastable<u16, u64> {}
impl UpcastableU16I64 of Upcastable<u16, i64> {}
impl UpcastableU16U128 of Upcastable<u16, u128> {}
impl UpcastableU16I128 of Upcastable<u16, i128> {}
impl UpcastableI16I32 of Upcastable<i16, i32> {}
impl UpcastableI16I64 of Upcastable<i16, i64> {}
impl UpcastableI16I128 of Upcastable<i16, i128> {}
impl UpcastableU32U64 of Upcastable<u32, u64> {}
impl UpcastableU32I64 of Upcastable<u32, i64> {}
impl UpcastableU32U128 of Upcastable<u32, u128> {}
impl UpcastableU32I128 of Upcastable<u32, i128> {}
impl UpcastableI32I64 of Upcastable<i32, i64> {}
impl UpcastableI32I128 of Upcastable<i32, i128> {}
impl UpcastableU64U128 of Upcastable<u64, u128> {}
impl UpcastableU64I128 of Upcastable<u64, i128> {}
impl UpcastableI64I128 of Upcastable<i64, i128> {}
// Marks `FromType` as downcastable to `ToType`.
// Do not add user code implementing this trait.
trait Downcastable<FromType, ToType>;
impl DowncastableU128I128 of Downcastable<u128, i128> {}
impl DowncastableU128U64 of Downcastable<u128, u64> {}
impl DowncastableU128I64 of Downcastable<u128, i64> {}
impl DowncastableU128U32 of Downcastable<u128, u32> {}
impl DowncastableU128I32 of Downcastable<u128, i32> {}
impl DowncastableU128U16 of Downcastable<u128, u16> {}
impl DowncastableU128I16 of Downcastable<u128, i16> {}
impl DowncastableU128U8 of Downcastable<u128, u8> {}
impl DowncastableU128I8 of Downcastable<u128, i8> {}
impl DowncastableI128U128 of Downcastable<i128, u128> {}
impl DowncastableI128U64 of Downcastable<i128, u64> {}
impl DowncastableI128I64 of Downcastable<i128, i64> {}
impl DowncastableI128U32 of Downcastable<i128, u32> {}
impl DowncastableI128I32 of Downcastable<i128, i32> {}
impl DowncastableI128U16 of Downcastable<i128, u16> {}
impl DowncastableI128I16 of Downcastable<i128, i16> {}
impl DowncastableI128U8 of Downcastable<i128, u8> {}
impl DowncastableI128I8 of Downcastable<i128, i8> {}

impl DowncastableU64I64 of Downcastable<u64, i64> {}
impl DowncastableU64U32 of Downcastable<u64, u32> {}
impl DowncastableU64I32 of Downcastable<u64, i32> {}
impl DowncastableU64U16 of Downcastable<u64, u16> {}
impl DowncastableU64I16 of Downcastable<u64, i16> {}
impl DowncastableU64U8 of Downcastable<u64, u8> {}
impl DowncastableU64I8 of Downcastable<u64, i8> {}
impl DowncastableI64U128 of Downcastable<i64, u128> {}
impl DowncastableI64U64 of Downcastable<i64, u64> {}
impl DowncastableI64U32 of Downcastable<i64, u32> {}
impl DowncastableI64I32 of Downcastable<i64, i32> {}
impl DowncastableI64U16 of Downcastable<i64, u16> {}
impl DowncastableI64I16 of Downcastable<i64, i16> {}
impl DowncastableI64U8 of Downcastable<i64, u8> {}
impl DowncastableI64I8 of Downcastable<i64, i8> {}

impl DowncastableU32I32 of Downcastable<u32, i32> {}
impl DowncastableU32U16 of Downcastable<u32, u16> {}
impl DowncastableU32I16 of Downcastable<u32, i16> {}
impl DowncastableU32U8 of Downcastable<u32, u8> {}
impl DowncastableU32I8 of Downcastable<u32, i8> {}
impl DowncastableI32U128 of Downcastable<i32, u128> {}
impl DowncastableI32U64 of Downcastable<i32, u64> {}
impl DowncastableI32U32 of Downcastable<i32, u32> {}
impl DowncastableI32U16 of Downcastable<i32, u16> {}
impl DowncastableI32I16 of Downcastable<i32, i16> {}
impl DowncastableI32U8 of Downcastable<i32, u8> {}
impl DowncastableI32I8 of Downcastable<i32, i8> {}

impl DowncastableU16I16 of Downcastable<u16, i16> {}
impl DowncastableU16U8 of Downcastable<u16, u8> {}
impl DowncastableU16I8 of Downcastable<u16, i8> {}
impl DowncastableI16U128 of Downcastable<i16, u128> {}
impl DowncastableI16U64 of Downcastable<i16, u64> {}
impl DowncastableI16U32 of Downcastable<i16, u32> {}
impl DowncastableI16U16 of Downcastable<i16, u16> {}
impl DowncastableI16U8 of Downcastable<i16, u8> {}
impl DowncastableI16I8 of Downcastable<i16, i8> {}

impl DowncastableU8I8 of Downcastable<u8, i8> {}
impl DowncastableI8U128 of Downcastable<i8, u128> {}
impl DowncastableI8U64 of Downcastable<i8, u64> {}
impl DowncastableI8U32 of Downcastable<i8, u32> {}
impl DowncastableI8U16 of Downcastable<i8, u16> {}
impl DowncastableI8U8 of Downcastable<i8, u8> {}

/// Default values
impl U8Default of Default<u8> {
    #[inline(always)]
    fn default() -> u8 nopanic {
        0_u8
    }
}

impl U16Default of Default<u16> {
    #[inline(always)]
    fn default() -> u16 nopanic {
        0_u16
    }
}

impl U32Default of Default<u32> {
    #[inline(always)]
    fn default() -> u32 nopanic {
        0_u32
    }
}

impl U64Default of Default<u64> {
    #[inline(always)]
    fn default() -> u64 nopanic {
        0_u64
    }
}

impl U128Default of Default<u128> {
    #[inline(always)]
    fn default() -> u128 nopanic {
        0_u128
    }
}

impl U256Default of Default<u256> {
    #[inline(always)]
    fn default() -> u256 nopanic {
        0_u256
    }
}


/// Default values for felt252_dict values.
impl U8Felt252DictValue of Felt252DictValue<u8> {
    #[inline(always)]
    fn zero_default() -> u8 nopanic {
        0
    }
}

impl U16Felt252DictValue of Felt252DictValue<u16> {
    #[inline(always)]
    fn zero_default() -> u16 nopanic {
        0
    }
}

impl U32Felt252DictValue of Felt252DictValue<u32> {
    #[inline(always)]
    fn zero_default() -> u32 nopanic {
        0
    }
}

impl U64Felt252DictValue of Felt252DictValue<u64> {
    #[inline(always)]
    fn zero_default() -> u64 nopanic {
        0
    }
}

impl U128Felt252DictValue of Felt252DictValue<u128> {
    #[inline(always)]
    fn zero_default() -> u128 nopanic {
        0
    }
}

impl UpcastableInto<From, To, impl FromToUpcastable: Upcastable<From, To>> of Into<From, To> {
    fn into(self: From) -> To {
        upcast(self)
    }
}

impl DowncastableTryInto<
    From, To, impl FromToDowncastable: Downcastable<From, To>
> of TryInto<From, To> {
    fn try_into(self: From) -> Option<To> {
        downcast(self)
    }
}

impl U8IntoU256 of Into<u8, u256> {
    fn into(self: u8) -> u256 {
        u256 { low: upcast(self), high: 0_u128 }
    }
}

impl U256TryIntoU8 of TryInto<u256, u8> {
    fn try_into(self: u256) -> Option<u8> {
        let u256{low: low, high: high } = self;

        if high != 0 {
            return Option::None;
        }

        low.try_into()
    }
}

impl U16IntoU256 of Into<u16, u256> {
    fn into(self: u16) -> u256 {
        u256 { low: upcast(self), high: 0_u128 }
    }
}

impl U256TryIntoU16 of TryInto<u256, u16> {
    fn try_into(self: u256) -> Option<u16> {
        let u256{low: low, high: high } = self;

        if high != 0 {
            return Option::None;
        }

        low.try_into()
    }
}

impl U32IntoU256 of Into<u32, u256> {
    fn into(self: u32) -> u256 {
        u256 { low: upcast(self), high: 0_u128 }
    }
}

impl U256TryIntoU32 of TryInto<u256, u32> {
    fn try_into(self: u256) -> Option<u32> {
        let u256{low: low, high: high } = self;

        if high != 0 {
            return Option::None;
        }

        low.try_into()
    }
}

impl U64IntoU256 of Into<u64, u256> {
    fn into(self: u64) -> u256 {
        u256 { low: upcast(self), high: 0_u128 }
    }
}

impl U256TryIntoU64 of TryInto<u256, u64> {
    fn try_into(self: u256) -> Option<u64> {
        let u256{low: low, high: high } = self;

        if high != 0 {
            return Option::None;
        }

        low.try_into()
    }
}

impl U128IntoU256 of Into<u128, u256> {
    fn into(self: u128) -> u256 {
        u256 { low: self, high: 0_u128 }
    }
}

impl U256TryIntoU128 of TryInto<u256, u128> {
    fn try_into(self: u256) -> Option<u128> {
        let u256{low: low, high: high } = self;

        if high != 0 {
            return Option::None;
        }

        Option::Some(low)
    }
}

// === Zeroable ===

impl U8Zeroable of Zeroable<u8> {
    fn zero() -> u8 {
        0
    }
    #[inline(always)]
    fn is_zero(self: u8) -> bool {
        self == U8Zeroable::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: u8) -> bool {
        self != U8Zeroable::zero()
    }
}

impl U16Zeroable of Zeroable<u16> {
    fn zero() -> u16 {
        0
    }
    #[inline(always)]
    fn is_zero(self: u16) -> bool {
        self == U16Zeroable::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: u16) -> bool {
        self != U16Zeroable::zero()
    }
}

impl U32Zeroable of Zeroable<u32> {
    fn zero() -> u32 {
        0
    }
    #[inline(always)]
    fn is_zero(self: u32) -> bool {
        self == U32Zeroable::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: u32) -> bool {
        self != U32Zeroable::zero()
    }
}

impl U64Zeroable of Zeroable<u64> {
    fn zero() -> u64 {
        0
    }
    #[inline(always)]
    fn is_zero(self: u64) -> bool {
        self == U64Zeroable::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: u64) -> bool {
        self != U64Zeroable::zero()
    }
}

impl U128Zeroable of Zeroable<u128> {
    fn zero() -> u128 {
        0
    }
    #[inline(always)]
    fn is_zero(self: u128) -> bool {
        self == U128Zeroable::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: u128) -> bool {
        self != U128Zeroable::zero()
    }
}

impl U256Zeroable of Zeroable<u256> {
    fn zero() -> u256 {
        0
    }
    #[inline(always)]
    fn is_zero(self: u256) -> bool {
        self == U256Zeroable::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: u256) -> bool {
        self != U256Zeroable::zero()
    }
}

enum SignedIntegerResult<T> {
    InRange: T,
    Underflow: T,
    Overflow: T,
}
impl SignedIntegerResultDrop<T, impl TDrop: Drop<T>> of Drop<SignedIntegerResult<T>>;

#[derive(Copy, Drop)]
extern type i8;
impl NumericLiterali8 of NumericLiteral<i8>;
extern fn i8_const<value>() -> i8 nopanic;
extern fn i8_try_from_felt252(a: felt252) -> Option<i8> implicits(RangeCheck) nopanic;
extern fn i8_to_felt252(a: i8) -> felt252 nopanic;

extern fn i8_is_zero(a: i8) -> IsZeroResult<i8> implicits() nopanic;
extern fn i8_eq(lhs: i8, rhs: i8) -> bool implicits() nopanic;

impl I8PartialEq of PartialEq<i8> {
    #[inline(always)]
    fn eq(lhs: @i8, rhs: @i8) -> bool {
        i8_eq(*lhs, *rhs)
    }
    #[inline(always)]
    fn ne(lhs: @i8, rhs: @i8) -> bool {
        !(*lhs == *rhs)
    }
}

extern fn i8_overflowing_add_impl(
    lhs: i8, rhs: i8
) -> SignedIntegerResult<i8> implicits(RangeCheck) nopanic;
extern fn i8_overflowing_sub_impl(
    lhs: i8, rhs: i8
) -> SignedIntegerResult<i8> implicits(RangeCheck) nopanic;
impl I8Add of Add<i8> {
    fn add(lhs: i8, rhs: i8) -> i8 {
        match i8_overflowing_add_impl(lhs, rhs) {
            SignedIntegerResult::InRange(result) => result,
            SignedIntegerResult::Underflow(_) => panic_with_felt252('i8_add Underflow'),
            SignedIntegerResult::Overflow(_) => panic_with_felt252('i8_add Overflow'),
        }
    }
}
impl I8AddEq of AddEq<i8> {
    #[inline(always)]
    fn add_eq(ref self: i8, other: i8) {
        self = Add::add(self, other);
    }
}
impl I8Sub of Sub<i8> {
    fn sub(lhs: i8, rhs: i8) -> i8 {
        match i8_overflowing_sub_impl(lhs, rhs) {
            SignedIntegerResult::InRange(result) => result,
            SignedIntegerResult::Underflow(_) => panic_with_felt252('i8_sub Underflow'),
            SignedIntegerResult::Overflow(_) => panic_with_felt252('i8_sub Overflow'),
        }
    }
}
impl I8SubEq of SubEq<i8> {
    #[inline(always)]
    fn sub_eq(ref self: i8, other: i8) {
        self = Sub::sub(self, other);
    }
}

impl I8Neg of Neg<i8> {
    #[inline(always)]
    fn neg(a: i8) -> i8 {
        0 - a
    }
}

extern fn i8_wide_mul(lhs: i8, rhs: i8) -> i16 implicits() nopanic;
impl I8Mul of Mul<i8> {
    fn mul(lhs: i8, rhs: i8) -> i8 {
        i8_wide_mul(lhs, rhs).try_into().expect('i8_mul Overflow')
    }
}
impl I8MulEq of MulEq<i8> {
    #[inline(always)]
    fn mul_eq(ref self: i8, other: i8) {
        self = Mul::mul(self, other);
    }
}

/// If `lhs` >= `rhs` returns `Ok(lhs - rhs)` else returns `Err(2**8 + lhs - rhs)`.
extern fn i8_diff(lhs: i8, rhs: i8) -> Result<u8, u8> implicits(RangeCheck) nopanic;
impl I8PartialOrd of PartialOrd<i8> {
    #[inline(always)]
    fn le(lhs: i8, rhs: i8) -> bool {
        i8_diff(rhs, lhs).into_is_ok()
    }
    #[inline(always)]
    fn ge(lhs: i8, rhs: i8) -> bool {
        i8_diff(lhs, rhs).into_is_ok()
    }
    #[inline(always)]
    fn lt(lhs: i8, rhs: i8) -> bool {
        i8_diff(lhs, rhs).into_is_err()
    }
    #[inline(always)]
    fn gt(lhs: i8, rhs: i8) -> bool {
        i8_diff(rhs, lhs).into_is_err()
    }
}

#[derive(Copy, Drop)]
extern type i16;
impl NumericLiterali16 of NumericLiteral<i16>;
extern fn i16_const<value>() -> i16 nopanic;
extern fn i16_try_from_felt252(a: felt252) -> Option<i16> implicits(RangeCheck) nopanic;
extern fn i16_to_felt252(a: i16) -> felt252 nopanic;

extern fn i16_is_zero(a: i16) -> IsZeroResult<i16> implicits() nopanic;
extern fn i16_eq(lhs: i16, rhs: i16) -> bool implicits() nopanic;

impl I16PartialEq of PartialEq<i16> {
    #[inline(always)]
    fn eq(lhs: @i16, rhs: @i16) -> bool {
        i16_eq(*lhs, *rhs)
    }
    #[inline(always)]
    fn ne(lhs: @i16, rhs: @i16) -> bool {
        !(*lhs == *rhs)
    }
}

extern fn i16_overflowing_add_impl(
    lhs: i16, rhs: i16
) -> SignedIntegerResult<i16> implicits(RangeCheck) nopanic;
extern fn i16_overflowing_sub_impl(
    lhs: i16, rhs: i16
) -> SignedIntegerResult<i16> implicits(RangeCheck) nopanic;
impl I16Add of Add<i16> {
    fn add(lhs: i16, rhs: i16) -> i16 {
        match i16_overflowing_add_impl(lhs, rhs) {
            SignedIntegerResult::InRange(result) => result,
            SignedIntegerResult::Underflow(_) => panic_with_felt252('i16_add Underflow'),
            SignedIntegerResult::Overflow(_) => panic_with_felt252('i16_add Overflow'),
        }
    }
}
impl I16AddEq of AddEq<i16> {
    #[inline(always)]
    fn add_eq(ref self: i16, other: i16) {
        self = Add::add(self, other);
    }
}
impl I16Sub of Sub<i16> {
    fn sub(lhs: i16, rhs: i16) -> i16 {
        match i16_overflowing_sub_impl(lhs, rhs) {
            SignedIntegerResult::InRange(result) => result,
            SignedIntegerResult::Underflow(_) => panic_with_felt252('i16_sub Underflow'),
            SignedIntegerResult::Overflow(_) => panic_with_felt252('i16_sub Overflow'),
        }
    }
}
impl I16SubEq of SubEq<i16> {
    #[inline(always)]
    fn sub_eq(ref self: i16, other: i16) {
        self = Sub::sub(self, other);
    }
}

impl I16Neg of Neg<i16> {
    #[inline(always)]
    fn neg(a: i16) -> i16 {
        0 - a
    }
}

extern fn i16_wide_mul(lhs: i16, rhs: i16) -> i32 implicits() nopanic;
impl I16Mul of Mul<i16> {
    fn mul(lhs: i16, rhs: i16) -> i16 {
        i16_wide_mul(lhs, rhs).try_into().expect('i16_mul Overflow')
    }
}
impl I16MulEq of MulEq<i16> {
    #[inline(always)]
    fn mul_eq(ref self: i16, other: i16) {
        self = Mul::mul(self, other);
    }
}

/// If `lhs` >= `rhs` returns `Ok(lhs - rhs)` else returns `Err(2**16 + lhs - rhs)`.
extern fn i16_diff(lhs: i16, rhs: i16) -> Result<u16, u16> implicits(RangeCheck) nopanic;
impl I16PartialOrd of PartialOrd<i16> {
    #[inline(always)]
    fn le(lhs: i16, rhs: i16) -> bool {
        i16_diff(rhs, lhs).into_is_ok()
    }
    #[inline(always)]
    fn ge(lhs: i16, rhs: i16) -> bool {
        i16_diff(lhs, rhs).into_is_ok()
    }
    #[inline(always)]
    fn lt(lhs: i16, rhs: i16) -> bool {
        i16_diff(lhs, rhs).into_is_err()
    }
    #[inline(always)]
    fn gt(lhs: i16, rhs: i16) -> bool {
        i16_diff(rhs, lhs).into_is_err()
    }
}

#[derive(Copy, Drop)]
extern type i32;
impl NumericLiterali32 of NumericLiteral<i32>;
extern fn i32_const<value>() -> i32 nopanic;
extern fn i32_try_from_felt252(a: felt252) -> Option<i32> implicits(RangeCheck) nopanic;
extern fn i32_to_felt252(a: i32) -> felt252 nopanic;

extern fn i32_is_zero(a: i32) -> IsZeroResult<i32> implicits() nopanic;
extern fn i32_eq(lhs: i32, rhs: i32) -> bool implicits() nopanic;

impl I32PartialEq of PartialEq<i32> {
    #[inline(always)]
    fn eq(lhs: @i32, rhs: @i32) -> bool {
        i32_eq(*lhs, *rhs)
    }
    #[inline(always)]
    fn ne(lhs: @i32, rhs: @i32) -> bool {
        !(*lhs == *rhs)
    }
}

extern fn i32_overflowing_add_impl(
    lhs: i32, rhs: i32
) -> SignedIntegerResult<i32> implicits(RangeCheck) nopanic;
extern fn i32_overflowing_sub_impl(
    lhs: i32, rhs: i32
) -> SignedIntegerResult<i32> implicits(RangeCheck) nopanic;
impl I32Add of Add<i32> {
    fn add(lhs: i32, rhs: i32) -> i32 {
        match i32_overflowing_add_impl(lhs, rhs) {
            SignedIntegerResult::InRange(result) => result,
            SignedIntegerResult::Underflow(_) => panic_with_felt252('i32_add Underflow'),
            SignedIntegerResult::Overflow(_) => panic_with_felt252('i32_add Overflow'),
        }
    }
}
impl I32AddEq of AddEq<i32> {
    #[inline(always)]
    fn add_eq(ref self: i32, other: i32) {
        self = Add::add(self, other);
    }
}
impl I32Sub of Sub<i32> {
    fn sub(lhs: i32, rhs: i32) -> i32 {
        match i32_overflowing_sub_impl(lhs, rhs) {
            SignedIntegerResult::InRange(result) => result,
            SignedIntegerResult::Underflow(_) => panic_with_felt252('i32_sub Underflow'),
            SignedIntegerResult::Overflow(_) => panic_with_felt252('i32_sub Overflow'),
        }
    }
}
impl I32SubEq of SubEq<i32> {
    #[inline(always)]
    fn sub_eq(ref self: i32, other: i32) {
        self = Sub::sub(self, other);
    }
}

impl I32Neg of Neg<i32> {
    #[inline(always)]
    fn neg(a: i32) -> i32 {
        0 - a
    }
}

extern fn i32_wide_mul(lhs: i32, rhs: i32) -> i64 implicits() nopanic;
impl I32Mul of Mul<i32> {
    fn mul(lhs: i32, rhs: i32) -> i32 {
        i32_wide_mul(lhs, rhs).try_into().expect('i32_mul Overflow')
    }
}
impl I32MulEq of MulEq<i32> {
    #[inline(always)]
    fn mul_eq(ref self: i32, other: i32) {
        self = Mul::mul(self, other);
    }
}

/// If `lhs` >= `rhs` returns `Ok(lhs - rhs)` else returns `Err(2**32 + lhs - rhs)`.
extern fn i32_diff(lhs: i32, rhs: i32) -> Result<u32, u32> implicits(RangeCheck) nopanic;
impl I32PartialOrd of PartialOrd<i32> {
    #[inline(always)]
    fn le(lhs: i32, rhs: i32) -> bool {
        i32_diff(rhs, lhs).into_is_ok()
    }
    #[inline(always)]
    fn ge(lhs: i32, rhs: i32) -> bool {
        i32_diff(lhs, rhs).into_is_ok()
    }
    #[inline(always)]
    fn lt(lhs: i32, rhs: i32) -> bool {
        i32_diff(lhs, rhs).into_is_err()
    }
    #[inline(always)]
    fn gt(lhs: i32, rhs: i32) -> bool {
        i32_diff(rhs, lhs).into_is_err()
    }
}

#[derive(Copy, Drop)]
extern type i64;
impl NumericLiterali64 of NumericLiteral<i64>;
extern fn i64_const<value>() -> i64 nopanic;
extern fn i64_try_from_felt252(a: felt252) -> Option<i64> implicits(RangeCheck) nopanic;
extern fn i64_to_felt252(a: i64) -> felt252 nopanic;

extern fn i64_is_zero(a: i64) -> IsZeroResult<i64> implicits() nopanic;
extern fn i64_eq(lhs: i64, rhs: i64) -> bool implicits() nopanic;

impl I64PartialEq of PartialEq<i64> {
    #[inline(always)]
    fn eq(lhs: @i64, rhs: @i64) -> bool {
        i64_eq(*lhs, *rhs)
    }
    #[inline(always)]
    fn ne(lhs: @i64, rhs: @i64) -> bool {
        !(*lhs == *rhs)
    }
}

extern fn i64_overflowing_add_impl(
    lhs: i64, rhs: i64
) -> SignedIntegerResult<i64> implicits(RangeCheck) nopanic;
extern fn i64_overflowing_sub_impl(
    lhs: i64, rhs: i64
) -> SignedIntegerResult<i64> implicits(RangeCheck) nopanic;
impl I64Add of Add<i64> {
    fn add(lhs: i64, rhs: i64) -> i64 {
        match i64_overflowing_add_impl(lhs, rhs) {
            SignedIntegerResult::InRange(result) => result,
            SignedIntegerResult::Underflow(_) => panic_with_felt252('i64_add Underflow'),
            SignedIntegerResult::Overflow(_) => panic_with_felt252('i64_add Overflow'),
        }
    }
}
impl I64AddEq of AddEq<i64> {
    #[inline(always)]
    fn add_eq(ref self: i64, other: i64) {
        self = Add::add(self, other);
    }
}
impl I64Sub of Sub<i64> {
    fn sub(lhs: i64, rhs: i64) -> i64 {
        match i64_overflowing_sub_impl(lhs, rhs) {
            SignedIntegerResult::InRange(result) => result,
            SignedIntegerResult::Underflow(_) => panic_with_felt252('i64_sub Underflow'),
            SignedIntegerResult::Overflow(_) => panic_with_felt252('i64_sub Overflow'),
        }
    }
}
impl I64SubEq of SubEq<i64> {
    #[inline(always)]
    fn sub_eq(ref self: i64, other: i64) {
        self = Sub::sub(self, other);
    }
}

impl I64Neg of Neg<i64> {
    #[inline(always)]
    fn neg(a: i64) -> i64 {
        0 - a
    }
}

extern fn i64_wide_mul(lhs: i64, rhs: i64) -> i128 implicits() nopanic;
impl I64Mul of Mul<i64> {
    fn mul(lhs: i64, rhs: i64) -> i64 {
        i64_wide_mul(lhs, rhs).try_into().expect('i64_mul Overflow')
    }
}
impl I64MulEq of MulEq<i64> {
    #[inline(always)]
    fn mul_eq(ref self: i64, other: i64) {
        self = Mul::mul(self, other);
    }
}

/// If `lhs` >= `rhs` returns `Ok(lhs - rhs)` else returns `Err(2**64 + lhs - rhs)`.
extern fn i64_diff(lhs: i64, rhs: i64) -> Result<u64, u64> implicits(RangeCheck) nopanic;
impl I64PartialOrd of PartialOrd<i64> {
    #[inline(always)]
    fn le(lhs: i64, rhs: i64) -> bool {
        i64_diff(rhs, lhs).into_is_ok()
    }
    #[inline(always)]
    fn ge(lhs: i64, rhs: i64) -> bool {
        i64_diff(lhs, rhs).into_is_ok()
    }
    #[inline(always)]
    fn lt(lhs: i64, rhs: i64) -> bool {
        i64_diff(lhs, rhs).into_is_err()
    }
    #[inline(always)]
    fn gt(lhs: i64, rhs: i64) -> bool {
        i64_diff(rhs, lhs).into_is_err()
    }
}

#[derive(Copy, Drop)]
extern type i128;
impl NumericLiterali128 of NumericLiteral<i128>;
extern fn i128_const<value>() -> i128 nopanic;
extern fn i128_try_from_felt252(a: felt252) -> Option<i128> implicits(RangeCheck) nopanic;
extern fn i128_to_felt252(a: i128) -> felt252 nopanic;

extern fn i128_is_zero(a: i128) -> IsZeroResult<i128> implicits() nopanic;
extern fn i128_eq(lhs: i128, rhs: i128) -> bool implicits() nopanic;

impl I128PartialEq of PartialEq<i128> {
    #[inline(always)]
    fn eq(lhs: @i128, rhs: @i128) -> bool {
        i128_eq(*lhs, *rhs)
    }
    #[inline(always)]
    fn ne(lhs: @i128, rhs: @i128) -> bool {
        !(*lhs == *rhs)
    }
}

extern fn i128_overflowing_add_impl(
    lhs: i128, rhs: i128
) -> SignedIntegerResult<i128> implicits(RangeCheck) nopanic;
extern fn i128_overflowing_sub_impl(
    lhs: i128, rhs: i128
) -> SignedIntegerResult<i128> implicits(RangeCheck) nopanic;
impl I128Add of Add<i128> {
    fn add(lhs: i128, rhs: i128) -> i128 {
        match i128_overflowing_add_impl(lhs, rhs) {
            SignedIntegerResult::InRange(result) => result,
            SignedIntegerResult::Underflow(_) => panic_with_felt252('i128_add Underflow'),
            SignedIntegerResult::Overflow(_) => panic_with_felt252('i128_add Overflow'),
        }
    }
}
impl I128AddEq of AddEq<i128> {
    #[inline(always)]
    fn add_eq(ref self: i128, other: i128) {
        self = Add::add(self, other);
    }
}
impl I128Sub of Sub<i128> {
    fn sub(lhs: i128, rhs: i128) -> i128 {
        match i128_overflowing_sub_impl(lhs, rhs) {
            SignedIntegerResult::InRange(result) => result,
            SignedIntegerResult::Underflow(_) => panic_with_felt252('i128_sub Underflow'),
            SignedIntegerResult::Overflow(_) => panic_with_felt252('i128_sub Overflow'),
        }
    }
}
impl I128SubEq of SubEq<i128> {
    #[inline(always)]
    fn sub_eq(ref self: i128, other: i128) {
        self = Sub::sub(self, other);
    }
}

impl I128Neg of Neg<i128> {
    #[inline(always)]
    fn neg(a: i128) -> i128 {
        0 - a
    }
}

/// If `lhs` >= `rhs` returns `Ok(lhs - rhs)` else returns `Err(2**128 + lhs - rhs)`.
extern fn i128_diff(lhs: i128, rhs: i128) -> Result<u128, u128> implicits(RangeCheck) nopanic;
impl I128PartialOrd of PartialOrd<i128> {
    #[inline(always)]
    fn le(lhs: i128, rhs: i128) -> bool {
        i128_diff(rhs, lhs).into_is_ok()
    }
    #[inline(always)]
    fn ge(lhs: i128, rhs: i128) -> bool {
        i128_diff(lhs, rhs).into_is_ok()
    }
    #[inline(always)]
    fn lt(lhs: i128, rhs: i128) -> bool {
        i128_diff(lhs, rhs).into_is_err()
    }
    #[inline(always)]
    fn gt(lhs: i128, rhs: i128) -> bool {
        i128_diff(rhs, lhs).into_is_err()
    }
}
