use zeroable::IsZeroResult;
use option::OptionTrait;
use result::ResultTrait;
use traits::Into;
use traits::TryInto;
use traits::Default;
use traits::Felt252DictValue;

// TODO(spapini): Add method for const creation from Integer.
trait NumericLiteral<T>;
impl NumericLiteralfelt252 of NumericLiteral<felt252>;

#[derive(Copy, Drop)]
extern type u128;
impl NumericLiteralu128 of NumericLiteral<u128>;
extern fn u128_const<value>() -> u128 nopanic;

enum U128sFromFelt252Result {
    Narrow: u128,
    Wide: (u128, u128),
}
extern fn u128s_from_felt252(a: felt252) -> U128sFromFelt252Result implicits(RangeCheck) nopanic;

#[panic_with('u128_from Overflow', u128_from_felt252)]
fn u128_try_from_felt252(a: felt252) -> Option<u128> implicits(RangeCheck) nopanic {
    match u128s_from_felt252(a) {
        U128sFromFelt252Result::Narrow(x) => Option::Some(x),
        U128sFromFelt252Result::Wide(x) => Option::None(()),
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

extern fn u128_wide_mul(lhs: u128, rhs: u128) -> (u128, u128) implicits(RangeCheck) nopanic;
extern fn u128_sqrt(value: u128) -> u128 implicits(RangeCheck) nopanic;

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
        Result::Err(r) => Option::None(()),
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
        Result::Err(r) => Option::None(()),
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
        _ => Option::None(()),
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
fn u128_try_as_non_zero(a: u128) -> Option<NonZero<u128>> implicits() nopanic {
    match u128_is_zero(a) {
        IsZeroResult::Zero(()) => Option::None(()),
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U128Div of Div<u128> {
    fn div(lhs: u128, rhs: u128) -> u128 {
        let (q, r) = u128_safe_divmod(lhs, u128_as_non_zero(rhs));
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
        let (q, r) = u128_safe_divmod(lhs, u128_as_non_zero(rhs));
        r
    }
}
impl U128RemEq of RemEq<u128> {
    #[inline(always)]
    fn rem_eq(ref self: u128, other: u128) {
        self = Rem::rem(self, other);
    }
}

extern fn u128_safe_divmod(
    lhs: u128, rhs: NonZero<u128>
) -> (u128, u128) implicits(RangeCheck) nopanic;

extern fn u128_lt(lhs: u128, rhs: u128) -> bool implicits(RangeCheck) nopanic;
extern fn u128_eq(lhs: u128, rhs: u128) -> bool implicits() nopanic;
extern fn u128_le(lhs: u128, rhs: u128) -> bool implicits(RangeCheck) nopanic;

impl U128PartialEq of PartialEq<u128> {
    #[inline(always)]
    fn eq(lhs: u128, rhs: u128) -> bool {
        u128_eq(lhs, rhs)
    }
    #[inline(always)]
    fn ne(lhs: u128, rhs: u128) -> bool {
        !(lhs == rhs)
    }
}

impl U128PartialOrd of PartialOrd<u128> {
    #[inline(always)]
    fn le(lhs: u128, rhs: u128) -> bool {
        u128_le(lhs, rhs)
    }
    #[inline(always)]
    fn ge(lhs: u128, rhs: u128) -> bool {
        u128_le(rhs, lhs)
    }
    #[inline(always)]
    fn lt(lhs: u128, rhs: u128) -> bool {
        u128_lt(lhs, rhs)
    }
    #[inline(always)]
    fn gt(lhs: u128, rhs: u128) -> bool {
        u128_lt(rhs, lhs)
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

extern fn u128_is_zero(a: u128) -> IsZeroResult<u128> implicits() nopanic;

#[derive(Copy, Drop)]
extern type u8;
impl NumericLiteralu8 of NumericLiteral<u8>;
extern fn u8_const<value>() -> u8 nopanic;
extern fn u8_to_felt252(a: u8) -> felt252 nopanic;

#[panic_with('u8_from Overflow', u8_from_felt252)]
extern fn u8_try_from_felt252(a: felt252) -> Option<u8> implicits(RangeCheck) nopanic;

extern fn u8_lt(lhs: u8, rhs: u8) -> bool implicits(RangeCheck) nopanic;
extern fn u8_eq(lhs: u8, rhs: u8) -> bool implicits() nopanic;
extern fn u8_le(lhs: u8, rhs: u8) -> bool implicits(RangeCheck) nopanic;

impl U8PartialEq of PartialEq<u8> {
    #[inline(always)]
    fn eq(lhs: u8, rhs: u8) -> bool {
        u8_eq(lhs, rhs)
    }
    #[inline(always)]
    fn ne(lhs: u8, rhs: u8) -> bool {
        !(lhs == rhs)
    }
}

impl U8PartialOrd of PartialOrd<u8> {
    #[inline(always)]
    fn le(lhs: u8, rhs: u8) -> bool {
        u8_le(lhs, rhs)
    }
    #[inline(always)]
    fn ge(lhs: u8, rhs: u8) -> bool {
        u8_le(rhs, lhs)
    }
    #[inline(always)]
    fn lt(lhs: u8, rhs: u8) -> bool {
        u8_lt(lhs, rhs)
    }
    #[inline(always)]
    fn gt(lhs: u8, rhs: u8) -> bool {
        u8_lt(rhs, lhs)
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
        Result::Err(r) => Option::None(()),
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
        Result::Err(r) => Option::None(()),
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
impl U8Mul of Mul<u8> {
    fn mul(lhs: u8, rhs: u8) -> u8 {
        u8_try_from_felt252(u16_to_felt252(u8_wide_mul(lhs, rhs))).expect('u8_mul Overflow')
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
fn u8_try_as_non_zero(a: u8) -> Option<NonZero<u8>> implicits() nopanic {
    match u8_is_zero(a) {
        IsZeroResult::Zero(()) => Option::None(()),
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U8Div of Div<u8> {
    fn div(lhs: u8, rhs: u8) -> u8 {
        let (q, r) = u8_safe_divmod(lhs, u8_as_non_zero(rhs));
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
        let (q, r) = u8_safe_divmod(lhs, u8_as_non_zero(rhs));
        r
    }
}
impl U8RemEq of RemEq<u8> {
    #[inline(always)]
    fn rem_eq(ref self: u8, other: u8) {
        self = Rem::rem(self, other);
    }
}

#[derive(Copy, Drop)]
extern type u16;
impl NumericLiteralu16 of NumericLiteral<u16>;
extern fn u16_const<value>() -> u16 nopanic;
extern fn u16_to_felt252(a: u16) -> felt252 nopanic;

#[panic_with('u16_from Overflow', u16_from_felt252)]
extern fn u16_try_from_felt252(a: felt252) -> Option<u16> implicits(RangeCheck) nopanic;

extern fn u16_lt(lhs: u16, rhs: u16) -> bool implicits(RangeCheck) nopanic;
extern fn u16_eq(lhs: u16, rhs: u16) -> bool implicits() nopanic;
extern fn u16_le(lhs: u16, rhs: u16) -> bool implicits(RangeCheck) nopanic;

impl U16PartialEq of PartialEq<u16> {
    #[inline(always)]
    fn eq(lhs: u16, rhs: u16) -> bool {
        u16_eq(lhs, rhs)
    }
    #[inline(always)]
    fn ne(lhs: u16, rhs: u16) -> bool {
        !(lhs == rhs)
    }
}

impl U16PartialOrd of PartialOrd<u16> {
    #[inline(always)]
    fn le(lhs: u16, rhs: u16) -> bool {
        u16_le(lhs, rhs)
    }
    #[inline(always)]
    fn ge(lhs: u16, rhs: u16) -> bool {
        u16_le(rhs, lhs)
    }
    #[inline(always)]
    fn lt(lhs: u16, rhs: u16) -> bool {
        u16_lt(lhs, rhs)
    }
    #[inline(always)]
    fn gt(lhs: u16, rhs: u16) -> bool {
        u16_lt(rhs, lhs)
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
        Result::Err(r) => Option::None(()),
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
        Result::Err(r) => Option::None(()),
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
impl U16Mul of Mul<u16> {
    fn mul(lhs: u16, rhs: u16) -> u16 {
        // TODO(orizi): Use direct conversion, instead of going through felt252.
        u16_try_from_felt252(u32_to_felt252(u16_wide_mul(lhs, rhs))).expect('u16_mul Overflow')
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
fn u16_try_as_non_zero(a: u16) -> Option<NonZero<u16>> implicits() nopanic {
    match u16_is_zero(a) {
        IsZeroResult::Zero(()) => Option::None(()),
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U16Div of Div<u16> {
    fn div(lhs: u16, rhs: u16) -> u16 {
        let (q, r) = u16_safe_divmod(lhs, u16_as_non_zero(rhs));
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
        let (q, r) = u16_safe_divmod(lhs, u16_as_non_zero(rhs));
        r
    }
}
impl U16RemEq of RemEq<u16> {
    #[inline(always)]
    fn rem_eq(ref self: u16, other: u16) {
        self = Rem::rem(self, other);
    }
}

#[derive(Copy, Drop)]
extern type u32;
impl NumericLiteralu32 of NumericLiteral<u32>;
extern fn u32_const<value>() -> u32 nopanic;
extern fn u32_to_felt252(a: u32) -> felt252 nopanic;

#[panic_with('u32_from Overflow', u32_from_felt252)]
extern fn u32_try_from_felt252(a: felt252) -> Option<u32> implicits(RangeCheck) nopanic;

extern fn u32_lt(lhs: u32, rhs: u32) -> bool implicits(RangeCheck) nopanic;
extern fn u32_eq(lhs: u32, rhs: u32) -> bool implicits() nopanic;
extern fn u32_le(lhs: u32, rhs: u32) -> bool implicits(RangeCheck) nopanic;

impl U32PartialEq of PartialEq<u32> {
    #[inline(always)]
    fn eq(lhs: u32, rhs: u32) -> bool {
        u32_eq(lhs, rhs)
    }
    #[inline(always)]
    fn ne(lhs: u32, rhs: u32) -> bool {
        !(lhs == rhs)
    }
}

impl U32PartialOrd of PartialOrd<u32> {
    #[inline(always)]
    fn le(lhs: u32, rhs: u32) -> bool {
        u32_le(lhs, rhs)
    }
    #[inline(always)]
    fn ge(lhs: u32, rhs: u32) -> bool {
        u32_le(rhs, lhs)
    }
    #[inline(always)]
    fn lt(lhs: u32, rhs: u32) -> bool {
        u32_lt(lhs, rhs)
    }
    #[inline(always)]
    fn gt(lhs: u32, rhs: u32) -> bool {
        u32_lt(rhs, lhs)
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
        Result::Err(r) => Option::None(()),
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
        Result::Err(r) => Option::None(()),
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
impl U32Mul of Mul<u32> {
    fn mul(lhs: u32, rhs: u32) -> u32 {
        // TODO(orizi): Use direct conversion, instead of going through felt252.
        u32_try_from_felt252(u64_to_felt252(u32_wide_mul(lhs, rhs))).expect('u32_mul Overflow')
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
fn u32_try_as_non_zero(a: u32) -> Option<NonZero<u32>> implicits() nopanic {
    match u32_is_zero(a) {
        IsZeroResult::Zero(()) => Option::None(()),
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U32Div of Div<u32> {
    fn div(lhs: u32, rhs: u32) -> u32 {
        let (q, r) = u32_safe_divmod(lhs, u32_as_non_zero(rhs));
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
        let (q, r) = u32_safe_divmod(lhs, u32_as_non_zero(rhs));
        r
    }
}
impl U32RemEq of RemEq<u32> {
    #[inline(always)]
    fn rem_eq(ref self: u32, other: u32) {
        self = Rem::rem(self, other);
    }
}

#[derive(Copy, Drop)]
extern type u64;
impl NumericLiteralu64 of NumericLiteral<u64>;
extern fn u64_const<value>() -> u64 nopanic;
extern fn u64_to_felt252(a: u64) -> felt252 nopanic;

#[panic_with('u64_from Overflow', u64_from_felt252)]
extern fn u64_try_from_felt252(a: felt252) -> Option<u64> implicits(RangeCheck) nopanic;

extern fn u64_lt(lhs: u64, rhs: u64) -> bool implicits(RangeCheck) nopanic;
extern fn u64_eq(lhs: u64, rhs: u64) -> bool implicits() nopanic;
extern fn u64_le(lhs: u64, rhs: u64) -> bool implicits(RangeCheck) nopanic;

impl U64PartialEq of PartialEq<u64> {
    #[inline(always)]
    fn eq(lhs: u64, rhs: u64) -> bool {
        u64_eq(lhs, rhs)
    }
    #[inline(always)]
    fn ne(lhs: u64, rhs: u64) -> bool {
        !(lhs == rhs)
    }
}

impl U64PartialOrd of PartialOrd<u64> {
    #[inline(always)]
    fn le(lhs: u64, rhs: u64) -> bool {
        u64_le(lhs, rhs)
    }
    #[inline(always)]
    fn ge(lhs: u64, rhs: u64) -> bool {
        u64_le(rhs, lhs)
    }
    #[inline(always)]
    fn lt(lhs: u64, rhs: u64) -> bool {
        u64_lt(lhs, rhs)
    }
    #[inline(always)]
    fn gt(lhs: u64, rhs: u64) -> bool {
        u64_lt(rhs, lhs)
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
        Result::Err(r) => Option::None(()),
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
        Result::Err(r) => Option::None(()),
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
impl U64Mul of Mul<u64> {
    fn mul(lhs: u64, rhs: u64) -> u64 {
        // TODO(orizi): Use direct conversion, instead of going through felt252.
        u64_try_from_felt252(u128_to_felt252(u64_wide_mul(lhs, rhs))).expect('u64_mul Overflow')
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
fn u64_try_as_non_zero(a: u64) -> Option<NonZero<u64>> implicits() nopanic {
    match u64_is_zero(a) {
        IsZeroResult::Zero(()) => Option::None(()),
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U64Div of Div<u64> {
    fn div(lhs: u64, rhs: u64) -> u64 {
        let (q, r) = u64_safe_divmod(lhs, u64_as_non_zero(rhs));
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
        let (q, r) = u64_safe_divmod(lhs, u64_as_non_zero(rhs));
        r
    }
}
impl U64RemEq of RemEq<u64> {
    #[inline(always)]
    fn rem_eq(ref self: u64, other: u64) {
        self = Rem::rem(self, other);
    }
}

#[derive(Copy, Drop, PartialEq, Serde)]
struct u256 {
    low: u128,
    high: u128,
}

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
            overflow_value1 != 0_u128 | overflow_value2 != 0_u128 | (lhs.high > 0_u128 & rhs.high > 0_u128)
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
        Option::None(())
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
        Option::None(())
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
        Option::None(())
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
extern fn u256_safe_divmod(
    lhs: u256, rhs: NonZero<u256>
) -> (u256, u256) implicits(RangeCheck) nopanic;

#[panic_with('u256 is 0', u256_as_non_zero)]
fn u256_try_as_non_zero(a: u256) -> Option<NonZero<u256>> implicits() nopanic {
    match u256_is_zero(a) {
        IsZeroResult::Zero(()) => Option::None(()),
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U256Div of Div<u256> {
    fn div(lhs: u256, rhs: u256) -> u256 {
        let (q, r) = u256_safe_divmod(lhs, u256_as_non_zero(rhs));
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
        let (q, r) = u256_safe_divmod(lhs, u256_as_non_zero(rhs));
        r
    }
}
impl U256RemEq of RemEq<u256> {
    #[inline(always)]
    fn rem_eq(ref self: u256, other: u256) {
        self = Rem::rem(self, other);
    }
}


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
        u256 { low: 0_u128, high: 0_u128 }
    }
    #[inline(always)]
    fn max() -> u256 nopanic {
        u256 { low: BoundedInt::max(), high: BoundedInt::max() }
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
impl U16TryIntoU8 of TryInto<u16, u8> {
    fn try_into(self: u16) -> Option<u8> {
        // TODO(orizi): Use direct conversion, instead of going through felt252.
        let as_felt252: felt252 = self.into();
        as_felt252.try_into()
    }
}
impl U32TryIntoU16 of TryInto<u32, u16> {
    fn try_into(self: u32) -> Option<u16> {
        // TODO(orizi): Use direct conversion, instead of going through felt252.
        let as_felt: felt252 = self.into();
        as_felt.try_into()
    }
}
impl U64TryIntoU32 of TryInto<u64, u32> {
    fn try_into(self: u64) -> Option<u32> {
        // TODO(orizi): Use direct conversion, instead of going through felt252.
        let as_felt: felt252 = self.into();
        as_felt.try_into()
    }
}
impl U128TryIntoU64 of TryInto<u128, u64> {
    fn try_into(self: u128) -> Option<u64> {
        // TODO(orizi): Use direct conversion, instead of going through felt252.
        let as_felt: felt252 = self.into();
        as_felt.try_into()
    }
}

// TODO(lior): Restrict the function (using traits) in the high-level compiler so that wrong types
//   will not lead to Sierra errors.
extern fn upcast<FromType, ToType>(x: FromType) -> ToType nopanic;

// TODO(lior): Restrict the function (using traits) in the high-level compiler so that wrong types
//   will not lead to Sierra errors.
extern fn downcast<FromType, ToType>(x: FromType) -> Option<ToType> implicits(RangeCheck) nopanic;

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
        u256 { low: 0_u128, high: 0_u128 }
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
