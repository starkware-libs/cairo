use result::ResultTrait;
use result::ResultTraitImpl;
use option::OptionTrait;
use option::OptionTraitImpl;

#[derive(Copy, Drop)]
extern type u128;
extern fn u128_const<value>() -> u128 nopanic;

enum U128sFromFeltResult {
    Narrow: u128,
    Wide: (u128, u128),
}
extern fn u128s_from_felt(a: felt) -> U128sFromFeltResult implicits(RangeCheck) nopanic;

#[panic_with('u128_from OF', u128_from_felt)]
fn u128_try_from_felt(a: felt) -> Option::<u128> implicits(RangeCheck) nopanic {
    match u128s_from_felt(a) {
        U128sFromFeltResult::Narrow(x) => Option::<u128>::Some(x),
        U128sFromFeltResult::Wide(x) => Option::<u128>::None(()),
    }
}

extern fn u128_to_felt(a: u128) -> felt nopanic;

extern fn u128_overflowing_add(
    a: u128, b: u128
) -> Result::<u128, u128> implicits(RangeCheck) nopanic;
extern fn u128_overflowing_sub(
    a: u128, b: u128
) -> Result::<u128, u128> implicits(RangeCheck) nopanic;

fn u128_wrapping_add(a: u128, b: u128) -> u128 implicits(RangeCheck) nopanic {
    match u128_overflowing_add(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

extern fn u128_wide_mul(a: u128, b: u128) -> (u128, u128) implicits(RangeCheck) nopanic;

fn u128_overflowing_mul(a: u128, b: u128) -> (u128, bool) implicits(RangeCheck) nopanic {
    let (top_word, bottom_word) = u128_wide_mul(a, b);
    match u128_to_felt(top_word) {
        0 => (bottom_word, false),
        _ => (bottom_word, true),
    }
}


fn u128_checked_add(a: u128, b: u128) -> Option::<u128> implicits(RangeCheck) nopanic {
    match u128_overflowing_add(a, b) {
        Result::Ok(r) => Option::<u128>::Some(r),
        Result::Err(r) => Option::<u128>::None(()),
    }
}

impl U128Add of Add::<u128> {
    #[inline(always)]
    fn add(a: u128, b: u128) -> u128 {
        u128_overflowing_add(a, b).expect('u128_add Overflow')
    }
}

#[panic_with('u128_sub OF', u128_sub)]
fn u128_checked_sub(a: u128, b: u128) -> Option::<u128> implicits(RangeCheck) nopanic {
    match u128_overflowing_sub(a, b) {
        Result::Ok(r) => Option::<u128>::Some(r),
        Result::Err(r) => Option::<u128>::None(()),
    }
}

impl U128Sub of Sub::<u128> {
    #[inline(always)]
    fn sub(a: u128, b: u128) -> u128 {
        u128_overflowing_sub(a, b).expect('u128_sub Overflow')
    }
}

fn u128_checked_mul(a: u128, b: u128) -> Option::<u128> implicits(RangeCheck) nopanic {
    let (top_word, bottom_word) = u128_wide_mul(a, b);
    match u128_to_felt(top_word) {
        0 => Option::<u128>::Some(bottom_word),
        _ => Option::<u128>::None(()),
    }
}

impl U128Mul of Mul::<u128> {
    #[inline(always)]
    fn mul(a: u128, b: u128) -> u128 {
        u128_checked_mul(a, b).expect('u128_mul Overflow')
    }
}

impl NonZeroU128Copy of Copy::<NonZero::<u128>>;
impl NonZeroU128Drop of Drop::<NonZero::<u128>>;

#[panic_with('u128 is 0', u128_as_non_zero)]
fn u128_checked_as_non_zero(a: u128) -> Option::<NonZero::<u128>> implicits() nopanic {
    match u128_jump_nz(a) {
        JumpNzResult::Zero(()) => Option::<NonZero::<u128>>::None(()),
        JumpNzResult::NonZero(x) => Option::<NonZero::<u128>>::Some(x),
    }
}

fn u128_safe_div(a: u128, b: NonZero::<u128>) -> u128 implicits(RangeCheck) nopanic {
    let (q, r) = u128_safe_divmod(a, b);
    q
}

impl U128Div of Div::<u128> {
    #[inline(always)]
    fn div(a: u128, b: u128) -> u128 {
        u128_safe_div(a, u128_as_non_zero(b))
    }
}

fn u128_safe_mod(a: u128, b: NonZero::<u128>) -> u128 implicits(RangeCheck) nopanic {
    let (q, r) = u128_safe_divmod(a, b);
    r
}

impl U128Rem of Rem::<u128> {
    #[inline(always)]
    fn rem(a: u128, b: u128) -> u128 {
        u128_safe_mod(a, u128_as_non_zero(b))
    }
}

extern fn u128_safe_divmod(
    a: u128, b: NonZero::<u128>
) -> (u128, u128) implicits(RangeCheck) nopanic;

extern fn u128_lt(a: u128, b: u128) -> bool implicits(RangeCheck) nopanic;
extern fn u128_eq(a: u128, b: u128) -> bool implicits() nopanic;
extern fn u128_le(a: u128, b: u128) -> bool implicits(RangeCheck) nopanic;

impl U128PartialEq of PartialEq::<u128> {
    #[inline(always)]
    fn eq(a: u128, b: u128) -> bool {
        u128_eq(a, b)
    }
    #[inline(always)]
    fn ne(a: u128, b: u128) -> bool {
        !(a == b)
    }
}

impl U128PartialOrd of PartialOrd::<u128> {
    #[inline(always)]
    fn le(a: u128, b: u128) -> bool {
        u128_le(a, b)
    }
    #[inline(always)]
    fn ge(a: u128, b: u128) -> bool {
        u128_le(b, a)
    }
    #[inline(always)]
    fn lt(a: u128, b: u128) -> bool {
        u128_lt(a, b)
    }
    #[inline(always)]
    fn gt(a: u128, b: u128) -> bool {
        u128_lt(b, a)
    }
}

extern type Bitwise;
extern fn bitwise(a: u128, b: u128) -> (u128, u128, u128) implicits(Bitwise) nopanic;
impl U128BitAnd of BitAnd::<u128> {
    #[inline(always)]
    fn bitand(a: u128, b: u128) -> u128 {
        let (v, _, _) = bitwise(a, b);
        v
    }
}
impl U128BitXor of BitXor::<u128> {
    #[inline(always)]
    fn bitxor(a: u128, b: u128) -> u128 {
        let (_, v, _) = bitwise(a, b);
        v
    }
}
impl U128BitOr of BitOr::<u128> {
    #[inline(always)]
    fn bitor(a: u128, b: u128) -> u128 {
        let (_, _, v) = bitwise(a, b);
        v
    }
}

extern fn u128_jump_nz(a: u128) -> JumpNzResult::<u128> implicits() nopanic;

#[derive(Copy, Drop)]
extern type u8;
extern fn u8_const<value>() -> u8 nopanic;
extern fn u8_to_felt(a: u8) -> felt nopanic;

#[panic_with('u8_from OF', u8_from_felt)]
extern fn u8_try_from_felt(a: felt) -> Option::<u8> implicits(RangeCheck) nopanic;

extern fn u8_lt(a: u8, b: u8) -> bool implicits(RangeCheck) nopanic;
extern fn u8_eq(a: u8, b: u8) -> bool implicits() nopanic;
extern fn u8_le(a: u8, b: u8) -> bool implicits(RangeCheck) nopanic;

impl U8PartialEq of PartialEq::<u8> {
    #[inline(always)]
    fn eq(a: u8, b: u8) -> bool {
        u8_eq(a, b)
    }
    #[inline(always)]
    fn ne(a: u8, b: u8) -> bool {
        !(a == b)
    }
}

impl U8PartialOrd of PartialOrd::<u8> {
    #[inline(always)]
    fn le(a: u8, b: u8) -> bool {
        u8_le(a, b)
    }
    #[inline(always)]
    fn ge(a: u8, b: u8) -> bool {
        u8_le(b, a)
    }
    #[inline(always)]
    fn lt(a: u8, b: u8) -> bool {
        u8_lt(a, b)
    }
    #[inline(always)]
    fn gt(a: u8, b: u8) -> bool {
        u8_lt(b, a)
    }
}

extern fn u8_overflowing_add(a: u8, b: u8) -> Result::<u8, u8> implicits(RangeCheck) nopanic;
extern fn u8_overflowing_sub(a: u8, b: u8) -> Result::<u8, u8> implicits(RangeCheck) nopanic;

fn u8_wrapping_add(a: u8, b: u8) -> u8 implicits(RangeCheck) nopanic {
    match u8_overflowing_add(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u8_wrapping_sub(a: u8, b: u8) -> u8 implicits(RangeCheck) nopanic {
    match u8_overflowing_sub(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u8_checked_add(a: u8, b: u8) -> Option::<u8> implicits(RangeCheck) nopanic {
    match u8_overflowing_add(a, b) {
        Result::Ok(r) => Option::<u8>::Some(r),
        Result::Err(r) => Option::<u8>::None(()),
    }
}

impl U8Add of Add::<u8> {
    #[inline(always)]
    fn add(a: u8, b: u8) -> u8 {
        u8_overflowing_add(a, b).expect('u8_add Overflow')
    }
}

fn u8_checked_sub(a: u8, b: u8) -> Option::<u8> implicits(RangeCheck) nopanic {
    match u8_overflowing_sub(a, b) {
        Result::Ok(r) => Option::<u8>::Some(r),
        Result::Err(r) => Option::<u8>::None(()),
    }
}

impl U8Sub of Sub::<u8> {
    #[inline(always)]
    fn sub(a: u8, b: u8) -> u8 {
        u8_overflowing_sub(a, b).expect('u8_sub Overflow')
    }
}

#[derive(Copy, Drop)]
extern type u64;
extern fn u64_const<value>() -> u64 nopanic;
extern fn u64_to_felt(a: u64) -> felt nopanic;

#[panic_with('u64_from OF', u64_from_felt)]
extern fn u64_try_from_felt(a: felt) -> Option::<u64> implicits(RangeCheck) nopanic;

extern fn u64_lt(a: u64, b: u64) -> bool implicits(RangeCheck) nopanic;
extern fn u64_eq(a: u64, b: u64) -> bool implicits() nopanic;
extern fn u64_le(a: u64, b: u64) -> bool implicits(RangeCheck) nopanic;

impl U64PartialEq of PartialEq::<u64> {
    #[inline(always)]
    fn eq(a: u64, b: u64) -> bool {
        u64_eq(a, b)
    }
    #[inline(always)]
    fn ne(a: u64, b: u64) -> bool {
        !(a == b)
    }
}

impl U64PartialOrd of PartialOrd::<u64> {
    #[inline(always)]
    fn le(a: u64, b: u64) -> bool {
        u64_le(a, b)
    }
    #[inline(always)]
    fn ge(a: u64, b: u64) -> bool {
        u64_le(b, a)
    }
    #[inline(always)]
    fn lt(a: u64, b: u64) -> bool {
        u64_lt(a, b)
    }
    #[inline(always)]
    fn gt(a: u64, b: u64) -> bool {
        u64_lt(b, a)
    }
}

extern fn u64_overflowing_add(a: u64, b: u64) -> Result::<u64, u64> implicits(RangeCheck) nopanic;
extern fn u64_overflowing_sub(a: u64, b: u64) -> Result::<u64, u64> implicits(RangeCheck) nopanic;

fn u64_wrapping_add(a: u64, b: u64) -> u64 implicits(RangeCheck) nopanic {
    match u64_overflowing_add(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u64_wrapping_sub(a: u64, b: u64) -> u64 implicits(RangeCheck) nopanic {
    match u64_overflowing_sub(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u64_checked_add(a: u64, b: u64) -> Option::<u64> implicits(RangeCheck) nopanic {
    match u64_overflowing_add(a, b) {
        Result::Ok(r) => Option::<u64>::Some(r),
        Result::Err(r) => Option::<u64>::None(()),
    }
}

impl U64Add of Add::<u64> {
    #[inline(always)]
    fn add(a: u64, b: u64) -> u64 {
        u64_overflowing_add(a, b).expect('u64_add Overflow')
    }
}

fn u64_checked_sub(a: u64, b: u64) -> Option::<u64> implicits(RangeCheck) nopanic {
    match u64_overflowing_sub(a, b) {
        Result::Ok(r) => Option::<u64>::Some(r),
        Result::Err(r) => Option::<u64>::None(()),
    }
}

impl U64Sub of Sub::<u64> {
    #[inline(always)]
    fn sub(a: u64, b: u64) -> u64 {
        u64_overflowing_sub(a, b).expect('u64_sub Overflow')
    }
}

#[derive(Copy, Drop)]
struct u256 {
    low: u128,
    high: u128,
}

fn u256_overflowing_add(a: u256, b: u256) -> (u256, bool) implicits(RangeCheck) nopanic {
    let (high, overflow) = match u128_overflowing_add(a.high, b.high) {
        Result::Ok(high) => (high, false),
        Result::Err(high) => (high, true),
    };
    match u128_overflowing_add(a.low, b.low) {
        Result::Ok(low) => (u256 { low, high }, overflow),
        Result::Err(low) => {
            match u128_overflowing_add(high, 1_u128) {
                Result::Ok(high) => (u256 { low, high }, overflow),
                Result::Err(high) => (u256 { low, high }, true),
            }
        },
    }
}

fn u256_overflow_sub(a: u256, b: u256) -> (u256, bool) implicits(RangeCheck) nopanic {
    let (high, overflow) = match u128_overflowing_sub(a.high, b.high) {
        Result::Ok(high) => (high, false),
        Result::Err(high) => (high, true),
    };
    match u128_overflowing_sub(a.low, b.low) {
        Result::Ok(low) => (u256 { low, high }, overflow),
        Result::Err(low) => {
            match u128_overflowing_sub(high, 1_u128) {
                Result::Ok(high) => (u256 { low, high }, overflow),
                Result::Err(high) => (u256 { low, high }, true),
            }
        },
    }
}

fn u256_overflow_mul(a: u256, b: u256) -> (u256, bool) {
    let (high1, low) = u128_wide_mul(a.low, b.low);
    let (overflow_value1, high2) = u128_wide_mul(a.low, b.high);
    let (overflow_value2, high3) = u128_wide_mul(a.high, b.low);
    let (high, overflow) = match u128_overflowing_add(high1, high2) {
        Result::Ok(high) => (
            high,
            overflow_value1 != 0_u128 | overflow_value2 != 0_u128 | (a.high > 0_u128 & b.high > 0_u128)
        ),
        Result::Err(high) => (high, true),
    };
    let (high, overflow) = match u128_overflowing_add(high, high3) {
        Result::Ok(high) => (high, overflow),
        Result::Err(high) => (high, true),
    };
    (u256 { low, high }, overflow)
}

fn u256_checked_add(a: u256, b: u256) -> Option::<u256> implicits(RangeCheck) nopanic {
    let (r, overflow) = u256_overflowing_add(a, b);
    if overflow {
        Option::<u256>::None(())
    } else {
        Option::<u256>::Some(r)
    }
}

impl U256Add of Add::<u256> {
    #[inline(always)]
    fn add(a: u256, b: u256) -> u256 {
        u256_checked_add(a, b).expect('u256_add Overflow')
    }
}

#[panic_with('u256_sub OF', u256_sub)]
fn u256_checked_sub(a: u256, b: u256) -> Option::<u256> implicits(RangeCheck) nopanic {
    let (r, overflow) = u256_overflow_sub(a, b);
    if overflow {
        Option::<u256>::None(())
    } else {
        Option::<u256>::Some(r)
    }
}

impl U256Sub of Sub::<u256> {
    #[inline(always)]
    fn sub(a: u256, b: u256) -> u256 {
        u256_checked_sub(a, b).expect('u256_sub Overflow')
    }
}

fn u256_checked_mul(a: u256, b: u256) -> Option::<u256> implicits(RangeCheck) {
    let (r, overflow) = u256_overflow_mul(a, b);
    if overflow {
        Option::<u256>::None(())
    } else {
        Option::<u256>::Some(r)
    }
}

impl U256Mul of Mul::<u256> {
    #[inline(always)]
    fn mul(a: u256, b: u256) -> u256 {
        u256_checked_mul(a, b).expect('u256_mul Overflow')
    }
}

impl U256PartialEq of PartialEq::<u256> {
    #[inline(always)]
    fn eq(a: u256, b: u256) -> bool {
        a.low == b.low & a.high == b.high
    }
    #[inline(always)]
    fn ne(a: u256, b: u256) -> bool {
        !(a == b)
    }
}

impl U256PartialOrd of PartialOrd::<u256> {
    #[inline(always)]
    fn le(a: u256, b: u256) -> bool {
        !(b < a)
    }
    #[inline(always)]
    fn ge(a: u256, b: u256) -> bool {
        !(a < b)
    }
    fn lt(a: u256, b: u256) -> bool {
        if a.high < b.high {
            true
        } else if a.high == b.high {
            a.low < b.low
        } else {
            false
        }
    }
    #[inline(always)]
    fn gt(a: u256, b: u256) -> bool {
        b < a
    }
}

impl U256BitAnd of BitAnd::<u256> {
    #[inline(always)]
    fn bitand(a: u256, b: u256) -> u256 {
        u256 { low: a.low & b.low, high: a.high & b.high }
    }
}
impl U256BitXor of BitXor::<u256> {
    #[inline(always)]
    fn bitxor(a: u256, b: u256) -> u256 {
        u256 { low: a.low ^ b.low, high: a.high ^ b.high }
    }
}
impl U256BitOr of BitOr::<u256> {
    #[inline(always)]
    fn bitor(a: u256, b: u256) -> u256 {
        u256 { low: a.low | b.low, high: a.high | b.high }
    }
}

fn u256_from_felt(a: felt) -> u256 implicits(RangeCheck) nopanic {
    match u128s_from_felt(a) {
        U128sFromFeltResult::Narrow(low) => u256 { low, high: 0_u128 },
        U128sFromFeltResult::Wide((high, low)) => u256 { low, high },
    }
}
