use result::ResultTrait;
use result::ResultTraitImpl;
use option::OptionTrait;
use option::OptionTraitImpl;
use traits::Into;
use traits::TryInto;

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
extern fn u128_sqrt(value: u128) -> u128 implicits(RangeCheck) nopanic;

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
    fn add(a: u128, b: u128) -> u128 {
        u128_overflowing_add(a, b).expect('u128_add Overflow')
    }
}
impl U128AddEq of AddEq::<u128> {
    #[inline(always)]
    fn add_eq(ref self: u128, other: u128) {
        self = Add::add(self, other);
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
    fn sub(a: u128, b: u128) -> u128 {
        u128_overflowing_sub(a, b).expect('u128_sub Overflow')
    }
}
impl U128SubEq of SubEq::<u128> {
    #[inline(always)]
    fn sub_eq(ref self: u128, other: u128) {
        self = Sub::sub(self, other);
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
    fn mul(a: u128, b: u128) -> u128 {
        u128_checked_mul(a, b).expect('u128_mul Overflow')
    }
}
impl U128MulEq of MulEq::<u128> {
    #[inline(always)]
    fn mul_eq(ref self: u128, other: u128) {
        self = Mul::mul(self, other);
    }
}

impl NonZeroU128Copy of Copy::<NonZero::<u128>>;
impl NonZeroU128Drop of Drop::<NonZero::<u128>>;

#[panic_with('u128 is 0', u128_as_non_zero)]
fn u128_try_as_non_zero(a: u128) -> Option::<NonZero::<u128>> implicits() nopanic {
    match u128_is_zero(a) {
        IsZeroResult::Zero(()) => Option::<NonZero::<u128>>::None(()),
        IsZeroResult::NonZero(x) => Option::<NonZero::<u128>>::Some(x),
    }
}

impl U128Div of Div::<u128> {
    fn div(a: u128, b: u128) -> u128 {
        let (q, r) = u128_safe_divmod(a, u128_as_non_zero(b));
        q
    }
}
impl U128DivEq of DivEq::<u128> {
    #[inline(always)]
    fn div_eq(ref self: u128, other: u128) {
        self = Div::div(self, other);
    }
}

impl U128Rem of Rem::<u128> {
    fn rem(a: u128, b: u128) -> u128 {
        let (q, r) = u128_safe_divmod(a, u128_as_non_zero(b));
        r
    }
}
impl U128RemEq of RemEq::<u128> {
    #[inline(always)]
    fn rem_eq(ref self: u128, other: u128) {
        self = Rem::rem(self, other);
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

extern fn u128_is_zero(a: u128) -> IsZeroResult::<u128> implicits() nopanic;

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
    fn add(a: u8, b: u8) -> u8 {
        u8_overflowing_add(a, b).expect('u8_add Overflow')
    }
}
impl U8AddEq of AddEq::<u8> {
    #[inline(always)]
    fn add_eq(ref self: u8, other: u8) {
        self = Add::add(self, other);
    }
}

fn u8_checked_sub(a: u8, b: u8) -> Option::<u8> implicits(RangeCheck) nopanic {
    match u8_overflowing_sub(a, b) {
        Result::Ok(r) => Option::<u8>::Some(r),
        Result::Err(r) => Option::<u8>::None(()),
    }
}

impl U8Sub of Sub::<u8> {
    fn sub(a: u8, b: u8) -> u8 {
        u8_overflowing_sub(a, b).expect('u8_sub Overflow')
    }
}
impl U8SubEq of SubEq::<u8> {
    #[inline(always)]
    fn sub_eq(ref self: u8, other: u8) {
        self = Sub::sub(self, other);
    }
}

extern fn u8_wide_mul(a: u8, b: u8) -> u16 implicits() nopanic;
impl U8Mul of Mul::<u8> {
    fn mul(a: u8, b: u8) -> u8 {
        u8_try_from_felt(u16_to_felt(u8_wide_mul(a, b))).expect('u8_mul Overflow')
    }
}

extern fn u8_is_zero(a: u8) -> IsZeroResult::<u8> implicits() nopanic;
extern fn u8_safe_divmod(a: u8, b: NonZero::<u8>) -> (u8, u8) implicits(RangeCheck) nopanic;

#[panic_with('u8 is 0', u8_as_non_zero)]
fn u8_try_as_non_zero(a: u8) -> Option::<NonZero::<u8>> implicits() nopanic {
    match u8_is_zero(a) {
        IsZeroResult::Zero(()) => Option::None(()),
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U8Div of Div::<u8> {
    fn div(a: u8, b: u8) -> u8 {
        let (q, r) = u8_safe_divmod(a, u8_as_non_zero(b));
        q
    }
}

impl U8Rem of Rem::<u8> {
    fn rem(a: u8, b: u8) -> u8 {
        let (q, r) = u8_safe_divmod(a, u8_as_non_zero(b));
        r
    }
}

#[derive(Copy, Drop)]
extern type u16;
extern fn u16_const<value>() -> u16 nopanic;
extern fn u16_to_felt(a: u16) -> felt nopanic;

#[panic_with('u16_from OF', u16_from_felt)]
extern fn u16_try_from_felt(a: felt) -> Option::<u16> implicits(RangeCheck) nopanic;

extern fn u16_lt(a: u16, b: u16) -> bool implicits(RangeCheck) nopanic;
extern fn u16_eq(a: u16, b: u16) -> bool implicits() nopanic;
extern fn u16_le(a: u16, b: u16) -> bool implicits(RangeCheck) nopanic;

impl U16PartialEq of PartialEq::<u16> {
    #[inline(always)]
    fn eq(a: u16, b: u16) -> bool {
        u16_eq(a, b)
    }
    #[inline(always)]
    fn ne(a: u16, b: u16) -> bool {
        !(a == b)
    }
}

impl U16PartialOrd of PartialOrd::<u16> {
    #[inline(always)]
    fn le(a: u16, b: u16) -> bool {
        u16_le(a, b)
    }
    #[inline(always)]
    fn ge(a: u16, b: u16) -> bool {
        u16_le(b, a)
    }
    #[inline(always)]
    fn lt(a: u16, b: u16) -> bool {
        u16_lt(a, b)
    }
    #[inline(always)]
    fn gt(a: u16, b: u16) -> bool {
        u16_lt(b, a)
    }
}

extern fn u16_overflowing_add(a: u16, b: u16) -> Result::<u16, u16> implicits(RangeCheck) nopanic;
extern fn u16_overflowing_sub(a: u16, b: u16) -> Result::<u16, u16> implicits(RangeCheck) nopanic;

fn u16_wrapping_add(a: u16, b: u16) -> u16 implicits(RangeCheck) nopanic {
    match u16_overflowing_add(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u16_wrapping_sub(a: u16, b: u16) -> u16 implicits(RangeCheck) nopanic {
    match u16_overflowing_sub(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u16_checked_add(a: u16, b: u16) -> Option::<u16> implicits(RangeCheck) nopanic {
    match u16_overflowing_add(a, b) {
        Result::Ok(r) => Option::<u16>::Some(r),
        Result::Err(r) => Option::<u16>::None(()),
    }
}

impl U16Add of Add::<u16> {
    fn add(a: u16, b: u16) -> u16 {
        u16_overflowing_add(a, b).expect('u16_add Overflow')
    }
}

fn u16_checked_sub(a: u16, b: u16) -> Option::<u16> implicits(RangeCheck) nopanic {
    match u16_overflowing_sub(a, b) {
        Result::Ok(r) => Option::<u16>::Some(r),
        Result::Err(r) => Option::<u16>::None(()),
    }
}

impl U16Sub of Sub::<u16> {
    fn sub(a: u16, b: u16) -> u16 {
        u16_overflowing_sub(a, b).expect('u16_sub Overflow')
    }
}

extern fn u16_wide_mul(a: u16, b: u16) -> u32 implicits() nopanic;
impl U16Mul of Mul::<u16> {
    fn mul(a: u16, b: u16) -> u16 {
        // TODO(orizi): Use direct conversion, instead of going through felt.
        u16_try_from_felt(u32_to_felt(u16_wide_mul(a, b))).expect('u16_mul Overflow')
    }
}

extern fn u16_is_zero(a: u16) -> IsZeroResult::<u16> implicits() nopanic;
extern fn u16_safe_divmod(a: u16, b: NonZero::<u16>) -> (u16, u16) implicits(RangeCheck) nopanic;

#[panic_with('u16 is 0', u16_as_non_zero)]
fn u16_try_as_non_zero(a: u16) -> Option::<NonZero::<u16>> implicits() nopanic {
    match u16_is_zero(a) {
        IsZeroResult::Zero(()) => Option::None(()),
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U16Div of Div::<u16> {
    fn div(a: u16, b: u16) -> u16 {
        let (q, r) = u16_safe_divmod(a, u16_as_non_zero(b));
        q
    }
}

impl U16Rem of Rem::<u16> {
    fn rem(a: u16, b: u16) -> u16 {
        let (q, r) = u16_safe_divmod(a, u16_as_non_zero(b));
        r
    }
}

#[derive(Copy, Drop)]
extern type u32;
extern fn u32_const<value>() -> u32 nopanic;
extern fn u32_to_felt(a: u32) -> felt nopanic;

#[panic_with('u32_from OF', u32_from_felt)]
extern fn u32_try_from_felt(a: felt) -> Option::<u32> implicits(RangeCheck) nopanic;

extern fn u32_lt(a: u32, b: u32) -> bool implicits(RangeCheck) nopanic;
extern fn u32_eq(a: u32, b: u32) -> bool implicits() nopanic;
extern fn u32_le(a: u32, b: u32) -> bool implicits(RangeCheck) nopanic;

impl U32PartialEq of PartialEq::<u32> {
    #[inline(always)]
    fn eq(a: u32, b: u32) -> bool {
        u32_eq(a, b)
    }
    #[inline(always)]
    fn ne(a: u32, b: u32) -> bool {
        !(a == b)
    }
}

impl U32PartialOrd of PartialOrd::<u32> {
    #[inline(always)]
    fn le(a: u32, b: u32) -> bool {
        u32_le(a, b)
    }
    #[inline(always)]
    fn ge(a: u32, b: u32) -> bool {
        u32_le(b, a)
    }
    #[inline(always)]
    fn lt(a: u32, b: u32) -> bool {
        u32_lt(a, b)
    }
    #[inline(always)]
    fn gt(a: u32, b: u32) -> bool {
        u32_lt(b, a)
    }
}

extern fn u32_overflowing_add(a: u32, b: u32) -> Result::<u32, u32> implicits(RangeCheck) nopanic;
extern fn u32_overflowing_sub(a: u32, b: u32) -> Result::<u32, u32> implicits(RangeCheck) nopanic;

fn u32_wrapping_add(a: u32, b: u32) -> u32 implicits(RangeCheck) nopanic {
    match u32_overflowing_add(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u32_wrapping_sub(a: u32, b: u32) -> u32 implicits(RangeCheck) nopanic {
    match u32_overflowing_sub(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

fn u32_checked_add(a: u32, b: u32) -> Option::<u32> implicits(RangeCheck) nopanic {
    match u32_overflowing_add(a, b) {
        Result::Ok(r) => Option::<u32>::Some(r),
        Result::Err(r) => Option::<u32>::None(()),
    }
}

impl U32Add of Add::<u32> {
    fn add(a: u32, b: u32) -> u32 {
        u32_overflowing_add(a, b).expect('u32_add Overflow')
    }
}

fn u32_checked_sub(a: u32, b: u32) -> Option::<u32> implicits(RangeCheck) nopanic {
    match u32_overflowing_sub(a, b) {
        Result::Ok(r) => Option::<u32>::Some(r),
        Result::Err(r) => Option::<u32>::None(()),
    }
}

impl U32Sub of Sub::<u32> {
    fn sub(a: u32, b: u32) -> u32 {
        u32_overflowing_sub(a, b).expect('u32_sub Overflow')
    }
}

extern fn u32_wide_mul(a: u32, b: u32) -> u64 implicits() nopanic;
impl U32Mul of Mul::<u32> {
    fn mul(a: u32, b: u32) -> u32 {
        // TODO(orizi): Use direct conversion, instead of going through felt.
        u32_try_from_felt(u64_to_felt(u32_wide_mul(a, b))).expect('u32_mul Overflow')
    }
}

extern fn u32_is_zero(a: u32) -> IsZeroResult::<u32> implicits() nopanic;
extern fn u32_safe_divmod(a: u32, b: NonZero::<u32>) -> (u32, u32) implicits(RangeCheck) nopanic;

#[panic_with('u32 is 0', u32_as_non_zero)]
fn u32_try_as_non_zero(a: u32) -> Option::<NonZero::<u32>> implicits() nopanic {
    match u32_is_zero(a) {
        IsZeroResult::Zero(()) => Option::None(()),
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U32Div of Div::<u32> {
    fn div(a: u32, b: u32) -> u32 {
        let (q, r) = u32_safe_divmod(a, u32_as_non_zero(b));
        q
    }
}

impl U32Rem of Rem::<u32> {
    fn rem(a: u32, b: u32) -> u32 {
        let (q, r) = u32_safe_divmod(a, u32_as_non_zero(b));
        r
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
    fn add(a: u64, b: u64) -> u64 {
        u64_overflowing_add(a, b).expect('u64_add Overflow')
    }
}
impl U64AddEq of AddEq::<u64> {
    #[inline(always)]
    fn add_eq(ref self: u64, other: u64) {
        self = Add::add(self, other);
    }
}

fn u64_checked_sub(a: u64, b: u64) -> Option::<u64> implicits(RangeCheck) nopanic {
    match u64_overflowing_sub(a, b) {
        Result::Ok(r) => Option::<u64>::Some(r),
        Result::Err(r) => Option::<u64>::None(()),
    }
}

impl U64Sub of Sub::<u64> {
    fn sub(a: u64, b: u64) -> u64 {
        u64_overflowing_sub(a, b).expect('u64_sub Overflow')
    }
}
impl U64SubEq of SubEq::<u64> {
    #[inline(always)]
    fn sub_eq(ref self: u64, other: u64) {
        self = Sub::sub(self, other);
    }
}

extern fn u64_wide_mul(a: u64, b: u64) -> u128 implicits() nopanic;
impl U64Mul of Mul::<u64> {
    fn mul(a: u64, b: u64) -> u64 {
        // TODO(orizi): Use direct conversion, instead of going through felt.
        u64_try_from_felt(u128_to_felt(u64_wide_mul(a, b))).expect('u64_mul Overflow')
    }
}

extern fn u64_is_zero(a: u64) -> IsZeroResult::<u64> implicits() nopanic;
extern fn u64_safe_divmod(a: u64, b: NonZero::<u64>) -> (u64, u64) implicits(RangeCheck) nopanic;

#[panic_with('u64 is 0', u64_as_non_zero)]
fn u64_try_as_non_zero(a: u64) -> Option::<NonZero::<u64>> implicits() nopanic {
    match u64_is_zero(a) {
        IsZeroResult::Zero(()) => Option::None(()),
        IsZeroResult::NonZero(x) => Option::Some(x),
    }
}

impl U64Div of Div::<u64> {
    fn div(a: u64, b: u64) -> u64 {
        let (q, r) = u64_safe_divmod(a, u64_as_non_zero(b));
        q
    }
}

impl U64Rem of Rem::<u64> {
    fn rem(a: u64, b: u64) -> u64 {
        let (q, r) = u64_safe_divmod(a, u64_as_non_zero(b));
        r
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
    fn add(a: u256, b: u256) -> u256 {
        u256_checked_add(a, b).expect('u256_add Overflow')
    }
}
impl U256AddEq of AddEq::<u256> {
    #[inline(always)]
    fn add_eq(ref self: u256, other: u256) {
        self = Add::add(self, other);
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
    fn sub(a: u256, b: u256) -> u256 {
        u256_checked_sub(a, b).expect('u256_sub Overflow')
    }
}
impl U256SubEq of SubEq::<u256> {
    #[inline(always)]
    fn sub_eq(ref self: u256, other: u256) {
        self = Sub::sub(self, other);
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
    fn mul(a: u256, b: u256) -> u256 {
        u256_checked_mul(a, b).expect('u256_mul Overflow')
    }
}
impl U256MulEq of MulEq::<u256> {
    #[inline(always)]
    fn mul_eq(ref self: u256, other: u256) {
        self = Mul::mul(self, other);
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

/// Conversions.
impl FeltTryIntoU8 of TryInto::<felt, u8> {
    fn try_into(self: felt) -> Option::<u8> {
        u8_try_from_felt(self)
    }
}
impl U8IntoFelt of Into::<u8, felt> {
    fn into(self: u8) -> felt {
        u8_to_felt(self)
    }
}
impl FeltTryIntoU16 of TryInto::<felt, u16> {
    fn try_into(self: felt) -> Option::<u16> {
        u16_try_from_felt(self)
    }
}
impl U16IntoFelt of Into::<u16, felt> {
    fn into(self: u16) -> felt {
        u16_to_felt(self)
    }
}
impl FeltTryIntoU32 of TryInto::<felt, u32> {
    fn try_into(self: felt) -> Option::<u32> {
        u32_try_from_felt(self)
    }
}
impl U32IntoFelt of Into::<u32, felt> {
    fn into(self: u32) -> felt {
        u32_to_felt(self)
    }
}
impl FeltTryIntoU64 of TryInto::<felt, u64> {
    fn try_into(self: felt) -> Option::<u64> {
        u64_try_from_felt(self)
    }
}
impl U64IntoFelt of Into::<u64, felt> {
    fn into(self: u64) -> felt {
        u64_to_felt(self)
    }
}
impl FeltTryIntoU128 of TryInto::<felt, u128> {
    fn try_into(self: felt) -> Option::<u128> {
        u128_try_from_felt(self)
    }
}
impl U128IntoFelt of Into::<u128, felt> {
    fn into(self: u128) -> felt {
        u128_to_felt(self)
    }
}
impl FeltIntoU256 of Into::<felt, u256> {
    fn into(self: felt) -> u256 {
        u256_from_felt(self)
    }
}
impl U16TryIntoU8 of TryInto::<u16, u8> {
    fn try_into(self: u16) -> Option::<u8> {
        // TODO(orizi): Use direct conversion, instead of going through felt.
        let as_felt: felt = self.into();
        as_felt.try_into()
    }
}
impl U32TryIntoU16 of TryInto::<u32, u16> {
    fn try_into(self: u32) -> Option::<u16> {
        // TODO(orizi): Use direct conversion, instead of going through felt.
        let as_felt: felt = self.into();
        as_felt.try_into()
    }
}
impl U64TryIntoU32 of TryInto::<u64, u32> {
    fn try_into(self: u64) -> Option::<u32> {
        // TODO(orizi): Use direct conversion, instead of going through felt.
        let as_felt: felt = self.into();
        as_felt.try_into()
    }
}
impl U128TryIntoU64 of TryInto::<u128, u64> {
    fn try_into(self: u128) -> Option::<u64> {
        // TODO(orizi): Use direct conversion, instead of going through felt.
        let as_felt: felt = self.into();
        as_felt.try_into()
    }
}

// TODO(lior): Restrict the function (using traits) in the high-level compiler so that wrong types
//   will not lead to Sierra errors.
extern fn upcast<FromType, ToType>(x: FromType) -> ToType nopanic;
