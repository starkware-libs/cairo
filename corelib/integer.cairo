extern type u128;
impl U128Copy of Copy::<u128>;
impl U128Drop of Drop::<u128>;
extern func u128_const<value>() -> u128 nopanic;

// TODO(orizi): Change all error codes into short-strings.

enum U128sFromFeltResult { Narrow: u128, Wide: (u128, u128), }
extern func u128s_from_felt(a: felt) -> U128sFromFeltResult implicits(RangeCheck) nopanic;

#[panic_with(1, u128_from_felt)]
func u128_try_from_felt(a: felt) -> Option::<u128> implicits(RangeCheck) nopanic {
    match u128s_from_felt(a) {
        U128sFromFeltResult::Narrow(x) => Option::<u128>::Some(x),
        U128sFromFeltResult::Wide(x) => Option::<u128>::None(()),
    }
}

extern func u128_to_felt(a: u128) -> felt nopanic;

extern func u128_overflow_add(
    a: u128, b: u128
) -> Result::<u128, u128> implicits(RangeCheck) nopanic;
extern func u128_overflow_sub(
    a: u128, b: u128
) -> Result::<u128, u128> implicits(RangeCheck) nopanic;

// TODO(orizi): This is a helper for `u128_wide_mul` - remove when becomes extern.
func u128_known_u64_mul(a: u128, b: u128) -> u128 implicits(RangeCheck) nopanic {
    match u128s_from_felt(u128_to_felt(a) * u128_to_felt(b)) {
        U128sFromFeltResult::Narrow(x) => x,
        U128sFromFeltResult::Wide((_, x)) => x,
    }
}

func u128_wrapping_add(a: u128, b: u128) -> u128 implicits(RangeCheck) nopanic {
    match u128_overflow_add(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

// TODO(orizi): Change to extern when added.
func u128_wide_mul(a: u128, b: u128) -> (u128, u128) implicits(RangeCheck) nopanic {
    let u2_64 = 0x10000000000000000_u128;
    let nz_u2_64 = match u128_checked_as_non_zero(u2_64) {
        Option::Some(x) => x,
        Option::None(x) => {
            // Can't really happen - this simply avoids the need to panic.
            return (a, b);
        },
    };
    let (a1, a0) = u128_safe_divmod(a, nz_u2_64);
    let (b1, b0) = u128_safe_divmod(b, nz_u2_64);
    let top_word = u128_known_u64_mul(a1, b1);
    let bottom_word = u128_known_u64_mul(a0, b0);
    let (a0b1_h, a0b1_l) = u128_safe_divmod(u128_known_u64_mul(a0, b1), nz_u2_64);
    let top_word = u128_wrapping_add(top_word, a0b1_h);
    let (a1b0_h, a1b0_l) = u128_safe_divmod(u128_known_u64_mul(a1, b0), nz_u2_64);
    let top_word = u128_wrapping_add(top_word, a1b0_h);
    let (bottom_word,
    top_word) = match u128_overflow_add(bottom_word, u128_known_u64_mul(a0b1_l, u2_64)) {
        Result::Ok(bottom_word) => (bottom_word, top_word),
        Result::Err(bottom_word) => (bottom_word, u128_wrapping_add(top_word, 1_u128)),
    };
    match u128_overflow_add(bottom_word, u128_known_u64_mul(a1b0_l, u2_64)) {
        Result::Ok(bottom_word) => (bottom_word, top_word),
        Result::Err(bottom_word) => (bottom_word, u128_wrapping_add(top_word, 1_u128)),
    }
}

func u128_overflow_mul(a: u128, b: u128) -> (u128, bool) implicits(RangeCheck) nopanic {
    let (bottom_word, top_word) = u128_wide_mul(a, b);
    match u128_to_felt(top_word) {
        0 => (bottom_word, false),
        _ => (bottom_word, true),
    }
}

#[panic_with(1, u128_add)]
func u128_checked_add(a: u128, b: u128) -> Option::<u128> implicits(RangeCheck) nopanic {
    match u128_overflow_add(a, b) {
        Result::Ok(r) => Option::<u128>::Some(r),
        Result::Err(r) => Option::<u128>::None(()),
    }
}

#[panic_with(1, u128_sub)]
func u128_checked_sub(a: u128, b: u128) -> Option::<u128> implicits(RangeCheck) nopanic {
    match u128_overflow_sub(a, b) {
        Result::Ok(r) => Option::<u128>::Some(r),
        Result::Err(r) => Option::<u128>::None(()),
    }
}

#[panic_with(1, u128_mul)]
func u128_checked_mul(a: u128, b: u128) -> Option::<u128> implicits(RangeCheck) nopanic {
    let (bottom_word, top_word) = u128_wide_mul(a, b);
    match u128_to_felt(top_word) {
        0 => Option::<u128>::Some(bottom_word),
        _ => Option::<u128>::None(()),
    }
}

impl NonZeroU128Copy of Copy::<NonZero::<u128>>;
impl NonZeroU128Drop of Drop::<NonZero::<u128>>;

#[panic_with(1, u128_as_non_zero)]
func u128_checked_as_non_zero(a: u128) -> Option::<NonZero::<u128>> implicits() nopanic {
    match u128_jump_nz(a) {
        JumpNzResult::Zero(()) => Option::<NonZero::<u128>>::None(()),
        JumpNzResult::NonZero(x) => Option::<NonZero::<u128>>::Some(x),
    }
}

func u128_safe_div(a: u128, b: NonZero::<u128>) -> u128 implicits(RangeCheck) nopanic {
    let (q, r) = u128_safe_divmod(a, b);
    q
}

func u128_div(a: u128, b: u128) -> u128 implicits(RangeCheck) {
    u128_safe_div(a, u128_as_non_zero(b))
}

func u128_safe_mod(a: u128, b: NonZero::<u128>) -> u128 implicits(RangeCheck) nopanic {
    let (q, r) = u128_safe_divmod(a, b);
    r
}

func u128_mod(a: u128, b: u128) -> u128 implicits(RangeCheck) {
    u128_safe_mod(a, u128_as_non_zero(b))
}

extern func u128_safe_divmod(
    a: u128, b: NonZero::<u128>
    ) -> (
    u128, u128
) implicits(RangeCheck) nopanic;

extern func u128_lt(a: u128, b: u128) -> bool implicits(RangeCheck) nopanic;
extern func u128_eq(a: u128, b: u128) -> bool implicits() nopanic;
extern func u128_le(a: u128, b: u128) -> bool implicits(RangeCheck) nopanic;

func u128_gt(a: u128, b: u128) -> bool implicits(RangeCheck) nopanic {
    u128_lt(b, a)
}

func u128_ge(a: u128, b: u128) -> bool implicits(RangeCheck) nopanic {
    u128_le(b, a)
}

func u128_ne(a: u128, b: u128) -> bool implicits() nopanic {
    !(a == b)
}

extern func u128_jump_nz(a: u128) -> JumpNzResult::<u128> implicits() nopanic;

#[derive(Copy, Drop)]
struct u256 { low: u128, high: u128, }

func u256_overflow_add(a: u256, b: u256) -> (u256, bool) implicits(RangeCheck) nopanic {
    let (high, overflow) = match u128_overflow_add(a.high, b.high) {
        Result::Ok(high) => (high, false),
        Result::Err(high) => (high, true),
    };
    match u128_overflow_add(a.low, b.low) {
        Result::Ok(low) => (u256 { low, high }, overflow),
        Result::Err(low) => {
            match u128_overflow_add(high, 1_u128) {
                Result::Ok(high) => (u256 { low, high }, overflow),
                Result::Err(high) => (u256 { low, high }, true),
            }
        },
    }
}

func u256_overflow_sub(a: u256, b: u256) -> (u256, bool) implicits(RangeCheck) nopanic {
    let (high, overflow) = match u128_overflow_sub(a.high, b.high) {
        Result::Ok(high) => (high, false),
        Result::Err(high) => (high, true),
    };
    match u128_overflow_sub(a.low, b.low) {
        Result::Ok(low) => (u256 { low, high }, overflow),
        Result::Err(low) => {
            match u128_overflow_sub(high, 1_u128) {
                Result::Ok(high) => (u256 { low, high }, overflow),
                Result::Err(high) => (u256 { low, high }, true),
            }
        },
    }
}

func u256_overflow_mul(a: u256, b: u256) -> (u256, bool) nopanic {
    let (low, high1) = u128_wide_mul(a.low, b.low);
    let (high2, overflow_value1) = u128_wide_mul(a.low, b.high);
    let (high3, overflow_value2) = u128_wide_mul(a.high, b.low);
    let (high, overflow) = match u128_overflow_add(high1, high2) {
        Result::Ok(high) => (
            high,
            overflow_value1 != 0_u128 | overflow_value2 != 0_u128 | (a.high > 0_u128 & b.high > 0_u128)
        ),
        Result::Err(high) => (high, true),
    };
    let (high, overflow) = match u128_overflow_add(high, high3) {
        Result::Ok(high) => (high, overflow),
        Result::Err(high) => (high, true),
    };
    (u256 { low, high }, overflow)
}

#[panic_with(1, u256_add)]
func u256_checked_add(a: u256, b: u256) -> Option::<u256> implicits(RangeCheck) nopanic {
    let (r, overflow) = u256_overflow_add(a, b);
    if overflow {
        Option::<u256>::None(())
    } else {
        Option::<u256>::Some(r)
    }
}

#[panic_with(1, u256_sub)]
func u256_checked_sub(a: u256, b: u256) -> Option::<u256> implicits(RangeCheck) nopanic {
    let (r, overflow) = u256_overflow_sub(a, b);
    if overflow {
        Option::<u256>::None(())
    } else {
        Option::<u256>::Some(r)
    }
}

#[panic_with(1, u256_mul)]
func u256_checked_mul(a: u256, b: u256) -> Option::<u256> implicits(RangeCheck) nopanic {
    let (r, overflow) = u256_overflow_mul(a, b);
    if overflow {
        Option::<u256>::None(())
    } else {
        Option::<u256>::Some(r)
    }
}

func u256_eq(a: u256, b: u256) -> bool implicits() {
    a.low == b.low & a.high == b.high
}

func u256_ne(a: u256, b: u256) -> bool implicits() {
    !(a == b)
}

func u256_from_felt(a: felt) -> u256 implicits(RangeCheck) nopanic {
    match u128s_from_felt(a) {
        U128sFromFeltResult::Narrow(low) => u256 { low, high: 0_u128 },
        U128sFromFeltResult::Wide((high, low)) => u256 { low, high },
    }
}
