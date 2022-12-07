extern type uint128;
impl Uint128Copy of Copy::<uint128>;
impl Uint128Drop of Drop::<uint128>;

// TODO(orizi): Change all error codes into short-strings.

enum Uint128sFromFeltResult { Narrow: uint128, Wide: (uint128, uint128), }
extern func uint128s_from_felt(a: felt) -> Uint128sFromFeltResult implicits(RangeCheck) nopanic;

#[panic_with(1, uint128_from_felt)]
func uint128_try_from_felt(a: felt) -> Option::<uint128> implicits(RangeCheck) nopanic {
    match uint128s_from_felt(a) {
        Uint128sFromFeltResult::Narrow(x) => Option::<uint128>::Some(x),
        Uint128sFromFeltResult::Wide(x) => Option::<uint128>::None(()),
    }
}

extern func uint128_to_felt(a: uint128) -> felt nopanic;

extern func uint128_overflow_add(
    a: uint128,
    b: uint128
) -> Result::<uint128, uint128> implicits(RangeCheck) nopanic;
extern func uint128_overflow_sub(
    a: uint128,
    b: uint128
) -> Result::<uint128, uint128> implicits(RangeCheck) nopanic;

// TODO(orizi): This is a helper for `uint128_wide_mul` - remove when becomes extern.
func uint128_from_felt_low(a: felt) -> uint128 implicits(RangeCheck) nopanic {
    match uint128s_from_felt(a) {
        Uint128sFromFeltResult::Narrow(x) => x,
        Uint128sFromFeltResult::Wide((x, _)) => x,
    }
}

// TODO(orizi): This is a helper for `uint128_wide_mul` - remove when becomes extern.
func uint128_known_u64_mul(a: uint128, b: uint128) -> uint128 implicits(RangeCheck) nopanic {
    uint128_from_felt_low(uint128_to_felt(a) * uint128_to_felt(b))
}

func uint128_wrapping_add(a: uint128, b: uint128) -> uint128 implicits(RangeCheck) nopanic {
    match uint128_overflow_add(a, b) {
        Result::Ok(x) => x,
        Result::Err(x) => x,
    }
}

// TODO(orizi): Change to extern when added.
func uint128_wide_mul(a: uint128, b: uint128) -> (uint128, uint128) implicits(RangeCheck) nopanic {
    let u2_64 = uint128_from_felt_low(18446744073709551616);
    let nz_u2_64 = match uint128_checked_as_non_zero(u2_64) {
        Option::Some(x) => x,
        Option::None(x) => {
            // Can't really happen - this simply avoids the need to panic.
            return (a, b);
        },
    };
    let u1 = uint128_from_felt_low(1);
    let (a1, a0) = uint128_safe_divmod(a, nz_u2_64);
    let (b1, b0) = uint128_safe_divmod(b, nz_u2_64);
    let top_word = uint128_known_u64_mul(a1, b1);
    let bottom_word = uint128_known_u64_mul(a0, b0);
    let (a0b1_h, a0b1_l) = uint128_safe_divmod(uint128_known_u64_mul(a0, b1), nz_u2_64);
    let top_word = uint128_wrapping_add(top_word, a0b1_h);
    let (a1b0_h, a1b0_l) = uint128_safe_divmod(uint128_known_u64_mul(a1, b0), nz_u2_64);
    let top_word = uint128_wrapping_add(top_word, a1b0_h);
    let (bottom_word, top_word) = match uint128_overflow_add(
        bottom_word,
        uint128_known_u64_mul(a0b1_l, u2_64)
    ) {
        Result::Ok(bottom_word) => (bottom_word, top_word),
        Result::Err(bottom_word) => (bottom_word, uint128_wrapping_add(top_word, u1)),
    };
    match uint128_overflow_add(bottom_word, uint128_known_u64_mul(a1b0_l, u2_64)) {
        Result::Ok(bottom_word) => (bottom_word, top_word),
        Result::Err(bottom_word) => (bottom_word, uint128_wrapping_add(top_word, u1)),
    }
}

func uint128_overflow_mul(
    a: uint128,
    b: uint128
) -> (uint128, bool) implicits(RangeCheck) nopanic {
    let (bottom_word, top_word) = uint128_wide_mul(a, b);
    match uint128_to_felt(top_word) {
        0 => (bottom_word, false),
        _ => (bottom_word, true),
    }
}

#[panic_with(1, uint128_add)]
func uint128_checked_add(
    a: uint128,
    b: uint128
) -> Option::<uint128> implicits(RangeCheck) nopanic {
    match uint128_overflow_add(a, b) {
        Result::Ok(r) => Option::<uint128>::Some(r),
        Result::Err(r) => Option::<uint128>::None(()),
    }
}

#[panic_with(1, uint128_sub)]
func uint128_checked_sub(
    a: uint128,
    b: uint128
) -> Option::<uint128> implicits(RangeCheck) nopanic {
    match uint128_overflow_sub(a, b) {
        Result::Ok(r) => Option::<uint128>::Some(r),
        Result::Err(r) => Option::<uint128>::None(()),
    }
}

#[panic_with(1, uint128_mul)]
func uint128_checked_mul(
    a: uint128,
    b: uint128
) -> Option::<uint128> implicits(RangeCheck) nopanic {
    let (bottom_word, top_word) = uint128_wide_mul(a, b);
    match uint128_to_felt(top_word) {
        0 => Option::<uint128>::Some(bottom_word),
        _ => Option::<uint128>::None(()),
    }
}

impl NonZeroUint128Copy of Copy::<NonZero::<uint128>>;
impl NonZeroUint128Drop of Drop::<NonZero::<uint128>>;

#[panic_with(1, uint128_as_non_zero)]
func uint128_checked_as_non_zero(a: uint128) -> Option::<NonZero::<uint128>> implicits() nopanic {
    match uint128_jump_nz(a) {
        JumpNzResult::Zero(()) => Option::<NonZero::<uint128>>::None(()),
        JumpNzResult::NonZero(x) => Option::<NonZero::<uint128>>::Some(x),
    }
}

func uint128_safe_div(a: uint128, b: NonZero::<uint128>) -> uint128 implicits(RangeCheck) nopanic {
    let (q, r) = uint128_safe_divmod(a, b);
    q
}

func uint128_div(a: uint128, b: uint128) -> uint128 implicits(RangeCheck) {
    uint128_safe_div(a, uint128_as_non_zero(b))
}

func uint128_safe_mod(a: uint128, b: NonZero::<uint128>) -> uint128 implicits(RangeCheck) nopanic {
    let (q, r) = uint128_safe_divmod(a, b);
    r
}

func uint128_mod(a: uint128, b: uint128) -> uint128 implicits(RangeCheck) {
    uint128_safe_mod(a, uint128_as_non_zero(b))
}

extern func uint128_safe_divmod(
    a: uint128,
    b: NonZero::<uint128>
) -> (uint128, uint128) implicits(RangeCheck) nopanic;

extern func uint128_lt(a: uint128, b: uint128) -> bool implicits(RangeCheck) nopanic;
extern func uint128_le(a: uint128, b: uint128) -> bool implicits(RangeCheck) nopanic;

func uint128_gt(a: uint128, b: uint128) -> bool implicits(RangeCheck) nopanic {
    uint128_lt(b, a)
}

func uint128_ge(a: uint128, b: uint128) -> bool implicits(RangeCheck) nopanic {
    uint128_le(b, a)
}

// TODO(orizi): Change to extern when added.
func uint128_eq(a: uint128, b: uint128) -> bool implicits() nopanic {
    uint128_to_felt(a) == uint128_to_felt(b)
}

func uint128_ne(a: uint128, b: uint128) -> bool implicits() nopanic {
    !(a == b)
}

extern func uint128_jump_nz(a: uint128) -> JumpNzResult::<uint128> implicits() nopanic;

#[derive(Copy, Drop)]
struct uint256 { low: uint128, high: uint128, }

func uint256_overflow_add(
    a: uint256,
    b: uint256
) -> (uint256, bool) implicits(RangeCheck) nopanic {
    let (high, overflow) = match uint128_overflow_add(a.high, b.high) {
        Result::Ok(high) => (high, false),
        Result::Err(high) => (high, true),
    };
    match uint128_overflow_add(a.low, b.low) {
        Result::Ok(low) => (uint256 { low, high }, overflow),
        Result::Err(low) => {
            match uint128_overflow_add(high, uint128_from_felt_low(1)) {
                Result::Ok(high) => (uint256 { low, high }, overflow),
                Result::Err(high) => (uint256 { low, high }, true),
            }
        },
    }
}

func uint256_overflow_sub(
    a: uint256,
    b: uint256
) -> (uint256, bool) implicits(RangeCheck) nopanic {
    let (high, overflow) = match uint128_overflow_sub(a.high, b.high) {
        Result::Ok(high) => (high, false),
        Result::Err(high) => (high, true),
    };
    match uint128_overflow_sub(a.low, b.low) {
        Result::Ok(low) => (uint256 { low, high }, overflow),
        Result::Err(low) => {
            match uint128_overflow_sub(high, uint128_from_felt_low(1)) {
                Result::Ok(high) => (uint256 { low, high }, overflow),
                Result::Err(high) => (uint256 { low, high }, true),
            }
        },
    }
}

func uint256_overflow_mul(a: uint256, b: uint256) -> (uint256, bool) nopanic {
    let u0 = uint128_from_felt_low(0);
    let (low, high1) = uint128_wide_mul(a.low, b.low);
    let (high2, overflow_value1) = uint128_wide_mul(a.low, b.high);
    let (high3, overflow_value2) = uint128_wide_mul(a.high, b.low);
    let (high, overflow) = match uint128_overflow_add(high1, high2) {
        Result::Ok(high) => (
            high,
            overflow_value1 != u0 | overflow_value2 != u0 | (a.high > u0 & b.high > u0)
        ),
        Result::Err(high) => (high, true),
    };
    let (high, overflow) = match uint128_overflow_add(high, high3) {
        Result::Ok(high) => (high, overflow),
        Result::Err(high) => (high, true),
    };
    (uint256 { low, high }, overflow)
}

#[panic_with(1, uint256_add)]
func uint256_checked_add(
    a: uint256,
    b: uint256
) -> Option::<uint256> implicits(RangeCheck) nopanic {
    let (r, overflow) = uint256_overflow_add(a, b);
    if overflow {
        Option::<uint256>::None(())
    } else {
        Option::<uint256>::Some(r)
    }
}

#[panic_with(1, uint256_sub)]
func uint256_checked_sub(
    a: uint256,
    b: uint256
) -> Option::<uint256> implicits(RangeCheck) nopanic {
    let (r, overflow) = uint256_overflow_sub(a, b);
    if overflow {
        Option::<uint256>::None(())
    } else {
        Option::<uint256>::Some(r)
    }
}

#[panic_with(1, uint256_mul)]
func uint256_checked_mul(
    a: uint256,
    b: uint256
) -> Option::<uint256> implicits(RangeCheck) nopanic {
    let (r, overflow) = uint256_overflow_mul(a, b);
    if overflow {
        Option::<uint256>::None(())
    } else {
        Option::<uint256>::Some(r)
    }
}

func uint256_eq(a: uint256, b: uint256) -> bool implicits() {
    a.low == b.low & a.high == b.high
}

func uint256_ne(a: uint256, b: uint256) -> bool implicits() {
    !(a == b)
}
