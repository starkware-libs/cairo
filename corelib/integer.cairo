extern type uint128;
impl Uint128Copy of Copy::<uint128>;
impl Uint128Drop of Drop::<uint128>;

// TODO(orizi): Change all error codes into short-strings.

#[panic_with(1, uint128_from_felt)]
extern func uint128_try_from_felt(a: felt) -> Option::<uint128> implicits(RangeCheck) nopanic;

extern func uint128_to_felt(a: uint128) -> felt nopanic;

extern func uint128_overflow_add(
    a: uint128,
    b: uint128
) -> Result::<uint128, uint128> implicits(RangeCheck) nopanic;
extern func uint128_overflow_sub(
    a: uint128,
    b: uint128
) -> Result::<uint128, uint128> implicits(RangeCheck) nopanic;
extern func uint128_overflow_mul(
    a: uint128,
    b: uint128
) -> Result::<uint128, uint128> implicits(RangeCheck) nopanic;

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
    match uint128_overflow_mul(a, b) {
        Result::Ok(r) => Option::<uint128>::Some(r),
        Result::Err(r) => Option::<uint128>::None(()),
    }
}

#[panic_with(1, uint128_as_non_zero)]
func uint128_checked_as_non_zero(a: uint128) -> Option::<NonZero::<uint128>> nopanic {
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
func uint128_eq(a: uint128, b: uint128) -> bool implicits(RangeCheck) {
    uint128_to_felt(a) == uint128_to_felt(b)
}

extern func uint128_jump_nz(a: uint128) -> JumpNzResult::<uint128> nopanic;
