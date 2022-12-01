extern type uint128;
impl Uint128Copy of Copy::<uint128>;
impl Uint128Drop of Drop::<uint128>;

// TODO(orizi): Change all error codes into short-strings.

#[panic_with(1, uint128_from_felt)]
extern func uint128_try_from_felt(a: felt) -> Option::<uint128> implicits(rc: RangeCheck) nopanic;

extern func uint128_to_felt(a: uint128) -> felt nopanic;

#[panic_with(1, uint128_add)]
extern func uint128_checked_add(
    a: uint128,
    b: uint128
) -> Option::<uint128> implicits(rc: RangeCheck) nopanic;

#[panic_with(1, uint128_sub)]
extern func uint128_checked_sub(
    a: uint128,
    b: uint128
) -> Option::<uint128> implicits(rc: RangeCheck) nopanic;

#[panic_with(1, uint128_mul)]
extern func uint128_checked_mul(
    a: uint128,
    b: uint128
) -> Option::<uint128> implicits(rc: RangeCheck) nopanic;

extern func uint128_div(
    a: uint128,
    b: NonZero::<uint128>
) -> uint128 implicits(rc: RangeCheck) nopanic;

extern func uint128_mod(
    a: uint128,
    b: NonZero::<uint128>
) -> uint128 implicits(rc: RangeCheck) nopanic;

extern func uint128_divmod(
    a: uint128,
    b: NonZero::<uint128>
) -> (uint128, uint128) implicits(rc: RangeCheck) nopanic;

extern func uint128_lt(a: uint128, b: uint128) -> bool implicits(rc: RangeCheck) nopanic;
extern func uint128_le(a: uint128, b: uint128) -> bool implicits(rc: RangeCheck) nopanic;

func uint128_gt(a: uint128, b: uint128) -> bool implicits(rc: RangeCheck) nopanic {
    uint128_lt(b, a)
}

func uint128_ge(a: uint128, b: uint128) -> bool implicits(rc: RangeCheck) nopanic {
    uint128_le(b, a)
}

// TODO(orizi): Change to extern when added.
func uint128_eq(a: uint128, b: uint128) -> bool implicits(rc: RangeCheck) {
    uint128_to_felt(a) == uint128_to_felt(b)
}

extern func uint128_jump_nz(a: uint128) -> JumpNzResult::<uint128> nopanic;
