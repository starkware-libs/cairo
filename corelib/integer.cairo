extern type uint128;
impl Uint128Copy of Copy::<uint128>;
impl Uint128Drop of Drop::<uint128>;

// TODO(orizi): Change all error codes into

#[panic_with(1)]
extern func uint128_from_felt(a: felt) -> Option::<uint128> implicits (rc: RangeCheck) nopanic;

extern func uint128_to_felt(a: uint128) -> felt nopanic;

#[panic_with(1)]
extern func uint128_add(
    a: uint128,
    b: uint128
    ) -> Option::<uint128> implicits (
    rc: RangeCheck
) nopanic;

#[panic_with(1)]
extern func uint128_sub(
    a: uint128,
    b: uint128
    ) -> Option::<uint128> implicits (
    rc: RangeCheck
) nopanic;

#[panic_with(1)]
extern func uint128_mul(
    a: uint128,
    b: uint128
    ) -> Option::<uint128> implicits (
    rc: RangeCheck
) nopanic;

extern func uint128_div(
    a: uint128,
    b: NonZero::<uint128>
    ) -> uint128 implicits (
    rc: RangeCheck
) nopanic;

extern func uint128_mod(
    a: uint128,
    b: NonZero::<uint128>
    ) -> uint128 implicits (
    rc: RangeCheck
) nopanic;

extern func uint128_lt(a: uint128, b: uint128) -> bool implicits (rc: RangeCheck) nopanic;

extern func uint128_jump_nz(a: uint128) -> JumpNzResult::<uint128> nopanic;
