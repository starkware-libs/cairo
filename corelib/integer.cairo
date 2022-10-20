extern type uint128;

// TODO(orizi): Consider making `Result::<(RangeCheck, uin128), RangeCheck>` into a new type, that
// would be marked as an extern enum.

extern func uint128_from_felt(rc: RangeCheck, a: felt) -> Result::<(
    RangeCheck,
    uint128
), RangeCheck>;
extern func uint128_to_felt(a: uint128) -> felt;
extern func uint128_add(rc: RangeCheck, a: uint128, b: uint128) -> Result::<(
    RangeCheck,
    uint128
), RangeCheck>;
extern func uint128_sub(rc: RangeCheck, a: uint128, b: uint128) -> Result::<(
    RangeCheck,
    uint128
), RangeCheck>;
extern func uint128_mul(rc: RangeCheck, a: uint128, b: uint128) -> Result::<(
    RangeCheck,
    uint128
), RangeCheck>;
extern func uint128_div(rc: RangeCheck, a: uint128, b: NonZero::<uint128>) -> Result::<(
    RangeCheck,
    uint128
), RangeCheck>;
extern func uint128_mod(rc: RangeCheck, a: uint128, b: NonZero::<uint128>) -> Result::<(
    RangeCheck,
    uint128
), RangeCheck>;

extern func uint128_jump_nz(a: uint128) -> JumpNzResult::<uint128>;
