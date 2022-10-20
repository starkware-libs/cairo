extern type uint128;
extern func uint128_from_felt(rc: RangeCheck, a: felt) -> (RangeCheck, Option::<uint128>);
extern func uint128_to_felt(a: uint128) -> felt;
extern func uint128_add(rc: RangeCheck, a: uint128, b: uint128) -> (RangeCheck, Option::<uint128>);
extern func uint128_sub(rc: RangeCheck, a: uint128, b: uint128) -> (RangeCheck, Option::<uint128>);
extern func uint128_mul(rc: RangeCheck, a: uint128, b: uint128) -> (RangeCheck, Option::<uint128>);
extern func uint128_div(rc: RangeCheck, a: uint128, b: NonZero::<uint128>) -> (
    RangeCheck,
    uint128
);
extern func uint128_mod(rc: RangeCheck, a: uint128, b: NonZero::<uint128>) -> (
    RangeCheck,
    uint128
);

extern func uint128_jump_nz(a: felt) -> JumpNzResult::<uint128>;
