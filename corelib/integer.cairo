extern type uint128;

// TODO(orizi): Consider making `Result::<(RangeCheck, uin128), RangeCheck>` into a new type, that
// would be marked as an extern enum.

extern func uint128_from_felt(ref rc: RangeCheck, a: felt) -> Option::<uint128>;
extern func uint128_to_felt(a: uint128) -> felt;
extern func uint128_add(ref rc: RangeCheck, a: uint128, b: uint128) -> Option::<uint128>;
extern func uint128_sub(ref rc: RangeCheck, a: uint128, b: uint128) -> Option::<uint128>;
extern func uint128_mul(ref rc: RangeCheck, a: uint128, b: uint128) -> Option::<uint128>;
extern func uint128_div(ref rc: RangeCheck, a: uint128, b: NonZero::<uint128>) -> uint128;
extern func uint128_mod(ref rc: RangeCheck, a: uint128, b: NonZero::<uint128>) -> uint128;

extern func uint128_jump_nz(a: uint128) -> JumpNzResult::<uint128>;
