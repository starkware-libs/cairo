enum bool {
    True: (),
    False: (),
}
extern func bool_dup(a: bool) -> (bool, bool);
extern func bool_drop(a: bool);
extern func bool_and(a: bool, b: bool) -> bool;
extern func bool_or(a: bool, b: bool) -> bool;
extern func bool_not(a: bool) -> bool;

extern type RangeCheck;

extern type felt;
extern func felt_dup(a: felt) -> (felt, felt);
extern func felt_drop(a: felt);
extern func felt_add(a: felt, b: felt) -> felt;
extern func felt_sub(a: felt, b: felt) -> felt;
extern func felt_mul(a: felt, b: felt) -> felt;

extern type non_zero_felt;
extern func non_zero_felt_dup(a: non_zero_felt) -> (non_zero_felt, non_zero_felt);
extern func non_zero_felt_drop(a: non_zero_felt);
extern func felt_div(a: felt, b: non_zero_felt) -> felt;

extern func felt_eq(a: felt, b: felt) -> bool;
extern func felt_le(rc: RangeCheck, a: felt, b: felt) -> (RangeCheck, bool);
