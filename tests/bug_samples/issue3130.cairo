#[feature("deprecated-consteval-int-macro")]
const a: felt252 = consteval_int!((4 + 2 * 3) * 256);
#[feature("deprecated-consteval-int-macro")]
const b: felt252 = consteval_int!(0xff & (24 + 5 * 2));
#[feature("deprecated-consteval-int-macro")]
const c: felt252 = consteval_int!(-0xff & (24 + 5 * 2));
#[feature("deprecated-consteval-int-macro")]
const d: felt252 = consteval_int!(0xff | (24 + 5 * 2));

#[test]
fn main() {}
