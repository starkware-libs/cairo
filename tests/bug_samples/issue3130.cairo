const a: felt252 = consteval_int!((4 + 2 * 3) * 256);
const b: felt252 = consteval_int!(0xff & (24 + 5 * 2));
const c: felt252 = consteval_int!(-0xff & (24 + 5 * 2));
const d: felt252 = consteval_int!(0xff | (24 + 5 * 2));

#[test]
fn main() {}
