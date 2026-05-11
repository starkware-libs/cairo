struct S {
    v: felt252,
}

// Wildcard pattern on non-enum type.
const CASE1: bool = match 1_u8 {
    _ => true,
};
// Literal pattern: non-matching arm skipped, wildcard fallthrough.
const CASE2: bool = match 2_u8 {
    1 => false,
    _ => true,
};
// Wildcard on felt252.
const CASE3: bool = match 1_felt252 {
    _ => true,
};
// Tuple destructuring in match.
const CASE4: bool = match (1_u8, 2_u8) {
    (_, _) => true,
};
// Struct destructuring in match.
const CASE5: bool = match (S { v: 1 }) {
    S { v: _ } => true,
};
// Unit pattern in match.
const CASE6: bool = match () {
    () => true,
};

#[test]
fn all_cases() {
    assert!(CASE1);
    assert!(CASE2);
    assert!(CASE3);
    assert!(CASE4);
    assert!(CASE5);
    assert!(CASE6);
}
