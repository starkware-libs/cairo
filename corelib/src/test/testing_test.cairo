use core::test::test_utils::assert_gt;

#[test]
#[should_panic(expected: ('panic_with_felt252()',))]
fn test_panic_with_felt252() {
    // No semicolon here: Missing implementation for core::traits::Drop::<core::never>
    core::panic_with_felt252('panic_with_felt252()')
}

#[test]
#[should_panic(expected: 'assert(false)')]
fn test_assert_false() {
    assert(false, 'assert(false)');
}

#[test]
fn test_assert_true() {
    assert(true, 'assert(true)');
}

#[test]
#[should_panic(expected: "assert(false)")]
fn test_assert_macro_false() {
    assert!(false, "assert(false)");
}

#[test]
fn test_assert_macro_true() {
    assert!(true, "assert(true)");
}

#[test]
fn test_assert_ne_with_description() {
    assert_ne!(1, 2, "Description");
}

#[test]
fn test_assert_ne_no_description() {
    assert_ne!(1, 2);
}

#[test]
#[should_panic(expected: "assertion failed: `false`.")]
fn test_assert_macro_no_input() {
    assert!(false);
}

#[test]
#[should_panic(expected: "assertion `1 == 2` failed: Description
1: 1
2: 2")]
fn test_assert_eq_with_description() {
    assert_eq!(1, 2, "Description");
}

#[test]
#[should_panic(expected: "assertion `1 == 2` failed: 1 != 2
1: 1
2: 2")]
fn test_assert_eq_with_formatted_description() {
    assert_eq!(1, 2, "{} != {}", 1, 2);
}

#[test]
#[should_panic(expected: "assertion `1 == 2` failed.
1: 1
2: 2")]
fn test_assert_eq_no_description() {
    assert_eq!(1, 2);
}

#[test]
#[available_gas(static)]
fn test_get_available_gas_no_gas_supply() {
    assert_eq!(core::testing::get_available_gas(), 0)
}

#[test]
#[available_gas(10000)]
fn test_get_available_gas_with_gas_supply() {
    assert_gt(core::testing::get_available_gas(), 5000, 'high amount of gas used')
}
