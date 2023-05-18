use array::ArrayTrait;
use test::test_utils::{assert_eq, assert_ne};

#[test]
fn main() {
    let mut a: Array<felt252> = Default::default();
    assert_eq(a.len(), 0_usize, 'Array length is not 0');
    assert_eq(a.is_empty(), true, 'Array is not empty');
}
