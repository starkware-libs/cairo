use core::test::test_utils::{assert_eq, assert_ne};

#[test]
fn test_box_unbox_felt252s() {
    let x = 10;
    let boxed_x = BoxTrait::new(x);
    let y = 11;
    let boxed_y = BoxTrait::new(y);
    assert_eq(@boxed_x.unbox(), @10, 'x != 10');
    assert_eq(@boxed_y.unbox(), @11, 'y != 11');
}

// Test objects of size>1.
#[test]
fn test_box_unbox_u256() {
    let x = u256 { low: 1, high: 0 };
    let boxed_x = BoxTrait::new(x);
    let y = u256 { low: 1, high: 1 };
    let boxed_y = BoxTrait::new(y);
    assert_eq(@boxed_x.unbox(), @x, 'unbox u256 x');
    assert_eq(@boxed_y.unbox(), @y, 'unbox u256 y');
}
