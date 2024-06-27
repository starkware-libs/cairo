use core::test::test_utils::{assert_eq, assert_ne};


#[test]
fn test_for_loop_array_sum() {
    let mut sum = 0;
    for x in array![10, 11, 12] {
        sum += x;
    };
    assert_eq!(sum, 33);
}

#[test]
fn test_for_loop_array_variables() {
    let mut i = 10;
    for x in array![10, 11, 12] {
        assert_eq!(i, x);
        i += 1;
    };
}

#[test]
fn test_for_loop_array_tuples() {
    let mut i = 10;
    for (
        x, y
    ) in array![(10, 10), (11, 11), (12, 12)] {
        assert_eq!(x, i);
        assert_eq!(y, i);
        i += 1;
    };
}
