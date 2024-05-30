use core::test::test_utils::{assert_eq, assert_ne};

fn array_sum(mut arr: Array<felt252>) -> felt252 {
    let mut sum = 0;
    while let Option::Some(x) = arr.pop_front() {
        sum += x;
    };
    sum
}

#[test]
fn test_while_let() {
    assert_eq!(array_sum(array![1, 2, 3, 4]), 10);
    assert_eq!(array_sum(array![]), 0);
}

#[test]
fn test_outer_loop_break() {
    let mut i = 0;
    while let true = i != 10 {
        while true {
            break;
        };
        i += 1;
    };
    assert_eq!(i, 10);
}
