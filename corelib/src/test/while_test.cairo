use core::test::test_utils::{assert_eq, assert_ne};

fn triangle_while(n: felt252) -> felt252 {
    let mut arr = array![];
    let mut i = 1;
    while i != n + 1 {
        arr.append(i);
        i += 1;
    };
    let mut sum = 0;
    while let Option::Some(x) = arr.pop_front() {
        sum += x;
    };
    sum
}

#[test]
fn test_triangle_while() {
    assert_eq!(triangle_while(10), 55);
    assert_eq!(triangle_while(20), 210);
}

#[test]
fn test_outer_loop_break() {
    let mut i = 0;
    let mut j = 0;
    while let true =
        i != 10 {
            j = 0;
            while let true = j != 10 {
                if j == 5 {
                    break;
                }
                j += 1;
            };
            i += 1;
        };
    assert_eq!(i, 10);
    assert_eq!(j, 5);
}
