fn array_sum(mut arr: Array<felt252>) -> felt252 {
    let mut sum = 0;
    while let Some(x) = arr.pop_front() {
        sum += x;
    }
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
        }
        i += 1;
    }
    assert_eq!(i, 10);
}

#[test]
fn test_borrow_usage() {
    let mut i = 0;
    let arr = array![1, 2, 3, 4];
    while i != arr.len() {
        i += 1;
    }
    assert_eq!(arr.len(), 4);
}

#[derive(Drop)]
struct NonCopy {
    x: felt252,
}

fn assert_x_eq(a: @NonCopy, x: felt252) {
    assert_eq!(a.x, @x);
}

#[test]
fn test_borrow_with_inner_change() {
    let mut a = NonCopy { x: 0 };
    let mut i = 0;
    while i != 5 {
        a.x = i;
        assert_x_eq(@a, i);
        i += 1;
    }
}

