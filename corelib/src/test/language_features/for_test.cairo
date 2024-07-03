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

#[test]
fn test_for_loop_span_iter() {
    let span_arr = array![1, 2, 3].span();
    let span_iter = span_arr.into_iter();
    let mut i = 1;
    let mut sum = 0;
    for x in span_iter {
        assert_eq!(x, @i);
        i += 1;
        sum += *x;
    };
    assert_eq!(sum, 6);
}

#[test]
fn test_for_loop_array_iter() {
    let arr = array![1, 2, 3];
    let arr_iter = arr.into_iter();
    let mut i = 1;
    let mut sum = 0;
    for x in arr_iter {
        assert_eq!(x, i);
        i += 1;
        sum += x;
    };
    assert_eq!(sum, 6);
}
