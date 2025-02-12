#[test]
fn test_nested_loop() {
    let loop_func = |a| {
        loop {
            loop {
                if a == 0 {
                    break;
                }
                return 6;
            }

            return 5;
        }
    };

    assert_eq!(loop_func(0), 5);
    assert_eq!(loop_func(1), 6);
}


#[test]
fn test_loop_for_while() {
    let loop_func = |a| {
        loop {
            for v in a {
                if v == 0 {
                    break;
                }
                while v != 2 {
                    return 6;
                }
                return 7;
            }

            return 5;
        }
    };

    assert_eq!(loop_func(array![0]), 5);
    assert_eq!(loop_func(array![1]), 6);
    assert_eq!(loop_func(array![2]), 7);
}


#[test]
fn test_while_for() {
    let loop_func = |a: Array<_>| {
        while a.len() != 0 {
            for v in a {
                if v == 0 {
                    break;
                }

                return 7;
            }

            return 5;
        }

        8
    };

    assert_eq!(loop_func(array![]), 8);
    assert_eq!(loop_func(array![0]), 5);
    assert_eq!(loop_func(array![1]), 7);
}


#[test]
fn test_loop_inside_for() {
    let loop_func = |a: Array<_>| {
        for v in a {
            loop {
                if v == 0 {
                    break;
                }

                return 5;
            }

            if v == 1 {
                continue;
            }
            return 6;
        }

        return 7;
    };

    assert_eq!(loop_func(array![]), 7);
    assert_eq!(loop_func(array![0, 1]), 6);
    assert_eq!(loop_func(array![1, 0]), 5);
}


#[test]
fn test_loop_inside_while() {
    let loop_func = |mut a: Array<_>| {
        while let Some(v) = a.pop_front() {
            if v == 1 {
                continue;
            }

            loop {
                if v == 0 {
                    break;
                }

                return 5;
            }

            return 6;
        }
        7
    };

    assert_eq!(loop_func(array![]), 7);
    assert_eq!(loop_func(array![0, 1]), 6);
    assert_eq!(loop_func(array![1, 1]), 7);
    assert_eq!(loop_func(array![1, 1]), 7);
}
