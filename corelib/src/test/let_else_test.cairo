#[test]
fn test_let_else_panic_return() {
    let v = array![Some(3), None];

    let Some(x) = v[0] else {
        panic!("Expected v[0] to be Some")
    };
    assert_eq!(*x, 3);
    let Some(_) = v[1] else {
        return;
    };
    panic!("Expected v[1] to be None");
}

#[test]
fn test_let_else_continue() {
    let v = array![Some(3), None, Some(5), Some(2), None];

    let mut s: felt252 = 0;

    for x in v {
        let Some(y) = x else {
            s += 100;
            continue;
        };
        s += y;
    }

    assert_eq!(s, 210);
}

#[test]
fn test_let_else_break() {
    let v = array![Some(6), Some(3), None, Some(5)];

    let mut s: felt252 = 0;

    for x in v {
        let Some(y) = x else {
            s += 100;
            break;
        };
        s += y;
    }

    assert_eq!(s, 109);
}
