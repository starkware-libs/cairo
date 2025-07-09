#[test]
fn test_let_else_panic_return() {
    let Some(x) = @Some(3) else {
        panic!("Did not match expected variant.")
    };
    assert_eq!(*x, 3);
    let Some(_) = @None::<felt252> else {
        return;
    };
    panic!("Unexpectedly matches variant.");
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
