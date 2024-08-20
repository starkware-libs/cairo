#[test]
fn test_const_to_const_shadowing() {
    const X: u8 = 1;
    assert_eq!(X, 1);
    {
        const X: u8 = 4;
        assert_eq!(X, 4);
    }
    assert_eq!(X, 1);
}

#[test]
fn test_let_to_const_shadowing() {
    let X = 2;
    assert_eq!(X, 2);
    {
        const X: u8 = 5;
        assert_eq!(X, 5);
    }
    assert_eq!(X, 2);
}

#[test]
fn test_const_to_let_shadowing() {
    const X: u8 = 3;
    assert_eq!(X, 3);
    {
        let X = 6;
        assert_eq!(X, 6);
    }
    assert_eq!(X, 3);
}

const A: u8 = 1;
#[test]
fn test_global_const_to_const_shadowing() {
    assert_eq!(A, 1);
    const A: u8 = 4;
    assert_eq!(A, 4);
}

#[test]
fn test_global_const_to_let_shadowing() {
    assert_eq!(A, 1);
    let A = 4;
    assert_eq!(A, 4);
}