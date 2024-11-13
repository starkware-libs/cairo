struct S {
    a: u64,
    b: u64,
}

const X: S = S { b: 31, a: 252 };

#[test]
fn test_const() {
    assert_eq!(X.a, 252);
    assert_eq!(X.b, 31);
}
