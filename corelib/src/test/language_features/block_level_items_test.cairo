#[test]
fn test_const_shadowing() {
    const A: u8 = 1;
    let B = 2;
    const C: u8 = 3;
    assert_eq!(A, 1);
    assert_eq!(B, 2);
    assert_eq!(C, 3);
    {
        const A: u8 = 4;
        const B: u8 = 5;
        let C = 6;
        assert_eq!(A, 4);
        assert_eq!(B, 5);
        assert_eq!(C, 6);
    }
    assert_eq!(A, 1);
    assert_eq!(B, 2);
    assert_eq!(C, 3);
}
