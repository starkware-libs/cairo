fn foo(a: u32) -> u32 {
    loop {
        loop {
            if a == 0 {
                break;
            }
            return 6;
        }

        return 5;
    }
}

#[test]
fn test_early_return() {
    assert_eq!(foo(0), 5);
    assert_eq!(foo(1), 6);
}
