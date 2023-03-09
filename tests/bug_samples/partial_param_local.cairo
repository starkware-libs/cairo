struct strct {
    x: felt
}

#[test]
fn test_foo() -> felt {
    let x = strct { x: 12 }.x;
    internal::revoke_ap_tracking();
    x
}
