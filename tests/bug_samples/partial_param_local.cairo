struct strct {
    x: felt252
}

#[test]
fn test_foo() -> felt252 {
    let x = strct { x: 12 }.x;
    internal::revoke_ap_tracking();
    x
}
