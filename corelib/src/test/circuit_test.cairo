use core::circuit::{RangeCheck96, u96};

#[test]
fn test_u96() {
    let _a: u96 = 0x123;
}


#[test]
fn test_rc96() {
    core::internal::require_implicit::<RangeCheck96>();
}
