use core::circuit::u384;

pub fn foo(a: u384, b: u384) -> u384 {
    a
}

#[test]
fn test_into_() {
    let b = 1_u256.into();
    let a = 0_u256.into();
    let _r = foo(a, b);
}
