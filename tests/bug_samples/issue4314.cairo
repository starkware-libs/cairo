#[inline]
fn one() {
    two();
}

#[inline(never)]
fn two() {
    three();
}

#[inline(never)]
fn three() {
    one();
}

#[test]
#[should_panic]
#[available_gas(10000)]
fn calls_inline_panic() {
    one();
}
