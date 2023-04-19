#[inline]
fn assert_eq<T, impl PartialEq<T>>(a: @T, b: @T, err_code: felt252) {
    assert(a == b, err_code);
}

#[inline]
fn assert_ne<T, impl PartialEq<T>>(a: @T, b: @T, err_code: felt252) {
    assert(a != b, err_code);
}

#[inline]
fn assert_le<T, impl PartialOrd<T>>(a: T, b: T, err_code: felt252) {
    assert(a <= b, err_code);
}

#[inline]
fn assert_lt<T, impl PartialOrd<T>>(a: T, b: T, err_code: felt252) {
    assert(a < b, err_code);
}

#[inline]
fn assert_ge<T, impl PartialOrd<T>>(a: T, b: T, err_code: felt252) {
    assert(a >= b, err_code);
}

#[inline]
fn assert_gt<T, impl PartialOrd<T>>(a: T, b: T, err_code: felt252) {
    assert(a > b, err_code);
}
