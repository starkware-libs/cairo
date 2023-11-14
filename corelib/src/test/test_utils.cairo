#[inline]
pub fn assert_eq<T, +PartialEq<T>>(a: @T, b: @T, err_code: felt252) {
    assert(a == b, err_code);
}

#[inline]
pub fn assert_ne<T, +PartialEq<T>>(a: @T, b: @T, err_code: felt252) {
    assert(a != b, err_code);
}

#[inline]
pub fn assert_le<T, +PartialOrd<T>>(a: T, b: T, err_code: felt252) {
    assert(a <= b, err_code);
}

#[inline]
pub fn assert_lt<T, +PartialOrd<T>>(a: T, b: T, err_code: felt252) {
    assert(a < b, err_code);
}

#[inline]
pub fn assert_ge<T, +PartialOrd<T>>(a: T, b: T, err_code: felt252) {
    assert(a >= b, err_code);
}

#[inline]
pub fn assert_gt<T, +PartialOrd<T>>(a: T, b: T, err_code: felt252) {
    assert(a > b, err_code);
}
