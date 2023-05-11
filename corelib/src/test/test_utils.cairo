#[inline]
fn assert_eq<T, impl TPartialEq: PartialEq<T>>(a: T, b: T, err_code: felt252) {
    assert(a == b, err_code);
}

#[inline]
fn assert_ne<T, impl TPartialEq: PartialEq<T>>(a: T, b: T, err_code: felt252) {
    assert(a != b, err_code);
}
