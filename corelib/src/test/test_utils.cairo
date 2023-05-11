#[inline]
fn assert_eq<T, impl TPartialEq: PartialEq<T>>(a: T, b: T, err_code: felt252) {
    if a != b {
        panic_with_felt252(err_code)
    }
}

#[inline]
fn assert_ne<T, impl TPartialEq: PartialEq<T>>(a: T, b: T, err_code: felt252) {
    if a == b {
        panic_with_felt252(err_code)
    }
}
