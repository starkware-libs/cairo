use clone::Clone;

#[test]
fn test_felt252_operators() {
    assert(1 + 3 == 4, '1 + 3 == 4');
    assert(3 + 6 == 9, '3 + 6 == 9');
    assert(3 - 1 == 2, '3 - 1 == 2');
    assert(1231 - 231 == 1000, '1231-231=1000');
    assert(1 * 3 == 3, '1 * 3 == 3');
    assert(3 * 6 == 18, '3 * 6 == 18');
    assert(-3 == 1 - 4, '-3 == 1 - 4');
}

#[test]
fn test_felt252_clone() {
    let felt252_snap = @2;
    let felt252_clone = felt252_snap.clone();
    assert(felt252_clone == 2, 'felt252_clone == 2');
}
