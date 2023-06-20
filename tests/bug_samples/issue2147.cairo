use hash::LegacyHash;
use integer::u256_from_felt252;

#[test]
fn test_bug_test() {
    let a = 1;
    let b = 2;
    let mut c = 0;
    if u256_from_felt252(a) < u256_from_felt252(b) {
        c = LegacyHash::hash(a, b);
    } else {
        c = LegacyHash::hash(b, a);
    }
}
